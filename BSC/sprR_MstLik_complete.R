rm(list=ls())

library("s2dverification")
library(SpecsVerification)
library(abind)
library(multiApply)
library(easyVerification)
library(ncdf4)
library(CSTools)

# Variables
ind<-'sprR'
model<-'SEAS5-c3s'
ref<-'JRA-55'
yearini<-'1993'
yearend<-'2017'

## case study (any year can be chosen. Let's select 1993 for this case.)
case<-'1993' 
exclude<-as.numeric(case)-as.numeric(yearini)+1

## Let's take lead zero i.e. startMonth 1, but we can take any leadtime.
startMonth<-'04' 

## output path
## Please don't copy the following path, specify your path where plots shuld be saved.

path_out<-paste0('/esarchive/scratch/pjha/plots_combd_scripts/',ind,'/',c('land_sea', 'land_only'),'/most_likely_plots/case_study/',case,'/startMonth/',startMonth,'/')

 ## Load input data
## Specify path of your input data. Don't copy the following path.
 
path_input<-paste0('/esarchive/scratch/pjha/plots_combd_scripts/BSC_demo_scripts_for_ICT_Platform/data/',ind,'/')
 
frc_mod<-nc_open(paste0(path_input, 'SEAS5-c3s_precip_startday_21April_1993_2017.nc'))
frc_raw<-ncvar_get(frc_mod, "frc_raw")

ob<-nc_open(paste0(path_input, 'JRA-55_precip_startday_21April1993_2017.nc'))
obs<-ncvar_get(ob, "frc_obs")

lat<-ncvar_get(ob, "lat")
lon<-ncvar_get(ob, "lon")

rm (frc_mod); rm(ob)
  
###################################################################################################################
######################################## (1) Raw Index ############################################################

## sum rain between April 21 to June 21
Index_frc_raw<-apply(frc_raw, c(1,2,4,5), sum)
Index_obs_raw<-apply(obs, c(1,3,4), sum)

rm(frc_raw); rm(obs)

### convert m/s to mm/day
Index_mod1<-Index_frc_raw*86400*1000
Index_obs1<-Index_obs_raw*86400*1000

rm(Index_frc_raw); rm(Index_obs_raw)

Index_obs2<-InsertDim(Index_obs1[-exclude, , ],1,1)

mo<-Index_mod1[ ,-exclude, , ] #exclude year 2002
v1<-aperm(mo,c(4,3,2,1))
#
obs2<-drop(Index_obs2)  # year 2002 already excluded before
v2<-aperm(obs2,c(3,2,1))

names(dim(v2))<-c('lon', 'lat', 'yr')
names(dim(v1))<-c('lon', 'lat', 'yr', 'memb')

RPSS_Index <- veriApply('FairRpss',fcst=v1,obs=v2,ensdim=4,tdim=3,prob =c(1/3,2/3))

## to mask out no skill regions (RPSS<=0)
mask1 <- 1 * (RPSS_Index$skillscore <= 0) * 0.99

########################################################################################################################
################# Bias Correcting Index
Index_obs2 <- InsertDim(InsertDim(InsertDim(Index_obs1,1,1),1,1),4,1) 
names(dim(Index_obs2))<-c('dataset','member','sdate','ftime','lat','lon')
Index_mod2<-InsertDim(InsertDim(Index_mod1,1,1),4,1)
names(dim(Index_mod2))<-c('dataset','member','sdate','ftime','lat','lon')

Index_bsCrtd<-CSTools:::Calibration(obs=Index_obs2,exp=Index_mod2) 

 ## Save bias corrected Index. If you don't want to save comment this option.
saveRDS(drop(Index_bsCrtd), paste0(path_out[1], model,'_bias_corrected_',ind,'_against_',ref,'_stdt_',startMonth,'_',yearini,'_',yearend,'.RDS'))

### Skill score bias corrected Index
#v1, mod in [lon,lat,year,memb]; v2, obs in [lon,lat,year]; v3, clim in [lon,lat,year,year]
v1<-aperm(drop(Index_bsCrtd),c(4,3,2,1))[ , ,-exclude, ] ## rearranging dim and excluding year 2002
ob2<-aperm(drop(Index_obs2),c(3,2,1)) 
v2<-ob2[ , ,-exclude]

names(dim(v2))<-c('lon', 'lat', 'sdate')

RPSS_Index_bsCrtd <- veriApply('FairRpss',fcst=v1,obs=v2,ensdim=4,tdim=3,prob =c(1/3,2/3))

mask2 <- 1 * (RPSS_Index_bsCrtd$skillscore <= 0) * 0.99

### Calculate most likely tercile
## rearranging dimensions to make [year,memb,lat,lon]
raw_Index4<-aperm(Index_mod1,c(2,1,3,4)) ## raw Index
rm(Index_mod1)

bias_corr_Index4<-aperm(drop(Index_bsCrtd), c(2,1,3,4)) ## bias corrected Index

rm(Index_bsCrtd)

time<-length(raw_Index4[ ,1,1,1])
memb<-length(raw_Index4[1, ,1,1])
latd<-length(raw_Index4[1,1, ,1])
lonmax<-length(raw_Index4[1,1,1, ])

## Calculate threshold position of terciles by excluding 2002
threshold_rwIndex<-array(numeric(), dim=c(memb,latd,lonmax,3))
threshold_bcIndex<-array(numeric(), dim=c(memb,latd,lonmax,3))

for (memb in 1:25) {
 for (i in 1:latd){
  for (j in 1:lonmax){
  threshold_rwIndex[memb,i,j, ]<-unname(quantile(raw_Index4[-exclude, memb,i,j], c(1/3, 2/3, 1))) 
  threshold_bcIndex[memb,i,j, ]<-unname(quantile(bias_corr_Index4[-exclude, memb,i,j], c(1/3, 2/3, 1))) 
  }
}
}


## Calcualte probability
## for raw data
time<-length(raw_Index4[ ,1,1,1])
memb<-length(raw_Index4[1, ,1,1])

probs_rawIndex<-array(0, dim=c(memb,latd,lonmax,3))

for (memb in 1:25) {
 for (i in 1:latd){
  for (j in 1:lonmax){
if (raw_Index4[exclude,memb,i,j] <= threshold_rwIndex[memb,i,j,1]) {
probs_rawIndex[memb,i,j,1]=1
} else if (raw_Index4[exclude,memb,i,j] > threshold_rwIndex[memb,i,j,1] & raw_Index4[exclude,memb,i,j] <= threshold_rwIndex[memb,i,j,2]) {
probs_rawIndex[memb,i,j,2]=1
} else {
probs_rawIndex[memb,i,j,3]=1
}
}
}
}

## for bias corrected data
time<-length(raw_Index4[ ,1,1,1])
memb<-length(raw_Index4[1, ,1,1])

probs_bcrIndex<-array(0, dim=c(memb,latd,lonmax,3))

for (memb in 1:25) {
 for (i in 1:latd){
  for (j in 1:lonmax){
  if (is.na(threshold_bcIndex[memb,i,j,1])==TRUE) {
  probs_bcrIndex[memb,i,j,1]=NA
  } else if (bias_corr_Index4[exclude,memb,i,j] <= threshold_bcIndex[memb,i,j,1]) {
probs_bcrIndex[memb,i,j,1]=1
} else if (bias_corr_Index4[exclude,memb,i,j] > threshold_bcIndex[memb,i,j,1] & bias_corr_Index4[exclude,memb,i,j] <= threshold_bcIndex[memb,i,j,2]) {
probs_bcrIndex[memb,i,j,2]=1
} else {
probs_bcrIndex[memb,i,j,3]=1
}
}
}
}

## ensemble mean prob of members
probs_rawIndex1<-Mean1Dim(probs_rawIndex,1)
rm(probs_rawIndex)

probs_bcrIndex1<-Mean1Dim(probs_bcrIndex,1)
rm(probs_bcrIndex)

## rearrange dim to make [bin,lat,lon]
probs_rawIndex_02<-aperm(probs_rawIndex1, c(3,1,2))
probs_bscorIndex_02<-aperm(probs_bcrIndex1, c(3,1,2))

#################################################################################################
################ Plot Most Likely Tercile ############################################
path_out<-paste0('/esarchive/scratch/pjha/plots_combd_scripts/',ind,'/',c('land_sea', 'land_only'),'/most_likely_plots/case_study/',case,'/startMonth/',startMonth,'/')

probs_all<-abind(probs_rawIndex_02,probs_bscorIndex_02, along=4)

type<-c('raw','bias-corrected')
Index<-c(paste0('raw_',ind),paste0('bias_corrected_',ind))
mask_c<-abind(mask1,mask2, along=3) ## combined RPSS of raw and bias corrected
mask_comb<-aperm(mask_c,c(2,1,3))

## mask sea 
aa<-nc_open('/esarchive/scratch/pjha/plots_combd_scripts/GST/data/land_sea_mask_1x1_c3s_latinv.nc')

lsm1<-t(ncvar_get(aa, "LSM"))
lsm1[lsm1==1]=NA
lsm1[lsm1==0]=1
lsm_chunk=lsm1[127:134,352:360]

for (i in 1:2) { ## raw and bias corrected
  probs<-probs_all[ , , ,i]

### rearrangeing dim a
mask_probs<-array(NA,dim=dim(probs))
for (j in 1:3) {  ## loop for bins
mask_probs[j, , ]<-probs[j, , ]*lsm_chunk
}

probability<-abind(probs, mask_probs, along=4)

## function added due to problem in ECTools to replace 0.4.
function1<-function(x){
    if (!any(is.na(x))){
      if (max(x)==0.4){
        x[which(x==max(x))]=0.400001}
    }
    x
  }
  
probability <- Apply(probability, margins = c(2,3,4), fun = function1)[[1]] ## calculate max for each lat, lon and type.

names(dim(probability))<-c('bin','lat','lon','type')

toptitle1<-paste0(model," ",'probability of most likely'," ",ind," ",'for'," ",case," ",c('raw','bias-corrected')," ",'stdt'," ",startMonth," ",'with RPSS mask')

toptitle2<-paste0(model," ",'probability of most likely'," ",ind," ",'for'," ",case," ",c('raw','bias-corrected')," ",'stdt'," ",startMonth," ",'without RPSS mask')


## Europe
for (m in 1:2) {  ## m = c(land sea and land-only); i=c(raw, bias_corrected)
## with rpss mask
png(paste0(path_out[m],model,'_most_likely_',ind,'_',type[i],'_with_rpss_mask_for_',case,'_stdt_',startMonth,'.png'), width =700, height=700)
PlotMostLikelyQuantileMap(Subset(probability,along='type',indices=c(m),drop='selected'),lon=lon, lat=lat, toptitle=toptitle1[i],bar_titles=c("Below normal (%)","Normal (%)","Above normal (%)"), intylat = 1, intxlon = 1, width=10, height=10, mask=mask_comb[ , ,i], col_mask='white', colNA='white')
dev.off()

## without rpss mask
png(paste0(path_out[m],model,'_most_likely_',ind,'_',type[i],'_without_rpss_mask_for_',case,'_stdt_',startMonth,'.png'), width =700, height=700)
PlotMostLikelyQuantileMap(Subset(probability,along='type',indices=c(m),drop='selected'), lon=lon, lat=lat, toptitle=toptitle2[i], bar_titles=c("Below normal (%)","Normal (%)","Above normal (%)"), intylat = 1, intxlon = 1, width=10, height=10, mask = NULL,  colNA='white')
dev.off()
}


## Seperae tercile maps for each category
probs1=probs*100
mask_probs1<-mask_probs*100

all_probs<-abind(probs1, mask_probs1, along=1)

part<-paste0(ind," ",'for'," ",case," ",'stdt'," ",startMonth)

Index<-c(paste0('raw_',ind),paste0('bias_corrected_',ind))

title1<-paste0(model," ",'probability of below normal'," ",ind," ",type[i]," ",'for'," ",case," ",'stdt'," ",startMonth)

title2<-paste0(model," ",'probability of normal'," ",ind," ",type[i]," ",'for'," ",case," ",'stdt'," ",startMonth)

title3<-paste0(model," ",'probability of above normal'," ",ind," ",type[i]," ",'for'," ",case," ",'stdt'," ",startMonth)

all_title<-c(title1,title2,title3,title1,title2,title3)

## output title

main<-paste0('_probability_of_',c('below_normal','normal','above_normal'),'_',Index[i],'_for_',case,'_stdt_',startMonth,'.png')

out_tit1<-paste0(path_out[1],model,main)

out_tit2<-paste0(path_out[2],model,main)

all_tit<-c(out_tit1, out_tit2)

### color
col=c(c('#6baed6','#4292c6','#2171b5','#08519c'), c('#ffeda0','#fed976','#feb24c','#fd8d3c'),c('#fc4e2a','#e31a1c','#bd0026','#800026'))
col1<-array(col,c(4,3))
all_col<-abind(col1,col1,along=2)

#### Europe
## output title
## EU
out1<-paste0(path_out[1],model,main)
out2<-paste0(path_out[2],model,main)
all_out<-c(out1, out2)


for (n in 1:6) {
PlotEquiMap(all_probs[n, , ], lon=lon, lat=lat, intylat = 1, intxlon = 1, width=10, height=10, 
filled.continents=FALSE,  units = '%', title_scale=0.7,axes_label_scale=0.55, axes_tick_scale=0.5,
             margin_scale=c(1,1,4,2), cols=all_col[ ,n], colNA='white', brks=seq(40, 100 ,by=15),
            bar_extra_margin = c(2,0,0,0), bar_label_scale = 1.5,
            toptitle=all_title[n], 
            file=all_out[n])
}
}

q(save="no")              
