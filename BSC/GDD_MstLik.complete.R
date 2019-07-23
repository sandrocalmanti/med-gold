#################################################################################
# Date             : 10 Jan 2019
# Person           : Prakash Kumar Jha
# Aim of script    : To calculate the indicator growing degree days (Index) (Fontes unpublished) in model 
# and obs. 
                      # Bias correct Index / use bias corrected input to compute Index  
# Input            : raw data experiment and reanalysis
# Output           : raw and bias corrected Index
#################################################################################
## What is the growing degree days (Index) ?
## "Growing Degree days" is defined as summation of daily differences between daily average temperature and  
## 10 degC (vegetative growth minimum temperature) between April 1st and October 31st (Northern Hemisphere)
## In this script instead of using 10 degC, I am using its corresponding percentile in model and observation to ## avoid bias in model.

#clear workspace
rm(list=ls())

library("s2dverification")
library(SpecsVerification)
library(abind)
library(multiApply)
library(easyVerification)
library(ncdf4)
library(CSTools)

# Variables
ind<-'gdd'
model<-'SEAS5-c3s'
ref<-'JRA-55'
startMonth<-'04'  
yearini<-'1993'
yearend<-'2017'
leadtimemax = 214

## case study (any year can be chosen. Let's select 1993 for this case.)
case<-'1993'
exclude<-as.numeric(case)-as.numeric(yearini)+1
year<-as.numeric(yearend)-as.numeric(yearini)+1
nmemb<-25

## output path
## Please don't copy the following path, specify your path where plots shuld be saved.

path_out<-paste0('/esarchive/scratch/pjha/plots_combd_scripts/',ind,'/',c('land_sea', 'land_only'),'/most_likely_plots/case_study/',case,'/startMonth/',startMonth,'/')

 ## Load input data
## Specify path of your input data. Don't copy the following path.

path_input<-paste0('/esarchive/scratch/pjha/plots_combd_scripts/BSC_demo_scripts_for_ICT_Platform/data/',ind,'/')

tmx_mod<-nc_open(paste0(path_input, 'SEAS5-c3s_tmx_stdt_04_1993_2017.nc'))
tmxfrc.dat<-ncvar_get(tmx_mod, "tmxfrc.dat")

tmin_mod<-nc_open(paste0(path_input, 'SEAS5-c3s_tmin_stdt_04_1993_2017.nc'))
tminfrc.dat<-ncvar_get(tmin_mod, "tminfrc.dat")

tmx_obs<-nc_open(paste0(path_input, 'JRA-55_tmx_1993_2017.nc'))
tmxobs.dat<-ncvar_get(tmx_obs, "tmxobs.dat")

tmin_obs<-nc_open(paste0(path_input, 'JRA-55_tmin_1993_2017.nc')) 
tminobs.dat<-ncvar_get(tmin_obs, "tminobs.dat") 
 
lat<-ncvar_get(tmin_obs, "lat") 
lon<-ncvar_get(tmin_obs, "lon") 

rm(tmx_mod); rm(tmin_mod); rm(tmx_obs); rm(tmin_obs)

####################################################################################
########################### (1) Index for obs data
tavg<-array(numeric(), dim=c(length(lon),length(lat),year,leadtimemax)) ## daily average temperature
tmxobs.dat<-aperm(tmxobs.dat, c(4,3,1,2)) 
tminobs.dat<-aperm(tminobs.dat,c(4,3,1,2)) 

for (i in 1:length(lon)) {
   for (j in 1:length(lat)) {
    for (t in 1:year) {
     for (k in 1:leadtimemax) {
       tavg[i,j,t,k]=(tmxobs.dat[i,j,t,k] + tminobs.dat[i,j,t,k])/2
                     }
     }
   }
 }

Index_obs=array(numeric(), dim=c(length(lon),length(lat),year,leadtimemax))
## corresponding percentile for 10 degC in observation. We need this for forecast
perc_10deg<-array(numeric(), dim=c(length(lon),length(lat),leadtimemax))

for (i in 1:length(lon)) {
   for (j in 1:length(lat)) {
     for (k in 1:leadtimemax) {
    perc_10deg[i,j,k] = ecdf(tavg[i,j,-exclude,k])(10)[1]
              for (t in 1:year) {
              if (tavg[i,j,t,k]>=10) {
       Index_obs[i,j,t,k]=tavg[i,j,t,k] - 10
                     } else {
                     Index_obs[i,j,t,k]=0
                     }                                      
     }
   }
 }
 }

## Season total Index obs
Index_seasTot_obs<-array(numeric(), dim=c(length(lon),length(lat),year))
 for (i in 1:length(lon)) {
   for (j in 1:length(lat)) {
    for (t in 1:year) {
    Index_seasTot_obs[i,j,t]<-sum(Index_obs[i,j,t, ])
     }
    }
  }
 
 
################################## (2) forecasts
tmxfrc.dat<-aperm(tmxfrc.dat, c(5,4,1,2,3))
tminfrc.dat<-aperm(tminfrc.dat, c(5,4,1,2,3))

## calcualte daily average temp of forecasts
tavg_frc <- (tmxfrc.dat + tminfrc.dat) / 2


Index_frc<-array(numeric(), dim=c(length(lon),length(lat),nmemb, year,leadtimemax)) ## daily average temperature

for (i in 1:length(lon)) {
   for (j in 1:length(lat)) {
    for (m in 1:nmemb) {
     for (t in 1:year) {
       for (k in 1:leadtimemax) {
          q <- quantile(tavg_frc[i,j,m,t, ], perc_10deg[i,j,k], na.rm=TRUE)
          if (perc_10deg[i,j,k]==0) {
          q=10 
          } else {
          q=q
          }
       if (tavg_frc[i,j,m,t,k]>=q) {
       Index_frc[i,j,m,t,k]=tavg_frc[i,j,m,t,k] - q
       } else {
       Index_frc[i,j,m,t,k]=0
                     }
     }
   }
 }
}
}


## season total Index forecast
Index_seasTot_frc<-array(numeric(), dim=c(length(lon),length(lat),nmemb,year))

for (i in 1:length(lon)) {
   for (j in 1:length(lat)) {
    for (m in 1:nmemb) {
     for (t in 1:year) {
    Index_seasTot_frc[i,j,m,t]<-sum(Index_frc[i,j,m,t, ])
     }
    }
  }
}

saveRDS(Index_seasTot_frc, paste0(path_out[1], model,'_',ind,'_stdt_',startMonth,'_',yearini,'_',yearend,'.RDS'))

## Ensemble mean of total number of days
Index_frc1<-Mean1Dim(Index_seasTot_frc,3)


## (1.2) Skill score Index
#v1, mod in [lon,lat,year,memb]; v2, obs in [lon,lat,year]; v3, clim in [lon,lat,year,year]
Index_frc3<-aperm(Index_seasTot_frc, c(1,2,4,3))
v1<-Index_frc3[ , ,-exclude, ] #exclude year 2002

#
v2<-Index_seasTot_obs[ , ,-exclude]# year 2002 already excluded before

names(dim(v2))<-c('lon', 'lat', 'yr')
names(dim(v1))<-c('lon', 'lat', 'yr', 'memb')

RPSS_Index <- veriApply('FairRpss',fcst=v1,obs=v2,ensdim=4,tdim=3,prob =c(1/3,2/3))

## to mask out no skill regions (RPSS<=0)
mask1 <- 1 * (RPSS_Index$skillscore <= 0) * 0.99

### Calculate most likely tercile
## rearranging dimensions to make [year,memb,lat,lon]
raw_Index4<-aperm(Index_seasTot_frc,c(4,3,2,1)) ## raw Index

time<-length(raw_Index4[ ,1,1,1])
memb<-length(raw_Index4[1, ,1,1])
latd<-length(raw_Index4[1,1, ,1])
lonmax<-length(raw_Index4[1,1,1, ])

## Calculate threshold position of terciles by excluding 2002
threshold_rwIndex<-array(numeric(), dim=c(memb,latd,lonmax,3))

for (memb in 1:25) {
 for (i in 1:latd){
  for (j in 1:lonmax){
  threshold_rwIndex[memb,i,j, ]<-unname(quantile(raw_Index4[-exclude, memb,i,j], c(1/3, 2/3, 1))) 
 
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

## ensemble mean prob of members
probs_rawIndex1<-Mean1Dim(probs_rawIndex,1)
rm(probs_rawIndex)

## rearrange dim to make [bin,lat,lon]
probs_rawIndex_02<-aperm(probs_rawIndex1, c(3,1,2))

################ Plot Most Likely Tercile ############################################
Index<-paste0('raw_',ind)
mask_comb<- t(mask1) ## combined RPSS of raw and bias corrected

## mask sea 
aa<-nc_open('/esarchive/scratch/pjha/plots_combd_scripts/GST/data/land_sea_mask_1x1_c3s_latinv.nc')
lsm1<-t(ncvar_get(aa, "LSM"))
lsm1[lsm1==1]=NA
lsm1[lsm1==0]=1
lsm_chunk=lsm1[131:134,352:353]

probs<-probs_rawIndex_02

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

toptitle1<-paste0(model," ",'probability of most likely'," ",ind," ",'for'," ",case," ",'with RPSS mask'," ",'stdt'," ",startMonth)
toptitle2<-paste0(model," ",'probability of most likely'," ",ind," ",'for'," ",case," ",'without RPSS mask'," ",'stdt'," ",startMonth)

## Europe
for (m in 1:2) {  ## m = c(land sea and land-only); i=c(raw, bias_corrected)
## with rpss mask
png(paste0(path_out[m],model,'_most_likely_',Index,'_with_rpss_mask_for_',case,'_stdt_',startMonth,'.png'), width =700, height=700)
PlotMostLikelyQuantileMap(Subset(probability,along='type',indices=c(m),drop='selected'),lon=lon, lat=lat, toptitle=toptitle1, bar_titles=c("Below normal (%)","Normal (%)","Above normal (%)"), intylat = 1, intxlon = 1, width=10, height=10, mask=mask_comb[ , ],col_mask='white', colNA='white')
dev.off()

## without rpss mask
png(paste0(path_out[m],model,'_most_likely_',Index,'_without_rpss_mask_for_',case,'_stdt_',startMonth,'.png'), width =700, height=700)
PlotMostLikelyQuantileMap(Subset(probability,along='type',indices=c(m),drop='selected'), lon=lon, lat=lat, toptitle=toptitle2, bar_titles=c("Below normal (%)","Normal (%)","Above normal (%)"), intylat = 1, intxlon = 1, width=10, height=10,mask = NULL,colNA='white')
dev.off()
}


## Seperae tercile maps for each category
probs1=probs*100
mask_probs1<-mask_probs*100

all_probs<-abind(probs1, mask_probs1, along=1)

part<-paste0(ind," ",'for'," ",case," ",'stdt'," ",startMonth)

title1<-c(paste0(model," ",'probability of below normal'," ",part), 
paste0(model," ",'probability of normal'," ",part), 
paste0(model," ",'probability of above normal'," ",part))

all_title<-c(title1, title1)

## output title
main<-paste0('_probability_of_',c('below_normal','normal','above_normal'),'_',Index,'_for_',case,'_stdt_',startMonth,'.png')

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

q(save="no")                                     
