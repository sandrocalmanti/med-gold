#################################################################################
# Date             : 10 Jan 2019
# Person           : Prakash Kumar Jha
# Aim of script    : To calculate the indicator Warm Spell Duration Index (Index) (Fontes unpublished) in model and obs. 
                      # Bias correct Index / use bias corrected input to compute Index  
# Input            : raw data experiment and reanalysis
# Output           : raw and bias corrected Index
#################################################################################
## What is the Warm Spell Duration Index (Index) ?
# A "warm spell" is defined as a sequence of 6 or more days in which the daily maximum temperature #exceeds the 90th percentile of daily maximum temperature for a 5-day running window surrounding #this day during the baseline period. (https://gmao.gsfc.nasa.gov/research/subseasonal/atlas/#Tindices-html/Index-# ts.html; https://rdrr.io/cran/climdex.pcic/man/climdex.Index.html)

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
ind<-'wsdi'
model<-'SEAS5-c3s'
ref<-'JRA-55'
startMonth<-'06'
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

tmx_mod<-nc_open(paste0(path_input, 'SEAS5-c3s_tmx_stdt_06_1993_2017.nc'))
tmxfrc.dat<-ncvar_get(tmx_mod, "tmxfrc.dat")

tmx_obs<-nc_open(paste0(path_input, 'JRA-55_tmx_1993_2017.nc'))
tmxobs.dat<-ncvar_get(tmx_obs, "tmxobs_annual")

lat<-ncvar_get(tmx_obs, "lat") 
lon<-ncvar_get(tmx_obs, "lon") 

rm(tmx_mod); rm(tmx_obs)
  
## Aggregate daily data to monthly
## forecasts
frcm6<-tmxfrc.dat[ , ,1:30, , ]
frcm7<-tmxfrc.dat[ , ,31:61, , ]
frcm8<-tmxfrc.dat[ , ,62:92, , ]
frcm9<-tmxfrc.dat[ , ,93:122, , ]
frcm10<-tmxfrc.dat[ , ,123:153, , ] 
frcm11<-tmxfrc.dat[ , ,154:183, , ]
frcm12<-tmxfrc.dat[ , ,184:214, , ]

## obs
obsm1<-tmxobs.dat[ ,1:31, , ]
obsm2<-tmxobs.dat[ ,32:59, , ]
obsm3<-tmxobs.dat[ ,60:90, , ]
obsm4<-tmxobs.dat[ ,91:120, , ]
obsm5<-tmxobs.dat[ ,121:151, , ]
obsm6<-tmxobs.dat[ ,152:181, , ]
obsm7<-tmxobs.dat[ ,182:212, , ]
obsm8<-tmxobs.dat[ ,213:243, , ]
obsm9<-tmxobs.dat[ ,244:273, , ]
obsm10<-tmxobs.dat[ ,274:304, , ] 
obsm11<-tmxobs.dat[ ,305:334, , ]
obsm12<-tmxobs.dat[ ,335:365, , ]

####################################################################################
########################### (1) Index for obs data

## calculate 90th percentile for each daily data based on
## climatology of the past/hindcast data
## calculate obs tmx90

month=12

aa<-list()
aa[[1]]<-obsm1; aa[[2]]<-obsm2; aa[[3]]<-obsm3; aa[[4]]<-obsm4; aa[[5]]<-obsm5; aa[[6]]<-obsm6
aa[[7]]<-obsm7; aa[[8]]<-obsm8; aa[[9]]<-obsm9; aa[[10]]<-obsm10; aa[[11]]<-obsm11; aa[[12]]<-obsm12

days<-c(31,28,31,30,31,30,31,31,30,31,30,31)

grt<-array(list(), dim=c(length(lon), length(lat), year, month))
cnt<-array(numeric(),c(length(lon), length(lat), year, month))


obs_tmx90<-list()
for (m in 1:month){
obs_tmx90[[m]]<-array(numeric(), dim=c(length(lon), length(lat),days[m],month))
for (i in 1:length(lon)) {
 for (j in 1:length(lat)) {
  for (k in 1:days[m]) {
      if (k <= (days[m]-5)) { ## for the last 4 values tmx90 is calculated for single day because ## otherwise # the matrix would be out of bound.
       obs_tmx90[[m]][i,j,k,m]<-quantile(aa[[m]][-exclude,k:(k+4),j,i], 0.9)[[1]]
       ## from (5 days  * all years) ## exclude the year 2002
       } else {
       obs_tmx90[[m]][i,j,k,m]<-quantile(aa[[m]][-exclude,k,j,i], 0.9)[[1]]
              }
   }
 }
}


## which days are greater than 90th percentile

for (i in 1:length(lon)) {
  for (j in 1:length(lat)) {
    for (t in 1:year) {
   
grt[[i,j,t,m]]<-rle(as.numeric(unlist(aa[[m]][t, ,j,i]) >= obs_tmx90[[m]][i,j, ,m]))

cnt[i,j,t,m]<-sum(grt[[i,j,t,m]]$lengths[grt[[i,j,t,m]]$lengths >= 6 & grt[[i,j,t,m]]$values==TRUE])

    }
  }
}
}


################################## (2) forecasts
month=7

frc<-list()
frc[[1]]<-frcm6; frc[[2]]<-frcm7; frc[[3]]<-frcm8; frc[[4]]<-frcm9; frc[[5]]<-frcm10; 
frc[[6]]<-frcm11; frc[[7]]<-frcm12

days=c(30,31,31,30,31,30,31)

## total number of days greater than tmx90
grt_frc<-array(list(), dim=c(length(lon), length(lat), year,month, nmemb))
cnt_frc<-array(numeric(),c(length(lon), length(lat), year,month, nmemb))

## Calculate forecast tmx90
frc_tmx90<-list()
for (m in 1:month){
frc_tmx90[[m]]<-array(numeric(), dim=c(nmemb,month,days[m],length(lat),length(lon)))
for (n in 1:nmemb){
 for (i in 1:length(lon)) {
  for (j in 1:length(lat)) {
   for (k in 1:days[m]) {
     if (k <= (days[m]-5)) { 
    frc_tmx90[[m]][n,m,k,j,i]<-quantile(frc[[m]][n,-exclude,k:(k+4),j,i], 0.9)[[1]] ## exclude year 2002
     } else {
   frc_tmx90[[m]][n,m,k,j,i]<-quantile(frc[[m]][n,-exclude,k,j,i], 0.9)[[1]]
   }
   }
  }
 } 
}

## which days are greater than 90th percentile
for (i in 1:length(lon)) {
 for (j in 1:length(lat)) {
  for (t in 1:year) {
   for (n in 1:nmemb) {
grt_frc[[i,j,t,m,n]]<-rle(as.numeric(unlist(frc[[m]][n,t, ,j,i]) >= frc_tmx90[[m]][n,m, ,j,i]))
cnt_frc[i,j,t,m,n]<-sum(grt_frc[[i,j,t,m,n]]$lengths[grt_frc[[i,j,t,m,n]]$lengths >= 6 & grt_frc[[i,j,t,m,n]]$values==TRUE])
   }
  }
 }
}
}

## seasonal total Index
Index_seasTot_frc<-round(apply(cnt_frc,c(1,2,3,5),sum), digits=0)
names(dim(Index_seasTot_frc))<-c('lon','lat','year','member')

saveRDS(Index_seasTot_frc, paste0(path_out[1], model,'_',ind,'_stdt_',startMonth,'_',yearini,'_',yearend,'.RDS'))


## RPSS is computed for forecast seasonl only
Index_obs_seas<-cnt[ , , ,6:12]
seasTot_Index_obs<-round(apply(Index_obs_seas,c(1,2,3),sum), digits=0)

## (1.2) Skill score Index
#v1, mod in [lon,lat,year,memb]; v2, obs in [lon,lat,year]; v3, clim in [lon,lat,year,year]
v1<-Index_seasTot_frc[ , ,-exclude, ] #exclude year 2002

# exclude year 2002 
v2<-seasTot_Index_obs[, ,-exclude]
 
names(dim(v2))<-c('lon', 'lat', 'yr')
names(dim(v1))<-c('lon', 'lat', 'yr', 'memb')

RPSS_Index <- veriApply('FairRpss',fcst=v1,obs=v2,ensdim=4,tdim=3,prob =c(1/3,2/3))

## to mask out no skill regions (RPSS<=0)
mask1 <- 1 * (RPSS_Index$skillscore <= 0) * 0.99

### Calculate most likely tercile
## rearranging dimensions to make [year,memb,lat,lon]
raw_Index4<-aperm(Index_seasTot_frc,c(4,3,2,1)) ## raw Index
rm(Index_seasTot_frc)

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
 for (i in 1:latd) {
  for (j in 1:lonmax) {
  if (is.na(raw_Index4[exclude,memb,i,j]==TRUE)) {
  next
  } else if (raw_Index4[exclude,memb,i,j] <= threshold_rwIndex[memb,i,j,1]) {
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
PlotMostLikelyQuantileMap(Subset(probability,along='type',indices=c(m),drop='selected'),lon=lon, lat=lat, toptitle=toptitle1, bar_titles=c("Below normal (%)","Normal (%)","Above normal (%)"), intylat = 1, intxlon = 1, width=10, height=10, mask=mask_comb[ , ], col_mask='white', colNA='white')
dev.off()

## without rpss mask
png(paste0(path_out[m],model,'_most_likely_',Index,'_without_rpss_mask_for_',case,'_stdt_',startMonth,'.png'), width =700, height=700)
PlotMostLikelyQuantileMap(Subset(probability,along='type',indices=c(m),drop='selected'), lon=lon, lat=lat, toptitle=toptitle2, bar_titles=c("Below normal (%)","Normal (%)","Above normal (%)"), intylat = 1, intxlon = 1, width=10, height=10,mask = NULL, colNA='white')
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

                         
