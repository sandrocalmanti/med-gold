#
library(tidyverse)

smonth <- 11
ovar <- 'Rainfall'

filepath <- '/home/sandro/DATA/PROJECTS/MEDGOLD/HORTA/Ravenna_giornalieri.csv'

obs <- read.csv(filepath,sep=";",dec=",",skip=1,header=FALSE)
names(obs) <- c('Date','Leaf Wetness','Rainfall','Tmax','Tave','Tmin','Hmax','Have','Hmin')
obs$Date <- as.Date(obs$Date,"%d/%m/%Y")

obs  <- obs %>% filter(month(Date) %in% c(11,12,1,2,3,4))
obs  <- obs %>% filter(!year(Date) %in% c(2019))

#Verify obs
years <- levels(year(obs$Date))

vobs <- obs[ovar]

dim(obs1) <- c(dataset = 1, member = 1, sdate = 25, ftime = 100, lat = 1, lon = 1)