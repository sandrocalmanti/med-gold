#Clean all
rm(list=ls())

library(s2dverification)
library(SpecsVerification)
library(abind)
library(multiApply)
library(easyVerification)
library(ncdf4)
library(CSTools)
library(zeallot)
library(lubridate)

#Set global parameters. These parameters will not change
start_time <- Sys.time()
dpath <- '/home/sandro/DATA/PROJECTS/MEDGOLD/GRANODURO/BC/DAILY/'

dayst <- '01'
monst <- '11'
yrst  <- 1988
yren  <- 2017
sdates <- paste0(as.character(seq(yrst,yren)),monst,dayst)

exp_name <- 'ecmf'
nc_domain_fname <- 'it-medgold'

grids <- list(
  list(name='RAVENNA',
       lat=44.48,
       lon=12.17),
  list(name='JESI',
       lat=43.53,
       lon=13.27),
  list(name='FOGGIA',
       lat=41.34,
       lon=15.56)
)

for (igrid in seq(1:length(grids))) {

glat <- grids[[igrid]]$lat
glon <- grids[[igrid]]$lat


fi_var_names <- c('tmin2m','tmax2m','totprec')
nc_var_names <- c('mn2t24', 'mx2t24', 'tp')
nc_var_longnames <-
  c('Minimum temperature at 2 metres in the last 24 hours',
    'Maximum temperature at 2 metres in the last 24 hours',
    'Total precipitation'
  )
nc_var_units <- c('K', 'K', 'm')
nvars <- length(nc_var_units)

#Extract variables at the selcted grid first.
#The zip file for each year, with all variables, is created in a separated loop
# after the loop over vars

for (ivar in seq(1:nvars)) {
  
  fi_var_name <- fi_var_names[ivar]
  nc_var_name <- nc_var_names[ivar]
  nc_var_longname <- nc_var_longnames[ivar]
  nc_var_unit <- nc_var_units[ivar]
  
  exp <- list(
    list(
      name = exp_name,
      path = file.path('/home/sandro/DATA/SEASONAL/ECMF/BC/',
                       '$VAR_NAME$_$EXP_NAME$_$START_DATE$_$SUFFIX$.nc'),
      nc_var_name = nc_var_name,
      suffix = nc_domain_fname,
      var_min = '-300'
    )
  )
  
  c(exp) %<-% CST_Load(var=fi_var_name,
                       exp=exp,obs=NULL,
                       sdates=sdates,
                       storefreq='daily',
                       output='lonlat',
                       nprocs=8)
  
  #Check dimensions
  nsdates <- dim(exp$data)['sdate']
  ftime   <- dim(exp$data)['ftime']
  nmem    <- dim(exp$data)['member']
  nlon    <- dim(exp$data)['lon']
  nlat    <- dim(exp$data)['lat']
  
  
  for ( idate in seq(1:nsdates)) {
    
    #Define starting year and month for the file name
    isdate <- 1 + (idate - 1) * ftime
    iedate <- idate * ftime
    sdate  <- exp$Dates[[1]][isdate]
    edate  <- exp$Dates[[1]][iedate]  
    
    csyear <- sprintf('%g',year(sdate))
    csmonth <- sprintf('%g',month(sdate))
    
    zipname <- paste0(dpath,paste(exp_name,paste(glat,glon,sep='-'),csyear,csmonth,paste(fi_var_names,collapse='_'),sep='_'),'.zip')
    
    dirname <- paste0(dpath,csyear,csmonth,fi_var_name,'/')
    unlink(dirname,recursive=TRUE)
    dir.create(dirname)
    
    for ( iens in seq(1:nmem)) {
      
      cens <- sprintf('%g',iens)
      filename <- paste0(dirname,exp_name,'_',csyear,csmonth,fi_var_name,'_',cens,'.csv')
      print(filename)
      
      exp_tosave <- exp$data[1,iens,idate,,which.min(abs(glat-exp$lat)),which.min(abs(glon-exp$lon))]
      dates_tosave <- as.Date(exp$Dates[[1]][isdate:iedate])
      
      data_tosave <- cbind.data.frame(dates_tosave,exp_tosave)
      names(data_tosave) <- c()
      
      write.csv(format(data_tosave,digits=1,scientific=FALSE),filename,append=FALSE,sep=',', row.names=FALSE,col.names=c(),quote=FALSE)
      
    }
    
    
    
  }
  
}


#Create the ZIP file

setwd(dpath)
for ( idate in seq(1:nsdates)) {
  
  #Define starting year and month for the file name
  isdate <- 1 + (idate - 1) * ftime
  iedate <- idate * ftime
  sdate  <- exp$Dates[[1]][isdate]
  edate  <- exp$Dates[[1]][iedate]  
  
  csyear <- sprintf('%g',year(sdate))
  csmonth <- sprintf('%g',month(sdate))
  

  zipname <- paste0(dpath,'ZIP/',paste(exp_name,paste(glat,glon,sep='-'),csyear,csmonth,paste(fi_var_names,collapse='_'),sep='_'),'.zip')
  zip_files <- paste0('./',dir(path='.',pattern=paste0('^',csyear,csmonth)))
  zip(zipname,zip_files,zip='/usr/bin/zip')
}


} # End loop over grids