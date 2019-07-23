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

start_time <- Sys.time()

fi_var_name <- 'tmin2m'
nc_var_name <- 'mn2t24'
nc_var_longname <- 'Minimum temperature at 2 metres in the last 24 hours'
nc_var_unit <- 'K'

nc_domain_fname <- 'it-medgold'

dayst <- '01'
monst <- '11'
yrst  <- 1988
yren  <- 2018
sdates <- paste0(as.character(seq(yrst,yren)),monst,dayst)


exp <- list(
  list(
    name = 'ecmf',
    path = file.path('/home/sandro/DATA/SEASONAL/ECMF/BC/',
                     '$VAR_NAME$_$EXP_NAME$_$START_DATE$_$SUFFIX$.nc'),
    nc_var_name = nc_var_name,
    suffix = nc_domain_fname,
    var_min = '-300'
  )
)

exp_cst <- CST_Load(var=fi_var_name,
                                  exp=exp,obs=NULL,
                                  sdates=sdates,
                                  storefreq='daily',
                                  output='lonlat',
                                  nprocs=8)

