#####################################################################
#
#     MED-GOLD Bias Correction
#
#    v1.0 02/07/2019 Sandro Calmanti - Initial code based on CSTools
#    
#
#####################################################################

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


start_time <- Sys.time()

exp <- list(
  list(
    name = 'ecmf',
    path = file.path('/home/sandro/DATA/SEASONAL/ECMF/',
                     '$VAR_NAME$_$EXP_NAME$_$START_DATE$.nc'),
    nc_var_name = 'mn2t24',
    var_min = '-300'
  )
)


obs <- list(
  list(
    name = 'ERA5',
    path = file.path('/home/sandro/DATA/REANALYSIS/ERA5/ERA5-EU-t2m.$YEAR$_daymin.nc'),
    nc_var_name = 't2m',
    var_min = '-300'
  )
)


dayst <- '01'
monst <- '11'
yrst  <- 1988
yren  <- 2017
sdates <- paste0(as.character(seq(yrst,yren)),monst,dayst)



c(exp_dat, obs_dat) %<-% CST_Load(var='tmin2m',
                 exp=exp,obs=obs,
                 sdates=sdates,
                 storefreq='daily',
                 output='lonlat',
                 latmin=36.0,
                 latmax=47.5,
                 lonmin=7,
                 lonmax=19,
                 grid='r1440x720',
                 nprocs=8)

attr(exp_dat, 'class') <- 's2dv_cube'
attr(obs_dat, 'class') <- 's2dv_cube'

a <- CST_Calibration(exp = exp_dat, obs = obs_dat)

end_time <- Sys.time()

elt <- end_time - start_time
elt

PlotEquiMap(obs_dat$data[1, 1, 1, 1, , ], obs_dat$lon, obs_dat$lat,
            toptitle = 'ERA 5',
            title_scale = 0.5,
            filled.continents = FALSE)

PlotEquiMap(exp_dat$data[1, 1, 1, 1, , ], exp_dat$lon, exp_dat$lat,
            toptitle = 'System 5',
            title_scale = 0.5,
            filled.continents = FALSE)

PlotEquiMap(a$data[1, 1, 1, 1, , ], a$lon, a$lat,
            toptitle = 'System 5 - Bias Corrected',
            title_scale = 0.5,
            filled.continents = FALSE)


# # Creation of sample s2dverification objects. These are not complete
# # s2dverification objects though. The Load function returns complete objects.
# mod1 <-  0.5 + 2*rnorm(1 * 51 * 25 * 100 * 1 * 1)
# dim(mod1) <- c(dataset = 1, member = 51, sdate = 25, ftime = 100, lat = 1, lon = 1)
# obs1 <- rnorm(1 * 1 * 25 * 100 * 1 * 1)
# dim(obs1) <- c(dataset = 1, member = 1, sdate = 25, ftime = 100, lat = 1, lon = 1)
# lon <- seq(0, 30, 5)
# lat <- seq(0, 25, 5)
# exp <- list(data = mod1, lat = lat, lon = lon)
# obs <- list(data = obs1, lat = lat, lon = lon)
# attr(exp, 'class') <- 's2dv_cube'
# attr(obs, 'class') <- 's2dv_cube'
# a <- CST_BiasCorrection(exp = exp, obs = obs)
# str(a)
# 
# ap <- exp$data[1,,25,,1,1]
# sa <- stack(as.data.frame(ap))
# sa$x <- rep(seq_len(nrow(ap)), ncol(ap))
# 
# qplot(x, values, data = sa, group = ind, colour = 'black', geom = "point")
# 
