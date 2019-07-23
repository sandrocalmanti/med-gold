################################################################
# Version history
# V1.0 read data and plot monthly forecasts, one plot each year
# V1.1 Overlap climatology
################################################################
#Pulisce
rm(list=ls())

#Carica le librerie
library(ggplot2)
library(RColorBrewer)
library(lubridate)
#source('multiplot.R')

dpath <- '/home/sandro/DATA/PROJECTS/MEDGOLD/GRANODURO/'
gpath <- '/home/sandro/R/PLOT/MEDGOLD/'

#Set file parameters
site <- 'foggia'
varname <- 'prlr'
startdate <- 2
startyear <- 1993
nyears <- 24
nmem <- 25



#Set dates
months <- (startdate:(startdate+6))
cmonths <- sprintf("%02d",months)

#Extract standard variable name from  file
filename <- (paste0(dpath,'BIASCORRECTED/',site,'_',varname,'_startdate_',cmonths[1],'.RData'))
load(filename)
monvar <- get(paste0(site,'_',varname))

#From 3d array to data.frame
monvar <- as.data.frame(matrix(monvar,ncol=7,byrow=FALSE))
names(monvar) <- cmonths

#Add member counter to the dataframe
member <- rep(1:25,24)
monvar <- cbind.data.frame(member,monvar)

#Add year to the dataframe
years<-t(matrix(rep(seq(nyears),nmem),ncol=nmem))
dim(years) <- nyears*nmem
years <- years + startyear - 1
monvar <- cbind.data.frame(years,monvar)

#Compute climatology
ens <- melt(monvar,id=c("years","member"))
names(ens) <- c('year','member', 'month','value')
ens_mean <- aggregate(ens$value,list(month=ens$month),mean)
ens_std  <- aggregate(ens$value,list(month=ens$month),sd)
ens_clim <- cbind.data.frame(month=ens_mean$month,emean=ens_mean$x,estd=ens_std$x)


#Unit (to be confirmed)
cunit <- "mm"


#Plot

p <-geom_blank(data=ens, aes(x=month, y=value)) +
   geom_ribbon(data=ens_clim, aes(x=as.numeric(month), ymin=emean-estd, ymax=emean+estd), alpha=0.2)+
   geom_boxplot(data=ens, aes(x=month, y=value, group=month,fill="black",colour="black"), outlier.shape = NA, coef=0) +
  facet_wrap(~ years, ncol = 4)
p


# p <- ggplot(ens,aes(factor(variable),value,fill="black")) +
#   geom_boxplot(outlier.size=0.1,outlier.color="black",coef=0) +
#   guides(fill=FALSE)+
#   labs(y=cunit,x="Month")+
#   theme(panel.background = element_rect(fill = "white"),
#         panel.grid.major = element_line(colour = "gray",size=0.2),
#         panel.grid.minor = element_blank(),
#         axis.ticks = element_blank(), 
#         axis.text = element_text( size = 6),
#         axis.text.x = element_text( colour = "white" ) ) +
#   facet_wrap(~ years, ncol = 4)
# p
