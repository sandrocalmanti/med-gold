################################################################
# Version history
# V1.0 read data and plot monthly forecasts, one plot each year
################################################################
#Pulisce
rm(list=ls())

#Carica le librerie
library(ggplot2)
library(RColorBrewer)
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

#Unit (to be confirmed)
cunit <- "mm"


#Plot
ens <- melt(monvar,id=c("years","member"))
p <- ggplot(ens,aes(factor(variable),value,fill="black")) +
  geom_boxplot(outlier.size=0.1,outlier.color="black",coef=0) +
  guides(fill=FALSE)+
  labs(y=cunit,x="Month")+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "gray",size=0.2),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(), 
        axis.text = element_text( size = 6),
        axis.text.x = element_text( colour = "white" ) ) +
  facet_wrap(~ years, ncol = 4)
p
