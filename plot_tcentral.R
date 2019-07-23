################################################################
#Pulisce
rm(list=ls())

#Carica le librerie
library(RNetCDF)
library(ggplot2)
library(RColorBrewer)
#source('multiplot.R')

varname <- 'tmin2m'
cfact <- 1.
cunit <- 'K'

dpath <- '/home/sandro/DATA/PROJECTS/MEDGOLD/GRANODURO/'
gpath <- '/home/sandro/R/PLOT/MEDGOLD/'
basename <- paste0(varname,'_201710_075_')

locs <- c('JESI','RAVENNA','FOGGIA')

ens_all <- data.frame()

for (loc in locs) {

unzip(paste0(dpath,basename,loc,'.zip'))

r_dat<-as.data.frame(read.csv(paste0('./',basename,loc,'_00.csv'),header=FALSE))
names(r_dat) <- c("Date","00")
file.remove(paste0('./',basename,loc,'_00.csv'))
            
for (ifile in 1:50) {
  cmem <- sprintf("%02d",ifile)
  filename <- paste0('./',basename,loc,'_',cmem,'.csv')
  dd<-as.data.frame(read.csv(filename,header=FALSE))
  names(dd) <- c("Date",cmem)
  r_dat <- cbind.data.frame(r_dat,dd[cmem])
  file.remove(filename)
}


ens_all <- rbind(ens_all,cbind.data.frame(loc,r_dat))

}

# ensmean <- apply(r_dat[,2:52],1,mean)
# ensmin  <- apply(r_dat[,2:52],1,min)
# ensmax  <- apply(r_dat[,2:52],1,max)
# enssd  <- apply(r_dat[,2:52],1,sd)
# day    <- seq(1:length(ensmean))
# 
# ens <- cbind.data.frame(day,ensmean,ensmin,ensmax,enssd)

ens <- melt(ens_all,id=c("Date","loc"))

p <- ggplot(ens,aes(factor(Date),value*cfact,fill="black")) +
  geom_boxplot(outlier.size=0.1,outlier.color="black",coef=0) +
  guides(fill=FALSE)+
  labs(y=cunit,x="Day")+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "gray",size=0.2),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(), 
        axis.text = element_text( size = 6),
        axis.text.x = element_text( colour = "white" ) ) +
  facet_wrap(~ loc, ncol = 1)
p


file.png <- paste0(gpath,varname,".png")
ggsave(file.png, width=20,height=17, units="cm", dpi=200)

