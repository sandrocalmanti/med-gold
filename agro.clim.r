agro.clim = function(meteo, lat, sowing=NA, types=c(1:10))
{
  # meteo - daily meteorological data (obligatory)
  ########## data frame with meteorological data
  ########## 1) in the case indicators should be calculated for fixed periods, the columns of meteo must be as follows: 
  ##########    DAY (class: Date, format: YYYY-MM-DD), 
  ##########    TEMPERATURE_MAX (class numeri, unit: °C), 
  ##########    TEMPERATURE_MIN (class numeric, unit: °C), 
  ##########    TEMPERATURE_AVG (class numeric, unit: °C), 
  ##########    PRECIPITATION (class numeric, unit: mm)
  ########## 2) in the case indicators should be calculated for periods defined by phenological stages, DVS column needs to be added
  ##########    DVS (class: numeric, unitless)
  ##########    DVS ranges from 0 (sowing) to maturity (2)
  
  # lat - latitude of location (obligatory)
  ########## latitude in degrees
  
  # sowing - if calculation is bnased on DVS, sowing dates need to be provided (obligatory in case of DVS presence in meteo data)
  ########## vector of class Date
  
  # type of indicator
  ########## 1 - drought between heading and sowing
  ########## 2 - drought around sowing
  ########## 3 - drought between sowing and heading
  ########## 4 - drought between heading and maturity
  ########## 5 - heavy rain during winter
  ########## 6 - excessive rain during vegetative period
  ########## 7 - number of days with minimum temperature below critical low temperature for the periofd after terminal spikelet
  ########## 8 - heat stress after heading, indicated by heat degree days for those days with maximum daily temperature above 35 °C
  ########## 9 - heat stress around flowering, indicated by heat degree days for those days with maximum daily temperature above 31 °C

  ##########
  # OUTPUT: data frame with the following columns: 
  # type of indicator, year, cumulate of precipitation in relevant period, cumulate of reference evapotranspiration in relevant period, number of cases of missing precipitation, number of cases of missing evapotranspiration, value of indicator 

  
  meteo$YEAR=as.integer(format(meteo$DAY, "%Y"))
  meteo$MONTH=as.integer(format(meteo$DAY, "%m"))
  
  curr.year = as.integer(format(Sys.Date(), "%Y"))
  curr.month = as.integer(format(Sys.Date(), "%m"))
  
  years = unique(meteo$YEAR)
  if(length(which(meteo$YEAR==curr.year && meteo$MONTH==curr.month && curr.month>=a$end.month)) == 0)
    years = years[-length(years)]

  indexes = c()
  
  ####### 
  # type=1
  ### drought between sowing and harvesting
  ### no DVS: SPEI for period between October and June
  ### DVS present: SPEI for period between DVS 0 (sowing) and 2 (maturity)
  # type=2
  ### drought around sowing
  ### no DVS: SPEI for period between October and November
  ### DVS present: SPEI for period between DVS 0 (sowing) and 0.09 (emergence)
  # type=3
  ### drought between sowing and heading
  ### no DVS: SPEI for period between October and March
  ### DVS present: SPEI for period between DVS 0 (sowing) and 0.89 (end of heading)
  # type=4
  ### drought between heading and maturity
  ### no DVS: SPEI for period between April and June
  ### DVS present: SPEI for period between DVS 0.34 (heading) and 2 (maturity)
  for(type in types)
  {
    if(type >=1 && type <=4)
    {
      a = assign.type(type)
      meteo.ind = c()
      if(is.null(meteo$DVS))
      {
         
        for(year in years) {
          if(a$start.month > a$end.month && length(which((meteo$YEAR==year-1&meteo$MONTH>=a$start.month))) > 0)
            which_ = which((meteo$YEAR==year-1&meteo$MONTH>=a$start.month) | (meteo$YEAR==year&meteo$MONTH<=a$end.month))
          else 
            which_ = which(meteo$YEAR==year&meteo$MONTH>=a$start.month&meteo$MONTH<=a$end.month)
          
          if(length(which_)>0)
            meteo.ind = rbind(meteo.ind, data.frame(type=type, year=year, prec=sum(meteo$PRECIPITATION[which_], na.rm=TRUE), 
                                                      etp=sum(evap(ra=raest(meteo$MONTH[which_], lat), tmean=meteo$TEMPERATURE_AVG[which_], trange=meteo$TEMPERATURE_MAX[which_]-meteo$TEMPERATURE_MIN[which_])), 
                                                      na.prec=length(which(is.na(meteo$PRECIPITATION[which_]))), 
                                                      na.temp=length(which(is.na(meteo$TEMPERATURE_AVG[which_])))))
        }
        meteo.ind = cbind(meteo.ind, value=spei.period(meteo.ind))
      }
      
      if(!is.null(meteo$DVS))
      {
        years = unique(meteo$YEAR)
        b = assign.dvs(type)
        if(max(meteo$DVS[meteo$YEAR==max(years)], na.rm=TRUE) < 2)
           meteo = meteo[meteo$YEAR < max(meteo$YEAR),]
        for(i in 1:length(sowing))
        {
          meteo_ = meteo[meteo$DAY>=sowing[i] & meteo$DAY<(sowing[i]+365),]
          if(max(meteo_$DVS,na.rm=TRUE) >= 2)
          {
            which_ = which(meteo_$DVS >= b$start.dvs & meteo_$DVS <=b$end.dvs)
            meteo.ind = rbind(meteo.ind, data.frame(type=type, year = as.integer(format(meteo_$DAY[which_[length(which_)]], "%Y")),
                                                      prec = sum(meteo_$PRECIPITATION[which_], na.rm=TRUE),
                                                      etp=sum(evap(ra=raest(meteo_$MONTH[which_], lat), tmean=meteo_$TEMPERATURE_AVG[which_], trange=meteo_$TEMPERATURE_MAX[which_]-meteo_$TEMPERATURE_MIN[which_])), 
                                                      na.prec=length(which(is.na(meteo_$PRECIPITATION[which_]))),
                                                      na.temp=length(which(is.na(meteo_$TEMPERATURE_AVG[which_])))))
          }
        }
        meteo.ind = cbind(meteo.ind, value=spei.period(meteo.ind))
      }
    }
    
    ##########
    # heavy rain - number of days with rainfall cumulate above 40 mm
    ## no DVS: number of days between December and February
    ## with DVS: number of days for DVS between 0.1 (emergence) and 0.33 (tillering)
    if(type == 5)
    {
      a = assign.type(type)
      meteo.ind = c()
      if(is.null(meteo$DVS))
      {
        for(year in years) {
          if(a$start.month > a$end.month && length(which((meteo$YEAR==year-1&meteo$MONTH>=a$start.month))) > 0)
            which_ = which((meteo$YEAR==year-1&meteo$MONTH>=a$start.month) | (meteo$YEAR==year&meteo$MONTH<=a$end.month))
          else 
            which_ = which(meteo$YEAR==year&meteo$MONTH>=a$start.month&meteo$MONTH<=a$end.month)
          
          if(length(which_)>0)
            meteo.ind = rbind(meteo.ind, data.frame(type=type, year=year, prec=sum(meteo$PRECIPITATION[which_], na.rm=TRUE), 
                                                    etp=sum(evap(ra=raest(meteo$MONTH[which_], lat), tmean=meteo$TEMPERATURE_AVG[which_], trange=meteo$TEMPERATURE_MAX[which_]-meteo$TEMPERATURE_MIN[which_])), 
                                                    na.prec=length(which(is.na(meteo$PRECIPITATION[which_]))), 
                                                    na.temp=length(which(is.na(meteo$TEMPERATURE_AVG[which_]))),
                                                    value = length(which(meteo$PRECIPITATION[which_]>=40))))
        }
      }
      
      if(!is.null(meteo$DVS))
      {
        years = unique(meteo$YEAR)
        b = assign.dvs(type)
        if(max(meteo$DVS[meteo$YEAR==max(years)], na.rm=TRUE) < 2)
          meteo = meteo[meteo$YEAR < max(meteo$YEAR),]
        for(i in 1:length(sowing))
        {
          meteo_ = meteo[meteo$DAY>=sowing[i] & meteo$DAY<(sowing[i]+365),]
          if(max(meteo_$DVS,na.rm=TRUE) >= 2)
          {
            which_ = which(meteo_$DVS >= b$start.dvs & meteo_$DVS <=b$end.dvs)
            meteo.ind = rbind(meteo.ind, data.frame(type=type, year = as.integer(format(meteo_$DAY[which_[length(which_)]], "%Y")),
                                                    prec = sum(meteo_$PRECIPITATION[which_], na.rm=TRUE),
                                                    etp=sum(evap(ra=raest(meteo_$MONTH[which_], lat), tmean=meteo_$TEMPERATURE_AVG[which_], trange=meteo_$TEMPERATURE_MAX[which_]-meteo_$TEMPERATURE_MIN[which_])), 
                                                    na.prec=length(which(is.na(meteo_$PRECIPITATION[which_]))),
                                                    na.temp=length(which(is.na(meteo_$TEMPERATURE_AVG[which_]))),
                                                    value = length(which(meteo_$PRECIPITATION[which_]>=40))))
          }
        }
      }
    }
    
    ####### 
    # excessive rain - SPI
    # no DVS: SPI for period between October and April
    # DVS present: SPI for period between DVS 0 (sowing) and 0.89 (end of heading)
    if(type == 6)
    {
      a = assign.type(type)
      meteo.ind = c()
      if(is.null(meteo$DVS))
      {
        for(year in years) {
          if(a$start.month > a$end.month && length(which((meteo$YEAR==year-1&meteo$MONTH>=a$start.month))) > 0)
            which_ = which((meteo$YEAR==year-1&meteo$MONTH>=a$start.month) | (meteo$YEAR==year&meteo$MONTH<=a$end.month))
          else 
            which_ = which(meteo$YEAR==year&meteo$MONTH>=a$start.month&meteo$MONTH<=a$end.month)
          
          if(length(which_)>0)
            meteo.ind = rbind(meteo.ind, data.frame(type=type, year=year, prec=sum(meteo$PRECIPITATION[which_], na.rm=TRUE), 
                                                    etp=sum(evap(ra=raest(meteo$MONTH[which_], lat), tmean=meteo$TEMPERATURE_AVG[which_], trange=meteo$TEMPERATURE_MAX[which_]-meteo$TEMPERATURE_MIN[which_])), 
                                                    na.prec=length(which(is.na(meteo$PRECIPITATION[which_]))), 
                                                    na.temp=length(which(is.na(meteo$TEMPERATURE_AVG[which_])))))
        }
        meteo.ind = cbind(meteo.ind, value=spi.period(meteo.ind))
      }
      
      if(!is.null(meteo$DVS))
      {
        years = unique(meteo$YEAR)
        b = assign.dvs(type)
        if(max(meteo$DVS[meteo$YEAR==max(years)], na.rm=TRUE) < 2)
          meteo = meteo[meteo$YEAR < max(meteo$YEAR),]
        for(i in 1:length(sowing))
        {
          meteo_ = meteo[meteo$DAY>=sowing[i] & meteo$DAY<(sowing[i]+365),]
          if(max(meteo_$DVS,na.rm=TRUE) >= 2)
          {
            which_ = which(meteo_$DVS >= b$start.dvs & meteo_$DVS <=b$end.dvs)
            meteo.ind = rbind(meteo.ind, data.frame(type=type, year = as.integer(format(meteo_$DAY[which_[length(which_)]], "%Y")),
                                                    prec = sum(meteo_$PRECIPITATION[which_], na.rm=TRUE),
                                                    etp=sum(evap(ra=raest(meteo_$MONTH[which_], lat), tmean=meteo_$TEMPERATURE_AVG[which_], trange=meteo_$TEMPERATURE_MAX[which_]-meteo_$TEMPERATURE_MIN[which_])), 
                                                    na.prec=length(which(is.na(meteo_$PRECIPITATION[which_]))),
                                                    na.temp=length(which(is.na(meteo_$TEMPERATURE_AVG[which_])))))
          }
        }
        meteo.ind = cbind(meteo.ind, value=spi.period(meteo.ind))
      }
    }
    
    ####### 
    # frost risk
    ## number of days with minimum temperature below 2 °C
    ## no DVS: number of days between April and June
    ## DVS present: number of days between DVS 0.34 (heading) and 1.11 (end of flowering)
    if(type == 7)
    {
      a = assign.type(type)
      meteo.ind = c()
      if(is.null(meteo$DVS))
      {
         for(year in years) {
          if(a$start.month > a$end.month && length(which((meteo$YEAR==year-1&meteo$MONTH>=a$start.month))) > 0)
            which_ = which((meteo$YEAR==year-1&meteo$MONTH>=a$start.month) | (meteo$YEAR==year&meteo$MONTH<=a$end.month))
          else 
            which_ = which(meteo$YEAR==year&meteo$MONTH>=a$start.month&meteo$MONTH<=a$end.month)
          
          if(length(which_)>0)
            meteo.ind = rbind(meteo.ind, data.frame(type=type, year=year, prec=sum(meteo$PRECIPITATION[which_], na.rm=TRUE), 
                                                    etp=sum(evap(ra=raest(meteo$MONTH[which_], lat), tmean=meteo$TEMPERATURE_AVG[which_], trange=meteo$TEMPERATURE_MAX[which_]-meteo$TEMPERATURE_MIN[which_])), 
                                                    na.prec=length(which(is.na(meteo$PRECIPITATION[which_]))), 
                                                    na.temp=length(which(is.na(meteo$TEMPERATURE_AVG[which_]))),
                                                    value=length(which(meteo$TEMPERATURE_MIN[which_]<=2))))
        }

      }
      
      if(!is.null(meteo$DVS))
      {
        years = unique(meteo$YEAR)
        b = assign.dvs(type)
        if(max(meteo$DVS[meteo$YEAR==max(years)], na.rm=TRUE) < 2)
          meteo = meteo[meteo$YEAR < max(meteo$YEAR),]
        for(i in 1:length(sowing))
        {
          meteo_ = meteo[meteo$DAY>=sowing[i] & meteo$DAY<(sowing[i]+365),]
          if(max(meteo_$DVS,na.rm=TRUE) >= 2)
          {
            which_ = which(meteo_$DVS >= b$start.dvs & meteo_$DVS <=b$end.dvs)
            meteo.ind = rbind(meteo.ind, data.frame(type=type, year = as.integer(format(meteo_$DAY[which_[length(which_)]], "%Y")),
                                                    prec = sum(meteo_$PRECIPITATION[which_], na.rm=TRUE),
                                                    etp=sum(evap(ra=raest(meteo_$MONTH[which_], lat), tmean=meteo_$TEMPERATURE_AVG[which_], trange=meteo_$TEMPERATURE_MAX[which_]-meteo_$TEMPERATURE_MIN[which_])), 
                                                    na.prec=length(which(is.na(meteo_$PRECIPITATION[which_]))),
                                                    na.temp=length(which(is.na(meteo_$TEMPERATURE_AVG[which_]))),
                                                    value=length(which(meteo_$TEMPERATURE_MIN[which_]<=2))))
          }
        }
      }
    }
    
    ####### 
    # heat stress
    # HDD for temperature above 35°C
    ## no DVS: accumulated temperature surplus above 35 °C for period between April and June
    ## DVS present: accumulated temperature surplus above 35 °C for DVS between 0.34 (heading) and 2 (maturity)
    if(type == 8)
    {
      a = assign.type(type)
      meteo.ind = c()
      if(is.null(meteo$DVS))
      {
         for(year in years) {
          if(a$start.month > a$end.month && length(which((meteo$YEAR==year-1&meteo$MONTH>=a$start.month))) > 0)
            which_ = which((meteo$YEAR==year-1&meteo$MONTH>=a$start.month) | (meteo$YEAR==year&meteo$MONTH<=a$end.month))
          else 
            which_ = which(meteo$YEAR==year&meteo$MONTH>=a$start.month&meteo$MONTH<=a$end.month)
          
          if(length(which_)>0)
            meteo.ind = rbind(meteo.ind, data.frame(type=type, year=year, prec=sum(meteo$PRECIPITATION[which_], na.rm=TRUE), 
                                                    etp=sum(evap(ra=raest(meteo$MONTH[which_], lat), tmean=meteo$TEMPERATURE_AVG[which_], trange=meteo$TEMPERATURE_MAX[which_]-meteo$TEMPERATURE_MIN[which_])), 
                                                    na.prec=length(which(is.na(meteo$PRECIPITATION[which_]))), 
                                                    na.temp=length(which(is.na(meteo$TEMPERATURE_AVG[which_]))),
                                                    value=hdd.period(meteo[which_,], 35)))
        }
        
      }
      
      if(!is.null(meteo$DVS))
      {
        years = unique(meteo$YEAR)
        b = assign.dvs(type)
        if(max(meteo$DVS[meteo$YEAR==max(years)], na.rm=TRUE) < 2)
          meteo = meteo[meteo$YEAR < max(meteo$YEAR),]
        for(i in 1:length(sowing))
        {
          meteo_ = meteo[meteo$DAY>=sowing[i] & meteo$DAY<(sowing[i]+365),]
          if(max(meteo_$DVS,na.rm=TRUE) >= 2)
          {
            which_ = which(meteo_$DVS >= b$start.dvs & meteo_$DVS <=b$end.dvs)
            meteo.ind = rbind(meteo.ind, data.frame(type=type, year = as.integer(format(meteo_$DAY[which_[length(which_)]], "%Y")),
                                                    prec = sum(meteo_$PRECIPITATION[which_], na.rm=TRUE),
                                                    etp=sum(evap(ra=raest(meteo_$MONTH[which_], lat), tmean=meteo_$TEMPERATURE_AVG[which_], trange=meteo_$TEMPERATURE_MAX[which_]-meteo_$TEMPERATURE_MIN[which_])), 
                                                    na.prec=length(which(is.na(meteo_$PRECIPITATION[which_]))),
                                                    na.temp=length(which(is.na(meteo_$TEMPERATURE_AVG[which_]))),
                                                    value=hdd.period(meteo_[which_,], 35)))
          }
        }
      }
    }
    
    ####### 
    # heat stress around flowering
    # HDD for temperature above 31°C
    ## no DVS: accumulated temperature surplus above 31 °C for period between April and June
    ## DVS present: accumulated temperature surplus above 31 °C for DVS between 0.9 (beginning of flowering) and 1.11 (end of flowering)
    if(type == 9)
    {
      a = assign.type(type)
      meteo.ind = c()
      if(is.null(meteo$DVS))
      {
        for(year in years) {
          if(a$start.month > a$end.month && length(which((meteo$YEAR==year-1 & meteo$MONTH>=a$start.month))) > 0)
            which_ = which((meteo$YEAR==year-1&meteo$MONTH>=a$start.month) | (meteo$YEAR==year&meteo$MONTH<=a$end.month))
          else 
            which_ = which(meteo$YEAR==year&meteo$MONTH>=a$start.month&meteo$MONTH<=a$end.month)
          
          if(length(which_)>0)
            meteo.ind = rbind(meteo.ind, data.frame(type=type, year=year, prec=sum(meteo$PRECIPITATION[which_], na.rm=TRUE), 
                                                    etp=sum(evap(ra=raest(meteo$MONTH[which_], lat), tmean=meteo$TEMPERATURE_AVG[which_], trange=meteo$TEMPERATURE_MAX[which_]-meteo$TEMPERATURE_MIN[which_])), 
                                                    na.prec=length(which(is.na(meteo$PRECIPITATION[which_]))), 
                                                    na.temp=length(which(is.na(meteo$TEMPERATURE_AVG[which_]))),
                                                    value=hdd.period(meteo[which_,], 31)))
        }
        
      }
      
      if(!is.null(meteo$DVS))
      {
        years = unique(meteo$YEAR)
        b = assign.dvs(type)
        if(max(meteo$DVS[meteo$YEAR==max(years)], na.rm=TRUE) < 2)
          meteo = meteo[meteo$YEAR < max(meteo$YEAR),]
        for(i in 1:length(sowing))
        {
          meteo_ = meteo[meteo$DAY>=sowing[i] & meteo$DAY<(sowing[i]+365),]
          if(max(meteo_$DVS,na.rm=TRUE) >= 2)
          {
            which_ = which(meteo_$DVS >= b$start.dvs & meteo_$DVS <=b$end.dvs)
            meteo.ind = rbind(meteo.ind, data.frame(type=type, year = as.integer(format(meteo_$DAY[which_[length(which_)]], "%Y")),
                                                    prec = sum(meteo_$PRECIPITATION[which_], na.rm=TRUE),
                                                    etp=sum(evap(ra=raest(meteo_$MONTH[which_], lat), tmean=meteo_$TEMPERATURE_AVG[which_], trange=meteo_$TEMPERATURE_MAX[which_]-meteo_$TEMPERATURE_MIN[which_])), 
                                                    na.prec=length(which(is.na(meteo_$PRECIPITATION[which_]))),
                                                    na.temp=length(which(is.na(meteo_$TEMPERATURE_AVG[which_]))),
                                                    value=hdd.period(meteo_[which_,], 31)))
          }
        }
      }
    }
    
    #########
    # disease risk
    ## no DVS: SPI for the period between April and June
    ## DVS present: SPI for DVS between 0.34 (heading) and 2 (maturity)
    if(type == 10)
    {
      a = assign.type(type)
      meteo.ind = c()
      if(is.null(meteo$DVS))
      {
         for(year in years) {
          if(a$start.month > a$end.month && length(which((meteo$YEAR==year-1&meteo$MONTH>=a$start.month))) > 0)
            which_ = which((meteo$YEAR==year-1&meteo$MONTH>=a$start.month) | (meteo$YEAR==year&meteo$MONTH<=a$end.month))
          else 
            which_ = which(meteo$YEAR==year&meteo$MONTH>=a$start.month&meteo$MONTH<=a$end.month)
          
          if(length(which_)>0)
            meteo.ind = rbind(meteo.ind, data.frame(type=type, year=year, prec=sum(meteo$PRECIPITATION[which_], na.rm=TRUE), 
                                                    etp=sum(evap(ra=raest(meteo$MONTH[which_], lat), tmean=meteo$TEMPERATURE_AVG[which_], trange=meteo$TEMPERATURE_MAX[which_]-meteo$TEMPERATURE_MIN[which_])), 
                                                    na.prec=length(which(is.na(meteo$PRECIPITATION[which_]))), 
                                                    na.temp=length(which(is.na(meteo$TEMPERATURE_AVG[which_])))))
        }
        meteo.ind = cbind(meteo.ind, value=spi.period(meteo.ind))
      }
      
      if(!is.null(meteo$DVS))
      {
        years = unique(meteo$YEAR)
        b = assign.dvs(type)
        if(max(meteo$DVS[meteo$YEAR==max(years)], na.rm=TRUE) < 2)
          meteo = meteo[meteo$YEAR < max(meteo$YEAR),]
        for(i in 1:length(sowing))
        {
          meteo_ = meteo[meteo$DAY>=sowing[i] & meteo$DAY<(sowing[i]+365),]
          if(max(meteo_$DVS,na.rm=TRUE) >= 2)
          {
            which_ = which(meteo_$DVS >= b$start.dvs & meteo_$DVS <=b$end.dvs)
            meteo.ind = rbind(meteo.ind, data.frame(type=type, year = as.integer(format(meteo_$DAY[which_[length(which_)]], "%Y")),
                                                    prec = sum(meteo_$PRECIPITATION[which_], na.rm=TRUE),
                                                    etp=sum(evap(ra=raest(meteo_$MONTH[which_], lat), tmean=meteo_$TEMPERATURE_AVG[which_], trange=meteo_$TEMPERATURE_MAX[which_]-meteo_$TEMPERATURE_MIN[which_])), 
                                                    na.prec=length(which(is.na(meteo_$PRECIPITATION[which_]))),
                                                    na.temp=length(which(is.na(meteo_$TEMPERATURE_AVG[which_])))))
          }
        }
        meteo.ind = cbind(meteo.ind, value=spi.period(meteo.ind))
      }
    }
    indexes = rbind(indexes, meteo.ind) 
  }
  return(indexes)
}
  
assign.type = function(type)
{
  period = matrix(data = c(10,10,10,4,12,10,4,4,4,4,6,11,3,6,2,4,6,6,5,6), ncol=2)
  return(list(start.month=period[type,1],end.month=period[type,2]))
}

assign.dvs = function(type)
{
  period = matrix(data = c(0,0,0,0.34,0.1,0,0.34,0.34,0.9,0.34,2,0.09,0.89,2,0.33,0.89,1.11,2,1.11,2), ncol=2)
  return(list(start.dvs=period[type,1],end.dvs=period[type,2]))
}

hdd.period = function(meteo,thr)
{
  meteo$TEMPERATURE_MAX[meteo$TEMPERATURE_MAX<=thr] = 0
  return(sum(meteo$TEMPERATURE_MAX,na.rm=TRUE))
}

spei.period = function(meteo.spei)
{
 
  meteo.spei$spei = meteo.spei$prec - meteo.spei$etp
  meteo.spei$spei.d = NA
  mycdf.clino = ecdf(c(meteo.spei$spei))
  pze = length(which(meteo.spei$spei==0))/length(meteo.spei$spei)
  
  for(year in meteo.spei$year)
    meteo.spei$spei.d[meteo.spei$year==year]=qnorm(pze+(1-pze)*pnorm(qnorm(mycdf.clino(meteo.spei$spei[meteo.spei$year==year]))))
 
  
  my_inf=which(is.infinite(meteo.spei$spei.d))
  meteo.spei$spei.d[my_inf]=NA
  
  return(meteo.spei$spei.d)
}

spi.period = function(meteo.spei)
{
  
  meteo.spei$spei = meteo.spei$prec
  meteo.spei$spei.d = NA
  mycdf.clino = ecdf(c(meteo.spei$spei))
  pze = length(which(meteo.spei$spei==0))/length(meteo.spei$spei)
  
  for(year in meteo.spei$year)
    meteo.spei$spei.d[meteo.spei$year==year]=qnorm(pze+(1-pze)*pnorm(qnorm(mycdf.clino(meteo.spei$spei[meteo.spei$year==year]))))
  
  
  my_inf=which(is.infinite(meteo.spei$spei.d))
  meteo.spei$spei.d[my_inf]=NA
  
  return(meteo.spei$spei.d)
}

# global radiation
raest=function(c,lat){
  J <- as.integer(30.5 * c - 14.6)
  delta <- 0.409 * sin(0.0172 * J - 1.39)
  dr <- 1 + 0.033 * cos(0.0172 * J)
  latr <- lat/57.2957795
  sset <- -tan(latr) * tan(delta)
  omegas <- sset * 0
  omegas[sset >= {-1} & sset <= 1] <- acos(sset[sset >= { -1 } & sset <= 1])
  omegas[sset < { -1 }] <- max(omegas)
  Ra <- 37.6 * dr * (omegas * sin(latr) * sin(delta) +  cos(latr) * cos(delta) * sin(omegas))
  return(Ra)
}

#evapotranspiration
evap=function(ra,tmean,trange){
  trange[trange<=0] = 0.1
  ev = 0.0013*0.408*ra*(tmean+17)*(trange-0.01239)^0.76
  return(ev)
}