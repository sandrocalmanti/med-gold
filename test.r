### EXAMPLE OF USE - FOGGIA
# 1- calculate indicators based on meteorological data only
source("agro.clim.r")
# type of indicator in agro.clim function
########## 1 - drought between heading and sowing
########## 2 - drought around sowing
########## 3 - drought between sowing and heading
########## 4 - drought between heading and maturity
########## 5 - heavy rain during winter
########## 6 - excessive rain during vegetative period
########## 7 - number of days with minimum temperature below critical low temperature for the periofd after terminal spikelet
########## 8 - heat stress after heading, indicated by heat degree days for those days with maximum daily temperature above 35 °C
########## 9 - heat stress around flowering, indicated by heat degree days for those days with maximum daily temperature above 31 °C
########## 10 - disease risk


#read meteo data
meteo = read.csv("MeteoFoggia.csv", sep=",", header = TRUE)
# convert dates in meteo to class Date
meteo$DAY = as.Date(meteo$DAY, format="%d/%m/%Y")

# calculate all indicators fo fixed periods
foggia = agro.clim(meteo, lat = 41.5)


# 2 - calculate indicators based on development stages DVS
source("phenology.r")
parameters = read.csv("ParametersFoggia.csv", header=TRUE, sep=",")
sowing = read.csv("SowingFoggia.csv", header=TRUE, sep=",")
sowing$DAY = as.Date(sowing$DAY, format="%d/%m/%Y")

meteo = phenology(meteo, parameters, sowing$DAY, 41.5)
foggia.dvs = agro.clim(meteo,41.5,sowing$DAY)


# PORTO SAN ELPIDIO
#read meteo data - Porto San Elpidio
meteo = read.csv("MeteoElpidio.csv", sep=",", header = TRUE)
# convert dates in meteo to class Date
meteo$DAY = as.Date(meteo$DAY, format="%d/%m/%Y")

# calculate all indicators fo fixed periods
elpidio = agro.clim(meteo, lat = 43.2)

sowing = read.csv("SowingElpidio.csv", header=TRUE, sep=",")
sowing$DAY = as.Date(sowing$DAY, format="%d/%m/%Y")

meteo = phenology(meteo, parameters, sowing$DAY, 43.2)
elpidio.dvs = agro.clim(meteo,41.5,sowing$DAY)


