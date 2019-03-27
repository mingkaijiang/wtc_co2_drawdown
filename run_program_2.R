#### This script is used to process WCT1 CO2 drawdown experiment data
###
### author: Mingkai Jiang
###         m.jiang@westernsydney.edu.au
###

############################# set up #################################
#### read in necessary stuffs
source("prepare.R")

############################# check basic data structure - canopy #################################
#### data explained
#### column canopy: 12345 - full canopy
####                345 - middle canopy
####                45 - lower canopy
####                0 - no canopy


# descriptions	
# values calculated using SAS prog 	
# chamber	chamber number
# canopy	canopy layers present during drawdown
# vCo2	CO2 concentration in chamber
# vT	Air temp (by vaisala not shielded)
# vtime2	date and time 
# Tair	Air temp from WTC system
# DPLicorCh	Dew point in chamber updated every 14 mins
# PAR	PAR as measured by WTC system
# slope2	dCO2/dt umol tree-1 s-1
# cmarea	leaf area in chamber m2
# nslope2	normalised flux umol m-2 leaf s-1
# k	leak coefficient
# time	time of day
# leak	leak =(vco2-380)*k
# corrflux	leak corrected flux umol tree-1 s-1
# ncorrflux	normalised corrected flux umol m-2 leaf s-1#

#### read in raw data
myDF <- read.csv("data/drawdownanalysis9Sepb.csv")

### set up dataset
myDF$canopy <- as.character(myDF$canopy)

colnames(myDF) <- c("Chamber", "Canopy","Ci", "Tleaf", "datetime", "Tair",
                           "DPLicorCh", "PARi", "slope2", "cmarea", "nslope2",
                           "k", "time", "leak", "corrflux", "Photo")

#### Fitting ACI curve
myDF.clean <- myDF[complete.cases(myDF$Photo), ]

fits <- fitacis(myDF.clean, group="Chamber", fitmethod="bilinear")

plot(fits[[1]])
plot(fits, how="oneplot")


test <- subset(myDF.clean, Chamber=="1"&Canopy=="12345")
fit <- fitaci(test, fitmethod="bilinear")
plot(fit)
coef(fit)

with(test, plot(Photo~Ci))

with(myDF, plot(ncorrflux~corrflux))



