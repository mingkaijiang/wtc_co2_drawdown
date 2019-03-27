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

#### read in raw data
myDF <- read.table("data/mergeall.txt", sep=",", header=T)

### set up dataset
myDF$canopy <- as.character(myDF$canopy)
myDF$datetime <- as.POSIXct(as.character(myDF$datetime))

### extract time information
myDF$time <- strftime(myDF$datetime, format="%H:%M:%S")
myDF$time <- as.POSIXct(myDF$time, format="%H:%M:%S")
myDF$date <- strftime(myDF$datetime, format="%Y-%m-%d")

#### note: need to correct for different sizes of trees

### check canopy data structure
canopy_data_check_and_plot(myDF)

### Calculate CO2 flux for each minute and output in the unit of ppm CO2 s-1
myDF2 <- calculate_co2_flux_per_second(myDF)

### plotting co2 flux at per second rate for different treatments
canopy_data_per_second_check_and_plot(myDF2)

### fit canopy ACI curve for each treatment and chamber
fits.canopy <- canopy_ACI_processing(myDF2)

### look at chambers where CO2 concentration is unstable
test <- subset(myDF2, chamber==7)

### plot time series data to confirm status of CO2 concentration over time
with(test[test$canopy=="45",], plot(CO2Local~datetime))
with(test[test$canopy=="45",], plot(CO2Local~time))
with(test[test$canopy=="45",], plot(CO2Local~time_elapsed))

### to do next
### work on individual regression models to see if the slopes differ statistically
### if not, then one equation can be used to represent CO2 drawdown and therefore can use it to contrast with leaf-scale measurements




############################# fit A-CI curve - leaf #################################
fits.leaf <- leaf_ACI_processing()







