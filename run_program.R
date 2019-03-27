#### This script is used to process WCT1 CO2 drawdown experiment data
###
### author: Mingkai Jiang
###         m.jiang@westernsydney.edu.au
###

############################# set up #################################
#### read in necessary stuffs
source("prepare.R")


############################# fit A-CI curve - leaf #################################
fits.leaf <- leaf_ACI_processing()


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
myDF$vtime <- as.POSIXct(as.character(myDF$vtime))

### extract time information
myDF$time <- strftime(myDF$datetime, format="%H:%M:%S")
myDF$time <- as.POSIXct(myDF$time, format="%H:%M:%S")
myDF$date <- strftime(myDF$datetime, format="%Y-%m-%d")

#### note: need to correct for different sizes of trees

### check canopy data structure
canopy_data_check_and_plot(myDF)

### time series data correct to control for breaks in the dataseries
myDF <- canopy_data_control(myDF)

### Calculate CO2 flux for each minute and output in the unit of ppm CO2 min-1
myDF2 <- calculate_co2_flux_per_second(myDF)

### plotting co2 flux at per second rate for different treatments
canopy_data_per_second_check_and_plot(myDF2)

### fit canopy ACI curve for each treatment and chamber
fits.canopy <- canopy_ACI_processing(myDF2)


############################# to do list #################################
#### 1. check data quality on canopy:
####    because very small vcmax and jmax values,
####    it seems that some data are jumping around, so need to smooth them,
####    also, need to understand what slope really mean.
####    and get PARi data
#### 2. check leaf data:
####    some Jmax/Vcmax ratio seems not correct
####    splitt data into different canopy treatment?
####    is there any more data available on HIEv?
#### 3. make plots so that we can compare the canopy and leaf data together
####    create plots for each chamber: one plot on leaf response, one plot on different canopy response.


### look at chambers where CO2 concentration is unstable
test <- subset(myDF, chamber==1)
test2 <- subset(test, canopy=="345")









