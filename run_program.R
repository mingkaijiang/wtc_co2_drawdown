#### This script is used to process WCT1 CO2 drawdown experiment data
###
### author: Mingkai Jiang
###         m.jiang@westernsydney.edu.au
###

############################# set up #################################
#### clear wk space
rm(list=ls(all=TRUE))

#### read in necessary stuffs
source("prepare.R")


############################# fit A-CI curve - leaf #################################
fits.leaf <- leaf_ACI_processing()


############################# processing canopy data #################################
#### data explained
#### column canopy: 12345 - full canopy
####                345 - middle canopy
####                45 - lower canopy
####                0 - no canopy
canopyDF <- processing_canopy_data()

### fit canopy ACI curve for each treatment and chamber
### this is just for fun, there's no Ci so can't do it!
#fits.canopy <- canopy_ACI_processing(canopyDF)


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







