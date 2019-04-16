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
leaf_ACI_processing()

############################# generate met data for two-leaf model ###################
### this script generate met data based on 2009 met data collected at WTC
### need to combine with canopy drawdown data from inside the chambers
generate_met_data_2009()

#### this script generate met data for period 2008.04.14 to 2009.03.06
#### which is before the co2 drawdown experiment
#### but with hourly gap-filled data for each chamber
#generate_met_data_2008_2009()


############################# compare scaled canopy with canopy fluxes ##############
compare_chamber_results_at_canopy_level()

### to do list: 
### 1. continue statistical tests on treatment effect on vcmax, jmax and j-v ratios
### 2. draw a figure on this and make it a supplementary material
### 3. subset recent dates and decided what values to use for modeling
### 4. make a table summarizing the values for modeling
### 5. send email to Martin and check on two-leaf modeling
### end of 03/29

### 6. prepare met data
### 7. run the two-leaf model
### 8. work out canopy data plots at individual chamber and canopy level
### end of 04/16

### 9. contact Craig for canopy data check
###    in particular, what met data to use, which files to use (mergeall or the current one)
### 10. check why we have modelled CO2 curve higher than leaf-scaled curve. In theory should be the opposite.
### 11. check the theoretical plot in Rogers 2017 and replot.

### 12. Write as things move forward 

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






