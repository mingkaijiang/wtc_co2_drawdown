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

############################# conceptual figure #####################################
#### Make conceptual figure based on data from Rogers et al. 2017 NP
make_conceptual_figure()

############################# processing canopy data #################################
#### data explained
#### column canopy: 12345 - full canopy present
####                345 - middle canopy, top canopy missing, middle + bottom canopy present
####                45 - lower canopy, top 2/3 canopy missing, bottom canopy present
####                0 - no canopy present
canopyDF <- processing_canopy_data()


### note:
## 3. next to calculate stomatal conductance, ci, and Jmax, Vcmax based on canopy data
## 4. if the value don't make sense, change bin size and redo things.
## 5. also need to update met data with the correct VPD data

with(canopyDF[canopyDF$Canopy=="12345" & canopyDF$Chamber == "4", ], plot(rh ~ datetime))

############################# plot leaf and canopy A-CA curves ######################
#### compares leaf and canopy level A-CA curves and the shape of the curves
cDF=canopyDF
plot_A_Ca_for_leaf_and_canopy_data(cDF=canopyDF)


############################# fit A-CI curve - leaf #################################
#### Fit leaf level A-Ci curves to generate parameters for two-leaf modeling
leaf_ACI_processing()

############################# generate met data for two-leaf model ###################
### this script generate met data based on 2009 met data collected at WTC
### need to combine with canopy drawdown data from inside the chambers
generate_met_data_2009()

#### this script generate met data for period 2008.04.14 to 2009.03.06
#### which is before the co2 drawdown experiment
#### but with hourly gap-filled data for each chamber
#generate_met_data_2008_2009()


############## compare modeled (leaf scaled-up) with canopy (CO2 drawdown) fluxes ##############
compare_chamber_results_at_canopy_level()

############## compare canopy scaled down data with leaf data to compute A-Ci curves and stats ##############
cDF <- canopyDF
compare_chamber_results_at_leaf_level(cDF=canopyDF)



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
### 12. read and write as things move forward






### fit canopy ACI curve for each treatment and chamber
### this is just for fun, there's no Ci so can't do it!
#fits.canopy <- canopy_ACI_processing(canopyDF)






