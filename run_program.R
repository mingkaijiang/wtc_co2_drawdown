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

############################# plot leaf and canopy A-CA curves ######################
#### compares leaf and canopy level A-CA curves and the shape of the curves
cDF=canopyDF
plot_A_Ca_for_leaf_and_canopy_data(cDF=canopyDF)


############################# fit A-CI curve  #################################
#### Fit leaf level A-Ci curves to:
###                                1. generate parameters for two-leaf modeling
###                                2. compare treatment effect (i.e. group by treatment, chamber as random variable)
###                                3. layer effect (i.e. top and bottom layers)
###                                4. save stats and figures
###                                5. output parameters for leaf and canopy comparison
leaf_ACI_processing()

#### Fit canopy level A-Ci curves to:
###                                1. generate parameters to compare against leaf scale A-Ci
###                                2. compare treatment effect (reduced sample size)
###                                3. layer effect (whole, top, middle and bottom)
###                                4. save stats and figures
###                                5. output parameters for leaf and canopy comparison
canopy_ACI_processing(cDF=canopyDF)

#### compare canopy scaled down data with leaf data 
compare_ACI_results_with_canopy_and_leaf_data(cDF=canopyDF)


#############################  two-leaf modeling met data generation ###################
### this script generate met data based on 2009 met data collected at WTC
### need to combine with canopy drawdown data from inside the chambers
update VPD calculations !!!

generate_met_data_2009()


#### this script generate met data for period 2008.04.14 to 2009.03.06
#### which is before the co2 drawdown experiment
#### but with hourly gap-filled data for each chamber
#generate_met_data_2008_2009()


############## compare modeled (leaf scaled-up) with canopy (CO2 drawdown) fluxes ##############
compare_chamber_results_at_canopy_level()




########## End ############





