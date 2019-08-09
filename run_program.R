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
#make_conceptual_figure()


############################# fit A-CI curve  #################################
#### Fit leaf level A-Ci curves to:
###                                1. generate parameters for two-leaf modeling
###                                2. compare treatment effect (i.e. group by treatment, chamber as random variable)
###                                3. layer effect (i.e. top and bottom layers)
###                                4. save stats and figures
###                                5. output parameters for leaf and canopy comparison
leaf_ACI_processing()

############################# processing canopy data #################################
#### data explained
#### column canopy: 12345 - full canopy present
####                345 - middle + top canopy present
####                45 - top canopy present
####                0 - no canopy present
canopyDF <- processing_canopy_data()

############################# plot leaf and canopy A-CA curves ######################
#### compares leaf and canopy level A-CA curves and the shape of the curves
cDF=canopyDF

### the full dataset
#plot_A_Ca_for_leaf_and_canopy_data(cDF=canopyDF)

### this is now the code to use
plot_A_Ca_for_leaf_and_canopy_data_ambient_treatment(cDF=canopyDF)

summary_statistics_A_Ci_curves()


############################# fit A-CI curve  #################################
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
### this resulted in half hourly met data, which is probably too coarse
generate_met_data_2009()

### second method: to linearly interpolate hourly met data onto per minute basis, then run the simulation
generate_met_data_2009_method_2()

#### this script generate met data for period 2008.04.14 to 2009.03.06
#### which is before the co2 drawdown experiment
#### but with hourly gap-filled data for each chamber
#generate_met_data_2008_2009()


############## compare modeled (leaf scaled-up) with canopy (CO2 drawdown) fluxes ##############
compare_chamber_results_at_canopy_level()




########## End ############

### to do:
### 1. generate met data at per minute interval and re-run the models
### 2. compare predicted canopy response at this time interval
### 3. resolve the issue of canopy E: what unit in the predicted E in the model?
###                                   what unit in the data?
###                                   why such a large contrast?

### to decide:
### 1. what parameters to use to drive the model? Is it based on all data (i.e. 2008-2009) for each chamber?
### 2. how to parameterize the two-leaf model? One using up canopy parameters, or use the averaged parameter?
### 3. are the parameters currently used logical? Check, especially Rd. 
### 


### to do: compare WTC leaf A-CI with EucFACE A-CI to check the CO2 responsiveness (WTC only 10%, not right)
### Plot Ci = 280, not Ca = 400 on individual A-CI plots
### a subplot for the region of 400-600 ppm, to see the slope of the curve
### Rd, take the average of all.
### don't use Km. 
### for leaf-scale A-CI, fit all dates, and average for each chamber and position. 

