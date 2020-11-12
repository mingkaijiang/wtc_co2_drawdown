#### This script is used to process WCT1 CO2 drawdown experiment data
###
### author: Mingkai Jiang
###         m.jiang@westernsydney.edu.au
###

### Structure:
### 1. Clean leaf and canopy-scale A-Ca data, generate cleaned data
### 2. Prepare input (met, parameters) to MAAT and two-leaf modelling
### 3. Generate figures and summary tables, including output from MAAT and two-leaf model



################################### set up ##########################################
#### clear wk space
rm(list=ls(all=TRUE))

#### read in necessary stuffs
source("prepare.R")

############################# conceptual figure #####################################
#### Make conceptual figure based on data from Rogers et al. 2017 NP
#reproduce_Rogers_2017()

# make conceptual figure based on data in Kumarathunge et al. 2019. PCE
make_conceptual_overview()

############################# fit leaf A-CI curve  #################################
#### Fit leaf level A-Ci curves to:
###                                1. generate parameters for two-leaf modeling
###                                2. compare treatment effect (i.e. group by treatment, chamber as random variable)
###                                3. layer effect (i.e. top and bottom layers)
###                                4. save stats and figures
###                                5. output parameters for leaf and canopy comparison

### first need to compute theta and alpha J based on light response curves
fit_light_response_curves()

### fit leaf ACi of WTC1
leafACI <- leaf_ACI_processing(plot.option = T)

#### Make some plots for leaf-scale data only
#### not used in the main text
#### can delete after the manuscript is fully written. 
#### go into function to plot!
#plot_leaf_ACI_curves(plotDF=leafACI)


#### create leaf-scale biochemical parameter summary table
summarize_leaf_scale_biochemical_parameters()


############################# processing canopy data #################################
#### Data note:
#### 1. Column canopy: 12345 - full canopy present
####                    345 - middle + top canopy present
####                    45 - top canopy present
####                    0 - no canopy present
#### 2. Need to quality control the data because 
####    there were time-series breaks in the measurements (more details in the code)
#### 3. Need to correct for tree size, according to Drake's method
####    i.e. normalized to per leaf area (more details in the code)
canopyDF <- processing_canopy_data(leafACI=leafACI, plot.option = T)

#### check goodness-of-fit for inferred gs with normalized H2O flux
check_goodness_of_fit_for_H2O_flux(canopyDF)

#### plot leaf area and biomass based on final harvest data
plot_chamber_leaf_area(canopyDF)

### plot chamber leaf N and SLA
plot_chamber_leaf_N_and_SLA()

############################# fit canopy A-CI curve  #################################
#### Fit canopy level A-Ci curves to:
###                                1. generate parameters to compare against leaf scale A-Ci
###                                2. compare treatment effect (reduced sample size)
###                                3. position effect (whole, top, middle and bottom)
###                                4. save stats and figures
###                                5. output parameters for leaf and canopy comparison
#canopyACI <- canopy_ACI_processing(cDF=canopyDF)


############################# leaf & canopy result comparison ######################
#### merge leaf and canopy raw data
#### well-watered treatment only
mgDF <- merge_leaf_and_canopy_raw_data(cDF=canopyDF)


####### Plot A-CA at each chamber, compare the shape of the curves
### need to go into function to plot
#plot_individual_A_Ca_curves(mgDF)

### include both A-Ca curves over 0 - 1200 and 350 to 650 range
### for Ca range of 350 - 650, fitted a linear curve
### results include A sensitivity over the Ca range of 400 to 600
### i.e. delta A / A400
plot_A_Ca_and_perform_linear_fit_over_Ca_of_400_to_600(mgDF)

plot_A_Ca_and_perform_linear_fit_over_Ca_of_280_to_400(mgDF)

### same as above for the sensitivity, 
### but A400 and A600 predicted by the fitaci function, hence non-linear
plot_A_Ca_sensitivity_based_on_fitaci_function_result(mgDF)


### plot biochemical parameters based on fitaci results
plot_biochemical_parameters(mgDF)


#### plot Aj on the x-axis and Ac on the y-axis
### include both canopy and leaf scales
### include both aCO2 and eCO2 treatment
plot_Aj_Ac_comparison(mgDF)

### plot Aj vs. Ac, based on mate simulation result
plot_Aj_Ac_comparison_of_MATE_result(mgDF)

### plot Aj vs. Ac based on the data, mate model and two-leaf model
plot_Aj_Ac_comparison_of_data_and_model(mgDF)



############################# Roger 2017 results ######################
###
plot_roger_2017_model_result_comparison()
plot_roger_2017_model_result_comparison_280_400()

############################# EucFACE results ##########################
### EucFACE A-Ci curves
plot_eucface_A_Ci()



#############################  two-leaf modeling met data generation ###################
### this script generate met data based on 2009 met data collected at WTC
### need to combine with canopy drawdown data from inside the chambers
### this resulted in half hourly met data, which is probably too coarse
#generate_met_data_2009()

### second method: to linearly interpolate hourly met data onto per minute basis, then run the simulation
#generate_met_data_2009_method_2()

#### this script generate met data for period 2008.04.14 to 2009.03.06
#### which is before the co2 drawdown experiment
#### but with hourly gap-filled data for each chamber
#generate_met_data_2008_2009()


############## compare modeled (leaf scaled-up) with canopy (CO2 drawdown) fluxes ##############
compare_simulated_results_at_canopy_level()




############################# Generate input to MATT ###################
generate_input_to_MATT(canopyDF)



########## End ############

### to do:
### 1. generate met data at per minute interval and re-run the models
### 2. compare predicted canopy response at this time interval

