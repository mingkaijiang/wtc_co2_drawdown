#### This script is used to process WCT1 CO2 drawdown experiment data
###
### author: Mingkai Jiang
###         m.jiang@westernsydney.edu.au
###

### Structure:
### 1. Generate conceptual figures, based on Rogers and Kumarathunge dataset
###
### 2. Fit leaf-scale A-Ci curves, generate clean dataset
###
### 3. Clean and fit canopy-scale A-Ci data, generate clean dataset
###
### 4. Prepare input (met, parameters) to MAAT and two-leaf modelling
###
### 5. Make leaf- and canopy-scale data comparison
###
### 6. Compare data with models
###

################################### 0. set up ##########################################
#### clear wk space
rm(list=ls(all=TRUE))

#### read in necessary stuffs
source("prepare.R")

############################# 1. Conceptual figure #####################################
### 1. To generate conceptual figures
###    these are raw figures and still need to be processed in PPT

### Make conceptual figure based on data from Rogers et al. 2017 NP
reproduce_Rogers_2017()

### make conceptual figure based on data in Kumarathunge et al. 2019. PCE
make_conceptual_overview()


############################# 2. Leaf-scale data processing #################################
#### 2. Leaf-scale data processing:
###     Fit light response curve;
###     
###     
###                                1. generate biochemical parameters for modeling and plotting
###                                2. compare treatment effect (i.e. group by treatment, chamber as random variable)
###                                3. layer effect (i.e. top and bottom layers)
###                                4. save stats and figures
###                                5. output parameters for leaf and canopy comparison

### Fit light response curve, 
### and obtain theta and alpha J, which are input to
### fit ACi function and modeling
### output saved as csv
fit_light_response_curves()

### fit leaf ACi of WTC1 data
### save raw A-CI curves (optional)
### save fitted parameter and predicted A as csv
### results are provided on a per chamber basis
leaf_ACI_processing(plot.option = F)

### create leaf-scale biochemical parameter summary table
### results are summarized per CO2 treatment 
summarize_leaf_scale_biochemical_parameters()


### Make some plots for leaf-scale data only
### not used in the main text
### can delete after the manuscript is fully written. 
### go into function to plot!
#plot_leaf_ACI_curves()

### EucFACE A-Ci curves
### need to enter the function to plot
### serves as contexualization purpose, to understand the WTC data
### does not go into the manuscript
#plot_eucface_A_Ci()



############################# 3. processing canopy data #################################
#### Data note:
#### 1. Column canopy: 12345 - full canopy present
####                    345 - middle + top canopy present
####                    45 - top canopy present
####                    0 - no canopy present
#### 2. Need to quality control the data because 
####    there were time-series breaks in the measurements (more details in the code)
#### 3. Need to correct for tree size, according to Drake's method
####    i.e. normalized to per leaf area (more details in the code)


### Processing canopy data, including:
### Cleanning,
### Adding variables,
### Quality controlling,
### assuming leaf-scale g1 value
### fitting canopy-scale A-Ca
### saving canopy-scale cleaned ACa data as csv
processing_canopy_data()

### check two methods of inferring transpiration flux
###  by goodness-of-fit of the inferred gs and normalized H2O flux
### result print to screen
### which can be entered to the manuscript
check_goodness_of_fit_for_H2O_flux()


### Fit canopy A-Ca
### saving canopy-scale biochemical parameters as csv
### output figure (optional)
fit_canopy_ACa(plot.option = F)



#### plot leaf area and biomass based on final harvest data
### biomass not included yet, as possibly not useful for the main text
plot_chamber_leaf_area()

### plot chamber leaf N and SLA
### need to enter the function to plot
plot_chamber_leaf_N_and_SLA()



############################# 4. generate input to models #################################

### Generate input to MAAT,
### include parameters, LAI, and met forcing
### need to read in leaf-scale and canopy-scale parameters and met data
### currently MAAT has single big-leaf model and multi-layer model
generate_input_to_MAAT()

### Generate input to two-leaf model
### this script generate met data based on 2009 met data collected at WTC
### need to combine with canopy drawdown data from inside the chambers
### this resulted in half hourly met data, which is probably too coarse
#generate_met_data_2009()

### this script generate met data for period 2008.04.14 to 2009.03.06
### which is before the co2 drawdown experiment
### but with hourly gap-filled data for each chamber
#generate_met_data_2008_2009()




############################# 5. Make data-based leaf and canopy comparison ######################
#### merge leaf and canopy raw data
#### well-watered treatment only
mgDF <- merge_leaf_and_canopy_raw_data()


####### Plot A-CA at each chamber, compare the shape of the curves
### need to go into function to plot
### not important figure, ignore for manuscript
#plot_individual_A_Ca_curves(mgDF)

### include both A-Ca curves over 0 - 1200 and 350 to 650 range
### for Ca range of 350 - 650, fitted a linear curve
### results include A sensitivity over the Ca range of 400 to 600
### i.e. delta A / A400

### need to revise and clean these figure scripts!!!!!!!!
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



############################# 6. Data-model intercomparisons ######################
### Plot comparison against Rogers 2017
plot_roger_2017_model_result_comparison()
plot_roger_2017_model_result_comparison_280_400()


############## compare modeled (leaf scaled-up) with canopy (CO2 drawdown) fluxes ##############
compare_simulated_results_at_canopy_level()








########## End ############

### to do:
### 1. generate met data at per minute interval and re-run the models
### 2. compare predicted canopy response at this time interval

