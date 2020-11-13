process_two_leaf_model_results <- function() {
    
    ### set source dir
    source.dir <- "/Users/mingkaijiang/Documents/Research/Projects/WCT1_CO2_drawdown/Two_leaf_model/outputs/"
    met.dir <- "/Users/mingkaijiang/Documents/Research/Projects/WCT1_CO2_drawdown/Two_leaf_model/met_data/"
    
    ### read input
    aCO2 <- read.csv(paste0(source.dir, "wtc_two_leaf_aCO2.csv"))
    eCO2 <- read.csv(paste0(source.dir, "wtc_two_leaf_aCO2.csv"))
    metDF <- read.csv(paste0(met.dir, "met_constant_forcing_aCO2.csv"))
    
    ### merge met and simulation output
    aCO2 <- cbind(aCO2, metDF)
    eCO2 <- cbind(eCO2, metDF)
    
    
    ### split the ID column and then select the right data to look at
    ### save them in this folder
    ### currently we have light response curves (at two CO2 levels) and 
    ### CO2 response curves (at high light) for different canopy LAI and 
    ### for trees of different CO2 exposure histories.
    ### to directly compare WTC and model result, we need 
    ### the CO2 response curves to see (A600 - A400) / A400
    
    
    
    
    
}