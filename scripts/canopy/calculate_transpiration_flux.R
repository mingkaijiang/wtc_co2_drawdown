calculate_transpiration_flux <- function(myDF) {
    ### total canopy transpiration should equal to changes in RH and water condensation
    
    ### Cond_water probably in unit of g H2O per chamber per minute
    
    ### normalize based on leaf area
    ### from per chamber per minute to per leaf area
    myDF$Norm_H2O_flux <- myDF$Cond_water / myDF$Leaf_area
    
    ### return
    return(myDF)

}

