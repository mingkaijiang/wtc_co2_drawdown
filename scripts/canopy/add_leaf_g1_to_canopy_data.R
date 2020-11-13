add_leaf_g1_to_canopy_data <- function (canopyDF) {
    
    ### read in leaf-scale data
    leafACI <- read.csv("output/leaf/leaf_scale_parameters.csv")
    
    ### summary leaf g1 per chamber
    g1DF <- summaryBy(G1~Chamber, FUN=mean, data=leafACI, keep.names=T)
    
    ### 
    g1DF$Chamber <- as.numeric(gsub("ch", "", g1DF$Chamber))
    
    
    ### add g1 onto canopyDF
    out <- merge(canopyDF, g1DF, by="Chamber", all=T)
    
    return(out)
    
}