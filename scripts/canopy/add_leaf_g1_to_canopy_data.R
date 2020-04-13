add_leaf_g1_to_canopy_data <- function (leafDF, canopyDF) {
    
    ### summary leaf g1 per chamber
    g1DF <- summaryBy(G1~Chamber, FUN=mean, data=leafDF, keep.names=T)
    
    ### 
    g1DF$Chamber <- as.numeric(gsub("ch", "", g1DF$Chamber))
    
    
    ### add g1 onto canopyDF
    out <- merge(canopyDF, g1DF, by="Chamber", all=T)
    
    return(out)
    
}