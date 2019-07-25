### a function to combine leaf and canopy data together at each chamber level
leaf_canopy_combined_for_ACA_ACI_curves <- function(lDF, cDF, ch.l, ch.c) {
    
    ### subset 
    ch01.l <- subset(lDF, chamber==ch.l)
    ch01.c <- subset(cDF, Chamber==ch.c)
    
    
    ### get columns
    ch01.l <- ch01.l[,c("Photo", "CO2S", "Height", "Cond", "Ci", "Tair", "Tleaf", "PARi", "VpdL")]
    ch01.c <- ch01.c[,c("Photo", "Ca", "Canopy", "transpiration", "Ci", "Tair", "Tleaf", "PARi", "VPD")]
    
    
    ### merge
    colnames(ch01.l) <- colnames(ch01.c) <- c("Photo", "Ca", "Position",
                                              "Cond", "Ci", "Tair", "Tleaf", "PARi", "VpdL")
    ch01.l$Source <- "leaf"
    ch01.c$Source <- "canopy"
    
    outDF <- rbind(ch01.l, ch01.c)
    
    return(outDF)
}