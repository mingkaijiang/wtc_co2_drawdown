convert_modeled_data_format <- function(inDF) {
    
    inDF.s1 <- inDF[,c("canopy", "Ca", "APAR_can", "An_obs", "LAI_can", "T_can")]
    inDF.s2 <- inDF[,c("canopy", "Ca", "APAR_can", "An_can", "LAI_can", "T_can")]
    colnames(inDF.s2) <- colnames(inDF.s1) <- c("canopy", "Ca", "APAR_can", "An", "LAI_can", "T_can")
    inDF.s2$canopy <- "modeled"
    inDF.s1$canopy <- gsub("12345", "whole", inDF.s1$canopy)
    inDF.s1$canopy <- gsub("345", "m+b", inDF.s1$canopy)
    inDF.s1$canopy <- gsub("45", "bottom", inDF.s1$canopy)
    outDF <- rbind(inDF.s1, inDF.s2)
    outDF$canopy <- as.factor(outDF$canopy)
    
    return(outDF)
}