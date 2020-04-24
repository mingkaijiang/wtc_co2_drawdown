#### individual canopy ACi curve measurement processing
#### Fit A-CI curve for each chamber

canopy_ACI_processing <- function(cDF) {

    ### assign CO2, water treatment
    ## chambers 1, 3, 5, 7, 9, 11 are CO2 ambient
    ## chambers 1, 3, 4, 6, 8, 11 are wet 
    
    for (i in c(1, 3, 5, 7, 9, 11)) {
        cDF[cDF$Chamber == i, "CO2_treatment"] <- "ambient"
    }
    
    for (i in c(2, 4, 6, 8, 10, 12)) {
        cDF[cDF$Chamber == i, "CO2_treatment"] <- "elevated"
    }
    
    for (i in c(1, 3, 4, 6, 8, 11)) {
        cDF[cDF$Chamber == i, "Water_treatment"] <- "wet"
    }
    
    for (i in c(2, 5, 7, 9, 10, 12)) {
        cDF[cDF$Chamber == i, "Water_treatment"] <- "dry"
    }
    
    
    ### make a list of identify
    id.list <- unique(cDF$Identity)
    
    ### prepare an output df
    outDF <- data.frame(id.list, NA, NA, NA, NA, NA, NA,
                        NA, NA, NA, NA, NA, NA,
                        NA, NA, NA, NA, NA, NA,
                        NA, NA, NA, NA, NA, NA,
                        NA, NA, NA, NA, NA, NA)
    colnames(outDF) <- c("Identity", "Chamber", "CO2_treatment", "Water_treatment", "Canopy",
                         "RMSE", "Vcmax", "Vcmax.se", "Jmax", "Jmax.se", "Rd", "Rd.se",
                         "Ci", "ALEAF", "GS", "ELEAF", "Ac", "Aj", "Ap", "Rd2", "VPD",
                         "Tleaf", "Ca", "Cc", "PPFD", "Ci_transition_Ac_Aj",
                         "curve.fitting", "Patm", "GammaStar", "Km", "G1")
    
    ### prepare an output list
    outlist <- list()
    
    ### the for loop
    for (i in 1:length(id.list)) {
        ## subset each data
        test <- subset(cDF, Identity == id.list[i])
        
        ## fit
        fit1 <- fitaci(test, varnames = list(ALEAF = "Norm_corr_CO2_flux",
                                             Tleaf = "Tleaf",
                                             Ci = "Ci",
                                             PPFD = "WTC_PAR",
                                             Rd = "Rd"),
                       fitmethod="bilinear", 
                       Tcorrect=T,
                       fitTPU=F)
        
        fit2 <- fitBB(test, varnames = list(ALEAF = "Norm_corr_CO2_flux", 
                                            GS = "gs", 
                                            VPD = "VPD",
                                            Ca = "WTC_CO2", 
                                            RH = "RH"),
                      gsmodel="BBOpti")
            
        ## assign to list
        outlist[[i]] <- fit1
        
        ## get information on identity
        outDF[outDF$Identity == id.list[i], "CO2_treatment"] <- unique(test$CO2_treatment)
        outDF[outDF$Identity == id.list[i], "Water_treatment"] <- unique(test$Water_treatment)
        outDF[outDF$Identity == id.list[i], "Chamber"] <- unique(test$Chamber)
        outDF[outDF$Identity == id.list[i], "Canopy"] <- unique(test$Canopy)
        outDF[outDF$Identity == id.list[i], "curve.fitting"] <- fit1$fitmethod
        
        ## assign fitted values
        outDF[outDF$Identity == id.list[i], "RMSE"] <- fit1$RMSE
        outDF[outDF$Identity == id.list[i], "Vcmax"] <- fit1$pars[1,1]
        outDF[outDF$Identity == id.list[i], "Vcmax.se"] <- fit1$pars[1,2]
        outDF[outDF$Identity == id.list[i], "Jmax"] <- fit1$pars[2,1]
        outDF[outDF$Identity == id.list[i], "Jmax.se"] <- fit1$pars[2,2]
        outDF[outDF$Identity == id.list[i], "Rd"] <- fit1$pars[3,1]
        outDF[outDF$Identity == id.list[i], "Rd.se"] <- fit1$pars[3,2]
        
        outDF[outDF$Identity == id.list[i], "Ci"] <- fit1$Photosyn()[1]
        outDF[outDF$Identity == id.list[i], "ALEAF"] <- fit1$Photosyn()[2]
        outDF[outDF$Identity == id.list[i], "GS"] <- fit1$Photosyn()[3]
        outDF[outDF$Identity == id.list[i], "ELEAF"] <- fit1$Photosyn()[4]
        outDF[outDF$Identity == id.list[i], "Ac"] <- fit1$Photosyn()[5]
        outDF[outDF$Identity == id.list[i], "Aj"] <- fit1$Photosyn()[6]
        outDF[outDF$Identity == id.list[i], "Ap"] <- fit1$Photosyn()[7]
        outDF[outDF$Identity == id.list[i], "Rd2"] <- fit1$Photosyn()[8]
        outDF[outDF$Identity == id.list[i], "VPD"] <- fit1$Photosyn()[9]
        outDF[outDF$Identity == id.list[i], "Tleaf"] <- fit1$Photosyn()[10]
        outDF[outDF$Identity == id.list[i], "Ca"] <- fit1$Photosyn()[11]
        outDF[outDF$Identity == id.list[i], "Cc"] <- fit1$Photosyn()[12]
        outDF[outDF$Identity == id.list[i], "PPFD"] <- fit1$Photosyn()[13]
        outDF[outDF$Identity == id.list[i], "Patm"] <- fit1$Photosyn()[14]
        
        outDF[outDF$Identity == id.list[i], "Ci_transition_Ac_Aj"] <- fit1$Ci_transition
        outDF[outDF$Identity == id.list[i], "GammaStar"] <- fit1$GammaStar
        outDF[outDF$Identity == id.list[i], "Km"] <- fit1$Km
        
        ## g1
        outDF[outDF$Identity == id.list[i], "G1"] <- coef(fit2)[2]
        
    }
    
    ### add vcmax to jmax ratio
    outDF$JVratio <- outDF$Jmax/outDF$Vcmax
    
    ### save
    write.csv(outDF, "output/canopy/canopy_scale_parameters.csv", row.names=F)
    
    
    return(outDF)
}
