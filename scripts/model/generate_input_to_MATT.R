generate_input_to_MATT <- function(canopyDF) {
    
    
    ### Input parameters required:
    ### Leaf-scale Vcmax, Jmax, JVratio, Theta, mean and sd (up leaf, low leaf, and CO2 treatment)
    ### Canopy-scale LAI for different canopy layers
    
    ### Variable list
    var <- c("up-Vcmax", "up-Jmax", "up-JVratio", "up-theta", "up-g1",
             "low-Vcmax", "low-Jmax", "low-JVratio", "low-theta", "low-g1",
             "canopy-LAI", "top-LAI", "TM-LAI",
             "PAR", "Tair", "Tleaf", "VPD")
    
    ### create output DF
    outDF <- data.frame(var, NA, NA, NA, NA)
    colnames(outDF) <- c("variable", "aCa", "eCa", 
                         "aCaSE", "eCaSE")
    
    
    ### read in leaf-scale parameters
    leafDF <- read.csv("output/leaf/leaf_scale_parameter_summary_table_for_two_leaf_model.csv")
    
    ### assign values
    ## up, ambient, mean
    outDF$aCa[outDF$variable == "up-Vcmax"] <- leafDF$Vcmax.mean[leafDF$CO2_treatment=="ambient"&leafDF$Position=="up"]
    outDF$aCa[outDF$variable == "up-Jmax"] <- leafDF$Jmax.mean[leafDF$CO2_treatment=="ambient"&leafDF$Position=="up"]
    outDF$aCa[outDF$variable == "up-JVratio"] <- leafDF$JVratio.mean[leafDF$CO2_treatment=="ambient"&leafDF$Position=="up"]
    outDF$aCa[outDF$variable == "up-g1"] <- leafDF$G1.mean[leafDF$CO2_treatment=="ambient"&leafDF$Position=="up"]

    ## low, ambient, mean
    outDF$aCa[outDF$variable == "low-Vcmax"] <- leafDF$Vcmax.mean[leafDF$CO2_treatment=="ambient"&leafDF$Position=="low"]
    outDF$aCa[outDF$variable == "low-Jmax"] <- leafDF$Jmax.mean[leafDF$CO2_treatment=="ambient"&leafDF$Position=="low"]
    outDF$aCa[outDF$variable == "low-JVratio"] <- leafDF$JVratio.mean[leafDF$CO2_treatment=="ambient"&leafDF$Position=="low"]
    outDF$aCa[outDF$variable == "low-g1"] <- leafDF$G1.mean[leafDF$CO2_treatment=="ambient"&leafDF$Position=="low"]
    
    ## up, elevated, mean
    outDF$eCa[outDF$variable == "up-Vcmax"] <- leafDF$Vcmax.mean[leafDF$CO2_treatment=="elevated"&leafDF$Position=="up"]
    outDF$eCa[outDF$variable == "up-Jmax"] <- leafDF$Jmax.mean[leafDF$CO2_treatment=="elevated"&leafDF$Position=="up"]
    outDF$eCa[outDF$variable == "up-JVratio"] <- leafDF$JVratio.mean[leafDF$CO2_treatment=="elevated"&leafDF$Position=="up"]
    outDF$eCa[outDF$variable == "up-g1"] <- leafDF$G1.mean[leafDF$CO2_treatment=="elevated"&leafDF$Position=="up"]
    
    ## low, elevated, mean
    outDF$eCa[outDF$variable == "low-Vcmax"] <- leafDF$Vcmax.mean[leafDF$CO2_treatment=="elevated"&leafDF$Position=="low"]
    outDF$eCa[outDF$variable == "low-Jmax"] <- leafDF$Jmax.mean[leafDF$CO2_treatment=="elevated"&leafDF$Position=="low"]
    outDF$eCa[outDF$variable == "low-JVratio"] <- leafDF$JVratio.mean[leafDF$CO2_treatment=="elevated"&leafDF$Position=="low"]
    outDF$eCa[outDF$variable == "low-g1"] <- leafDF$G1.mean[leafDF$CO2_treatment=="elevated"&leafDF$Position=="low"]
    
    
    ## up, ambient, se
    outDF$aCaSE[outDF$variable == "up-Vcmax"] <- leafDF$Vcmax.se[leafDF$CO2_treatment=="ambient"&leafDF$Position=="up"]
    outDF$aCaSE[outDF$variable == "up-Jmax"] <- leafDF$Jmax.se[leafDF$CO2_treatment=="ambient"&leafDF$Position=="up"]
    outDF$aCaSE[outDF$variable == "up-JVratio"] <- leafDF$JVratio.se[leafDF$CO2_treatment=="ambient"&leafDF$Position=="up"]
    outDF$aCaSE[outDF$variable == "up-g1"] <- leafDF$G1.se[leafDF$CO2_treatment=="ambient"&leafDF$Position=="up"]
    
    ## low, ambient, se
    outDF$aCaSE[outDF$variable == "low-Vcmax"] <- leafDF$Vcmax.se[leafDF$CO2_treatment=="ambient"&leafDF$Position=="low"]
    outDF$aCaSE[outDF$variable == "low-Jmax"] <- leafDF$Jmax.se[leafDF$CO2_treatment=="ambient"&leafDF$Position=="low"]
    outDF$aCaSE[outDF$variable == "low-JVratio"] <- leafDF$JVratio.se[leafDF$CO2_treatment=="ambient"&leafDF$Position=="low"]
    outDF$aCaSE[outDF$variable == "low-g1"] <- leafDF$G1.se[leafDF$CO2_treatment=="ambient"&leafDF$Position=="low"]
    
    ## up, elevated, se
    outDF$eCaSE[outDF$variable == "up-Vcmax"] <- leafDF$Vcmax.se[leafDF$CO2_treatment=="elevated"&leafDF$Position=="up"]
    outDF$eCaSE[outDF$variable == "up-Jmax"] <- leafDF$Jmax.se[leafDF$CO2_treatment=="elevated"&leafDF$Position=="up"]
    outDF$eCaSE[outDF$variable == "up-JVratio"] <- leafDF$JVratio.se[leafDF$CO2_treatment=="elevated"&leafDF$Position=="up"]
    outDF$eCaSE[outDF$variable == "up-g1"] <- leafDF$G1.se[leafDF$CO2_treatment=="elevated"&leafDF$Position=="up"]
    
    ## low, elevated, se
    outDF$eCaSE[outDF$variable == "low-Vcmax"] <- leafDF$Vcmax.se[leafDF$CO2_treatment=="elevated"&leafDF$Position=="low"]
    outDF$eCaSE[outDF$variable == "low-Jmax"] <- leafDF$Jmax.se[leafDF$CO2_treatment=="elevated"&leafDF$Position=="low"]
    outDF$eCaSE[outDF$variable == "low-JVratio"] <- leafDF$JVratio.se[leafDF$CO2_treatment=="elevated"&leafDF$Position=="low"]
    outDF$eCaSE[outDF$variable == "low-g1"] <- leafDF$G1.se[leafDF$CO2_treatment=="elevated"&leafDF$Position=="low"]
    
    ### fixed values
    outDF$aCa[outDF$variable == "up-theta"] <- 0.85
    outDF$eCa[outDF$variable == "up-theta"] <- 0.85
    
    outDF$aCa[outDF$variable == "low-theta"] <- 0.85
    outDF$eCa[outDF$variable == "low-theta"] <- 0.85
    
    ### read in canopy DF
    cDF <- canopyDF
    
    cDF$CO2 <- "aCa"
    cDF$CO2[cDF$Chamber%in%c(4,8)] <- "eCa"
    
    cDF$Leaf_area <- cDF$Leaf_area / ((3.25/2)^2 * pi)
    
    ### convert from leaf area to leaf area index
    cDF2 <- summaryBy(Leaf_area~CO2+Canopy, FUN=c(mean, se), data=cDF,
                      keep.names=T, na.rm=T)
    
    ### assign values
    ## aCa, mean
    outDF$aCa[outDF$variable=="canopy-LAI"] <- cDF2$Leaf_area.mean[cDF2$CO2=="aCa"&cDF2$Canopy=="12345"]
    outDF$aCa[outDF$variable=="top-LAI"] <- cDF2$Leaf_area.mean[cDF2$CO2=="aCa"&cDF2$Canopy=="45"]
    outDF$aCa[outDF$variable=="TM-LAI"] <- cDF2$Leaf_area.mean[cDF2$CO2=="aCa"&cDF2$Canopy=="345"]

    ## eCa, mean
    outDF$eCa[outDF$variable=="canopy-LAI"] <- cDF2$Leaf_area.mean[cDF2$CO2=="eCa"&cDF2$Canopy=="12345"]
    outDF$eCa[outDF$variable=="top-LAI"] <- cDF2$Leaf_area.mean[cDF2$CO2=="eCa"&cDF2$Canopy=="45"]
    outDF$eCa[outDF$variable=="TM-LAI"] <- cDF2$Leaf_area.mean[cDF2$CO2=="eCa"&cDF2$Canopy=="345"]
    
    ## aCa, se
    outDF$aCaSE[outDF$variable=="canopy-LAI"] <- cDF2$Leaf_area.se[cDF2$CO2=="aCa"&cDF2$Canopy=="12345"]
    outDF$aCaSE[outDF$variable=="top-LAI"] <- cDF2$Leaf_area.se[cDF2$CO2=="aCa"&cDF2$Canopy=="45"]
    outDF$aCaSE[outDF$variable=="TM-LAI"] <- cDF2$Leaf_area.se[cDF2$CO2=="aCa"&cDF2$Canopy=="345"]
    
    ## eCa, se
    outDF$eCaSE[outDF$variable=="canopy-LAI"] <- cDF2$Leaf_area.se[cDF2$CO2=="eCa"&cDF2$Canopy=="12345"]
    outDF$eCaSE[outDF$variable=="top-LAI"] <- cDF2$Leaf_area.se[cDF2$CO2=="eCa"&cDF2$Canopy=="45"]
    outDF$eCaSE[outDF$variable=="TM-LAI"] <- cDF2$Leaf_area.se[cDF2$CO2=="eCa"&cDF2$Canopy=="345"]
    
    
    ### abiotic variables
    cDF3 <- summaryBy(WTC_T+WTC_PAR+VPD+Tleaf~CO2, FUN=c(mean, se), data=cDF,
                      keep.names=T, na.rm=T)
    
    ### assign value
    ## aCO2, mean
    outDF$aCa[outDF$variable=="PAR"] <- cDF3$WTC_PAR.mean[cDF3$CO2=="aCa"]
    outDF$aCa[outDF$variable=="Tair"] <- cDF3$WTC_T.mean[cDF3$CO2=="aCa"]
    outDF$aCa[outDF$variable=="Tleaf"] <- cDF3$Tleaf.mean[cDF3$CO2=="aCa"]
    outDF$aCa[outDF$variable=="VPD"] <- cDF3$VPD.mean[cDF3$CO2=="aCa"]
    
    ## eCO2, mean
    outDF$eCa[outDF$variable=="PAR"] <- cDF3$WTC_PAR.mean[cDF3$CO2=="eCa"]
    outDF$eCa[outDF$variable=="Tair"] <- cDF3$WTC_T.mean[cDF3$CO2=="eCa"]
    outDF$eCa[outDF$variable=="Tleaf"] <- cDF3$Tleaf.mean[cDF3$CO2=="eCa"]
    outDF$eCa[outDF$variable=="VPD"] <- cDF3$VPD.mean[cDF3$CO2=="eCa"]
    
    ## aCO2, se
    outDF$aCaSE[outDF$variable=="PAR"] <- cDF3$WTC_PAR.se[cDF3$CO2=="aCa"]
    outDF$aCaSE[outDF$variable=="Tair"] <- cDF3$WTC_T.se[cDF3$CO2=="aCa"]
    outDF$aCaSE[outDF$variable=="Tleaf"] <- cDF3$Tleaf.se[cDF3$CO2=="aCa"]
    outDF$aCaSE[outDF$variable=="VPD"] <- cDF3$VPD.se[cDF3$CO2=="aCa"]
    
    ## eCO2, se
    outDF$eCaSE[outDF$variable=="PAR"] <- cDF3$WTC_PAR.se[cDF3$CO2=="eCa"]
    outDF$eCaSE[outDF$variable=="Tair"] <- cDF3$WTC_T.se[cDF3$CO2=="eCa"]
    outDF$eCaSE[outDF$variable=="Tleaf"] <- cDF3$Tleaf.se[cDF3$CO2=="eCa"]
    outDF$eCaSE[outDF$variable=="VPD"] <- cDF3$VPD.se[cDF3$CO2=="eCa"]
    
    write.csv(outDF, "output/MATT/WTC_input_to_MATT.csv", row.names=F)
    
}