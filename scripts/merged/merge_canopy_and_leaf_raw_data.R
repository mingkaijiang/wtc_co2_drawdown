merge_leaf_and_canopy_raw_data <- function() {
    
    
    
    ################################# plot A-Ca #################################
    #### read in leaf-scale data
    lDF1 <- read.csv("data/ACi_curves/HFE_Aci_2008-2009.csv",stringsAsFactors=FALSE)
    lDF2  <- read.csv("data/ACi_curves/HFE_Aci_lowcanopy_2008-2009.csv",stringsAsFactors=FALSE)
    
    ### combine the datasets
    lDF <- rbind(lDF1, lDF2)
    
    #### subset 2009
    lDF$year <- year(lDF$Date)
    lDF$Date <- as.Date(lDF$Date)
    
    
    ### add type information
    lDF$Type <- "leaf"
    
    ### canopy data read in
    cDF <- read.csv("output/canopy/canopy_scale_processed_ACa_curves.csv")
    
    cDF$Type <- "canopy"
    
    ### select columns
    lDF.sub <- lDF[,c("Identity", "chamber", "Height", "Date",
                      "Photo", "CO2S", "Cond", 
                      "Ci", "Tair", "Tleaf", "PARi", "VpdL", "Type")]
    
    cDF.sub <- cDF[,c("Identity", "Chamber", "Canopy", "date",
                      "Norm_corr_CO2_flux", "WTC_CO2", "Norm_H2O_flux", 
                      "Ci", "WTC_T", "Tleaf", "WTC_PAR", "VPD", 'Type')]
    
    
    colnames(lDF.sub) <- colnames(cDF.sub) <- c("Identity", "Chamber", "Position", "Date",
                                                "Photo", "Ca", "Cond", 
                                                "Ci", "Tair", "Tleaf", "PAR", "VPD", "Type")
    
    ### adjust WTC Tleaf
    #cDF.sub$Tleaf <- cDF.sub$Tair * coef(mod)[2] + coef(mod)[1]
    
    myDF <- rbind(lDF.sub, cDF.sub)
    
    myDF$Identity <- as.numeric(as.character(myDF$Identity))
    
    myDF$Chamber <- gsub("ch", "", myDF$Chamber)
    myDF$Chamber <- as.numeric(as.character(myDF$Chamber))
    
    ### assign CO2, water treatment
    ## chambers 1, 3, 5, 7, 9, 11 are CO2 ambient
    ## chambers 1, 3, 4, 6, 8, 11 are wet 
    ## do not include drought treatment chambers
    myDF <- myDF[myDF$Chamber%in%c(1,3,11,4,8),]
    myDF$CO2_treatment <- "aCO2"
    myDF$CO2_treatment[myDF$Chamber%in%c(4,8)] <- "eCO2"
    
    ### return
    return(myDF)
}