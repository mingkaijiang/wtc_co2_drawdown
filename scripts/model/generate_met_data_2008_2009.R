generate_met_data_2008_2009 <- function() {
    #### this script generates hourly met data for two-leaf model
    #### it reads year 2008-04-14 to 2009-03-06 met data from WTC
    #### which is chamber-specific
    
    
    #### need the following variables
    ### tair
    ### par
    ### vpd
    ### wind
    ### pressure
    ### Ca
    ### doy
    ### hod
    ### lai
    ### Vcmax25
    ### Jmax25
    
    ########################  read in WTC met data ###########################
    DatFiles <- list.files(path = "data/met/Chamber_fluxes/", pattern = "\\.csv")
    
    ### create directory
    if(!dir.exists("data/met/Chamber_fluxes_processed")) {
        dir.create("data/met/Chamber_fluxes_processed", showWarnings = FALSE)
    }
    
    for (i in 1:length(DatFiles)) {
        metDF <- read.csv(paste0("data/met/Chamber_fluxes/", DatFiles[i]))
        
        ### convert format
        metDF$DateTime <- as.POSIXct(as.character(metDF$DateTime), format="%Y-%m-%d %H:%M:%S")
        metDF$doy <- metDF$DOY
        
        ### subset only hourly data, ignore half hourly timesteps
        metDF$time1 <- sub(".+? ", "", metDF$DateTime)
        metDF$hod <- as.numeric(sub(":.*", "", metDF$time1))

        
        ### get the easy stuff ready
        metDF$vpd <- metDF$VPDwtc
        metDF$par <- metDF$PAR
        metDF$tair <- metDF$TairRef
        metDF$pressure <- metDF$Patm
        metDF$Ca <- metDF$RefCO2
        metDF$FluxCO2 <- metDF$FluxCO2totSOLO
        metDF$FluxH2O <- metDF$FluxH2OtotSOLO
        
        
        outDF <- metDF[,c("DateTime", "doy", "hod", "tair", "par", "vpd", "pressure", "Ca",
                          "FluxCO2", "FluxH2O")]
        
        outDF$leafArea <- 60
        outDF$wind <- 1.5
        outDF$chamber <- paste0("chamber", i)
        outDF$T_treatment <- "ambient"
        outDF$Water_treatment <- "control"

        write.csv(outDF, paste0("data/met/Chamber_fluxes_processed/",DatFiles[i]), row.names=F)
        
    }

    
}
