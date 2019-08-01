process_canopy_second_dataset_to_get_H2O_flux <- function() {
    ########################  using mergeall.text file ###########################
    ##### read in raw data - this dataset does not have PAR 
    myDF <- read.csv("data/canopy_drawdown/mergeall.csv")
    
    myDF$date <- sub(" .*", "", myDF$datetime)
    myDF$date <- as.Date(as.character(myDF$date), format="%m/%d/%y")
    myDF$time1 <- sub(".+? ", "", myDF$datetime)
    myDF$time <- paste0(myDF$time1, ":00")
    
    myDF$datetime <- as.POSIXct(paste(myDF$date, myDF$time), format="%Y-%m-%d %H:%M:%S")
    myDF$time <- strftime(myDF$datetime, format="%H:%M:%S")
    myDF$time <- as.POSIXct(myDF$time, format="%H:%M:%S")
    
    myDF <- myDF[,!(colnames(myDF)%in% c("time1"))]
    
    
    ### calculate RH for each time point
    myDF$rh <- 100 - 5 * (myDF$Tair - myDF$DPLicorCh)
    myDF$es <- 0.6106 * exp(17.27 * myDF$Tair / (myDF$Tair + 237.3))
    myDF$ea <- myDF$rh / 100 * myDF$es
    
    ### add volumn information - does not include the effect of cone top
    ## note that chamber 11 = 49800 L
    ## all else = 52800 L
    myDF$volume <- 52800 / 1000
    myDF$volume[myDF$chamber == "11"] <- 49800 / 1000
    
    ### calculate water fluxes - converting RH to water unit g/m3
    myDF$rh_water <- 2.16679 * (myDF$ea * 1000) / (myDF$Tair + 273.2) 
    
    ### convert to get total chamber water content in g/chamber
    myDF$rh_total <- myDF$rh_water * myDF$volume
    
    #test2 <- subset(myDF, chamber == "4" & canopy == "345")
    #with(test2, plot(rh~datetime))
    
    ### convert H2O flux from unit of g per chamber per minute to umol H2O per leaf area per second
    ## average across 5 mins to compute a running mean difference at 5 min time interval
    ## the final value is quite sensitive to the size of bin, i.e. 5 min vs. 10 mins 
    myDF2 <- calculate_h2o_flux_per_second2(myDF)
    myDF2$H2O_flux_total <- rowSums(data.frame(myDF2$CondWater, myDF2$h2o_rh), na.rm=T)
    myDF2$H2O_flux_normalized <- myDF2$H2O_flux_total / myDF2$SumOfarea_fully_exp / 60 / 18 * 1000000
    
    
    #with(myDF2[myDF2$canopy=="12345" & myDF2$chamber == "1", ], plot(H2O_flux_normalized ~ datetime))
    
    #test2 <- subset(myDF2, chamber == "4" & canopy == "345")
    #with(test2, plot(rh~datetime))
    
    ## return selected columns
    
    outDF <- myDF2[,c("chamber", "canopy", "datetime", "H2O_flux_normalized")]
    colnames(outDF) <- c("Chamber", "Canopy", "datetime", "H2O_flux_normalized")
    
    return(outDF)

}

