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
    #myDF$rh <- 100 - 5 * (myDF$Tair - myDF$DPLicorCh)
    #myDF$es <- 0.6106 * exp(17.27 * myDF$Tair / (myDF$Tair + 237.3))
    #myDF$ea <- myDF$rh / 100 * myDF$es
    #
    #### add volumn information - does not include the effect of cone top
    ### note that chamber 11 is slightly smaller
    #myDF$volume <- 9 * pi * (3.25/2)^2
    #
    #### calculate water fluxes - converting RH to water unit kg over the entire volume
    #myDF$rh_water <- (myDF$ea * 1000) / (461.5 * (myDF$Tair + 273.2)) 
    #
    ## (55.31 - 51.06)/60/63.49 = 0.0011 g H2O m-2 leaf area s-1
    #myDF$delta_rh_water <- "NA"
    #myDF$delta_cond_water <- "NA"
    
    ### convert H2O flux from unit of g per chamber per minute to umol H2O per leaf area per second
    myDF2 <- calculate_h2o_flux_per_second2(myDF)
    myDF2$H2O_flux_total <- myDF2$h2o_cond + myDF2$h2o_rh
    myDF2$H2O_flux_normalized <- myDF2$H2O_flux_total / myDF2$SumOfarea_fully_exp / 60 / 18 * 1000000
    
    
    # with(myDF2[myDF2$canopy=="12345" & myDF2$chamber == "3", ], plot(H2O_flux_normalized ~ datetime))
    
    
    ### note:
    ## 1. average across 15 mins to compute a running mean difference at 15 min time interval
    ## 2. the final value is very sensitive to the size of bin, i.e. 15 min vs. 10 mins 
    ## 3. next to calculate stomatal conductance, ci, and Jmax, Vcmax based on canopy data
    ## 4. if the value don't make sense, change bin size and redo things.
    ## 5. also need to update met data with the correct VPD data

    
    ## return selected columns
    
    outDF <- myDF2[,c("chamber", "canopy", "datetime", "H2O_flux_normalized")]
    colnames(outDF) <- c("Chamber", "Canopy", "datetime", "H2O_flux_normalized")
    
    return(outDF)

}

