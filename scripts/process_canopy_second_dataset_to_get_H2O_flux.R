process_canopy_second_dataset_to_get_H2O_flux <- function() {
    ########################  using mergeall.text file ###########################
    ##### read in raw data - this dataset does not have PAR 
    myDF <- read.csv("data/canopy_drawdown/mergeall.csv")
    
    myDF$date <- sub(" .*", "", myDF$datetime)
    myDF$date <- as.Date(as.character(myDF$date), format="%m/%d/%y")
    myDF$time1 <- sub(".+? ", "", myDF$datetime)
    myDF$time2 <- sub(" .*", "", myDF$time)
    myDF$time3 <- str_sub(myDF$time2, start=-3)
    myDF$time <- paste0(myDF$time1, myDF$time3)
    
    myDF$datetime <- as.POSIXct(paste(myDF$date, myDF$time), format="%Y-%m-%d %H:%M:%S")
    myDF$time <- strftime(myDF$datetime, format="%H:%M:%S")
    myDF$time <- as.POSIXct(myDF$time, format="%H:%M:%S")
    
    myDF <- myDF[,!(colnames(myDF)%in% c("time1", "time2", "time3"))]
    
    
    ### time series data correct to control for breaks in the dataseries
    myDF <- canopy_data_control(myDF)
    
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
    
    ### process each chamber

    
    
    
    
    
    
    
    
    
    
    with(myDF[myDF$canopy=="12345" & myDF$chamber == "3", ], plot(rh_water ~ datetime))
    

}

