processing_canopy_data <- function() {
    #### There are two possible datasets,
    #### they should be almost identical.
    #### The dataset "mergeall.text" is a potentially processed file,
    #### but it does not have canopy of 0, and PAR information, 
    #### and, it has less number of data entries (not much less).
    #### In comparison, the "drawdownanalysis9Sepb.csv" contains
    #### canopy of 0, the un-normalized data, PAR information.
    ### Here I am using the drawdownalaysis9Sepb.csv file
    ### but codes for mergeall.txt is available, but commented out.
    
    
    #                                    descriptions	
    # values calculated using SAS prog 	
    # chamber	chamber number
    # canopy	canopy layers present during drawdown
    # vCo2	CO2 concentration in chamber
    # vT	Air temp (by vaisala not shielded)
    # vtime2	date and time 
    # Tair	Air temp from WTC system
    # DPLicorCh	Dew point in chamber updated every 14 mins
    # PAR	PAR as measured by WTC system
    # slope2	dCO2/dt umol tree-1 s-1
    # cmarea	leaf area in chamber m2
    # nslope2	normalised flux umol m-2 leaf s-1
    # k	leak coefficient
    # time	time of day
    # leak	leak =(vco2-380)*k
    # corrflux	leak corrected flux umol tree-1 s-1
    # ncorrflux	normalised corrected flux umol m-2 leaf s-1#
    
    #### note: corrected for different sizes of trees,
    ####       by Drake's method (Drake et al. 2018),
    ####       i.e. express raw canopy flux on a leaf area basis by dividing by total canopy leaf area
    

    ########################  using drawdownanalysis9Sepb.csv file ###########################
    ### read in raw data
    myDF <- read.csv("data/canopy_drawdown/drawdownanalysis9Sepb.csv")
    
    ### set up dataset
    myDF$canopy <- as.character(myDF$canopy)
    
    colnames(myDF) <- c("Chamber", "Canopy","vCo2", 
                        "vT", "datetime", "Tair",
                        "DPLicorCh", "PARi", "slope2", 
                        "cmarea", "nslope2","k", "time", 
                        "leak", "corrflux", "ncorrflux")
    
    ### set up data and time
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
    
    
    ### only include the complete data where normalized flux is available
    myDF <- myDF[complete.cases(myDF$ncorrflux), ]

    ### check canopy data structure
    #canopy_data_check_and_plot2(myDF)
    
    ### time series data correct to control for breaks in the dataseries
    myDF <- canopy_data_control2(myDF)
    
    ### Calculate CO2 flux for each minute and output in the unit of ppm CO2 min-1
    myDF <- calculate_co2_flux_per_second2(myDF)
    
    ### plotting co2 flux at per second rate for different treatments
    canopy_data_per_second_check_and_plot2(myDF)
    
    
    ### exclude problematic data
    myDF[myDF$Chamber == "2" && myDF$ncorrflux > 15] <- "NA"
    myDF[myDF$Chamber == "2" && myDF$ncorrflux < 0] <- "NA"
    myDF[myDF$Chamber == "3" && myDF$Canopy == "45" && myDF$datetime < "2009-03-19 09:18:40", "ncorrflux"] <- NA
    myDF[myDF$Chamber == "4" && myDF$ncorrflux < 0] <- "NA"
    myDF[myDF$Chamber == "4" && myDF$ncorrflux > 11] <- "NA"
    myDF[myDF$Chamber == "7" && myDF$ncorrflux < 0] <- "NA"
    myDF[myDF$Chamber == "7" && myDF$ncorrflux > 25] <- "NA"
    myDF[myDF$Chamber == "8" && myDF$ncorrflux < -5] <- "NA"
    myDF[myDF$Chamber == "11" && myDF$ncorrflux < 0] <- "NA"
    myDF[myDF$Chamber == "12" && myDF$ncorrflux < 0] <- "NA"
    myDF <- myDF[complete.cases(myDF$ncorrflux), ]
    
    #test <- subset(myDF, Chamber == "8" & Canopy == "45")
    #with(test, plot(ncorrflux~time))
    
    ### add VPD 
    ## Saturation Vapor Pressure (es) = 0.6108 * exp(17.27 * T / (T + 237.3))
    myDF$es <- 0.6106 * exp(17.27 * myDF$vT / (myDF$vT + 237.3))
    
    ## calculate RH
    myDF$rh <- 100 - 5 * (myDF$vT - myDF$DPLicorCh)
    
    ## Actual Vapor Pressure (ea) = RH / 100 * es 
    myDF$ea <- myDF$rh / 100 * myDF$es
    
    ##  VPD = ea - es
    #myDF$VPD <- myDF$ea - myDF$es
    myDF$VPD <- myDF$es * (100 - myDF$rh)/100
    
    
    ### add H2O flux
    myDF2 <- process_canopy_second_dataset_to_get_H2O_flux()
    
    
    ### reprocess time
    myDF$time1 <- sub(".+? ", "", myDF$datetime)
    myDF$time2 <- substr(myDF$time1,1,nchar(myDF$time1)-3)
    myDF$time <- paste0(myDF$time2, ":00")
    myDF$datetime <- as.POSIXct(paste(myDF$date, myDF$time), format="%Y-%m-%d %H:%M:%S")
    myDF <- myDF[,!(colnames(myDF)%in% c("time1", "time2"))]
    

    ### combine both datasets
    cDF <- merge(myDF, myDF2, by.x=c("Chamber", "Canopy", "datetime"), 
                 by.y=c("Chamber", "Canopy", "datetime"))

    ### only include the complete data where normalized flux is available
    cDF <- cDF[complete.cases(cDF$ncorrflux), ]
    cDF <- cDF[complete.cases(cDF$H2O_flux_normalized), ]
    
    ### return
    outDF <- cDF[,c("Chamber", "Canopy", "vCo2", 
                     "vT", "date", "time",  "datetime", 
                    "Tair", "VPD", 
                     "DPLicorCh", "PARi", "slope2", 
                     "cmarea", "nslope2","k", 
                     "leak", "corrflux", "ncorrflux", "rh", "co2_flux", "H2O_flux_normalized")]
    
    colnames(outDF) <- c("Chamber", "Canopy", "vCo2", 
                         "vT", "date", "time",  "datetime", 
                         "Tair", "VPD", 
                         "DPLicorCh", "PARi", "slope2", 
                         "cmarea", "nslope2","k", 
                         "leak", "corrflux", "co2_flux", "rh", "my_co2_flux", "H2O_flux_normalized")
    return(outDF)

    
}
