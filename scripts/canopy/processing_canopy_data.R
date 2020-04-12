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
    
    
    #                                    Descriptions	
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
    
    myDF$datetime <- as.POSIXct(paste(as.character(myDF$date), myDF$time), format="%Y-%m-%d %H:%M:%S")
    myDF$time <- strftime(myDF$datetime, format="%H:%M:%S")
    myDF$time <- as.POSIXct(myDF$time, format="%H:%M:%S")
    
    myDF <- myDF[,!(colnames(myDF)%in% c("time1", "time2", "time3"))]
    
    
    ### only include the complete data where normalized flux is available
    myDF <- myDF[complete.cases(myDF$ncorrflux), ]
    
    ### ignore unreasonable data
    myDF <- subset(myDF, ncorrflux >= -2)
    
    ### update names to improve readability
    names(myDF)[names(myDF) == "vCo2"] <- "WTC_CO2"
    names(myDF)[names(myDF) == "vT"] <- "WTC_T"
    names(myDF)[names(myDF) == "DPLicorCh"] <- "WTC_dew_point"
    names(myDF)[names(myDF) == "PARi"] <- "WTC_PAR"
    names(myDF)[names(myDF) == "slope2"] <- "CO2_flux"
    names(myDF)[names(myDF) == "nslope2"] <- "Norm_CO2_flux"
    names(myDF)[names(myDF) == "cmarea"] <- "Leaf_area"
    names(myDF)[names(myDF) == "k"] <- "Leak_coef"
    names(myDF)[names(myDF) == "leak"] <- "Leak"
    names(myDF)[names(myDF) == "corrflux"] <- "Corr_CO2_flux"
    names(myDF)[names(myDF) == "ncorrflux"] <- "Norm_corr_CO2_flux"
    
    myDF <- myDF[,c("Chamber", "Canopy", "datetime", "date", "time", 
                    "WTC_CO2", "WTC_T", "WTC_dew_point", "WTC_PAR", 
                    "Tair", "Leaf_area", "Leak_coef", "Leak", 
                    "CO2_flux", "Norm_CO2_flux", "Corr_CO2_flux", 
                    "Norm_corr_CO2_flux")]
    
    
    ### check canopy data structure
    #canopy_data_check_and_plot(myDF)
    
    ### time series data correction to control for breaks in the dataseries
    myDF <- canopy_data_control(myDF)
    
    ### Calculate CO2 flux for each minute and output in the unit of ppm CO2 min-1
    ### also delete some unstable data points (mostly earlier period of the experiments)
    myDF <- calculate_co2_flux_per_second(myDF)

    ### continue cleaning data
    ### remove missing data points
    myDF$Norm_corr_CO2_flux <- as.numeric(myDF$Norm_corr_CO2_flux)
    myDF <- myDF[complete.cases(myDF$Norm_corr_CO2_flux), ]
    
    myDF <- manually_delete_unreasonable_data(myDF)
    
    
    ### plotting co2 flux at per second rate for each chamber
    canopy_data_per_second_check_and_plot(myDF)
    
    
    
    
    
    
    
    
    
    
    #test <- subset(myDF, Chamber == "12" & Canopy == "345")
    #with(test, plot(ncorrflux~time))
    #with(test, plot(ncorrflux~vCo2))
    #test.fit <- fitaci(test, fitmethod="bilinear", Tcorrect=T, fitTPU = T)
    #coef(test.fit)
    
    ### add VPD 
    ## Saturation Vapor Pressure (es) = 0.6108 * exp(17.27 * T / (T + 237.3))
    ## kPa
    myDF$es <- 0.6106 * exp(17.27 * myDF$Tair / (myDF$Tair + 237.3))
    
    ## calculate RH
    myDF$rh <- 100 - 5 * (myDF$Tair - myDF$DPLicorCh)
    
    ## Actual Vapor Pressure (ea) = RH / 100 * es 
    ## kPa
    myDF$ea <- myDF$es * myDF$rh / 100 
    
    ##  VPD = ea - es
    #myDF$VPD <- myDF$ea - myDF$es
    ## kPa
    myDF$VPD <- myDF$es * (100 - myDF$rh)/100
    
    ## calculate based on planteophys
    #require(plantecophys)
    #myDF$VPD2 <- RHtoVPD(myDF$rh, myDF$Tair, Pa = 101)
    # method 1 and 2 agree with each other, good!
    
    ### add H2O flux
    #myDF2 <- process_canopy_second_dataset_to_get_H2O_flux()
    myDF2 <- process_canopy_second_dataset_to_get_H2O_flux_2()
    
    
    ### reprocess time
    myDF$time1 <- sub(".+? ", "", myDF$datetime)
    myDF$time2 <- substr(myDF$time1,1,nchar(myDF$time1)-3)
    myDF$time <- paste0(myDF$time2, ":00")
    
    #test <- subset(myDF, Chamber == "4" & Canopy == "345")
    #with(test, plot(ncorrflux~datetime))
    
    myDF$date <- gsub( " .*$", "", myDF$datetime)
    myDF$datetime <- as.POSIXct(paste(myDF$date, myDF$time), format="%Y-%m-%d %H:%M:%S")
    myDF <- myDF[,!(colnames(myDF)%in% c("time1", "time2"))]
    
    #test <- subset(myDF, Chamber == "8" & Canopy == "45")
    #with(test, plot(ncorrflux~datetime))
    
    #test2 <- subset(myDF2, Chamber == "8" & Canopy == "45")
    #with(test2, plot(H2O_flux_normalized~datetime))
    
    ### combine both datasets
    cDF <- merge(myDF, myDF2, by.x=c("Chamber", "Canopy", "datetime"), 
                 by.y=c("Chamber", "Canopy", "datetime"))

    ### only include the complete data where normalized flux is available
    cDF <- cDF[complete.cases(cDF$ncorrflux), ]
    cDF <- cDF[complete.cases(cDF$H2O_flux_normalized), ]
    
    #test3 <- subset(cDF, Chamber == "8" & Canopy == "45")
    #with(test3, plot(ncorrflux~datetime))
    #with(test3, plot(ncorrflux~vCo2))
    
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
