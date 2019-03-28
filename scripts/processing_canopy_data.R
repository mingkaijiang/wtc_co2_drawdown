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
    
    
    ########################  using mergeall.text file ###########################
    ##### read in raw data - this dataset does not have PAR 
    ##myDF <- read.table("data/mergeall.txt", sep=",", header=T)
    #
    #### set up dataset
    ##myDF$canopy <- as.character(myDF$canopy)
    ##myDF$datetime <- as.POSIXct(as.character(myDF$datetime))
    ##myDF$vtime <- as.POSIXct(as.character(myDF$vtime))
    #
    #### extract time information
    ##myDF$time <- strftime(myDF$datetime, format="%H:%M:%S")
    ##myDF$time <- as.POSIXct(myDF$time, format="%H:%M:%S")
    ##myDF$date <- strftime(myDF$datetime, format="%Y-%m-%d")
    #
    #### check canopy data structure
    #canopy_data_check_and_plot(myDF)
    #
    #### time series data correct to control for breaks in the dataseries
    #myDF <- canopy_data_control(myDF)
    #
    #### Calculate CO2 flux for each minute and output in the unit of ppm CO2 min-1
    #myDF2 <- calculate_co2_flux_per_second(myDF)
    #
    #### plotting co2 flux at per second rate for different treatments
    #canopy_data_per_second_check_and_plot(myDF2)
    
    
    
    ########################  using drawdownanalysis9Sepb.csv file ###########################
    ### read in raw data
    myDF <- read.csv("data/drawdownanalysis9Sepb.csv")
    
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
    canopy_data_check_and_plot2(myDF)
    
    ### time series data correct to control for breaks in the dataseries
    myDF <- canopy_data_control2(myDF)
    
    ### Calculate CO2 flux for each minute and output in the unit of ppm CO2 min-1
    myDF <- calculate_co2_flux_per_second2(myDF)
    
    ### plotting co2 flux at per second rate for different treatments
    canopy_data_per_second_check_and_plot2(myDF)
    
    return(myDF)

    
}
