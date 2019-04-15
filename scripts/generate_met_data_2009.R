generate_met_data_2009 <- function() {
    #### this script generates hourly met data for two-leaf model
    #### it reads year 2009 met data from WTC
    #### which is not chamber-specific
    #### then it combines with chamber-specific flux data
    #### to generate met data for each chamber
    
    
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
    metDF <- read.csv("data/met/WTC_met_2009/HFE 30min Metdata 2009.csv")

    ### check which date do we run out of data
    subDF <- metDF[complete.cases(metDF$PAR),]
    subDF$DateTime <- as.POSIXct(as.character(subDF$DateTime), format="%Y-%m-%d %H:%M:%S")
    subDF$doy <- subDF$DOY
    
    ### subset only hourly data, ignore half hourly timesteps
    subDF2 <- subset(subDF, minute == "0")
    subDF2$hod <- subDF2$hour
    
    ### get the easy stuff ready
    subDF2$tair <- subDF2$TairRef
    subDF2$par <- subDF2$PAR
    subDF2$vpd <- subDF2$VPDdem
    subDF2$wind <- subDF2$winddem
    subDF2$pressure <- subDF2$AirPress
    
    subDF3 <- subDF2[,c("doy", "hod", "tair", "par", "vpd", "wind", "pressure")]
    
    
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
    canopy_data_check_and_plot2(myDF)
    
    ### time series data correct to control for breaks in the dataseries
    myDF <- canopy_data_control2(myDF)
    
    ### Calculate CO2 flux for each minute and output in the unit of ppm CO2 min-1
    myDF <- calculate_co2_flux_per_second2(myDF)
    
    ### plotting co2 flux at per second rate for different treatments
    canopy_data_per_second_check_and_plot2(myDF)
    
    return(myDF)

    
}
