processing_canopy_data <- function(leafDF) {
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
    

    ########################  set-up variable names ###########################
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
    myDF$time <- paste0(myDF$time1, ":00")#myDF$time3)
    
    myDF$datetime <- as.POSIXct(paste(as.character(myDF$date), myDF$time), format="%Y-%m-%d %H:%M:%S")
    myDF$time <- strftime(myDF$datetime, format="%H:%M:%S")
    myDF$time <- as.POSIXct(myDF$time, format="%H:%M:%S")
    
    myDF <- myDF[,!(colnames(myDF)%in% c("time1", "time2", "time3"))]
    
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
    
    ### add VPD 
    ## Saturation Vapor Pressure (es) = 0.6108 * exp(17.27 * T / (T + 237.3))
    ## kPa
    myDF$ES <- 0.6106 * exp(17.27 * myDF$WTC_T / (myDF$WTC_T + 237.3))
    
    ## calculate RH
    myDF$RH <- 100 - 5 * (myDF$WTC_T - myDF$WTC_dew_point)
    
    myDF$RH <- ifelse(myDF$RH > 100, 100, myDF$RH)
    
    ## Actual Vapor Pressure (ea) = RH / 100 * es 
    ## kPa
    myDF$EA <- myDF$ES * myDF$RH / 100 
    
    ##  VPD = ea - es
    #myDF$VPD <- myDF$ea - myDF$es
    ## kPa
    myDF$VPD <- myDF$ES * (100 - myDF$RH)/100
    
    ### ignore VPD = 0 and NA
    myDF <- subset(myDF, VPD > 0)
    myDF <- myDF[complete.cases(myDF$VPD), ]
    
    ## calculate based on planteophys
    #require(plantecophys)
    #myDF$VPD2 <- RHtoVPD(myDF$RH, myDF$Tair, Pa = 101)
    
    ### merge with H2O flux dataset
    myDF <- merge_with_H2O_flux_dataset(myDF)
    
    ### ignore unreasonable data
    myDF <- subset(myDF, Norm_corr_CO2_flux >= -2)
    
    ########################  quality control ###########################
    
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
    #myDF <- myDF[complete.cases(myDF$Norm_corr_CO2_flux), ]
    myDF <- manually_delete_unreasonable_data(myDF)
    
    
    ########################  add transpiration to get Ci ###########################
    ### add H2O flux
    outDF <- calculate_transpiration_flux(myDF)

    ### calculate gs
    outDF$gs1 <- outDF$Norm_H2O_flux / outDF$VPD
    
    outDF <- subset(outDF, gs1 > 0)
    
    ### read in leaf-scale g1 value to represent canopy g1
    ### i.e. assuming same g1 value for leaf and canopy
    outDF <- add_leaf_g1_to_canopy_data(leafDF=leafDF, canopyDF=outDF)
    
    ### alterantive way of calculating gs
    outDF$gs <- (1+(outDF$G1/sqrt(outDF$VPD))) * (outDF$Norm_corr_CO2_flux/outDF$WTC_CO2)
    
    ### alternative way of calculating water transpiration flux
    outDF$Norm_H2O_flux2 <- outDF$Norm_corr_CO2_flux * (outDF$G1*sqrt(outDF$VPD) + outDF$VPD) / outDF$WTC_CO2
    
    ### calculate Ci
    outDF$Ci <- outDF$WTC_CO2 - (outDF$Norm_corr_CO2_flux/outDF$gs)
    
    ### plotting CO2 and H2O flux at per second rate for each chamber
    canopy_data_per_second_check_and_plot(inDF=outDF)
    
    
    ### return output
    return(outDF)

    
}
