process_canopy_second_dataset_to_get_H2O_flux <- function(myDF) {
    ########################  using mergeall.text file ###########################
    ##### read in raw data - this dataset does not have PAR 
    myDF2 <- read.csv("data/canopy_drawdown/mergeall.csv")
    
    myDF2$date <- sub(" .*", "", myDF2$datetime)
    myDF2$date <- as.Date(as.character(myDF2$date), format="%m/%d/%y")
    myDF2$time1 <- sub(".+? ", "", myDF2$datetime)
    myDF2$time <- paste0(myDF2$time1, ":00")
    
    myDF2$datetime <- as.POSIXct(paste(myDF2$date, myDF2$time), format="%Y-%m-%d %H:%M:%S")
    myDF2$time <- strftime(myDF2$datetime, format="%H:%M:%S")
    myDF2$time <- as.POSIXct(myDF2$time, format="%H:%M:%S")
    
    myDF2 <- myDF2[,!(colnames(myDF2)%in% c("time1"))]
    
    
    ### merge two datasets
    mgDF <- merge(myDF, myDF2, by.x=c("Chamber", "Canopy", "datetime"),
                 by.y=c("chamber", "canopy", "datetime"),all=T)
    
    mgDF <- mgDF[,c("Chamber", "Canopy", "datetime", "date.x", "time.x", 
                    "WTC_CO2", "WTC_T", "WTC_dew_point", "WTC_PAR", 
                    "Tair.x", "ES", "RH", "EA", "VPD",
                    "Leaf_area", "Leak_coef", "Leak", 
                    "CO2_flux", "Norm_CO2_flux", "Corr_CO2_flux", 
                    "Norm_corr_CO2_flux", "CondWater", 
                    "SumOfarea_fully_exp")]
    
    colnames(mgDF) <- c("Chamber", "Canopy", "datetime", "date", "time", 
                        "WTC_CO2", "WTC_T", "WTC_dew_point", "WTC_PAR", 
                        "Tair", "ES", "RH", "EA", "VPD",
                        "Leaf_area", "Leak_coef", "Leak", 
                        "CO2_flux", "Norm_CO2_flux", "Corr_CO2_flux", 
                        "Norm_corr_CO2_flux", "Cond_water", 
                        "Leaf_area_fully_exp")
    
    ### add volumn information - does not include the effect of cone top
    ## note that chamber 11 = 49800 L
    ## all else = 52800 L
    ## output unit m3
    mgDF$WTC_volume_m3 <- 52800 / 1000
    mgDF$WTC_volume_m3[mgDF$Chamber == "11"] <- 49800 / 1000
    
    ### calculate water fluxes - converting RH to water unit g/m3
    ## convert ea from kPa to Pa
    mgDF$rh_water <- 2.16679 * (mgDF$EA * 1000) / (mgDF$Tair + 273.2) 

    ### convert to get total chamber water content in g/chamber
    myDF2$rh_total <- myDF2$rh_water #* myDF2$volume
    
    #test2 <- subset(myDF2, chamber == "4" & canopy == "345")
    #with(test2, plot(rh~datetime))
    
    ### convert H2O flux from unit of g per chamber per minute to umol H2O per leaf area per second
    ## average across 5 mins to compute a running mean difference at 5 min time interval
    ## the final value is quite sensitive to the size of bin, i.e. 5 min vs. 10 mins 
    myDF3 <- calculate_h2o_flux_per_second2(myDF2)
    #myDF4 <- calculate_condensed_water_flux_per_second(myDF3)
    
    ### convert from g of water per chamber per minute to umol H2O m-2 leaf area s-1
    myDF3$h2o_rh_normalized <- myDF3$h2o_rh / 60 / 18 * 1000000 / myDF3$SumOfarea_fully_exp
    
    ### convert unit, CondWater from mol h-1 chamber-1 to umol s-1 leaf area -1
    ### convert unit, CondWater from mmol h-1 chamber -1 to umol s-1 leaf area-1
    #myDF3$CondWater_normalized <- myDF3$CondWater * 1000000 / myDF3$SumOfarea_fully_exp / 3600
    myDF3$CondWater_normalized <- myDF3$CondWater * 1000 / myDF3$SumOfarea_fully_exp / 3600
    
    ### normalized to umol H2O m-2 leaf s-1
    myDF3$H2O_flux_normalized <-  rowSums(data.frame(myDF3$CondWater_normalized, myDF3$h2o_rh_normalized), na.rm=T)
    
    
    #myDF4$h2o_rh_normalized <- myDF4$h2o_rh / 60 / 18 * 1000000 / myDF4$SumOfarea_fully_exp
    
    ### convert unit, CondWater from mol h-1 chamber-1 to umol s-1 leaf area -1
    #myDF4$CondWater_normalized <- myDF4$CondWater_smooth * 1000000 / myDF4$SumOfarea_fully_exp / 3600
    
    ### normalized to umol H2O m-2 leaf s-1
    #myDF4$H2O_flux_normalized <-  rowSums(data.frame(myDF4$CondWater_normalized, myDF4$h2o_rh_normalized), na.rm=T)
    
    ## return selected columns
    
    outDF <- myDF3[,c("chamber", "canopy", "datetime", "H2O_flux_normalized")]
    colnames(outDF) <- c("Chamber", "Canopy", "datetime", "H2O_flux_normalized")
    
    return(outDF)

}

