merge_with_H2O_flux_dataset <- function(myDF) {
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
                    "Norm_corr_CO2_flux", "CondWater")]
    
    colnames(mgDF) <- c("Chamber", "Canopy", "datetime", "date", "time", 
                        "WTC_CO2", "WTC_T", "WTC_dew_point", "WTC_PAR", 
                        "Tair", "ES", "RH", "EA", "VPD",
                        "Leaf_area", "Leak_coef", "Leak", 
                        "CO2_flux", "Norm_CO2_flux", "Corr_CO2_flux", 
                        "Norm_corr_CO2_flux", "Cond_water")
    
    ### add volumn information - does not include the effect of cone top
    ## note that chamber 11 = 49800 L
    ## all else = 52800 L
    ## output unit m3
    mgDF$WTC_volume_m3 <- 52800 / 1000
    mgDF$WTC_volume_m3[mgDF$Chamber == "11"] <- 49800 / 1000
    
    return(mgDF)

}

