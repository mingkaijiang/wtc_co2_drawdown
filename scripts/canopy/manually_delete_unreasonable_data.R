manually_delete_unreasonable_data <- function(myDF) {
    ### exclude problematic data
    myDF[myDF$Chamber == "1" & myDF$Canopy == "45" & myDF$WTC_CO2 > 1500 & myDF$Norm_corr_CO2_flux < 8, "Norm_corr_CO2_flux"] <- NA
    myDF <- myDF[complete.cases(myDF$Norm_corr_CO2_flux), ]
    
    myDF[myDF$Chamber == "2" & myDF$Norm_corr_CO2_flux > 15, "Norm_corr_CO2_flux"] <- NA
    myDF <- myDF[complete.cases(myDF$Norm_corr_CO2_flux), ]
    
    myDF[myDF$Chamber == "2" & myDF$Norm_corr_CO2_flux < -1, "Norm_corr_CO2_flux"] <- NA
    myDF <- myDF[complete.cases(myDF$Norm_corr_CO2_flux), ]
    
    myDF[myDF$Chamber == "3" & myDF$Canopy == 45 & myDF$datetime < "2009-03-19 09:18:40", "Norm_corr_CO2_flux"] <- NA
    myDF <- myDF[complete.cases(myDF$Norm_corr_CO2_flux), ]
    
    myDF[myDF$Chamber == "4" & myDF$Canopy == "12345" & myDF$WTC_CO2 > 1200 & myDF$Norm_corr_CO2_flux < 6, "Norm_corr_CO2_flux"] <- NA
    myDF <- myDF[complete.cases(myDF$Norm_corr_CO2_flux), ]
    
    myDF[myDF$Chamber == "4" & myDF$Norm_corr_CO2_flux < 0, "Norm_corr_CO2_flux"] <- NA
    myDF <- myDF[complete.cases(myDF$Norm_corr_CO2_flux), ]
    
    myDF[myDF$Chamber == "4" & myDF$Norm_corr_CO2_flux > 12, "Norm_corr_CO2_flux"] <- NA
    myDF <- myDF[complete.cases(myDF$Norm_corr_CO2_flux), ]
    
    myDF[myDF$Chamber == "4" & myDF$Canopy == "345" & myDF$Norm_corr_CO2_flux > 10, "Norm_corr_CO2_flux"] <- NA
    myDF <- myDF[complete.cases(myDF$Norm_corr_CO2_flux), ]
    
    myDF[myDF$Chamber == "4" & myDF$Canopy == "45" & myDF$WTC_CO2 >= 1200, "Norm_corr_CO2_flux"] <- NA
    myDF <- myDF[complete.cases(myDF$Norm_corr_CO2_flux), ]
    
    myDF[myDF$Chamber == "7" & myDF$Norm_corr_CO2_flux < -2, "Norm_corr_CO2_flux"] <- NA
    myDF <- myDF[complete.cases(myDF$Norm_corr_CO2_flux), ]
    
    myDF[myDF$Chamber == "7" & myDF$Norm_corr_CO2_flux > 20, "Norm_corr_CO2_flux"] <- NA
    myDF <- myDF[complete.cases(myDF$Norm_corr_CO2_flux), ]
    
    myDF[myDF$Chamber == "7" & myDF$Canopy == "345" & myDF$Norm_corr_CO2_flux > 16, "Norm_corr_CO2_flux"] <- NA
    myDF <- myDF[complete.cases(myDF$Norm_corr_CO2_flux), ]
    
    myDF[myDF$Chamber == "7" & myDF$Canopy == "45" & myDF$WTC_CO2 > 1000 & myDF$Norm_corr_CO2_flux < 12, "Norm_corr_CO2_flux"] <- NA
    myDF <- myDF[complete.cases(myDF$Norm_corr_CO2_flux), ]
    
    myDF[myDF$Chamber == "8" & myDF$Norm_corr_CO2_flux < 0, "Norm_corr_CO2_flux"] <- NA
    myDF <- myDF[complete.cases(myDF$Norm_corr_CO2_flux), ]
    
    myDF[myDF$Chamber == "8" & myDF$Canopy == "12345" & myDF$WTC_CO2 > 300 & myDF$Norm_corr_CO2_flux < 3, "Norm_corr_CO2_flux"] <- NA
    myDF <- myDF[complete.cases(myDF$Norm_corr_CO2_flux), ]
    
    myDF[myDF$Chamber == "8" & myDF$Canopy == "345" & myDF$WTC_CO2 > 200 & myDF$Norm_corr_CO2_flux < 3, "Norm_corr_CO2_flux"] <- NA
    myDF <- myDF[complete.cases(myDF$Norm_corr_CO2_flux), ]
    
    myDF[myDF$Chamber == "8" & myDF$Canopy == "345" & myDF$Norm_corr_CO2_flux > 8, "Norm_corr_CO2_flux"] <- NA
    myDF <- myDF[complete.cases(myDF$Norm_corr_CO2_flux), ]
    
    myDF[myDF$Chamber == "8" & myDF$Canopy == "345" & myDF$WTC_CO2 > 300 & myDF$Norm_corr_CO2_flux < 4, "Norm_corr_CO2_flux"] <- NA
    myDF <- myDF[complete.cases(myDF$Norm_corr_CO2_flux), ]
    
    myDF[myDF$Chamber == "8" & myDF$Canopy == "345" & myDF$WTC_CO2 > 420 & myDF$Norm_corr_CO2_flux < 4.5, "Norm_corr_CO2_flux"] <- NA
    myDF <- myDF[complete.cases(myDF$Norm_corr_CO2_flux), ]
    
    myDF[myDF$Chamber == "8" & myDF$Canopy == "345" & myDF$WTC_CO2 > 380 & myDF$Norm_corr_CO2_flux < 4.3, "Norm_corr_CO2_flux"] <- NA
    myDF <- myDF[complete.cases(myDF$Norm_corr_CO2_flux), ]
    
    myDF[myDF$Chamber == "8" & myDF$Canopy == "45" & myDF$Norm_corr_CO2_flux > 10, "Norm_corr_CO2_flux"] <- NA
    myDF <- myDF[complete.cases(myDF$Norm_corr_CO2_flux), ]
    
    myDF[myDF$Chamber == "8" & myDF$Canopy == "45" & myDF$WTC_CO2 > 1200, "Norm_corr_CO2_flux"] <- NA
    myDF <- myDF[complete.cases(myDF$Norm_corr_CO2_flux), ]
    
    myDF[myDF$Chamber == "8" & myDF$Canopy == "45" & myDF$WTC_CO2 > 400 & myDF$Norm_corr_CO2_flux < 5, "Norm_corr_CO2_flux"] <- NA
    myDF <- myDF[complete.cases(myDF$Norm_corr_CO2_flux), ]
    
    myDF[myDF$Chamber == "11" & myDF$Norm_corr_CO2_flux < 0, "Norm_corr_CO2_flux"] <- NA
    myDF <- myDF[complete.cases(myDF$Norm_corr_CO2_flux), ]
    
    myDF[myDF$Chamber == "11" & myDF$Canopy == "12345" & myDF$WTC_CO2 >= 1400, "Norm_corr_CO2_flux"] <- NA
    myDF <- myDF[complete.cases(myDF$Norm_corr_CO2_flux), ]
    
    myDF[myDF$Chamber == "12" & myDF$Norm_corr_CO2_flux < 0, "Norm_corr_CO2_flux"] <- NA
    myDF <- myDF[complete.cases(myDF$Norm_corr_CO2_flux), ]
    
    myDF[myDF$Chamber == "12" & myDF$Norm_corr_CO2_flux > 15, "Norm_corr_CO2_flux"] <- NA
    myDF <- myDF[complete.cases(myDF$Norm_corr_CO2_flux), ]
    
    myDF[myDF$Chamber == "12" & myDF$Canopy == "45" & myDF$WTC_CO2 >= 1200 & myDF$Norm_corr_CO2_flux < 4, "Norm_corr_CO2_flux"] <- NA
    myDF <- myDF[complete.cases(myDF$Norm_corr_CO2_flux), ]
    
    
    
    myDF[myDF$Chamber == "12" & myDF$Canopy == "345" & myDF$WTC_CO2 >= 1400, "Norm_corr_CO2_flux"] <- NA
    myDF <- myDF[complete.cases(myDF$Norm_corr_CO2_flux), ]
    
    myDF[myDF$Chamber == "12" & myDF$Canopy == "345" & myDF$WTC_CO2 >= 300 & myDF$Norm_corr_CO2_flux < 5, "Norm_corr_CO2_flux"] <- NA
    myDF <- myDF[complete.cases(myDF$Norm_corr_CO2_flux), ]
    
    myDF[myDF$Chamber == "12" & myDF$Canopy == "345" & myDF$WTC_CO2 <= 450 & myDF$Norm_corr_CO2_flux > 9, "Norm_corr_CO2_flux"] <- NA
    myDF <- myDF[complete.cases(myDF$Norm_corr_CO2_flux), ]
    
    myDF[myDF$Chamber == "12" & myDF$Canopy == "345" & myDF$WTC_CO2 >= 1200 & myDF$Norm_corr_CO2_flux < 10, "Norm_corr_CO2_flux"] <- NA
    myDF <- myDF[complete.cases(myDF$Norm_corr_CO2_flux), ]
    
    return(myDF)
}

