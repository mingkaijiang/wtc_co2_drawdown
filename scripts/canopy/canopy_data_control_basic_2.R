canopy_data_control_basic_2 <- function(myDF) {
    
    ### Canopy code: 12345 full 
    ###              345 top + middle
    ###              45 top
    
    ### prepare outDF
    outDF <- c()
    
    ### keep data that has good qualities
    subDF1 <- subset(myDF, Chamber=="2"&Canopy=="345")
    subDF2 <- subset(myDF, Chamber=="3"&Canopy=="345")
    subDF3 <- subset(myDF, Chamber=="3"&Canopy=="12345")
    subDF4 <- subset(myDF, Chamber=="11"&Canopy=="345")
    subDF5 <- subset(myDF, Chamber=="12"&Canopy=="12345")
    
    outDF <- rbind(outDF, subDF1, subDF2, subDF3, subDF4, subDF5)
    
    
    
    ### Chamber 2, canopy 45, delete outliers
    subDF1 <- subset(myDF, Chamber=="2"&Canopy=="45"&date=="2009-03-20"&WTC_CO2<=650)
    subDF2 <- subset(myDF, Chamber=="2"&Canopy=="45"&date=="2009-03-18")
    outDF <- rbind(outDF, subDF1, subDF2)
    
    
    ### Chamber 1, 345
    subDF1 <- subset(myDF, Chamber=="1"&Canopy=="345")
    subDF2 <- subset(subDF1, WTC_CO2<800)
    subDF3 <- subset(subDF1, WTC_CO2>=800&Norm_corr_CO2_flux<=7.5)
    subDF4 <- rbind(subDF2, subDF3)
    subDF5 <- subset(subDF4, WTC_CO2<=300&Norm_corr_CO2_flux<=4)
    subDF6 <- subset(subDF4, WTC_CO2>300&Norm_corr_CO2_flux>=5)
    subDF7 <- rbind(subDF5, subDF6)
    outDF <- rbind(outDF, subDF7)
    
    
    #### Chamber 1, 12345
    subDF1 <- subset(myDF, Chamber=="1"&Canopy=="12345"&WTC_CO2<=1200&Norm_corr_CO2_flux<=7)
    subDF2 <- subset(subDF1, WTC_CO2<150&Norm_corr_CO2_flux<=2.5)
    subDF3 <- subset(subDF1, WTC_CO2>=150)
    subDF4 <- rbind(subDF2, subDF3)
    outDF <- rbind(outDF, subDF4)
    
    
    #### Chamber 1, canopy 45, delete outliers
    subDF <- subset(myDF, Chamber=="1"&Canopy=="45"&WTC_CO2<=1500)
    outDF <- rbind(outDF, subDF)
    
    #### Chamber 3, canopy 45, delete outliers
    subDF <- subset(myDF, Chamber=="3"&Canopy=="45"&WTC_CO2<=1550&Norm_corr_CO2_flux<=14)
    outDF <- rbind(outDF, subDF)
    
    ### Chamber 4, canopy 12345, delete outliers
    subDF <- subset(myDF, Chamber=="4"&Canopy=="12345"&WTC_CO2<=1200)
    outDF <- rbind(outDF, subDF)
    
    ### Chamer 4, 345, date 2009-03-19 ignore completely
    #subDF1 <- subset(myDF, Chamber=="4"&Canopy=="345"&date=="2009-03-18"&Norm_corr_CO2_flux<=12)
    #subDF2 <- subset(subDF1, WTC_CO2<360)
    #subDF3 <- subset(subDF1, WTC_CO2>=360&Norm_corr_CO2_flux>=5&Norm_corr_CO2_flux<=8)
    #subDF4 <- rbind(subDF2, subDF3)
    subDF4 <- subset(myDF, Chamber=="4"&Canopy=="345"&Norm_corr_CO2_flux<=9)
    outDF <- rbind(outDF, subDF4)
        
    ### Chamber 4, 45, ignore outliers
    subDF <- subset(myDF, Chamber=="4"&Canopy=="45"&WTC_CO2<=1000)
    outDF <- rbind(outDF, subDF)
    
    ### Chamber 7, 12345, ignore outliers
    subDF <- subset(myDF, Chamber=="7"&Canopy=="12345"&WTC_CO2<=1200)
    outDF <- rbind(outDF, subDF)
    
    ### Chamber 7, 345, ignore outliers
    subDF <- subset(myDF, Chamber=="7"&Canopy=="345"&WTC_CO2<=1500&Norm_corr_CO2_flux<=15)
    outDF <- rbind(outDF, subDF)
    
    ### Chamber 7, 45, ignore outliers
    subDF <- subset(myDF, Chamber=="7"&Canopy=="45"&WTC_CO2<=1500&Norm_corr_CO2_flux<=30)
    outDF <- rbind(outDF, subDF)
    
    ### Chamber 8, 12345, use two dates, ignore outliers
    subDF1 <- subset(myDF, Chamber=="8"&Canopy=="12345"&WTC_CO2<=1600)
    subDF2 <- subset(subDF1, WTC_CO2<220)
    subDF3 <- subset(subDF1, WTC_CO2>=220&Norm_corr_CO2_flux>=3.0&Norm_corr_CO2_flux<=5.5)
    subDF4 <- rbind(subDF2, subDF3)
    outDF <- rbind(outDF, subDF4)
    
    ### CHamber 8, 345
    subDF1 <- subset(myDF, Chamber=="8"&Canopy=="345"&date=="2009-03-20")
    subDF2 <- subset(myDF, Chamber=="8"&Canopy=="345"&date=="2009-03-19"&Norm_corr_CO2_flux<=8&WTC_CO2>=600)
    outDF <- rbind(outDF, subDF1, subDF2)
    
    ### Chamber 8, 45
    subDF <- subset(myDF, Chamber=="8"&Canopy=="45"&date=="2009-03-23"&Norm_corr_CO2_flux<=10&WTC_CO2<=1200)
    outDF <- rbind(outDF, subDF)
    
    
    ### Chamber 11, 45
    subDF <- subset(myDF, Chamber=="11"&Canopy=="45")#&WTC_CO2<=1400)
    outDF <- rbind(outDF, subDF)
    
    
    ### Chamber 11, 12345
    subDF <- subset(myDF, Chamber=="11"&Canopy=="12345"&WTC_CO2<=1400)
    outDF <- rbind(outDF, subDF)
    
    
    ### Chamber 12, 345
    subDF1 <- subset(myDF, Chamber=="12"&Canopy=="345"&date=="2009-03-25"&Norm_corr_CO2_flux<=15)
    subDF2 <- subset(subDF1, WTC_CO2<350)
    subDF3 <- subset(subDF1, WTC_CO2>=350&Norm_corr_CO2_flux>=6)
    subDF4 <- rbind(subDF2, subDF3)
    subDF5 <- subset(subDF4, WTC_CO2<1600)
    outDF <- rbind(outDF, subDF5)
    
    ### Chamber 12, 45
    subDF <- subset(myDF, Chamber=="12"&Canopy=="45"&WTC_CO2<=1300&Norm_corr_CO2_flux<=15)
    outDF <- rbind(outDF, subDF)
    
    ### Chamber 2, 12345
    subDF <- subset(myDF, Chamber=="2"&Canopy=="12345"&WTC_CO2<=1300)
    outDF <- rbind(outDF, subDF)
    
    ### return 
    return(outDF)
    
    
}