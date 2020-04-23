canopy_data_control_basic <- function(myDF) {
    
    ### Canopy code: 12345 full 
    ###              345 top + middle
    ###              45 top

    
    ### testing script
    #tDF1 <- subset(myDF, Chamber=="2"&Canopy=="45")
    #unique(tDF1$date)
    #with(tDF1, plot(Norm_corr_CO2_flux~WTC_CO2))
    #
    #tDF2 <- subset(tDF1, WTC_CO2<800&Norm_corr_CO2_flux<7)
    #with(tDF2, plot(Norm_corr_CO2_flux~WTC_CO2))
    #
    #
    #tDF3 <- subset(tDF1, WTC_CO2>=800&Norm_corr_CO2_flux<=8)
    #with(tDF3, plot(Norm_corr_CO2_flux~WTC_CO2))
    #
    #tDF4 <- rbind(tDF2, tDF3)
    #with(tDF4, plot(Norm_corr_CO2_flux~WTC_CO2))
    #
    #tDF5 <- subset(tDF4, WTC_CO2<=300&Norm_corr_CO2_flux<=4)
    #with(tDF5, plot(Norm_corr_CO2_flux~WTC_CO2))
    #
    #tDF6 <- subset(tDF4, WTC_CO2>300&Norm_corr_CO2_flux>=5)
    #with(tDF6, plot(Norm_corr_CO2_flux~WTC_CO2))
    #tDF7 <- rbind(tDF5, tDF6)
    #with(tDF7, plot(Norm_corr_CO2_flux~WTC_CO2))
    
    ### prepare outDF

    ### Chamber 2, canopy 45, delete outliers
    outDF <- subset(myDF, Chamber!="2"|Canopy!="45")
    subDF <- subset(myDF, Chamber=="2"&Canopy=="45"&date=="2009-03-20"&WTC_CO2<=650)
    outDF <- rbind(outDF, subDF)
    
    
    ### Chamber 1, 345
    subDF1 <- subset(outDF, Chamber=="1"&Canopy=="345")
    outDF <- subset(outDF, Chamber!="1"|Canopy!="345")
    subDF2 <- subset(subDF1, WTC_CO2<800)
    subDF3 <- subset(subDF1, WTC_CO2>=800&Norm_corr_CO2_flux<=7.5)
    subDF4 <- rbind(subDF2, subDF3)
    subDF5 <- subset(subDF4, WTC_CO2<=300&Norm_corr_CO2_flux<=4)
    subDF6 <- subset(subDF4, WTC_CO2>300&Norm_corr_CO2_flux>=5)
    subDF7 <- rbind(subDF5, subDF6)
    outDF <- rbind(outDF, subDF7)
    
    #### Chamber 1, 12345
    subDF1 <- subset(outDF, Chamber=="1"&Canopy=="12345"&WTC_CO2<=1200&Norm_corr_CO2_flux<=7)
    outDF <- subset(outDF, Chamber!="1"|Canopy!="12345")
    subDF2 <- subset(subDF1, WTC_CO2<150&Norm_corr_CO2_flux<=2.5)
    subDF3 <- subset(subDF1, WTC_CO2>=150)
    subDF4 <- rbind(subDF2, subDF3)
    outDF <- rbind(outDF, subDF4)
    
    
    #### Chamber 1, canopy 45, delete outliers
    subDF <- subset(outDF, Chamber=="1"&Canopy=="45"&WTC_CO2<=1500)
    outDF <- subset(outDF, Chamber!="1"|Canopy!="45")
    outDF <- rbind(outDF, subDF)
    
    #### Chamber 3, canopy 45, delete outliers
    #subDF <- subset(outDF, Chamber=="3"&Canopy=="45"&WTC_CO2<=1550&Norm_corr_CO2_flux<=14)
    #outDF <- subset(outDF, Chamber!="3"|Canopy!="45")
    #outDF <- rbind(outDF, subDF)
    #
    #### Chamber 4, canopy 12345, delete outliers
    #subDF <- subset(outDF, Chamber=="4"&Canopy=="12345"&WTC_CO2<=1200)
    #outDF <- subset(outDF, Chamber!="4"|Canopy!="12345")
    #outDF <- rbind(outDF, subDF)
    #
    #### Chamer 4, 345, date 2009-03-19 ignore completely
    #outDF <- subset(outDF, Chamber!="4"|Canopy!="345"|date!="2009-03-19")
    #
    #### Chamber 4, 345
    #subDF1 <- subset(outDF, Chamber=="4"&Canopy=="345"&date=="2009-03-18"&Norm_corr_CO2_flux<=12)
    #outDF <- subset(outDF, Chamber!="4"|Canopy!="345"|date!="2009-03-18")
    #subDF2 <- subset(subDF1, WTC_CO2<360)
    #subDF3 <- subset(subDF1, WTC_CO2>=360&Norm_corr_CO2_flux>=5&Norm_corr_CO2_flux<=8)
    #subDF4 <- rbind(subDF2, subDF3)
    #outDF <- rbind(outDF, subDF4)
#
    #    
    #### Chamber 4, 45, ignore outliers
    #subDF <- subset(outDF, Chamber=="4"&Canopy=="45"&WTC_CO2<=1000)
    #outDF <- subset(outDF, Chamber!="4"|Canopy!="45")
    #outDF <- rbind(outDF, subDF)
    #
    #### Chamber 7, 12345, ignore outliers
    #subDF <- subset(outDF, Chamber=="7"&Canopy=="12345"&WTC_CO2<=1200)
    #outDF <- subset(outDF, Chamber!="7"|Canopy!="12345")
    #outDF <- rbind(outDF, subDF)
    #
    #### Chamer 7, 345, date 2009-03-23 ignore completely
    #outDF <- subset(outDF, Chamber!="7"|Canopy!="345"|date!="2009-03-23")
    #
    #### Chamber 7, 345, ignore outliers
    #subDF <- subset(outDF, Chamber=="7"&Canopy=="345"&WTC_CO2<=1500&Norm_corr_CO2_flux<=15)
    #outDF <- subset(outDF, Chamber!="7"|Canopy!="345")
    #outDF <- rbind(outDF, subDF)
    #
    #### chamber 7, 45, ignore date 2009-03-24
    #outDF <- subset(outDF, Chamber!="7"|Canopy!="45"|date!="2009-03-24")
    #
    #### Chamber 7, 45, ignore outliers
    #subDF <- subset(outDF, Chamber=="7"&Canopy=="45"&WTC_CO2<=1500&Norm_corr_CO2_flux<=30)
    #outDF <- subset(outDF, Chamber!="7"|Canopy!="45")
    #outDF <- rbind(outDF, subDF)
    #
    #### Chamber 8, 12345, use two dates, ignore outliers
    #subDF1 <- subset(outDF, Chamber=="8"&Canopy=="12345"&WTC_CO2<=1650)
    #outDF <- subset(outDF, Chamber!="8"|Canopy!="12345")
    #subDF2 <- subset(subDF1, WTC_CO2<200)
    #subDF3 <- subset(subDF1, WTC_CO2>=200&Norm_corr_CO2_flux>=2.5&Norm_corr_CO2_flux<=6.5)
    #subDF4 <- rbind(subDF2, subDF3)
    #outDF <- rbind(outDF, subDF4)
    #
    #### CHamber 8, 345
    #subDF <- subset(outDF, Chamber=="8"&Canopy=="345"&date=="2009-03-20"&WTC_CO2<=650)
    #outDF <- subset(outDF, Chamber!="8"|Canopy!="345")
    #outDF <- rbind(outDF, subDF)
    #
    #### Chamber 8, 45
    #subDF <- subset(outDF, Chamber=="8"&Canopy=="45"&date=="2009-03-23"&Norm_corr_CO2_flux<=10&WTC_CO2<=1200)
    #outDF <- subset(outDF, Chamber!="8"|Canopy!="45")
    #outDF <- rbind(outDF, subDF)
    #
    #
    #### Chamber 11, 45
    #subDF <- subset(outDF, Chamber=="11"&Canopy=="45"&WTC_CO2<=1400)
    #outDF <- subset(outDF, Chamber!="11"|Canopy!="45")
    #outDF <- rbind(outDF, subDF)
    #
    #
    #### Chamber 11, 12345
    #subDF <- subset(outDF, Chamber=="11"&Canopy=="12345"&WTC_CO2<=1400)
    #outDF <- subset(outDF, Chamber!="11"|Canopy!="12345")
    #outDF <- rbind(outDF, subDF)
    #
    #
    #### Chamber 12, 345
    #subDF1 <- subset(outDF, Chamber=="12"&Canopy=="345"&date=="2009-03-25"&Norm_corr_CO2_flux<=15)
    #outDF <- subset(outDF, Chamber!="12"|Canopy!="345")
    #subDF2 <- subset(subDF1, WTC_CO2<350)
    #subDF3 <- subset(subDF1, WTC_CO2>=350&Norm_corr_CO2_flux>=6)
    #subDF4 <- rbind(subDF2, subDF3)
    #subDF5 <- subset(subDF4, WTC_CO2<1600)
    #outDF <- rbind(outDF, subDF5)
    #
    #### Chamber 12, 45
    #subDF <- subset(outDF, Chamber=="12"&Canopy=="45"&WTC_CO2<=1300&Norm_corr_CO2_flux<=15)
    #outDF <- subset(outDF, Chamber!="12"|Canopy!="45")
    #outDF <- rbind(outDF, subDF)
    #
    #### Chamber 2, 12345
    #subDF <- subset(outDF, Chamber=="2"&Canopy=="12345"&WTC_CO2<=1300)
    #outDF <- subset(outDF, Chamber!="2"|Canopy!="12345")
    #outDF <- rbind(outDF, subDF)
    
    ### return 
    return(outDF)
    
    
}