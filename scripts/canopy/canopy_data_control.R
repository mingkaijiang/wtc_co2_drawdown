canopy_data_control <- function(myDF) {
    ### smooth Canopy time series data 
    ### Chamber 2: top Canopy, link two periods together
    ### Chamber 4: top + middle
    ### Chamber 8: all
    ### Chamber 12: top + middle
    
    ### Canopy code: 12345 full 
    ###              345 top + middle
    ###              45 top
    
    ### three options: one is to drop the second half of the data entirely
    ###                two is to merge the two time series with some regression analysis
    ###                three is to hard merge the two time series 
    
    
    #test <- subset(myDF, Chamber=="8"&Canopy=="12345")
    #tDF1 <- subset(myDF, Chamber=="8"&Canopy=="12345"&date=="2009-03-18")
    #tDF2 <- subset(myDF, Chamber=="8"&Canopy=="12345"&date=="2009-03-19")
    #
    #with(tDF1, plot(WTC_CO2~time))
    #with(tDF2, plot(WTC_CO2~time))
    
    ### Chamber 2, 45
    tDF1 <- subset(myDF, Chamber=="2"&Canopy=="45"&date=="2009-03-18"&WTC_CO2>=700)
    tDF2 <- subset(myDF, Chamber=="2"&Canopy=="45"&date=="2009-03-20"&WTC_CO2<=700)
    tDF3 <- rbind(tDF1, tDF2)
    
    # new date starts on nd row
    nd <- length(tDF1$Chamber)+1
    td <- length(tDF3$Chamber)
    time.diff1 <- tDF3$datetime[nd] - (tDF3$datetime[nd-1] + 60)
    time.diff3 <- tDF3$time[nd] - (tDF3$time[nd-1] + 60)
    
    for (i in nd:td) {
        tDF3$datetime[i] <- tDF3$datetime[i] - time.diff1
        tDF3$time[i] <- tDF3$time[i] - time.diff3
    }
    
    # add back this dataframe
    myDF.p1 <- myDF[!c(myDF$Chamber=="2"&myDF$Canopy=="45"),] 
    myDF.p1 <- rbind(myDF.p1, tDF3)   
    
    ### Chamber 4, 345
    myDF.p2 <- myDF.p1[!c(myDF.p1$Chamber=="4"&myDF.p1$Canopy=="345"&myDF.p1$date=="2009-03-19"),] 
    
    tDF1 <- subset(myDF, Chamber=="4"&Canopy=="345"&date=="2009-03-18"&WTC_CO2>=500)
    tDF2 <- subset(myDF, Chamber=="4"&Canopy=="345"&date=="2009-03-19"&WTC_CO2<=500)
    tDF2 <- tDF2[7:length(tDF2$Chamber),]
    tDF3 <- rbind(tDF1, tDF2)
    
    # new date starts on nd row
    nd <- length(tDF1$Chamber)+1
    td <- length(tDF3$Chamber)
    time.diff1 <- tDF3$datetime[nd] - (tDF3$datetime[nd-1] + 60)
    time.diff3 <- tDF3$time[nd] - (tDF3$time[nd-1] + 60)
    
    for (i in nd:td) {
        tDF3$datetime[i] <- tDF3$datetime[i] - time.diff1
        tDF3$time[i] <- tDF3$time[i] - time.diff3
    }
    
    # add back this dataframe
    myDF.p2 <- myDF.p1[!c(myDF.p1$Chamber=="4"&myDF.p1$Canopy=="345"),] 
    myDF.p2 <- rbind(myDF.p2, tDF3)    

    
    #### Chamber 8, 12345
    myDF.p5 <- myDF.p2
    tDF1 <- subset(myDF, Chamber=="8"&Canopy=="12345"&date=="2009-03-18"&WTC_CO2>=350)
    tDF2 <- subset(myDF, Chamber=="8"&Canopy=="12345"&date=="2009-03-19"&WTC_CO2<=350)
    tDF3 <- rbind(tDF1, tDF2)
    
    # new date starts on nd row
    nd <- length(tDF1$Chamber)+1
    td <- length(tDF3$Chamber)
    time.diff1 <- tDF3$datetime[nd] - (tDF3$datetime[nd-1] + 60)
    time.diff3 <- tDF3$time[nd] - (tDF3$time[nd-1] + 60)
    
    for (i in nd:td) {
        tDF3$datetime[i] <- tDF3$datetime[i] - time.diff1
        tDF3$time[i] <- tDF3$time[i] - time.diff3
    }
    
    ## add back this dataframe
    myDF.p6 <- myDF.p5[!c(myDF.p5$Chamber=="8"&myDF.p5$Canopy=="12345"),] 
    myDF.p6 <- rbind(myDF.p6, tDF3)  
    
    
    #### Chamber 7, 45
    tDF1 <- subset(myDF, Chamber=="7"&Canopy=="45"&date=="2009-03-24"&WTC_CO2>=1450)
    tDF2 <- subset(myDF, Chamber=="7"&Canopy=="45"&date=="2009-03-25"&WTC_CO2<=1450)
    tDF3 <- rbind(tDF1, tDF2)
    
    # new date starts on nd row
    nd <- length(tDF1$Chamber)+1
    td <- length(tDF3$Chamber)
    time.diff1 <- tDF3$datetime[nd] - (tDF3$datetime[nd-1] + 60)
    time.diff3 <- tDF3$time[nd] - (tDF3$time[nd-1] + 60)
    
    for (i in nd:td) {
        tDF3$datetime[i] <- tDF3$datetime[i] - time.diff1
        tDF3$time[i] <- tDF3$time[i] - time.diff3
    }
    
    # add back this dataframe
    myDF.p7 <- myDF.p6[!c(myDF.p6$Chamber=="7"&myDF.p6$Canopy=="45"),] 
    myDF.p7 <- rbind(myDF.p7, tDF3)  
    
    
    #### Chamber 7, 345
    tDF1 <- subset(myDF, Chamber=="7"&Canopy=="345"&date=="2009-03-23"&WTC_CO2>=1450)
    tDF2 <- subset(myDF, Chamber=="7"&Canopy=="345"&date=="2009-03-24"&WTC_CO2<=1450)
    tDF3 <- rbind(tDF1, tDF2)
    
    # new date starts on nd row
    nd <- length(tDF1$Chamber)+1
    td <- length(tDF3$Chamber)
    time.diff1 <- tDF3$datetime[nd] - (tDF3$datetime[nd-1] + 60)
    time.diff3 <- tDF3$time[nd] - (tDF3$time[nd-1] + 60)
    
    for (i in nd:td) {
        tDF3$datetime[i] <- tDF3$datetime[i] - time.diff1
        tDF3$time[i] <- tDF3$time[i] - time.diff3
    }
    
    # add back this dataframe
    myDF.p8 <- myDF.p7[!c(myDF.p7$Chamber=="7"&myDF.p7$Canopy=="345"),] 
    myDF.p8 <- rbind(myDF.p8, tDF3)  
    
    
    
    #### Chamber 8, 45
    tDF1 <- subset(myDF, Chamber=="8"&Canopy=="45"&date=="2009-03-20"&WTC_CO2>=1000)
    tDF2 <- subset(myDF, Chamber=="8"&Canopy=="45"&date=="2009-03-23"&WTC_CO2<=1000)
    tDF3 <- rbind(tDF1, tDF2)
    
    # new date starts on nd row
    nd <- length(tDF1$Chamber)+1
    td <- length(tDF3$Chamber)
    time.diff1 <- tDF3$datetime[nd] - (tDF3$datetime[nd-1] + 60)
    time.diff3 <- tDF3$time[nd] - (tDF3$time[nd-1] + 60)
    
    for (i in nd:td) {
        tDF3$datetime[i] <- tDF3$datetime[i] - time.diff1
        tDF3$time[i] <- tDF3$time[i] - time.diff3
    }
    
    # add back this dataframe
    myDF.p9 <- myDF.p8[!c(myDF.p8$Chamber=="8"&myDF.p8$Canopy=="45"),] 
    myDF.p9 <- rbind(myDF.p9, tDF3)  
    

    #### Chamber 8, 345
    tDF1 <- subset(myDF, Chamber=="8"&Canopy=="345"&date=="2009-03-19"&WTC_CO2>=400)
    tDF2 <- subset(myDF, Chamber=="8"&Canopy=="345"&date=="2009-03-20"&WTC_CO2<=400)
    tDF3 <- rbind(tDF1, tDF2)
    
    # new date starts on nd row
    nd <- length(tDF1$Chamber)+1
    td <- length(tDF3$Chamber)
    time.diff1 <- tDF3$datetime[nd] - (tDF3$datetime[nd-1] + 60)
    time.diff3 <- tDF3$time[nd] - (tDF3$time[nd-1] + 60)
    
    for (i in nd:td) {
        tDF3$datetime[i] <- tDF3$datetime[i] - time.diff1
        tDF3$time[i] <- tDF3$time[i] - time.diff3
    }
    
    # add back this dataframe
    myDF.p10 <- myDF.p9[!c(myDF.p9$Chamber=="8"&myDF.p9$Canopy=="345"),] 
    myDF.p10 <- rbind(myDF.p10, tDF3)  
    
    ### return 
    return(myDF.p10)
    
    
}