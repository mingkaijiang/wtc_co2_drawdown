canopy_data_control_old <- function(myDF) {
    ### smooth canopy time series data 
    ### chamber 2: top canopy, link two periods together
    ### chamber 4: top + middle
    ### chamber 8: all
    ### chamber 12: top + middle
    
    ### canopy code: 12345 full 
    ###              345 top + middle
    ###              45 top
    
    ### three options: one is to drop the second half of the data entirely
    ###                two is to merge the two time series with some regression analysis
    ###                three is to hard merge the two time series 
    
    ### chamber 2, 45
    tDF1 <- subset(myDF, chamber=="2"&canopy=="45"&date=="2009-03-18"&vCo2>=700)
    tDF2 <- subset(myDF, chamber=="2"&canopy=="45"&date=="2009-03-20"&vCo2<=700)
    tDF3 <- rbind(tDF1, tDF2)
    
    # new date starts on nd row
    nd <- length(tDF1$chamber)+1
    td <- length(tDF3$chamber)
    time.diff1 <- tDF3$datetime[nd] - (tDF3$datetime[nd-1] + 60)
    time.diff2 <- tDF3$vtime[nd] - (tDF3$vtime[nd-1] + 60)
    time.diff3 <- tDF3$time[nd] - (tDF3$time[nd-1] + 60)
    
    for (i in nd:td) {
        tDF3$datetime[i] <- tDF3$datetime[i] - time.diff1
        tDF3$vtime[i] <- tDF3$vtime[i] - time.diff2
        tDF3$time[i] <- tDF3$time[i] - time.diff3
    }
    
    tDF3$date <- as.Date(tDF3$vtime)
    
    # add back this dataframe
    myDF.p1 <- myDF[!c(myDF$chamber=="2"&myDF$canopy=="45"),] 
    myDF.p1 <- rbind(myDF.p1, tDF3)   
    
    ### chamber 4, 345
    myDF.p2 <- myDF.p1[!c(myDF.p1$chamber=="4"&myDF.p1$canopy=="345"&myDF.p1$date=="2009-03-19"),] 
    
    tDF1 <- subset(myDF, chamber=="4"&canopy=="345"&date=="2009-03-18"&vCo2>=500)
    tDF2 <- subset(myDF, chamber=="4"&canopy=="345"&date=="2009-03-19"&vCo2<=500)
    tDF2 <- tDF2[7:length(tDF2$chamber),]
    tDF3 <- rbind(tDF1, tDF2)
    
    # new date starts on nd row
    nd <- length(tDF1$chamber)+1
    td <- length(tDF3$chamber)
    time.diff1 <- tDF3$datetime[nd] - (tDF3$datetime[nd-1] + 60)
    time.diff2 <- tDF3$vtime[nd] - (tDF3$vtime[nd-1] + 60)
    time.diff3 <- tDF3$time[nd] - (tDF3$time[nd-1] + 60)
    
    for (i in nd:td) {
        tDF3$datetime[i] <- tDF3$datetime[i] - time.diff1
        tDF3$vtime[i] <- tDF3$vtime[i] - time.diff2
        tDF3$time[i] <- tDF3$time[i] - time.diff3
    }
    
    tDF3$date <- as.Date(tDF3$vtime)
    
    # add back this dataframe
    myDF.p2 <- myDF.p1[!c(myDF.p1$chamber=="4"&myDF.p1$canopy=="345"),] 
    myDF.p2 <- rbind(myDF.p2, tDF3)    
    
    ### chamber 12, 345, drop second half, very problematic data
    myDF.p3 <- myDF.p2[!c(myDF.p2$chamber=="12"&myDF.p2$canopy=="345"&myDF.p2$date=="2009-03-25"),] 
    
    
    ### chamber 8, 45
    tDF1 <- subset(myDF, chamber=="8"&canopy=="45"&date=="2009-03-20"&vCo2>=1400)
    tDF2 <- subset(myDF, chamber=="8"&canopy=="45"&date=="2009-03-23"&vCo2<=1400)
    tDF2 <- tDF2[4:length(tDF2$chamber),]
    tDF3 <- rbind(tDF1, tDF2)
    
    # new date starts on nd row
    nd <- length(tDF1$chamber)+1
    td <- length(tDF3$chamber)
    time.diff1 <- tDF3$datetime[nd] - (tDF3$datetime[nd-1] + 60)
    time.diff2 <- tDF3$vtime[nd] - (tDF3$vtime[nd-1] + 60)
    time.diff3 <- tDF3$time[nd] - (tDF3$time[nd-1] + 60)
    
    for (i in nd:td) {
        tDF3$datetime[i] <- tDF3$datetime[i] - time.diff1
        tDF3$vtime[i] <- tDF3$vtime[i] - time.diff2
        tDF3$time[i] <- tDF3$time[i] - time.diff3
    }
    
    tDF3$date <- as.Date(tDF3$vtime)
    
    # add back this dataframe
    myDF.p4 <- myDF.p3[!c(myDF.p3$chamber=="8"&myDF.p3$canopy=="45"),] 
    myDF.p4 <- rbind(myDF.p4, tDF3)    
    
    
    ### combine 1st and 2nd half for chamber 8, 345
    tDF1 <- subset(myDF, chamber=="8"&canopy=="345"&date=="2009-03-19"&vCo2>=700)
    tDF2 <- subset(myDF, chamber=="8"&canopy=="345"&date=="2009-03-20"&vCo2<=700)
    tDF3 <- rbind(tDF1, tDF2)
    
    # new date starts on nd row
    nd <- length(tDF1$chamber)+1
    td <- length(tDF3$chamber)
    time.diff1 <- tDF3$datetime[nd] - (tDF3$datetime[nd-1] + 60)
    time.diff2 <- tDF3$vtime[nd] - (tDF3$vtime[nd-1] + 60)
    time.diff3 <- tDF3$time[nd] - (tDF3$time[nd-1] + 60)
    
    for (i in nd:td) {
        tDF3$datetime[i] <- tDF3$datetime[i] - time.diff1
        tDF3$vtime[i] <- tDF3$vtime[i] - time.diff2
        tDF3$time[i] <- tDF3$time[i] - time.diff3
    }
    
    tDF3$date <- as.Date(tDF3$vtime)
    
    # add back this dataframe
    myDF.p5 <- myDF.p4[!c(myDF.p4$chamber=="8"&myDF.p4$canopy=="345"),] 
    myDF.p5 <- rbind(myDF.p5, tDF3)    

    
    ### chamber 8, 12345
    tDF1 <- subset(myDF, chamber=="8"&canopy=="12345"&date=="2009-03-18"&vCo2>=350)
    tDF2 <- subset(myDF, chamber=="8"&canopy=="12345"&date=="2009-03-19"&vCo2<=350)
    tDF3 <- rbind(tDF1, tDF2)
    
    # new date starts on nd row
    nd <- length(tDF1$chamber)+1
    td <- length(tDF3$chamber)
    time.diff1 <- tDF3$datetime[nd] - (tDF3$datetime[nd-1] + 60)
    time.diff2 <- tDF3$vtime[nd] - (tDF3$vtime[nd-1] + 60)
    time.diff3 <- tDF3$time[nd] - (tDF3$time[nd-1] + 60)
    
    for (i in nd:td) {
        tDF3$datetime[i] <- tDF3$datetime[i] - time.diff1
        tDF3$vtime[i] <- tDF3$vtime[i] - time.diff2
        tDF3$time[i] <- tDF3$time[i] - time.diff3
    }
    
    tDF3$date <- as.Date(tDF3$vtime)
    
    # add back this dataframe
    myDF.p6 <- myDF.p5[!c(myDF.p5$chamber=="8"&myDF.p5$canopy=="12345"),] 
    myDF.p6 <- rbind(myDF.p6, tDF3)  
    
    return(myDF.p6)
    
    #test <- subset(myDF, chamber=="8"&canopy=="12345")
    #tDF1 <- subset(myDF, chamber=="8"&canopy=="12345"&date=="2009-03-18")
    #tDF2 <- subset(myDF, chamber=="8"&canopy=="12345"&date=="2009-03-19")
    #
    #with(tDF1, plot(vCo2~vtime))
    #with(tDF2, plot(vCo2~vtime))
    
    
}