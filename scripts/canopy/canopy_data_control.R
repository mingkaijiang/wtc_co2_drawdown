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
    #with(tDF1, plot(vCo2~vtime))
    #with(tDF2, plot(vCo2~vtime))
    
    ### Chamber 2, 45
    tDF1 <- subset(myDF, Chamber=="2"&Canopy=="45"&date=="2009-03-18"&vCo2>=700)
    tDF2 <- subset(myDF, Chamber=="2"&Canopy=="45"&date=="2009-03-20"&vCo2<=700)
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
    
    tDF1 <- subset(myDF, Chamber=="4"&Canopy=="345"&date=="2009-03-18"&vCo2>=500)
    tDF2 <- subset(myDF, Chamber=="4"&Canopy=="345"&date=="2009-03-19"&vCo2<=500)
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
    
    
    #### Chamber 12, 345, drop second half, very problematic data
    #myDF.p3 <- myDF.p2[!c(myDF.p2$Chamber=="12"&myDF.p2$Canopy=="345"&myDF.p2$date=="2009-03-25"),] 
    #
    #
    #### Chamber 8, 45
    #tDF1 <- subset(myDF, Chamber=="8"&Canopy=="45"&date=="2009-03-20"&vCo2>=1400)
    #tDF2 <- subset(myDF, Chamber=="8"&Canopy=="45"&date=="2009-03-23"&vCo2<=1400)
    #tDF2 <- tDF2[4:length(tDF2$Chamber),]
    #tDF3 <- rbind(tDF1, tDF2)
    #
    ## new date starts on nd row
    #nd <- length(tDF1$Chamber)+1
    #td <- length(tDF3$Chamber)
    #time.diff1 <- tDF3$datetime[nd] - (tDF3$datetime[nd-1] + 60)
    #time.diff3 <- tDF3$time[nd] - (tDF3$time[nd-1] + 60)
    #
    #for (i in nd:td) {
    #    tDF3$datetime[i] <- tDF3$datetime[i] - time.diff1
    #    tDF3$time[i] <- tDF3$time[i] - time.diff3
    #}
    #
#
    ## add back this dataframe
    #myDF.p4 <- myDF.p3[!c(myDF.p3$Chamber=="8"&myDF.p3$Canopy=="45"),] 
    #myDF.p4 <- rbind(myDF.p4, tDF3)    
    #
    #
    #### combine 1st and 2nd half for Chamber 8, 345
    #tDF1 <- subset(myDF, Chamber=="8"&Canopy=="345"&date=="2009-03-19"&vCo2>=700)
    #tDF2 <- subset(myDF, Chamber=="8"&Canopy=="345"&date=="2009-03-20"&vCo2<=700)
    #tDF3 <- rbind(tDF1, tDF2)
    #
    ## new date starts on nd row
    #nd <- length(tDF1$Chamber)+1
    #td <- length(tDF3$Chamber)
    #time.diff1 <- tDF3$datetime[nd] - (tDF3$datetime[nd-1] + 60)
    #time.diff3 <- tDF3$time[nd] - (tDF3$time[nd-1] + 60)
    #
    #for (i in nd:td) {
    #    tDF3$datetime[i] <- tDF3$datetime[i] - time.diff1
    #    tDF3$time[i] <- tDF3$time[i] - time.diff3
    #}
    #
#
    ## add back this dataframe
    #myDF.p5 <- myDF.p4[!c(myDF.p4$Chamber=="8"&myDF.p4$Canopy=="345"),] 
    #myDF.p5 <- rbind(myDF.p5, tDF3)    

    
    #### Chamber 8, 12345
    myDF.p5 <- myDF.p2
    tDF1 <- subset(myDF, Chamber=="8"&Canopy=="12345"&date=="2009-03-18"&vCo2>=350)
    tDF2 <- subset(myDF, Chamber=="8"&Canopy=="12345"&date=="2009-03-19"&vCo2<=350)
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
    #tDF1 <- subset(myDF, Chamber=="7"&Canopy=="45"&date=="2009-03-24"&vCo2>=1450)
    #tDF2 <- subset(myDF, Chamber=="7"&Canopy=="45"&date=="2009-03-25"&vCo2<=1450)
    #tDF3 <- rbind(tDF1, tDF2)
    #
    ## new date starts on nd row
    #nd <- length(tDF1$Chamber)+1
    #td <- length(tDF3$Chamber)
    #time.diff1 <- tDF3$datetime[nd] - (tDF3$datetime[nd-1] + 60)
    #time.diff3 <- tDF3$time[nd] - (tDF3$time[nd-1] + 60)
    #
    #for (i in nd:td) {
    #    tDF3$datetime[i] <- tDF3$datetime[i] - time.diff1
    #    tDF3$time[i] <- tDF3$time[i] - time.diff3
    #}
    #
    ## add back this dataframe
    #myDF.p7 <- myDF.p6[!c(myDF.p6$Chamber=="7"&myDF.p6$Canopy=="45"),] 
    #myDF.p7 <- rbind(myDF.p7, tDF3)  
    
    
    return(myDF.p6)
    
    
}