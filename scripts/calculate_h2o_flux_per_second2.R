calculate_h2o_flux_per_second2 <- function(myDF) {
    ### This function fit individual chamber and canopy data 
    ### to obtain the flux of co2 per second based on minute data
    
    ######## chamber 1, canopy 12345
    tDF1 <- subset(myDF, chamber == "1" & canopy == "12345")
    
    ### find row number with maximum CO2 concentration
    r.n <- which.max(tDF1$vCo2)
    
    ### find total row numbers (excluding pre-treatment rows)
    r.t <- length(tDF1$chamber[r.n:dim(tDF1)[1]])
    
    ### set up time points
    for (i in 1:dim(tDF1)[1]) {
        tDF1[i, "time_elapsed"] <- round(as.numeric(difftime(tDF1[i, "datetime"], tDF1[r.n, "datetime"], units="mins")),1)
    }
    
    #tDF1[r.n:dim(tDF1)[1], "time_elapsed"] <- 0:(r.t-1)
    #tDF1[1:r.n, "time_elapsed"] <- -(r.n-1):0
    
    
    ######## chamber 1, canopy 345
    tDF2 <- subset(myDF, chamber == "1" & canopy == "345")
    
    ### find row number with maximum CO2 concentration
    r.n <- which.max(tDF2$vCo2)
    
    ### fine total row numbers (excluding pre-treatment rows)
    r.t <- length(tDF2$chamber[r.n:dim(tDF2)[1]])
    
    ### set up time points
    for (i in 1:dim(tDF2)[1]) {
        tDF2[i, "time_elapsed"] <- round(as.numeric(difftime(tDF2[i, "datetime"], tDF2[r.n, "datetime"], units="mins")),1)
    }
    
    ######## chamber 1, canopy 45
    tDF3 <- subset(myDF, chamber == "1" & canopy == "45")
    
    ### find row number with maximum CO2 concentration
    r.n <- which.max(tDF3$vCo2)
    
    ### fine total row numbers (excluding pre-treatment rows)
    r.t <- length(tDF3$chamber[r.n:dim(tDF3)[1]])
    
    ### set up time points
    for (i in 1:dim(tDF3)[1]) {
        tDF3[i, "time_elapsed"] <- round(as.numeric(difftime(tDF3[i, "datetime"], tDF3[r.n, "datetime"], units="mins")),1)
    }
    
    
    ######## chamber 2, canopy 12345
    tDF4 <- subset(myDF, chamber == "2" & canopy == "12345")
    
    ### find row number with maximum CO2 concentration
    r.n <- which.max(tDF4$vCo2)
    
    ### fine total row numbers (excluding pre-treatment rows)
    r.t <- length(tDF4$chamber[r.n:dim(tDF4)[1]])
    
    ### set up time points
    for (i in 1:dim(tDF4)[1]) {
        tDF4[i, "time_elapsed"] <- round(as.numeric(difftime(tDF4[i, "datetime"], tDF4[r.n, "datetime"], units="mins")),1)
    }
    
    
    ######## chamber 2, canopy 345
    tDF5 <- subset(myDF, chamber == "2" & canopy == "345")
    
    ### find row number with maximum CO2 concentration
    r.n <- which.max(tDF5$vCo2)
    
    ### fine total row numbers (excluding pre-treatment rows)
    r.t <- length(tDF5$chamber[r.n:dim(tDF5)[1]])
    
    ### set up time points
    for (i in 1:dim(tDF5)[1]) {
        tDF5[i, "time_elapsed"] <- round(as.numeric(difftime(tDF5[i, "datetime"], tDF5[r.n, "datetime"], units="mins")),1)
    }
    
    
    ######## chamber 2, canopy 45
    ### Note: this chamber and canopy has a time gap of two days!
    tDF6 <- subset(myDF, chamber == "2" & canopy == "45")
    
    ### find row number with maximum CO2 concentration
    r.n <- which.max(tDF6$vCo2)
    
    ### fine total row numbers (excluding pre-treatment rows)
    r.t <- length(tDF6$chamber[r.n:dim(tDF6)[1]])
    
    ### set up time points
    for (i in 1:dim(tDF6)[1]) {
        tDF6[i, "time_elapsed"] <- round(as.numeric(difftime(tDF6[i, "datetime"], tDF6[r.n, "datetime"], units="mins")),1)
    }
    
    
    ######## chamber 3, canopy 12345
    tDF7 <- subset(myDF, chamber == "3" & canopy == "12345")
    
    ### find row number with maximum CO2 concentration
    r.n <- which.max(tDF7$vCo2)
    
    ### fine total row numbers (excluding pre-treatment rows)
    r.t <- length(tDF7$chamber[r.n:dim(tDF7)[1]])
    
    ### set up time points
    for (i in 1:dim(tDF7)[1]) {
        tDF7[i, "time_elapsed"] <- round(as.numeric(difftime(tDF7[i, "datetime"], tDF7[r.n, "datetime"], units="mins")),1)
    }
    
    
    ######## chamber 3, canopy 345
    tDF8 <- subset(myDF, chamber == "3" & canopy == "345")
    
    ### find row number with maximum CO2 concentration
    r.n <- which.max(tDF8$vCo2)
    
    ### fine total row numbers (excluding pre-treatment rows)
    r.t <- length(tDF8$chamber[r.n:dim(tDF8)[1]])
    
    ### set up time points
    for (i in 1:dim(tDF8)[1]) {
        tDF8[i, "time_elapsed"] <- round(as.numeric(difftime(tDF8[i, "datetime"], tDF8[r.n, "datetime"], units="mins")),1)
    }
    
    
    ######## chamber 3, canopy 45
    tDF9 <- subset(myDF, chamber == "3" & canopy == "45")
    
    ### find row number with maximum CO2 concentration
    r.n <- which.max(tDF9$vCo2)
    
    ### fine total row numbers (excluding pre-treatment rows)
    r.t <- length(tDF9$chamber[r.n:dim(tDF9)[1]])
    
    ### set up time points
    for (i in 1:dim(tDF9)[1]) {
        tDF9[i, "time_elapsed"] <- round(as.numeric(difftime(tDF9[i, "datetime"], tDF9[r.n, "datetime"], units="mins")),1)
    }
    
    
    ######## chamber 4, canopy 12345
    tDF10 <- subset(myDF, chamber == "4" & canopy == "12345")
    
    ### find row number with maximum CO2 concentration
    r.n <- which.max(tDF10$vCo2)
    
    ### fine total row numbers (excluding pre-treatment rows)
    r.t <- length(tDF10$chamber[r.n:dim(tDF10)[1]])
    
    ### set up time points
    for (i in 1:dim(tDF10)[1]) {
        tDF10[i, "time_elapsed"] <- round(as.numeric(difftime(tDF10[i, "datetime"], tDF10[r.n, "datetime"], units="mins")),1)
    }
    
    
    ######## chamber 4, canopy 345
    tDF11 <- subset(myDF, chamber == "4" & canopy == "345")
    
    ### find row number with maximum CO2 concentration
    r.n <- which.max(tDF11$vCo2)
    
    ### fine total row numbers (excluding pre-treatment rows)
    r.t <- length(tDF11$chamber[r.n:dim(tDF11)[1]])
    
    ### set up time points
    for (i in 1:dim(tDF11)[1]) {
        tDF11[i, "time_elapsed"] <- round(as.numeric(difftime(tDF11[i, "datetime"], tDF11[r.n, "datetime"], units="mins")),1)
    }
    
    
    ######## chamber 4, canopy 45
    tDF12 <- subset(myDF, chamber == "4" & canopy == "45")
    
    ### find row number with maximum CO2 concentration
    r.n <- which.max(tDF12$vCo2)
    
    ### fine total row numbers (excluding pre-treatment rows)
    r.t <- length(tDF12$chamber[r.n:dim(tDF12)[1]])
    
    ### set up time points
    for (i in 1:dim(tDF12)[1]) {
        tDF12[i, "time_elapsed"] <- round(as.numeric(difftime(tDF12[i, "datetime"], tDF12[r.n, "datetime"], units="mins")),1)
    }
    
    
    ######## chamber 7, canopy 12345
    tDF13 <- subset(myDF, chamber == "7" & canopy == "12345")
    
    ### find row number with maximum CO2 concentration
    r.n <- which.max(tDF13$vCo2)
    
    ### fine total row numbers (excluding pre-treatment rows)
    r.t <- length(tDF13$chamber[r.n:dim(tDF13)[1]])
    
    ### set up time points
    for (i in 1:dim(tDF13)[1]) {
        tDF13[i, "time_elapsed"] <- round(as.numeric(difftime(tDF13[i, "datetime"], tDF13[r.n, "datetime"], units="mins")),1)
    }
    
    
    ######## chamber 7, canopy 345
    tDF14 <- subset(myDF, chamber == "7" & canopy == "345")
    
    ### find row number with maximum CO2 concentration
    r.n <- which.max(tDF14$vCo2)
    
    ### fine total row numbers (excluding pre-treatment rows)
    r.t <- length(tDF14$chamber[r.n:dim(tDF14)[1]])
    
    ### set up time points
    for (i in 1:dim(tDF14)[1]) {
        tDF14[i, "time_elapsed"] <- round(as.numeric(difftime(tDF14[i, "datetime"], tDF14[r.n, "datetime"], units="mins")),1)
    }
    
    
    ######## chamber 7, canopy 45
    tDF15 <- subset(myDF, chamber == "7" & canopy == "45")
    
    ### find row number with maximum CO2 concentration
    r.n <- which.max(tDF15$vCo2)
    
    ### fine total row numbers (excluding pre-treatment rows)
    r.t <- length(tDF15$chamber[r.n:dim(tDF15)[1]])
    
    ### set up time points
    for (i in 1:dim(tDF15)[1]) {
        tDF15[i, "time_elapsed"] <- round(as.numeric(difftime(tDF15[i, "datetime"], tDF15[r.n, "datetime"], units="mins")),1)
    }
    
    
    ######## chamber 8, canopy 12345
    tDF16 <- subset(myDF, chamber == "8" & canopy == "12345")
    
    ### find row number with maximum CO2 concentration
    r.n <- which.max(tDF16$vCo2)
    
    ### fine total row numbers (excluding pre-treatment rows)
    r.t <- length(tDF16$chamber[r.n:dim(tDF16)[1]])
    
    ### set up time points
    for (i in 1:dim(tDF16)[1]) {
        tDF16[i, "time_elapsed"] <- round(as.numeric(difftime(tDF16[i, "datetime"], tDF16[r.n, "datetime"], units="mins")),1)
    }
    
    
    ######## chamber 8, canopy 345
    tDF17 <- subset(myDF, chamber == "8" & canopy == "345")
    
    ### find row number with maximum CO2 concentration
    r.n <- which.max(tDF17$vCo2)
    
    ### fine total row numbers (excluding pre-treatment rows)
    r.t <- length(tDF17$chamber[r.n:dim(tDF17)[1]])
    
    ### set up time points
    for (i in 1:dim(tDF17)[1]) {
        tDF17[i, "time_elapsed"] <- round(as.numeric(difftime(tDF17[i, "datetime"], tDF17[r.n, "datetime"], units="mins")),1)
    }
    
    
    ######## chamber 8, canopy 45
    tDF18 <- subset(myDF, chamber == "8" & canopy == "45")
    
    ### find row number with maximum CO2 concentration
    r.n <- which.max(tDF18$vCo2)
    
    ### fine total row numbers (excluding pre-treatment rows)
    r.t <- length(tDF18$chamber[r.n:dim(tDF18)[1]])
    
    ### set up time points
    for (i in 1:dim(tDF18)[1]) {
        tDF18[i, "time_elapsed"] <- round(as.numeric(difftime(tDF18[i, "datetime"], tDF18[r.n, "datetime"], units="mins")),1)
    }
    
    
    ######## chamber 11, canopy 12345
    tDF19 <- subset(myDF, chamber == "11" & canopy == "12345")
    
    ### find row number with maximum CO2 concentration
    r.n <- which.max(tDF19$vCo2)
    
    ### fine total row numbers (excluding pre-treatment rows)
    r.t <- length(tDF19$chamber[r.n:dim(tDF19)[1]])
    
    ### set up time points
    for (i in 1:dim(tDF19)[1]) {
        tDF19[i, "time_elapsed"] <- round(as.numeric(difftime(tDF19[i, "datetime"], tDF19[r.n, "datetime"], units="mins")),1)
    }
    
    
    ######## chamber 11, canopy 345
    tDF20 <- subset(myDF, chamber == "11" & canopy == "345")
    
    ### find row number with maximum CO2 concentration
    r.n <- which.max(tDF20$vCo2)
    
    ### fine total row numbers (excluding pre-treatment rows)
    r.t <- length(tDF20$chamber[r.n:dim(tDF20)[1]])
    
    ### set up time points
    for (i in 1:dim(tDF20)[1]) {
        tDF20[i, "time_elapsed"] <- round(as.numeric(difftime(tDF20[i, "datetime"], tDF20[r.n, "datetime"], units="mins")),1)
    }
    
    
    ######## chamber 11, canopy 45
    tDF21 <- subset(myDF, chamber == "11" & canopy == "45")
    
    ### find row number with maximum CO2 concentration
    r.n <- which.max(tDF21$vCo2)
    
    ### fine total row numbers (excluding pre-treatment rows)
    r.t <- length(tDF21$chamber[r.n:dim(tDF21)[1]])
    
    ### set up time points
    for (i in 1:dim(tDF21)[1]) {
        tDF21[i, "time_elapsed"] <- round(as.numeric(difftime(tDF21[i, "datetime"], tDF21[r.n, "datetime"], units="mins")),1)
    }
    
    
    ######## chamber 12, canopy 12345
    tDF22 <- subset(myDF, chamber == "12" & canopy == "12345")
    
    ### find row number with maximum CO2 concentration
    r.n <- which.max(tDF22$vCo2)
    
    ### fine total row numbers (excluding pre-treatment rows)
    r.t <- length(tDF22$chamber[r.n:dim(tDF22)[1]])
    
    ### set up time points
    for (i in 1:dim(tDF22)[1]) {
        tDF22[i, "time_elapsed"] <- round(as.numeric(difftime(tDF22[i, "datetime"], tDF22[r.n, "datetime"], units="mins")),1)
    }
    
    
    ######## chamber 12, canopy 345
    tDF23 <- subset(myDF, chamber == "12" & canopy == "345")
    
    ### find row number with maximum CO2 concentration
    r.n <- which.max(tDF23$vCo2)
    
    ### fine total row numbers (excluding pre-treatment rows)
    r.t <- length(tDF23$chamber[r.n:dim(tDF23)[1]])
    
    ### set up time points
    for (i in 1:dim(tDF23)[1]) {
        tDF23[i, "time_elapsed"] <- round(as.numeric(difftime(tDF23[i, "datetime"], tDF23[r.n, "datetime"], units="mins")),1)
    }
    
    
    ######## chamber 12, canopy 45
    tDF24 <- subset(myDF, chamber == "12" & canopy == "45")
    
    ### find row number with maximum CO2 concentration
    r.n <- which.max(tDF24$vCo2)
    
    ### fine total row numbers (excluding pre-treatment rows)
    r.t <- length(tDF24$chamber[r.n:dim(tDF24)[1]])
    
    ### set up time points
    for (i in 1:dim(tDF24)[1]) {
        tDF24[i, "time_elapsed"] <- round(as.numeric(difftime(tDF24[i, "datetime"], tDF24[r.n, "datetime"], units="mins")),1)
    }
    
    
    ###### combine all data
    out <- rbind(tDF1, tDF2, tDF3, tDF4, tDF5, 
                 tDF6, tDF7, tDF8, tDF9, tDF10,
                 tDF11, tDF12, tDF13, tDF14, tDF15, 
                 tDF16, tDF17, tDF18, tDF19, tDF20,
                 tDF21, tDF22, tDF23, tDF24)
    
    ### remove negative times
    myDF.out <- subset(out, time_elapsed >= 0)

    ### get the change in CO2 concentration over a minute interval, in unit of seconds
    for (i in unique(myDF.out$chamber)) {
        for (j in unique(myDF.out$canopy)) {
            #max.row <- max(myDF.out$time_elapsed[myDF.out$chamber==i & myDF.out$canopy==j])
            t.elapsed <- unique(myDF.out$time_elapsed[myDF.out$chamber==i & myDF.out$canopy==j])
            #t.elapsed <- t.elapsed[t.elapsed!=0]
            
            for (k in 16:length(t.elapsed)) {
                ## compute 10 min means, output unit in g chamber-1 m-1
                m1 <- mean(myDF.out[myDF.out$time_elapsed%in%t.elapsed[(k-15):(k-1)]&myDF.out$chamber==i&myDF.out$canopy==j,"CondWater"], na.rm=T)
                m2 <- mean(myDF.out[myDF.out$time_elapsed%in%t.elapsed[(k-14):(k)]&myDF.out$chamber==i&myDF.out$canopy==j,"CondWater"], na.rm=T)
                
    
                myDF.out[myDF.out$time_elapsed==t.elapsed[k]&myDF.out$chamber==i&myDF.out$canopy==j, "h2o_cond"] <- (m2-m1)/
                    as.numeric(difftime(myDF.out[myDF.out$time_elapsed==t.elapsed[k]&myDF.out$chamber==i&myDF.out$canopy==j,"time"],myDF.out[myDF.out$time_elapsed==t.elapsed[k-15]&myDF.out$chamber==i&myDF.out$canopy==j,"time"],unit="mins"))
                
                
                
                m3 <- mean(myDF.out[myDF.out$time_elapsed%in%t.elapsed[(k-15):(k-1)]&myDF.out$chamber==i&myDF.out$canopy==j,"rh_total"], na.rm=T)
                m4 <- mean(myDF.out[myDF.out$time_elapsed%in%t.elapsed[(k-14):(k)]&myDF.out$chamber==i&myDF.out$canopy==j,"rh_total"], na.rm=T)
                
                myDF.out[myDF.out$time_elapsed==t.elapsed[k]&myDF.out$chamber==i&myDF.out$canopy==j, "h2o_rh"] <- (m4-m3)/
                    as.numeric(difftime(myDF.out[myDF.out$time_elapsed==t.elapsed[k]&myDF.out$chamber==i&myDF.out$canopy==j,"time"],myDF.out[myDF.out$time_elapsed==t.elapsed[k-15]&myDF.out$chamber==i&myDF.out$canopy==j,"time"],unit="mins"))
            }
        }
    }
    
    return(myDF.out)
    
}
