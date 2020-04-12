calculate_co2_flux_per_second <- function(myDF) {
    ### This function fit individual Chamber and Canopy data 
    ### to obtain the flux of co2 per second based on minute data
    
    ######## Chamber 1, Canopy 12345
    tDF1 <- subset(myDF, Chamber == "1" & Canopy == "12345")
    
    ### find row number with maximum CO2 concentration
    r.n <- which.max(tDF1$WTC_CO2)
    
    ### fine total row numbers (excluding pre-treatment rows)
    r.t <- length(tDF1$Chamber[r.n:dim(tDF1)[1]])
    
    ### set up time points
    for (i in 1:dim(tDF1)[1]) {
        tDF1[i, "time_elapsed"] <- round(as.numeric(difftime(tDF1[i, "datetime"], tDF1[r.n, "datetime"], units="mins")),1)
    }
    
    #tDF1[r.n:dim(tDF1)[1], "time_elapsed"] <- 0:(r.t-1)
    #tDF1[1:r.n, "time_elapsed"] <- -(r.n-1):0
    
    
    ######## Chamber 1, Canopy 345
    tDF2 <- subset(myDF, Chamber == "1" & Canopy == "345")
    
    ### find row number with maximum CO2 concentration
    r.n <- which.max(tDF2$WTC_CO2)
    
    ### fine total row numbers (excluding pre-treatment rows)
    r.t <- length(tDF2$Chamber[r.n:dim(tDF2)[1]])
    
    ### set up time points
    for (i in 1:dim(tDF2)[1]) {
        tDF2[i, "time_elapsed"] <- round(as.numeric(difftime(tDF2[i, "datetime"], tDF2[r.n, "datetime"], units="mins")),1)
    }
    
    ######## Chamber 1, Canopy 45
    tDF3 <- subset(myDF, Chamber == "1" & Canopy == "45")
    
    ### find row number with maximum CO2 concentration
    r.n <- which.max(tDF3$WTC_CO2)
    
    ### fine total row numbers (excluding pre-treatment rows)
    r.t <- length(tDF3$Chamber[r.n:dim(tDF3)[1]])
    
    ### set up time points
    for (i in 1:dim(tDF3)[1]) {
        tDF3[i, "time_elapsed"] <- round(as.numeric(difftime(tDF3[i, "datetime"], tDF3[r.n, "datetime"], units="mins")),1)
    }
    
    
    ######## Chamber 2, Canopy 12345
    tDF4 <- subset(myDF, Chamber == "2" & Canopy == "12345")
    
    ### find row number with maximum CO2 concentration
    r.n <- which.max(tDF4$WTC_CO2)
    
    ### fine total row numbers (excluding pre-treatment rows)
    r.t <- length(tDF4$Chamber[r.n:dim(tDF4)[1]])
    
    ### set up time points
    for (i in 1:dim(tDF4)[1]) {
        tDF4[i, "time_elapsed"] <- round(as.numeric(difftime(tDF4[i, "datetime"], tDF4[r.n, "datetime"], units="mins")),1)
    }
    
    
    ######## Chamber 2, Canopy 345
    tDF5 <- subset(myDF, Chamber == "2" & Canopy == "345")
    
    ### find row number with maximum CO2 concentration
    r.n <- which.max(tDF5$WTC_CO2)
    
    ### fine total row numbers (excluding pre-treatment rows)
    r.t <- length(tDF5$Chamber[r.n:dim(tDF5)[1]])
    
    ### set up time points
    for (i in 1:dim(tDF5)[1]) {
        tDF5[i, "time_elapsed"] <- round(as.numeric(difftime(tDF5[i, "datetime"], tDF5[r.n, "datetime"], units="mins")),1)
    }
    
    
    ######## Chamber 2, Canopy 45
    ### Note: this Chamber and Canopy has a time gap of two days!
    tDF6 <- subset(myDF, Chamber == "2" & Canopy == "45")
    
    ### find row number with maximum CO2 concentration
    r.n <- which.max(tDF6$WTC_CO2)
    
    ### fine total row numbers (excluding pre-treatment rows)
    r.t <- length(tDF6$Chamber[r.n:dim(tDF6)[1]])
    
    ### set up time points
    for (i in 1:dim(tDF6)[1]) {
        tDF6[i, "time_elapsed"] <- round(as.numeric(difftime(tDF6[i, "datetime"], tDF6[r.n, "datetime"], units="mins")),1)
    }
    
    
    ######## Chamber 3, Canopy 12345
    tDF7 <- subset(myDF, Chamber == "3" & Canopy == "12345")
    
    ### find row number with maximum CO2 concentration
    r.n <- which.max(tDF7$WTC_CO2)
    
    ### fine total row numbers (excluding pre-treatment rows)
    r.t <- length(tDF7$Chamber[r.n:dim(tDF7)[1]])
    
    ### set up time points
    for (i in 1:dim(tDF7)[1]) {
        tDF7[i, "time_elapsed"] <- round(as.numeric(difftime(tDF7[i, "datetime"], tDF7[r.n, "datetime"], units="mins")),1)
    }
    
    
    ######## Chamber 3, Canopy 345
    tDF8 <- subset(myDF, Chamber == "3" & Canopy == "345")
    
    ### find row number with maximum CO2 concentration
    r.n <- which.max(tDF8$WTC_CO2)
    
    ### fine total row numbers (excluding pre-treatment rows)
    r.t <- length(tDF8$Chamber[r.n:dim(tDF8)[1]])
    
    ### set up time points
    for (i in 1:dim(tDF8)[1]) {
        tDF8[i, "time_elapsed"] <- round(as.numeric(difftime(tDF8[i, "datetime"], tDF8[r.n, "datetime"], units="mins")),1)
    }
    
    
    ######## Chamber 3, Canopy 45
    tDF9 <- subset(myDF, Chamber == "3" & Canopy == "45")
    
    ### find row number with maximum CO2 concentration
    r.n <- which.max(tDF9$WTC_CO2)
    
    ### fine total row numbers (excluding pre-treatment rows)
    r.t <- length(tDF9$Chamber[r.n:dim(tDF9)[1]])
    
    ### set up time points
    for (i in 1:dim(tDF9)[1]) {
        tDF9[i, "time_elapsed"] <- round(as.numeric(difftime(tDF9[i, "datetime"], tDF9[r.n, "datetime"], units="mins")),1)
    }
    
    
    ######## Chamber 4, Canopy 12345
    tDF10 <- subset(myDF, Chamber == "4" & Canopy == "12345")
    
    ### find row number with maximum CO2 concentration
    r.n <- which.max(tDF10$WTC_CO2)
    
    ### fine total row numbers (excluding pre-treatment rows)
    r.t <- length(tDF10$Chamber[r.n:dim(tDF10)[1]])
    
    ### set up time points
    for (i in 1:dim(tDF10)[1]) {
        tDF10[i, "time_elapsed"] <- round(as.numeric(difftime(tDF10[i, "datetime"], tDF10[r.n, "datetime"], units="mins")),1)
    }
    
    
    ######## Chamber 4, Canopy 345
    tDF11 <- subset(myDF, Chamber == "4" & Canopy == "345")
    
    ### find row number with maximum CO2 concentration
    r.n <- which.max(tDF11$WTC_CO2)
    
    ### fine total row numbers (excluding pre-treatment rows)
    r.t <- length(tDF11$Chamber[r.n:dim(tDF11)[1]])
    
    ### set up time points
    for (i in 1:dim(tDF11)[1]) {
        tDF11[i, "time_elapsed"] <- round(as.numeric(difftime(tDF11[i, "datetime"], tDF11[r.n, "datetime"], units="mins")),1)
    }
    
    
    ######## Chamber 4, Canopy 45
    tDF12 <- subset(myDF, Chamber == "4" & Canopy == "45")
    
    ### find row number with maximum CO2 concentration
    r.n <- which.max(tDF12$WTC_CO2)
    
    ### fine total row numbers (excluding pre-treatment rows)
    r.t <- length(tDF12$Chamber[r.n:dim(tDF12)[1]])
    
    ### set up time points
    for (i in 1:dim(tDF12)[1]) {
        tDF12[i, "time_elapsed"] <- round(as.numeric(difftime(tDF12[i, "datetime"], tDF12[r.n, "datetime"], units="mins")),1)
    }
    
    
    ######## Chamber 7, Canopy 12345
    tDF13 <- subset(myDF, Chamber == "7" & Canopy == "12345")
    
    ### find row number with maximum CO2 concentration
    r.n <- which.max(tDF13$WTC_CO2)
    
    ### fine total row numbers (excluding pre-treatment rows)
    r.t <- length(tDF13$Chamber[r.n:dim(tDF13)[1]])
    
    ### set up time points
    for (i in 1:dim(tDF13)[1]) {
        tDF13[i, "time_elapsed"] <- round(as.numeric(difftime(tDF13[i, "datetime"], tDF13[r.n, "datetime"], units="mins")),1)
    }
    
    
    ######## Chamber 7, Canopy 345
    tDF14 <- subset(myDF, Chamber == "7" & Canopy == "345")
    
    ### find row number with maximum CO2 concentration
    r.n <- which.max(tDF14$WTC_CO2)
    
    ### fine total row numbers (excluding pre-treatment rows)
    r.t <- length(tDF14$Chamber[r.n:dim(tDF14)[1]])
    
    ### set up time points
    for (i in 1:dim(tDF14)[1]) {
        tDF14[i, "time_elapsed"] <- round(as.numeric(difftime(tDF14[i, "datetime"], tDF14[r.n, "datetime"], units="mins")),1)
    }
    
    
    ######## Chamber 7, Canopy 45
    tDF15 <- subset(myDF, Chamber == "7" & Canopy == "45")
    
    ### find row number with maximum CO2 concentration
    r.n <- which.max(tDF15$WTC_CO2)
    
    ### fine total row numbers (excluding pre-treatment rows)
    r.t <- length(tDF15$Chamber[r.n:dim(tDF15)[1]])
    
    ### set up time points
    for (i in 1:dim(tDF15)[1]) {
        tDF15[i, "time_elapsed"] <- round(as.numeric(difftime(tDF15[i, "datetime"], tDF15[r.n, "datetime"], units="mins")),1)
    }
    
    
    ######## Chamber 8, Canopy 12345
    tDF16 <- subset(myDF, Chamber == "8" & Canopy == "12345")
    
    ### find row number with maximum CO2 concentration
    r.n <- which.max(tDF16$WTC_CO2)
    
    ### fine total row numbers (excluding pre-treatment rows)
    r.t <- length(tDF16$Chamber[r.n:dim(tDF16)[1]])
    
    ### set up time points
    for (i in 1:dim(tDF16)[1]) {
        tDF16[i, "time_elapsed"] <- round(as.numeric(difftime(tDF16[i, "datetime"], tDF16[r.n, "datetime"], units="mins")),1)
    }
    
    
    ######## Chamber 8, Canopy 345
    tDF17 <- subset(myDF, Chamber == "8" & Canopy == "345")
    
    ### find row number with maximum CO2 concentration
    r.n <- which.max(tDF17$WTC_CO2)
    
    ### fine total row numbers (excluding pre-treatment rows)
    r.t <- length(tDF17$Chamber[r.n:dim(tDF17)[1]])
    
    ### set up time points
    for (i in 1:dim(tDF17)[1]) {
        tDF17[i, "time_elapsed"] <- round(as.numeric(difftime(tDF17[i, "datetime"], tDF17[r.n, "datetime"], units="mins")),1)
    }
    
    
    ######## Chamber 8, Canopy 45
    tDF18 <- subset(myDF, Chamber == "8" & Canopy == "45")
    
    ### find row number with maximum CO2 concentration
    r.n <- which.max(tDF18$WTC_CO2)
    
    ### fine total row numbers (excluding pre-treatment rows)
    r.t <- length(tDF18$Chamber[r.n:dim(tDF18)[1]])
    
    ### set up time points
    for (i in 1:dim(tDF18)[1]) {
        tDF18[i, "time_elapsed"] <- round(as.numeric(difftime(tDF18[i, "datetime"], tDF18[r.n, "datetime"], units="mins")),1)
    }
    
    
    ######## Chamber 11, Canopy 12345
    tDF19 <- subset(myDF, Chamber == "11" & Canopy == "12345")
    
    ### find row number with maximum CO2 concentration
    r.n <- which.max(tDF19$WTC_CO2)
    
    ### fine total row numbers (excluding pre-treatment rows)
    r.t <- length(tDF19$Chamber[r.n:dim(tDF19)[1]])
    
    ### set up time points
    for (i in 1:dim(tDF19)[1]) {
        tDF19[i, "time_elapsed"] <- round(as.numeric(difftime(tDF19[i, "datetime"], tDF19[r.n, "datetime"], units="mins")),1)
    }
    
    
    ######## Chamber 11, Canopy 345
    tDF20 <- subset(myDF, Chamber == "11" & Canopy == "345")
    
    ### find row number with maximum CO2 concentration
    r.n <- which.max(tDF20$WTC_CO2)
    
    ### fine total row numbers (excluding pre-treatment rows)
    r.t <- length(tDF20$Chamber[r.n:dim(tDF20)[1]])
    
    ### set up time points
    for (i in 1:dim(tDF20)[1]) {
        tDF20[i, "time_elapsed"] <- round(as.numeric(difftime(tDF20[i, "datetime"], tDF20[r.n, "datetime"], units="mins")),1)
    }
    
    
    ######## Chamber 11, Canopy 45
    tDF21 <- subset(myDF, Chamber == "11" & Canopy == "45")
    
    ### find row number with maximum CO2 concentration
    r.n <- which.max(tDF21$WTC_CO2)
    
    ### fine total row numbers (excluding pre-treatment rows)
    r.t <- length(tDF21$Chamber[r.n:dim(tDF21)[1]])
    
    ### set up time points
    for (i in 1:dim(tDF21)[1]) {
        tDF21[i, "time_elapsed"] <- round(as.numeric(difftime(tDF21[i, "datetime"], tDF21[r.n, "datetime"], units="mins")),1)
    }
    
    
    ######## Chamber 12, Canopy 12345
    tDF22 <- subset(myDF, Chamber == "12" & Canopy == "12345")
    
    ### find row number with maximum CO2 concentration
    r.n <- which.max(tDF22$WTC_CO2)
    
    ### fine total row numbers (excluding pre-treatment rows)
    r.t <- length(tDF22$Chamber[r.n:dim(tDF22)[1]])
    
    ### set up time points
    for (i in 1:dim(tDF22)[1]) {
        tDF22[i, "time_elapsed"] <- round(as.numeric(difftime(tDF22[i, "datetime"], tDF22[r.n, "datetime"], units="mins")),1)
    }
    
    
    ######## Chamber 12, Canopy 345
    tDF23 <- subset(myDF, Chamber == "12" & Canopy == "345")
    
    ### find row number with maximum CO2 concentration
    r.n <- which.max(tDF23$WTC_CO2)
    
    ### fine total row numbers (excluding pre-treatment rows)
    r.t <- length(tDF23$Chamber[r.n:dim(tDF23)[1]])
    
    ### set up time points
    for (i in 1:dim(tDF23)[1]) {
        tDF23[i, "time_elapsed"] <- round(as.numeric(difftime(tDF23[i, "datetime"], tDF23[r.n, "datetime"], units="mins")),1)
    }
    
    
    ######## Chamber 12, Canopy 45
    tDF24 <- subset(myDF, Chamber == "12" & Canopy == "45")
    
    ### find row number with maximum CO2 concentration
    r.n <- which.max(tDF24$WTC_CO2)
    
    ### fine total row numbers (excluding pre-treatment rows)
    r.t <- length(tDF24$Chamber[r.n:dim(tDF24)[1]])
    
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
    for (i in unique(myDF.out$Chamber)) {
        for (j in unique(myDF.out$Canopy)) {
            #max.row <- max(myDF.out$time_elapsed[myDF.out$Chamber==i & myDF.out$Canopy==j])
            t.elapsed <- unique(myDF.out$time_elapsed[myDF.out$Chamber==i & myDF.out$Canopy==j])
            #t.elapsed <- t.elapsed[t.elapsed!=0]
            
            for (k in 5:length(t.elapsed)) {
                myDF.out[myDF.out$time_elapsed==t.elapsed[k]&myDF.out$Chamber==i&myDF.out$Canopy==j, "co2_flux"] <- (myDF.out[myDF.out$time_elapsed==t.elapsed[k-4]&myDF.out$Chamber==i&myDF.out$Canopy==j,"WTC_CO2"]-
                                                               myDF.out[myDF.out$time_elapsed==t.elapsed[k]&myDF.out$Chamber==i&myDF.out$Canopy==j,"WTC_CO2"])/
                    as.numeric(difftime(myDF.out[myDF.out$time_elapsed==t.elapsed[k]&myDF.out$Chamber==i&myDF.out$Canopy==j,"time"],myDF.out[myDF.out$time_elapsed==t.elapsed[k-4]&myDF.out$Chamber==i&myDF.out$Canopy==j,"time"],unit="mins"))
            }
        }
    }
    
    return(myDF.out)
    
}