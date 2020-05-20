fit_individual_data <- function(myDF) {
    ### This function fit individual chamber and canopy data 
    ### to obtain the slope of CO2 drawdown
    
    ######## Chamber 1, Canopy 12345
    tDF1 <- subset(myDF, chamber == "1" & canopy == "12345")
    
    ### find row number with maximum CO2 concentration
    r.n <- which.max(tDF1$CO2Local)
    
    ### fine total row numbers (excluding pre-treatment rows)
    r.t <- length(tDF1$chamber[r.n:dim(tDF1)[1]])
    
    ### set up time points
    tDF1[r.n:dim(tDF1)[1], "time_elapsed"] <- 0:(r.t-1)
    tDF1[1:r.n, "time_elapsed"] <- -(r.n-1):0
    
    ### get the change in CO2 concentration over a minute interval, in unit of seconds
    for (i in 1:r.t) {
        tDF1[tDF1$time_elapsed==i, "co2_flux"] <- (tDF1[tDF1$time_elapsed==i,"CO2Local"]-
                                                       tDF1[tDF1$time_elapsed==(i-1),"CO2Local"])/
            as.numeric(abs(tDF1[tDF1$time_elapsed==i,"time"]-tDF1[tDF1$time_elapsed==(i-1),"time"]))
    }
    
    
    ######## Chamber 1, Canopy 345
    tDF2 <- subset(myDF, chamber == "1" & canopy == "345")
    
    ### find row number with maximum CO2 concentration
    r.n <- which.max(tDF2$CO2Local)
    
    ### fine total row numbers (excluding pre-treatment rows)
    r.t <- length(tDF2$chamber[r.n:dim(tDF2)[1]])
    
    ### set up time points
    tDF2[r.n:dim(tDF2)[1], "time_elapsed"] <- 0:(r.t-1)
    tDF2[1:r.n, "time_elapsed"] <- -(r.n-1):0
    
    
    ######## Chamber 1, Canopy 45
    tDF3 <- subset(myDF, chamber == "1" & canopy == "45")
    
    ### find row number with maximum CO2 concentration
    r.n <- which.max(tDF3$CO2Local)
    
    ### fine total row numbers (excluding pre-treatment rows)
    r.t <- length(tDF3$chamber[r.n:dim(tDF3)[1]])
    
    ### set up time points
    tDF3[r.n:dim(tDF3)[1], "time_elapsed"] <- 0:(r.t-1)
    tDF3[1:r.n, "time_elapsed"] <- -(r.n-1):0
    
    
    ######## Chamber 2, Canopy 12345
    tDF4 <- subset(myDF, chamber == "2" & canopy == "12345")
    
    ### find row number with maximum CO2 concentration
    r.n <- which.max(tDF4$CO2Local)
    
    ### fine total row numbers (excluding pre-treatment rows)
    r.t <- length(tDF4$chamber[r.n:dim(tDF4)[1]])
    
    ### set up time points
    tDF4[r.n:dim(tDF4)[1], "time_elapsed"] <- 0:(r.t-1)
    tDF4[1:r.n, "time_elapsed"] <- -(r.n-1):0
    
    
    ######## Chamber 2, Canopy 345
    tDF5 <- subset(myDF, chamber == "2" & canopy == "345")
    
    ### find row number with maximum CO2 concentration
    r.n <- which.max(tDF5$CO2Local)
    
    ### fine total row numbers (excluding pre-treatment rows)
    r.t <- length(tDF5$chamber[r.n:dim(tDF5)[1]])
    
    ### set up time points
    tDF5[r.n:dim(tDF5)[1], "time_elapsed"] <- 0:(r.t-1)
    tDF5[1:r.n, "time_elapsed"] <- -(r.n-1):0
    
    
    ######## Chamber 2, Canopy 45
    tDF6 <- subset(myDF, chamber == "2" & canopy == "45")
    
    ### find row number with maximum CO2 concentration
    r.n <- which.max(tDF6$CO2Local)
    
    ### fine total row numbers (excluding pre-treatment rows)
    r.t <- length(tDF6$chamber[r.n:dim(tDF6)[1]])
    
    ### set up time points
    tDF6[r.n:dim(tDF6)[1], "time_elapsed"] <- 0:(r.t-1)
    tDF6[1:r.n, "time_elapsed"] <- -(r.n-1):0
    
    
    ######## Chamber 3, Canopy 12345
    tDF7 <- subset(myDF, chamber == "3" & canopy == "12345")
    
    ### find row number with maximum CO2 concentration
    r.n <- which.max(tDF7$CO2Local)
    
    ### fine total row numbers (excluding pre-treatment rows)
    r.t <- length(tDF7$chamber[r.n:dim(tDF7)[1]])
    
    ### set up time points
    tDF7[r.n:dim(tDF7)[1], "time_elapsed"] <- 0:(r.t-1)
    tDF7[1:r.n, "time_elapsed"] <- -(r.n-1):0
    
    
    ######## Chamber 3, Canopy 345
    tDF8 <- subset(myDF, chamber == "3" & canopy == "345")
    
    ### find row number with maximum CO2 concentration
    r.n <- which.max(tDF8$CO2Local)
    
    ### fine total row numbers (excluding pre-treatment rows)
    r.t <- length(tDF8$chamber[r.n:dim(tDF8)[1]])
    
    ### set up time points
    tDF8[r.n:dim(tDF8)[1], "time_elapsed"] <- 0:(r.t-1)
    tDF8[1:r.n, "time_elapsed"] <- -(r.n-1):0
    
    
    ######## Chamber 3, Canopy 45
    tDF9 <- subset(myDF, chamber == "3" & canopy == "45")
    
    ### find row number with maximum CO2 concentration
    r.n <- which.max(tDF9$CO2Local)
    
    ### fine total row numbers (excluding pre-treatment rows)
    r.t <- length(tDF9$chamber[r.n:dim(tDF9)[1]])
    
    ### set up time points
    tDF9[r.n:dim(tDF9)[1], "time_elapsed"] <- 0:(r.t-1)
    tDF9[1:r.n, "time_elapsed"] <- -(r.n-1):0
    
    
    ######## Chamber 4, Canopy 12345
    tDF10 <- subset(myDF, chamber == "4" & canopy == "12345")
    
    ### find row number with maximum CO2 concentration
    r.n <- which.max(tDF10$CO2Local)
    
    ### fine total row numbers (excluding pre-treatment rows)
    r.t <- length(tDF10$chamber[r.n:dim(tDF10)[1]])
    
    ### set up time points
    tDF10[r.n:dim(tDF10)[1], "time_elapsed"] <- 0:(r.t-1)
    tDF10[1:r.n, "time_elapsed"] <- -(r.n-1):0
    
    
    ######## Chamber 4, Canopy 345
    tDF11 <- subset(myDF, chamber == "4" & canopy == "345")
    
    ### find row number with maximum CO2 concentration
    r.n <- which.max(tDF11$CO2Local)
    
    ### fine total row numbers (excluding pre-treatment rows)
    r.t <- length(tDF11$chamber[r.n:dim(tDF11)[1]])
    
    ### set up time points
    tDF11[r.n:dim(tDF11)[1], "time_elapsed"] <- 0:(r.t-1)
    tDF11[1:r.n, "time_elapsed"] <- -(r.n-1):0
    
    
    ######## Chamber 4, Canopy 45
    tDF12 <- subset(myDF, chamber == "4" & canopy == "45")
    
    ### find row number with maximum CO2 concentration
    r.n <- which.max(tDF12$CO2Local)
    
    ### fine total row numbers (excluding pre-treatment rows)
    r.t <- length(tDF12$chamber[r.n:dim(tDF12)[1]])
    
    ### set up time points
    tDF12[r.n:dim(tDF12)[1], "time_elapsed"] <- 0:(r.t-1)
    tDF12[1:r.n, "time_elapsed"] <- -(r.n-1):0
    
    
    ######## Chamber 7, Canopy 12345
    tDF13 <- subset(myDF, chamber == "7" & canopy == "12345")
    
    ### find row number with maximum CO2 concentration
    r.n <- which.max(tDF13$CO2Local)
    
    ### fine total row numbers (excluding pre-treatment rows)
    r.t <- length(tDF13$chamber[r.n:dim(tDF13)[1]])
    
    ### set up time points
    tDF13[r.n:dim(tDF13)[1], "time_elapsed"] <- 0:(r.t-1)
    tDF13[1:r.n, "time_elapsed"] <- -(r.n-1):0
    
    
    ######## Chamber 7, Canopy 345
    tDF14 <- subset(myDF, chamber == "7" & canopy == "345")
    
    ### find row number with maximum CO2 concentration
    r.n <- which.max(tDF14$CO2Local)
    
    ### fine total row numbers (excluding pre-treatment rows)
    r.t <- length(tDF14$chamber[r.n:dim(tDF14)[1]])
    
    ### set up time points
    tDF14[r.n:dim(tDF14)[1], "time_elapsed"] <- 0:(r.t-1)
    tDF14[1:r.n, "time_elapsed"] <- -(r.n-1):0
    
    
    ######## Chamber 7, Canopy 45
    tDF15 <- subset(myDF, chamber == "7" & canopy == "45")
    
    ### find row number with maximum CO2 concentration
    r.n <- which.max(tDF15$CO2Local)
    
    ### fine total row numbers (excluding pre-treatment rows)
    r.t <- length(tDF15$chamber[r.n:dim(tDF15)[1]])
    
    ### set up time points
    tDF15[r.n:dim(tDF15)[1], "time_elapsed"] <- 0:(r.t-1)
    tDF15[1:r.n, "time_elapsed"] <- -(r.n-1):0
    
    
    ######## Chamber 8, Canopy 12345
    tDF16 <- subset(myDF, chamber == "8" & canopy == "12345")
    
    ### find row number with maximum CO2 concentration
    r.n <- which.max(tDF16$CO2Local)
    
    ### fine total row numbers (excluding pre-treatment rows)
    r.t <- length(tDF16$chamber[r.n:dim(tDF16)[1]])
    
    ### set up time points
    tDF16[r.n:dim(tDF16)[1], "time_elapsed"] <- 0:(r.t-1)
    tDF16[1:r.n, "time_elapsed"] <- -(r.n-1):0
    
    
    ######## Chamber 8, Canopy 345
    tDF17 <- subset(myDF, chamber == "8" & canopy == "345")
    
    ### find row number with maximum CO2 concentration
    r.n <- which.max(tDF17$CO2Local)
    
    ### fine total row numbers (excluding pre-treatment rows)
    r.t <- length(tDF17$chamber[r.n:dim(tDF17)[1]])
    
    ### set up time points
    tDF17[r.n:dim(tDF17)[1], "time_elapsed"] <- 0:(r.t-1)
    tDF17[1:r.n, "time_elapsed"] <- -(r.n-1):0
    
    
    ######## Chamber 8, Canopy 45
    tDF18 <- subset(myDF, chamber == "8" & canopy == "45")
    
    ### find row number with maximum CO2 concentration
    r.n <- which.max(tDF18$CO2Local)
    
    ### fine total row numbers (excluding pre-treatment rows)
    r.t <- length(tDF18$chamber[r.n:dim(tDF18)[1]])
    
    ### set up time points
    tDF18[r.n:dim(tDF18)[1], "time_elapsed"] <- 0:(r.t-1)
    tDF18[1:r.n, "time_elapsed"] <- -(r.n-1):0
    
    
    ######## Chamber 11, Canopy 12345
    tDF19 <- subset(myDF, chamber == "11" & canopy == "12345")
    
    ### find row number with maximum CO2 concentration
    r.n <- which.max(tDF19$CO2Local)
    
    ### fine total row numbers (excluding pre-treatment rows)
    r.t <- length(tDF19$chamber[r.n:dim(tDF19)[1]])
    
    ### set up time points
    tDF19[r.n:dim(tDF19)[1], "time_elapsed"] <- 0:(r.t-1)
    tDF19[1:r.n, "time_elapsed"] <- -(r.n-1):0
    
    
    ######## Chamber 11, Canopy 345
    tDF20 <- subset(myDF, chamber == "11" & canopy == "345")
    
    ### find row number with maximum CO2 concentration
    r.n <- which.max(tDF20$CO2Local)
    
    ### fine total row numbers (excluding pre-treatment rows)
    r.t <- length(tDF20$chamber[r.n:dim(tDF20)[1]])
    
    ### set up time points
    tDF20[r.n:dim(tDF20)[1], "time_elapsed"] <- 0:(r.t-1)
    tDF20[1:r.n, "time_elapsed"] <- -(r.n-1):0
    
    
    ######## Chamber 11, Canopy 45
    tDF21 <- subset(myDF, chamber == "11" & canopy == "45")
    
    ### find row number with maximum CO2 concentration
    r.n <- which.max(tDF21$CO2Local)
    
    ### fine total row numbers (excluding pre-treatment rows)
    r.t <- length(tDF21$chamber[r.n:dim(tDF21)[1]])
    
    ### set up time points
    tDF21[r.n:dim(tDF21)[1], "time_elapsed"] <- 0:(r.t-1)
    tDF21[1:r.n, "time_elapsed"] <- -(r.n-1):0
    
    
    ######## Chamber 12, Canopy 12345
    tDF22 <- subset(myDF, chamber == "12" & canopy == "12345")
    
    ### find row number with maximum CO2 concentration
    r.n <- which.max(tDF22$CO2Local)
    
    ### fine total row numbers (excluding pre-treatment rows)
    r.t <- length(tDF22$chamber[r.n:dim(tDF22)[1]])
    
    ### set up time points
    tDF22[r.n:dim(tDF22)[1], "time_elapsed"] <- 0:(r.t-1)
    tDF22[1:r.n, "time_elapsed"] <- -(r.n-1):0
    
    
    ######## Chamber 12, Canopy 345
    tDF23 <- subset(myDF, chamber == "12" & canopy == "345")
    
    ### find row number with maximum CO2 concentration
    r.n <- which.max(tDF23$CO2Local)
    
    ### fine total row numbers (excluding pre-treatment rows)
    r.t <- length(tDF23$chamber[r.n:dim(tDF23)[1]])
    
    ### set up time points
    tDF23[r.n:dim(tDF23)[1], "time_elapsed"] <- 0:(r.t-1)
    tDF23[1:r.n, "time_elapsed"] <- -(r.n-1):0
    
    
    ######## Chamber 12, Canopy 45
    tDF24 <- subset(myDF, chamber == "12" & canopy == "45")
    
    ### find row number with maximum CO2 concentration
    r.n <- which.max(tDF24$CO2Local)
    
    ### fine total row numbers (excluding pre-treatment rows)
    r.t <- length(tDF24$chamber[r.n:dim(tDF24)[1]])
    
    ### set up time points
    tDF24[r.n:dim(tDF24)[1], "time_elapsed"] <- 0:(r.t-1)
    tDF24[1:r.n, "time_elapsed"] <- -(r.n-1):0
    
    
    ###### combine all data
    out <- rbind(tDF1, tDF2, tDF3, tDF4, tDF5, 
                 tDF6, tDF7, tDF8, tDF9, tDF10,
                 tDF11, tDF12, tDF13, tDF14, tDF15, 
                 tDF16, tDF17, tDF18, tDF19, tDF20,
                 tDF21, tDF22, tDF23, tDF24)
    
    ### remove negative times
    myDF <- subset(out, time_elapsed >= 0)
    
    ### get the change in CO2 concentration over a minute interval, in unit of seconds
    for (i in unique(myDF$chamber)) {
        for (j in unique(myDF$canopy)) {
            max.row <- max(myDF$time_elapsed[myDF$chamber==i & myDF$canopy==j])
            
            for (k in 1:max.row) {
                myDF[myDF$time_elapsed==k&myDF$chamber==i&myDF$canopy==j, "co2_flux"] <- (myDF[myDF$time_elapsed==k&myDF$chamber==i&myDF$canopy==j,"CO2Local"]-
                                                               myDF[myDF$time_elapsed==(k-1)&myDF$chamber==i&myDF$canopy==j,"CO2Local"])/
                    as.numeric(difftime(myDF[myDF$time_elapsed==k&myDF$chamber==i&myDF$canopy==j,"time"],myDF[myDF$time_elapsed==(k-1)&myDF$chamber==i&myDF$canopy==j,"time"],unit="secs"))
            }
        }
    }
    
    p4 <- ggplot(myDF, aes(CO2Local))+
        geom_point(aes(y=co2_flux, shape=factor(canopy), color=factor(chamber)), size=2.0)+
        labs(x=expression(paste(CO[2], " concentration (ppm)")),
             y=expression(paste(CO[2], " flux (ppm ", CO[2], " ", s^-1, ")")))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="right")+
        scale_colour_manual(name="Chamber", values = c("1" = "brown", "2" = "red", "3" = "pink",
                                                       "4" = "orange", "7" = "yellow", "8" = "green",
                                                       "11" = "cyan", "12" = "blue"))+
        scale_shape_manual(name="Canopy", values = c("12345" = 16, "345" = 17, "45" = 15),
                           labels=c("full", "top + middle", "top"))
    
    
    plot(p4)
    
    return(myDF)
    
}