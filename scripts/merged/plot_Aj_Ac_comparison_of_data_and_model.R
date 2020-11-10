plot_Aj_Ac_comparison_of_data_and_model <- function(mgDF) {
    
    ####################################################################################
    ######################### start WTC data Ac vs. Aj #################################
    #### read in fitaci data at canopy and leaf scales and plot the Aj and Ac comparison
    ### we have multiple dates in leaf-scale measurements
    stDF.c <- read.csv("output/canopy/canopy_scale_parameters.csv", header=T)
    stDF.l <- read.csv("output/leaf/leaf_scale_parameters.csv", header=T)
    
    ### subset chambers
    subDF.l <- subset(stDF.l, Chamber%in%c("1", "3", "11", "4", "8"))
    subDF.c <- subset(stDF.c, Chamber%in%c("1", "3", "11", "4", "8"))
    
    ### generate identity list
    idDF <- unique(mgDF[,c("Identity", "Chamber", "Position", 
                           "Type", "CO2_treatment")])
    
    ### subset columns
    subDF.l <- subDF.l[,c("Identity", "RMSE", "Vcmax", "Vcmax.se", "Jmax",
                          "Jmax.se", "Rd", "Rd.se", "Ci_400", "ALEAF_400", "GS_400", "ELEAF_400", 
                          "Ac_400","Aj_400", "Ap_400", 
                          "Ci_600", "ALEAF_600", "GS_600", "ELEAF_600", 
                          "Ac_600","Aj_600", "Ap_600", 
                          "VPD", "Tleaf", "Ca", "Cc", "PPFD", 
                          "Ci_transition_Ac_Aj", "GammaStar", "Km", "G1", "JVratio")]
    
    subDF.c <- subDF.c[,c("Identity", "RMSE", "Vcmax", "Vcmax.se", "Jmax",
                          "Jmax.se", "Rd", "Rd.se", "Ci_400", "ALEAF_400", "GS_400", "ELEAF_400", 
                          "Ac_400", "Aj_400", "Ap_400", 
                          "Ci_600", "ALEAF_600", "GS_600", "ELEAF_600", 
                          "Ac_600","Aj_600", "Ap_600", 
                          "VPD", "Tleaf", "Ca", "Cc", "PPFD", 
                          "Ci_transition_Ac_Aj", "GammaStar", "Km", "G1", "JVratio")]
    
    ### change col names
    stDF <- rbind(subDF.l, subDF.c)
    stDF <- merge(stDF, idDF, by="Identity", all=T)
    
    ### convert into factors
    stDF$Type <- as.factor(stDF$Type)
    stDF$CO2_treatment <- as.factor(stDF$CO2_treatment)
    
    #### only keep aCO2 treatment
    stDF <- subset(stDF, CO2_treatment=="aCO2")

    #### plot A600/A400 ratio
    stDF$A600_over_A400 <- with(stDF, (ALEAF_600-ALEAF_400)/ALEAF_400)
    stDF$Ac600_over_Ac400 <- with(stDF, (Ac_600-Ac_400)/Ac_400)
    stDF$Aj600_over_Aj400 <- with(stDF, (Aj_600-Aj_400)/Aj_400)
    
    ### subset
    subDF1 <- stDF[,c("A600_over_A400", "CO2_treatment", "Chamber", "Position", "Type")]
    subDF2 <- stDF[,c("Ac600_over_Ac400", "CO2_treatment", "Chamber", "Position", "Type")]
    subDF3 <- stDF[,c("Aj600_over_Aj400", "CO2_treatment", "Chamber", "Position", "Type")]
    
    subDF1$lab <- "A"
    subDF2$lab <- "Ac"
    subDF3$lab <- "Aj"
    
    colnames(subDF1) <- colnames(subDF2) <- colnames(subDF3) <- c("ratio", "CO2_treatment", 
                                                                  "Chamber", "Position", "Type", "lab")
    
    plotDF1 <- rbind(subDF1, subDF2, subDF3)
    
    ### summary By
    wtcDF <- summaryBy(ratio~Position+lab, 
                       FUN=c(mean, se), data=plotDF1, keep.names=T)
    
    
    ### add WTC result and compare
    #wtcDF <- read.csv("output/A-Ca/linear_predicted_A_at_400_600_ppm.csv")
    #
    #sumDF2 <- summaryBy(A_sens_norm~Type+Position, FUN=c(mean,se),
    #                    data=wtcDF, keep.names=T, na.rm=T)
    
    
    ############################ end WTC data Ac vs. Aj ################################
    ####################################################################################

    
    ####################################################################################
    ############################## start MATE Ac vs. Aj ################################
    ### read in MATE simulation results
    inDF1 <- read.csv("~/Documents/Research/Projects/WCT1_CO2_drawdown/MATE_test/output/MATE_output_ch01.csv")
    inDF2 <- read.csv("~/Documents/Research/Projects/WCT1_CO2_drawdown/MATE_test/output/MATE_output_ch03.csv")
    inDF3 <- read.csv("~/Documents/Research/Projects/WCT1_CO2_drawdown/MATE_test/output/MATE_output_ch11.csv")
    inDF4 <- read.csv("~/Documents/Research/Projects/WCT1_CO2_drawdown/MATE_test/output/MATE_output_ch04.csv")
    inDF5 <- read.csv("~/Documents/Research/Projects/WCT1_CO2_drawdown/MATE_test/output/MATE_output_ch08.csv")
    inDF6 <- read.csv("~/Documents/Research/Projects/WCT1_CO2_drawdown/MATE_test/output/MATE_output_roadmap_vcmax45.csv")
    
    ### merge
    myDF <- rbind(inDF1, inDF2, inDF3, inDF4, inDF5)
    
    myDF$CO2_treatment <- myDF$T_treatment
    
    ### simplify
    myDF <- myDF[,c("DateTime", "chamber", "Canopy", "DOY", "hod", "year",
                    "Ca", "LAI", "CO2_treatment", "Aj", "Ac", "Asat")]
    
    ### subset Ca = 350 to 650 range
    subDF1 <- subset(myDF, Ca>=350&Ca<=650)
    subDF2 <- subset(myDF, Ca>=200&Ca<=350)
    
    subDF3 <- subset(inDF6, Ca>=300&Ca<=700)
    subDF4 <- subset(inDF6, Ca>=200&Ca<=350)
    
    ### prepare a storage DF
    plotDF <- data.frame(c("aCO2", "eCO2", "JV2"), NA, NA, NA, NA, NA,
                         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
    colnames(plotDF) <- c("CO2_treatment", "A280", "A400", "A600", "A_sens1", "A_sens2",
                          "Ac280", "Ac400", "Ac600", "Ac_sens1", "Ac_sens2",
                          "Aj280", "Aj400", "Aj600", "Aj_sens1", "Aj_sens2")
    
    ### get a linear fit, ignore LA and chamber but just split with CO2 treatment
    lm1 <- lm(Asat~Ca, data=subDF1[subDF1$chamber%in%c(1,3,11),])
    lm2 <- lm(Asat~Ca, data=subDF1[subDF1$chamber%in%c(4,8),])
    
    lm3 <- lm(Asat~Ca, data=subDF2[subDF2$chamber%in%c(1,3,11),])
    lm4 <- lm(Asat~Ca, data=subDF2[subDF2$chamber%in%c(4,8),])
    
    lm5 <- lm(Ac~Ca, data=subDF1[subDF1$chamber%in%c(1,3,11),])
    lm6 <- lm(Ac~Ca, data=subDF1[subDF1$chamber%in%c(4,8),])
    
    lm7 <- lm(Ac~Ca, data=subDF2[subDF2$chamber%in%c(1,3,11),])
    lm8 <- lm(Ac~Ca, data=subDF2[subDF2$chamber%in%c(4,8),])
    
    lm9 <- lm(Aj~Ca, data=subDF1[subDF1$chamber%in%c(1,3,11),])
    lm10 <- lm(Aj~Ca, data=subDF1[subDF1$chamber%in%c(4,8),])
    
    lm11 <- lm(Aj~Ca, data=subDF2[subDF2$chamber%in%c(1,3,11),])
    lm12 <- lm(Aj~Ca, data=subDF2[subDF2$chamber%in%c(4,8),])
    
    
    lm13 <- lm(Asat~Ca, data=subDF3)
    lm14 <- lm(Ac~Ca, data=subDF3)
    lm15 <- lm(Aj~Ca, data=subDF3)
    
    lm16 <- lm(Asat~Ca, data=subDF4)
    lm17 <- lm(Ac~Ca, data=subDF4)
    lm18 <- lm(Aj~Ca, data=subDF4)
    
    
    ### assign values
    plotDF$A280[plotDF$CO2_treatment=="aCO2"] <- 280 * coef(lm3)[2] + coef(lm3)[1]
    plotDF$A400[plotDF$CO2_treatment=="aCO2"] <- 400 * coef(lm1)[2] + coef(lm1)[1]
    plotDF$A600[plotDF$CO2_treatment=="aCO2"] <- 600 * coef(lm1)[2] + coef(lm1)[1]
    
    plotDF$A280[plotDF$CO2_treatment=="eCO2"] <- 280 * coef(lm4)[2] + coef(lm4)[1]
    plotDF$A400[plotDF$CO2_treatment=="eCO2"] <- 400 * coef(lm2)[2] + coef(lm2)[1]
    plotDF$A600[plotDF$CO2_treatment=="eCO2"] <- 600 * coef(lm2)[2] + coef(lm2)[1]
    
    plotDF$A280[plotDF$CO2_treatment=="JV2"] <- 280 * coef(lm16)[2] + coef(lm16)[1]
    plotDF$A400[plotDF$CO2_treatment=="JV2"] <- 400 * coef(lm13)[2] + coef(lm13)[1]
    plotDF$A600[plotDF$CO2_treatment=="JV2"] <- 600 * coef(lm13)[2] + coef(lm13)[1]
    
    plotDF$A_sens1 <- with(plotDF, (A400-A280)/A280)
    plotDF$A_sens2 <- with(plotDF, (A600-A400)/A400)
    
    
    plotDF$Ac280[plotDF$CO2_treatment=="aCO2"] <- 280 * coef(lm7)[2] + coef(lm7)[1]
    plotDF$Ac400[plotDF$CO2_treatment=="aCO2"] <- 400 * coef(lm5)[2] + coef(lm5)[1]
    plotDF$Ac600[plotDF$CO2_treatment=="aCO2"] <- 600 * coef(lm5)[2] + coef(lm5)[1]
    
    plotDF$Ac280[plotDF$CO2_treatment=="eCO2"] <- 280 * coef(lm8)[2] + coef(lm8)[1]
    plotDF$Ac400[plotDF$CO2_treatment=="eCO2"] <- 400 * coef(lm6)[2] + coef(lm6)[1]
    plotDF$Ac600[plotDF$CO2_treatment=="eCO2"] <- 600 * coef(lm6)[2] + coef(lm6)[1]
    
    plotDF$Ac280[plotDF$CO2_treatment=="JV2"] <- 280 * coef(lm17)[2] + coef(lm17)[1]
    plotDF$Ac400[plotDF$CO2_treatment=="JV2"] <- 400 * coef(lm14)[2] + coef(lm14)[1]
    plotDF$Ac600[plotDF$CO2_treatment=="JV2"] <- 600 * coef(lm14)[2] + coef(lm14)[1]
    
    plotDF$Ac_sens1 <- with(plotDF, (Ac400-Ac280)/Ac280)
    plotDF$Ac_sens2 <- with(plotDF, (Ac600-Ac400)/Ac400)
    #plotDF$Ac_sens1 <- with(plotDF, Ac400/Ac280)
    #plotDF$Ac_sens2 <- with(plotDF, Ac600/Ac400)
    
    plotDF$Aj280[plotDF$CO2_treatment=="aCO2"] <- 280 * coef(lm11)[2] + coef(lm11)[1]
    plotDF$Aj400[plotDF$CO2_treatment=="aCO2"] <- 400 * coef(lm9)[2] + coef(lm9)[1]
    plotDF$Aj600[plotDF$CO2_treatment=="aCO2"] <- 600 * coef(lm9)[2] + coef(lm9)[1]
    
    plotDF$Aj280[plotDF$CO2_treatment=="eCO2"] <- 280 * coef(lm12)[2] + coef(lm12)[1]
    plotDF$Aj400[plotDF$CO2_treatment=="eCO2"] <- 400 * coef(lm10)[2] + coef(lm10)[1]
    plotDF$Aj600[plotDF$CO2_treatment=="eCO2"] <- 600 * coef(lm10)[2] + coef(lm10)[1]
    
    plotDF$Aj280[plotDF$CO2_treatment=="JV2"] <- 280 * coef(lm18)[2] + coef(lm18)[1]
    plotDF$Aj400[plotDF$CO2_treatment=="JV2"] <- 400 * coef(lm15)[2] + coef(lm15)[1]
    plotDF$Aj600[plotDF$CO2_treatment=="JV2"] <- 600 * coef(lm15)[2] + coef(lm15)[1]
    
    plotDF$Aj_sens1 <- with(plotDF, (Aj400-Aj280)/Aj280)
    plotDF$Aj_sens2 <- with(plotDF, (Aj600-Aj400)/Aj400)
    #plotDF$Aj_sens1 <- with(plotDF, Aj400/Aj280)
    #plotDF$Aj_sens2 <- with(plotDF, Aj600/Aj400)
    
    ### plotting script
    subDF1 <- plotDF[,c("CO2_treatment", "A_sens1", "A_sens2")]
    subDF2 <- plotDF[,c("CO2_treatment", "Ac_sens1", "Ac_sens2")]
    subDF3 <- plotDF[,c("CO2_treatment", "Aj_sens1", "Aj_sens2")]
    
    subDF1$lab <- "A"
    subDF2$lab <- "Ac"
    subDF3$lab <- "Aj"
    
    colnames(subDF1) <- colnames(subDF2) <- colnames(subDF3) <- c("CO2_treatment", "ratio1", "ratio2", "lab")
    
    plotDF1 <- rbind(subDF1, subDF2, subDF3)
    mateDF1 <- subset(plotDF1, CO2_treatment == "aCO2")
    mateDF1$Position <- "4_MATE"
    mateDF1$ratio.se <- ""
    
    mateDF2 <- subset(plotDF1, CO2_treatment == "JV2")
    mateDF2$Position <- "5_MATE"
    mateDF2$ratio.se <- ""
    
    mateDF <- rbind(mateDF1, mateDF2)
    
    ################################ end MATE Ac vs. Aj ################################
    ####################################################################################
    
    
    
    
    ####################################################################################
    ############################ start two-leaf model ##################################
    
    ### read in simulated files
    ## aCO2
    ch01 <- read.csv("/Users/mingkaijiang/Documents/Research/Projects/WCT1_CO2_drawdown/Two_leaf_model/outputs/wtc_two_leaf_1.csv")
    ch03 <- read.csv("/Users/mingkaijiang/Documents/Research/Projects/WCT1_CO2_drawdown/Two_leaf_model/outputs/wtc_two_leaf_3.csv")
    ch11 <- read.csv("/Users/mingkaijiang/Documents/Research/Projects/WCT1_CO2_drawdown/Two_leaf_model/outputs/wtc_two_leaf_11.csv")
    
    ### merge by CO2 treatment
    plotDF1 <- rbind(ch01, ch03, ch11)
    subDF <- subset(plotDF1, Ca<=700&Ca>=300)
    
    lm1 <- lm(An_can~Ca, data=subDF)
    lm2 <- lm(An_sun~Ca, data=subDF)
    lm3 <- lm(An_sha~Ca, data=subDF)
    
    
    ### prepare a storage DF
    plotDF <- data.frame(NA, NA, NA, 
                           NA, NA, NA, 
                           NA, NA, NA)
    colnames(plotDF) <- c("A400", "A600", "A_sens", 
                          "Asun400", "Asun600", "Asun_sens",
                          "Asha400", "Asha600", "Asha_sens")
    
    ### assign values
    plotDF$A400 <- 400 * coef(lm1)[2] + coef(lm1)[1]
    plotDF$A600 <- 600 * coef(lm1)[2] + coef(lm1)[1]
    
    plotDF$Asun400 <- 400 * coef(lm2)[2] + coef(lm2)[1]
    plotDF$Asun600 <- 600 * coef(lm2)[2] + coef(lm2)[1]
    
    plotDF$Asha400 <- 400 * coef(lm3)[2] + coef(lm3)[1]
    plotDF$Asha600 <- 600 * coef(lm3)[2] + coef(lm3)[1]
    
    plotDF$A_sens <- with(plotDF, (A600-A400)/A400)
    plotDF$Asun_sens <- with(plotDF, (Asun600-Asun400)/Asun400)
    plotDF$Asha_sens <- with(plotDF, (Asha600-Asha400)/Asha400)
    
    ### plotting script
    twoDF <- data.frame("6_two_leaf", c("A", "A1sun", "A2sha"),
                        c(plotDF$A_sens, plotDF$Asun_sens, plotDF$Asha_sens),
                        NA)
    colnames(twoDF) <- c("Position", "lab", "ratio.mean", "ratio.se")
    
    ################################ end two-leaf model ################################
    ####################################################################################
    
    
    ####################################################################################
    ################################ start multi-layer model ###########################
    ### Read in MAAT simulation results
    aDF <- read.csv("data/MAAT/out_laicut.csv")

    ### subset
    subDF <- subset(aDF, canopy.ca_conc<=700&canopy.ca_conc>=300)
    
    lm1 <- lm(A~canopy.ca_conc, data=subDF[subDF$canopy.can_lai_upper==1,])
    lm2 <- lm(A~canopy.ca_conc, data=subDF[subDF$canopy.can_lai_upper==2,])
    lm3 <- lm(A~canopy.ca_conc, data=subDF[subDF$canopy.can_lai_upper==3,])
    lm4 <- lm(A~canopy.ca_conc, data=subDF[subDF$canopy.can_lai_upper==4,])
    lm5 <- lm(A~canopy.ca_conc, data=subDF[subDF$canopy.can_lai_upper==5,])
    
    ### prepare a storage DF
    plotDF <- data.frame(NA, NA, NA, c(1:5))
    colnames(plotDF) <- c("A400", "A600", "A_sens", "lai_upper")

    ### assign values
    plotDF$A400[plotDF$lai_upper==1] <- 400 * coef(lm1)[2] + coef(lm1)[1]
    plotDF$A600[plotDF$lai_upper==1] <- 600 * coef(lm1)[2] + coef(lm1)[1]
    
    plotDF$A400[plotDF$lai_upper==2] <- 400 * coef(lm2)[2] + coef(lm2)[1]
    plotDF$A600[plotDF$lai_upper==2] <- 600 * coef(lm2)[2] + coef(lm2)[1]
    
    plotDF$A400[plotDF$lai_upper==3] <- 400 * coef(lm3)[2] + coef(lm3)[1]
    plotDF$A600[plotDF$lai_upper==3] <- 600 * coef(lm3)[2] + coef(lm3)[1]
    
    plotDF$A400[plotDF$lai_upper==4] <- 400 * coef(lm4)[2] + coef(lm4)[1]
    plotDF$A600[plotDF$lai_upper==4] <- 600 * coef(lm4)[2] + coef(lm4)[1]
    
    plotDF$A400[plotDF$lai_upper==5] <- 400 * coef(lm5)[2] + coef(lm5)[1]
    plotDF$A600[plotDF$lai_upper==5] <- 600 * coef(lm5)[2] + coef(lm5)[1]
    
    plotDF$A_sens <- with(plotDF, (A600-A400)/A400)
    
    mean.v <- mean(plotDF$A_sens)
    se.v <- se(plotDF$A_sens)
    
    ### plotting script
    multiDF <- data.frame("7_multi", c("A", "Ac", "Aj"),
                        mean.v, se.v)
    colnames(multiDF) <- c("Position", "lab", "ratio.mean", "ratio.se")
    
    multiDF$ratio.mean[multiDF$lab=="Ac"] <- NA
    multiDF$ratio.mean[multiDF$lab=="Aj"] <- NA
    multiDF$ratio.se[multiDF$lab=="Ac"] <- NA
    multiDF$ratio.se[multiDF$lab=="Aj"] <- NA
    
    
    ################################ end multi-layer model ###########################
    ####################################################################################

    
    ####################################################################################
    ################################# Plotting script ##################################
    #### prepare plotting DF
    plotDF1 <- mateDF[,c("Position", "lab", "ratio2", "ratio.se")]
    colnames(plotDF1) <- c("Position", "lab", "ratio.mean", "ratio.se")
    
    plotDF2 <- wtcDF[,c("Position", "lab", "ratio.mean", "ratio.se")]
    plotDF2$Position <- gsub("12345", "3_Full", plotDF2$Position)
    plotDF2$Position <- gsub("up", "1_up", plotDF2$Position)
    plotDF2$Position <- gsub("low", "2_low", plotDF2$Position)
    
    plotDF2 <- plotDF2[plotDF2$Position%in%c("1_up", "2_low", "3_Full"),]
    
    
    plotDF <- rbind(plotDF1, plotDF2, twoDF, multiDF)
    plotDF$ratio.se <- as.numeric(plotDF$ratio.se)
    
    ### remove MATE2
    plotDF <- plotDF[plotDF$Position != "5_MATE",] 
    
    
    ### plotting
    p2 <- ggplot(data=plotDF, 
                 aes(Position, ratio.mean, group=lab)) +
        geom_rect(aes(xmin = 0, xmax = 2.5, 
                      ymin = 0.8, ymax = 1.6),
            alpha = 0.2, fill = "lightgray")+
        geom_bar(stat = "identity", aes(fill=lab), 
                 position="dodge") +
        geom_errorbar(aes(x=Position, ymin=ratio.mean-ratio.se, 
                          ymax=ratio.mean+ratio.se), 
                      position=position_dodge(0.9), width=0.2) +
        geom_vline(xintercept=2.5, lty=2)+
        geom_vline(xintercept=3.5, lty=2)+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=10),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="right",
              legend.box = 'vertical',
              legend.box.just = 'right',
              plot.title = element_text(size=16, face="bold", 
                                        hjust = 0.5),
              legend.text.align = 0)+
        ylab(expression(paste(delta * A * " / " * A[400])))+
        scale_x_discrete(breaks=c("1_up", "2_low", "3_Full", "4_MATE", "5_MATE", 
                                  "6_two_leaf", "7_multi"),
                          labels=c("Up", 
                                   "Low",
                                   "Full",
                                   "MATE",
                                   "MATE2",
                                   "Two-leaf",
                                   "Multi-layer"))+
        scale_fill_manual(name="",
                          breaks=c("A", "Ac", "Aj", "A1sun", "A2sha"),
                          labels=c("A", expression(paste(A[c])),
                                   expression(paste(A[j])),
                                   expression(paste(A[sun])),
                                   expression(paste(A[sha]))),
                          values = colorblind_pal()(5 + 1)[-1])+
        xlab("")+
        coord_cartesian(ylim = c(0, 0.5)) 
    
    
    #plot(p2)
    
    pdf("output/biochemical_parameters/relative_contribution_Ac_Aj_WTC_MATE.pdf", 
        width=6, height=4)
    plot(p2)
    dev.off()  
    
    

}