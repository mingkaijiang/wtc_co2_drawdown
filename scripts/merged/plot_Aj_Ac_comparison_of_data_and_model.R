plot_Aj_Ac_comparison_of_data_and_model <- function() {
    
    
    
    ####################################################################################
    ######################### start WTC data Ac vs. Aj #################################
    #### read in fitaci data at canopy and leaf scales and plot the Aj and Ac comparison
    ### we have multiple dates in leaf-scale measurements
    stDF.c <- read.csv("output/canopy/canopy_scale_parameters.csv", header=T)
    stDF.l <- read.csv("output/leaf/leaf_scale_parameters.csv", header=T)
    
    ### subset chambers
    subDF.l <- subset(stDF.l, Chamber%in%c("1", "3", "11", "4", "8"))
    subDF.c <- subset(stDF.c, Chamber%in%c("1", "3", "11", "4", "8"))
    
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
    stDF$A600_over_A400 <- with(stDF, ALEAF_600/ALEAF_400)
    stDF$Ac600_over_Ac400 <- with(stDF, Ac_600/Ac_400)
    stDF$Aj600_over_Aj400 <- with(stDF, Aj_600/Aj_400)
    
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
    
    ### merge
    myDF <- rbind(inDF1, inDF2, inDF3, inDF4, inDF5)
    
    myDF$CO2_treatment <- myDF$T_treatment
    
    ### simplify
    myDF <- myDF[,c("DateTime", "chamber", "Canopy", "DOY", "hod", "year",
                    "Ca", "LAI", "CO2_treatment", "Aj", "Ac", "Asat")]
    
    ### subset Ca = 350 to 650 range
    subDF1 <- subset(myDF, Ca>=350&Ca<=650)
    subDF2 <- subset(myDF, Ca>=200&Ca<=350)
    
    ### prepare a storage DF
    plotDF <- data.frame(c("aCO2", "eCO2"), NA, NA, NA, NA, NA,
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
    
    
    ### assign values
    plotDF$A280[plotDF$CO2_treatment=="aCO2"] <- 280 * coef(lm3)[2] + coef(lm3)[1]
    plotDF$A400[plotDF$CO2_treatment=="aCO2"] <- 400 * coef(lm1)[2] + coef(lm1)[1]
    plotDF$A600[plotDF$CO2_treatment=="aCO2"] <- 600 * coef(lm1)[2] + coef(lm1)[1]
    
    plotDF$A280[plotDF$CO2_treatment=="eCO2"] <- 280 * coef(lm4)[2] + coef(lm4)[1]
    plotDF$A400[plotDF$CO2_treatment=="eCO2"] <- 400 * coef(lm2)[2] + coef(lm2)[1]
    plotDF$A600[plotDF$CO2_treatment=="eCO2"] <- 600 * coef(lm2)[2] + coef(lm2)[1]
    
    #plotDF$A_sens1 <- with(plotDF, (A400-A280)/A280)
    #plotDF$A_sens2 <- with(plotDF, (A600-A400)/A400)
    plotDF$A_sens1 <- with(plotDF, A400/A280)
    plotDF$A_sens2 <- with(plotDF, A600/A400)
    
    
    plotDF$Ac280[plotDF$CO2_treatment=="aCO2"] <- 280 * coef(lm7)[2] + coef(lm7)[1]
    plotDF$Ac400[plotDF$CO2_treatment=="aCO2"] <- 400 * coef(lm5)[2] + coef(lm5)[1]
    plotDF$Ac600[plotDF$CO2_treatment=="aCO2"] <- 600 * coef(lm5)[2] + coef(lm5)[1]
    
    plotDF$Ac280[plotDF$CO2_treatment=="eCO2"] <- 280 * coef(lm8)[2] + coef(lm8)[1]
    plotDF$Ac400[plotDF$CO2_treatment=="eCO2"] <- 400 * coef(lm6)[2] + coef(lm6)[1]
    plotDF$Ac600[plotDF$CO2_treatment=="eCO2"] <- 600 * coef(lm6)[2] + coef(lm6)[1]
    
    #plotDF$Ac_sens1 <- with(plotDF, (Ac400-Ac280)/Ac280)
    #plotDF$Ac_sens2 <- with(plotDF, (Ac600-Ac400)/Ac400)
    plotDF$Ac_sens1 <- with(plotDF, Ac400/Ac280)
    plotDF$Ac_sens2 <- with(plotDF, Ac600/Ac400)
    
    plotDF$Aj280[plotDF$CO2_treatment=="aCO2"] <- 280 * coef(lm11)[2] + coef(lm11)[1]
    plotDF$Aj400[plotDF$CO2_treatment=="aCO2"] <- 400 * coef(lm9)[2] + coef(lm9)[1]
    plotDF$Aj600[plotDF$CO2_treatment=="aCO2"] <- 600 * coef(lm9)[2] + coef(lm9)[1]
    
    plotDF$Aj280[plotDF$CO2_treatment=="eCO2"] <- 280 * coef(lm12)[2] + coef(lm12)[1]
    plotDF$Aj400[plotDF$CO2_treatment=="eCO2"] <- 400 * coef(lm10)[2] + coef(lm10)[1]
    plotDF$Aj600[plotDF$CO2_treatment=="eCO2"] <- 600 * coef(lm10)[2] + coef(lm10)[1]
    
    #plotDF$Aj_sens1 <- with(plotDF, (Aj400-Aj280)/Aj280)
    #plotDF$Aj_sens2 <- with(plotDF, (Aj600-Aj400)/Aj400)
    plotDF$Aj_sens1 <- with(plotDF, Aj400/Aj280)
    plotDF$Aj_sens2 <- with(plotDF, Aj600/Aj400)
    
    ### plotting script
    subDF1 <- plotDF[,c("CO2_treatment", "A_sens1", "A_sens2")]
    subDF2 <- plotDF[,c("CO2_treatment", "Ac_sens1", "Ac_sens2")]
    subDF3 <- plotDF[,c("CO2_treatment", "Aj_sens1", "Aj_sens2")]
    
    subDF1$lab <- "A"
    subDF2$lab <- "Ac"
    subDF3$lab <- "Aj"
    
    colnames(subDF1) <- colnames(subDF2) <- colnames(subDF3) <- c("CO2_treatment", "ratio1", "ratio2", "lab")
    
    plotDF1 <- rbind(subDF1, subDF2, subDF3)
    mateDF <- subset(plotDF1, CO2_treatment == "aCO2")
    mateDF$Position <- "4_MATE"
    mateDF$ratio.se <- ""
    
    ################################ end MATE Ac vs. Aj ################################
    ####################################################################################
    
    
    
    
    ####################################################################################
    ############################ start two-leaf model ##################################
    
    ### rea in simulated files
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
    
    
    ################################ end two-leaf model ################################
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
    
    
    plotDF <- rbind(plotDF1, plotDF2)
    plotDF$ratio.se <- as.numeric(plotDF$ratio.se)
    
    
    ### WTC leaf up
    p1 <- ggplot(data=plotDF, 
                 aes(lab, ratio.mean, group=Position)) +
        geom_bar(stat = "identity", aes(fill=Position), 
                 position="dodge") +
        geom_errorbar(aes(x=lab, ymin=ratio.mean-ratio.se, 
                          ymax=ratio.mean+ratio.se), 
                      position=position_dodge(0.9), width=0.2) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="right",
              legend.box = 'vertical',
              legend.box.just = 'left',
              plot.title = element_text(size=16, face="bold", 
                                        hjust = 0.5))+
        ylab(expression(paste(A[600] * " / " * A[400])))+
        scale_fill_manual(name="",
                          breaks=c("1_up", "2_low", "3_Full", "4_MATE"),
                          labels=c("Up", 
                                   "Low",
                                   "Full",
                                   "MATE"),
                          values = colorblind_pal()(4 + 1)[-1])+
        xlab("")+
        scale_x_discrete(breaks=c("A", "Ac", "Aj"),
                         labels=c("A", expression(paste(A[c])),
                                  expression(paste(A[j]))))+
        #ylim(0, 2)+
        coord_cartesian(ylim = c(1, 1.5)) 
    
    
    p2 <- ggplot(data=plotDF, 
                 aes(Position, ratio.mean, group=lab)) +
        geom_bar(stat = "identity", aes(fill=lab), 
                 position="dodge") +
        geom_errorbar(aes(x=Position, ymin=ratio.mean-ratio.se, 
                          ymax=ratio.mean+ratio.se), 
                      position=position_dodge(0.9), width=0.2) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="right",
              legend.box = 'vertical',
              legend.box.just = 'left',
              plot.title = element_text(size=16, face="bold", 
                                        hjust = 0.5))+
        ylab(expression(paste(A[600] * " / " * A[400])))+
        scale_x_discrete(breaks=c("1_up", "2_low", "3_Full", "4_MATE"),
                          labels=c("Up", 
                                   "Low",
                                   "Full",
                                   "MATE"))+
        scale_fill_manual(name="",
                          breaks=c("A", "Ac", "Aj"),
                          labels=c("A", expression(paste(A[c])),
                                   expression(paste(A[j]))),
                          values = colorblind_pal()(3 + 1)[-1])+
        xlab("")+
        coord_cartesian(ylim = c(1, 1.5)) 
    
    
    pdf("output/biochemical_parameters/relative_contribution_Ac_Aj_WTC_MATE.pdf", width=6, height=4)
    plot(p2)
    dev.off()  
    
    

}