plot_Aj_Ac_comparison_of_MATE_result <- function() {

    
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
    
    p1 <- ggplot(data=plotDF1, 
                 aes(lab, ratio2, group=CO2_treatment)) +
        geom_bar(stat = "identity", aes(fill=lab, alpha=CO2_treatment), 
                 position="dodge") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'vertical',
              legend.box.just = 'left',
              plot.title = element_text(size=16, face="bold", 
                                        hjust = 0.5))+
        ylab(expression(paste(A[600] * " / " * A[400])))+
        scale_fill_manual(name="",
                          breaks=c("A", "Ac", "Aj"),
                          labels=c("A", expression(paste(A[c])),
                                   expression(paste(A[j]))),
                          values = colorblind_pal()(3 + 1)[-1])+
        xlab("")+
        scale_x_discrete(breaks=c("A", "Ac", "Aj"),
                         labels=c("A", expression(paste(A[c])),
                                  expression(paste(A[j]))))+
        scale_alpha_manual(breaks=c("aCO2", "eCO2"),
                           values=c(0.5, 1.0),
                           labels=c(expression(aC[a]),
                                    expression(eC[a])))+
        coord_cartesian(ylim = c(1, 1.5))
    
    pdf("output/biochemical_parameters/relative_contribution_Ac_Aj_MATE.pdf", width=4, height=4)
    plot(p1)
    dev.off()  
    
    
    
    
    
}