plot_A_Ca_sensitivity_based_on_fitaci_function_result <- function(mgDF) {
    #### This script plots A-CA curve for leaf and canopy data on to the same plot
    #### and compare the shapes
    #### which can be related to Rogers et al. 2017 conceptual figure
    #### Only based on a subset of data that is well-watered treatment
    
  ### generate identity list
  idDF <- unique(mgDF[,c("Identity", "Chamber", "Position", 
                         "Type", "CO2_treatment")])
  
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
  
  ### normalized sensitivity to per CO2 concentration
  stDF$A_sens <- with(stDF, (ALEAF_600-ALEAF_400)/(600-400))
  
  ### normalized sensitivity to A400
  stDF$A_sens_norm <- with(stDF, (ALEAF_600-ALEAF_400)/ALEAF_400)
  
  write.csv(stDF, "output/A-Ca/fitaci_predicted_A_at_400_600_ppm.csv", row.names=F)
  
  
  
  ### summarize
  plotDF <- summaryBy(Vcmax+Jmax+JVratio+Ci_transition_Ac_Aj+A_sens+A_sens_norm~Position+Type+CO2_treatment,
                       FUN=c(mean,se), keep.names=T, data=stDF)
  
  
  write.csv(plotDF, "output/leaf/leaf_and_canopy_scale_parameter_summary_table.csv",
            row.names=F)
  
  
  
  plotDF$Position <- gsub("12345", "5_Full", plotDF$Position)
  plotDF$Position <- gsub("345", "4_TM", plotDF$Position)
  plotDF$Position <- gsub("45", "3_Top", plotDF$Position)
  plotDF$Position <- gsub("low", "2_low", plotDF$Position)
  plotDF$Position <- gsub("up", "1_up", plotDF$Position)
  
  ### prepare data set for performing statistics
  stDF$FCa[stDF$CO2_treatment=="aCO2"] <- "aC"
  stDF$FCa[stDF$CO2_treatment=="eCO2"] <- "eC"
  
  stDF$FPos <- gsub("12345", "5_Full", stDF$Position)
  stDF$FPos <- gsub("345", "4_TM", stDF$Position)
  stDF$FPos <- gsub("45", "3_Top", stDF$Position)
  stDF$FPos <- gsub("low", "2_low", stDF$Position)
  stDF$FPos <- gsub("up", "1_up", stDF$Position)
  
  
  
  ### test statistics
  mod1 <- lmer(Vcmax ~ FPos * FCa + (1|Chamber), data=stDF)
  out1 <- anova(mod1)
  lab1 <- summary(glht(mod1, linfct = mcp(FPos = "Tukey")))
  
  mod1 <- lmer(Jmax ~ FPos * FCa + (1|Chamber), data=stDF)
  out1 <- anova(mod1)
  lab1 <- summary(glht(mod1, linfct = mcp(FPos = "Tukey")))
  
  mod1 <- lmer(JVratio ~ FPos * FCa + (1|Chamber), data=stDF)
  out1 <- anova(mod1)
  lab1 <- summary(glht(mod1, linfct = mcp(FPos = "Tukey")))
  
  mod1 <- lmer(Ci_transition_Ac_Aj ~ FPos * FCa + (1|Chamber), data=stDF)
  out1 <- anova(mod1)
  lab1 <- summary(glht(mod1, linfct = mcp(FPos = "Tukey")))
  
  mod1 <- lmer(A_sens_norm ~ FPos * FCa + (1|Chamber), data=stDF)
  out1 <- anova(mod1)
  lab1 <- summary(glht(mod1, linfct = mcp(FPos = "Tukey")))
  
    
  ################################# plotting
   p1 <- ggplot(plotDF, aes(Position, A_sens.mean, fill=CO2_treatment)) +
     geom_errorbar(aes(x=Position, ymin=A_sens.mean-A_sens.se,
                       ymax=A_sens.mean+A_sens.se,
                       col=CO2_treatment), 
                   position=position_dodge(width=0.2),
                   width=0.2)+
     geom_point(aes(Position, A_sens.mean, 
                    fill=CO2_treatment), 
                position=position_dodge(width=0.2),
                pch=21, size=4)+
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
           legend.box.just = 'left')+
     xlab("")+
     ylab(expression(paste(delta,  "A / ", delta, CO[2], " (", mu, "mol ", m^-2, " ", s^-1, " ", ppm^-1, ")")))+
     scale_fill_manual(name="",
                       limits=c("aCO2", "eCO2"),
                       values=c("blue2", "red3"),
                       labels=c(expression(paste(aCO[2])),
                                expression(paste(eCO[2]))))+
     scale_color_manual(name="",
                       limits=c("aCO2", "eCO2"),
                       values=c("blue2", "red3"),
                       labels=c(expression(paste(aCO[2])),
                                expression(paste(eCO[2]))))+
     scale_x_discrete(name="", 
                      breaks=c("5_Full", "4_TM", "3_Top", "2_low", "1_up"), 
                      labels=c("Full", "T+M", "Top", "Low", "Up"))
    
    p2 <- ggplot(plotDF, aes(Position, A_sens_norm.mean, fill=CO2_treatment)) +
      geom_errorbar(aes(x=Position, ymin=A_sens_norm.mean-A_sens_norm.se,
                        ymax=A_sens_norm.mean+A_sens_norm.se,
                        col=CO2_treatment), 
                    position=position_dodge(width=0.2),
                    width=0.2)+
      geom_point(aes(Position, A_sens_norm.mean, 
                     fill=CO2_treatment), 
                 position=position_dodge(width=0.2),
                 pch=21, size=4)+
      theme_linedraw() +
      theme(panel.grid.minor=element_blank(),
            axis.text.x=element_text(size=12),
            axis.title.x=element_text(size=14),
            axis.text.y=element_text(size=12),
            axis.title.y=element_text(size=14),
            legend.text=element_text(size=12),
            legend.title=element_text(size=14),
            panel.grid.major=element_blank(),
            legend.position="bottom",
            legend.box = 'vertical',
            legend.box.just = 'left')+
      xlab("")+
      ylab(expression(paste(delta,  "A / ", A[400])))+
      scale_fill_manual(name="",
                        limits=c("aCO2", "eCO2"),
                        values=c("blue2", "red3"),
                        labels=c(expression(paste(aC[a])),
                                 expression(paste(eC[a]))))+
      scale_color_manual(name="",
                         limits=c("aCO2", "eCO2"),
                         values=c("blue2", "red3"),
                         labels=c(expression(paste(aC[a])),
                                  expression(paste(eC[a]))))+
      scale_x_discrete(name="", 
                       breaks=c("5_Full", "4_TM", "3_Top", "2_low", "1_up"), 
                       labels=c("Full", "T+M", "Top", "Low", "Up"))
    
  
    
    #legend_shared <- get_legend(p1 + theme(legend.position="bottom",
    #                                       legend.box = 'vertical',
    #                                       legend.box.just = 'left'))
    #
    #combined_plots <- plot_grid(p1, p2, 
    #                            labels=c("(a)", "(b)"),
    #                            ncol=1, align="vh", axis = "l",
    #                            label_x=0.88, label_y=0.95,
    #                            label_size = 18)
    

    pdf("output/A-Ca/fitaci_predicted_A_sensitivity_plot.pdf", width=4, height=4)
    #plot_grid(combined_plots, legend_shared, ncol=1, rel_heights=c(1,0.1))
    plot(p2)
    dev.off()  
  
}
