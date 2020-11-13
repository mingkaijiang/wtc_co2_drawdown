plot_CO2_acclimation_effect_on_biochemical_parameters <- function(mgDF) {
    
    ### generate identity list
    idDF <- unique(mgDF[,c("Identity", "Chamber", "Position", 
                           "Type", "CO2_treatment")])
    
    ################################# plot statistics #################################
    #### read biochemical parameter summary table
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
    
    ### prepare plotDF
    plotDF3 <- summaryBy(Vcmax+Jmax+Ci_transition_Ac_Aj+JVratio~Position+Type+CO2_treatment,
                         FUN=c(mean,se), keep.names=T, data=stDF)
    
    plotDF3$Position <- gsub("12345", "5_Full", plotDF3$Position)
    plotDF3$Position <- gsub("345", "4_TM", plotDF3$Position)
    plotDF3$Position <- gsub("45", "3_Top", plotDF3$Position)
    plotDF3$Position <- gsub("low", "2_low", plotDF3$Position)
    plotDF3$Position <- gsub("up", "1_up", plotDF3$Position)
    
    
    ###################################################################################
    #### look at the CO2 difference in Jmax and Vcmax for each canopy position
    subDF1 <- subset(plotDF3, CO2_treatment=="aCO2")
    subDF2 <- subset(plotDF3, CO2_treatment=="eCO2")
    
    ### add sample size information
    subDF1$n[subDF1$Position%in%c("5_Full", "4_TM", "3_Top",
                                  "2_low")] <- 3.0
    
    subDF1$n[subDF1$Position=="1_up"] <- 20.0
    
    subDF2$n[subDF2$Position%in%c("5_Full", "4_TM", "3_Top", "2_low")] <- 2.0
    subDF2$n[subDF2$Position%in%c("1_up")] <- 15.0
    
    ### calculate SD
    subDF1$Vcmax.sd <- subDF1$Vcmax.se * subDF1$n
    subDF1$Jmax.sd <- subDF1$Jmax.se * subDF1$n
    subDF1$JVratio.sd <- subDF1$JVratio.se * subDF1$n
    subDF1$Ci_transition_Ac_Aj.sd <- subDF1$Ci_transition_Ac_Aj.se * subDF1$n
    
    subDF2$Vcmax.sd <- subDF2$Vcmax.se * subDF2$n
    subDF2$Jmax.sd <- subDF2$Jmax.se * subDF2$n
    subDF2$JVratio.sd <- subDF2$JVratio.se * subDF2$n
    subDF2$Ci_transition_Ac_Aj.sd <- subDF2$Ci_transition_Ac_Aj.se * subDF2$n
    
    plotDF4 <- subDF1
    plotDF4$CO2_treatment <- "diff"
    
    
    ### calculate pooled sd and mean for the difference
    for (i in unique(subDF1$Position)) {
        ### means
        plotDF4$Vcmax.diff.mean[plotDF4$Position==i] <- subDF2$Vcmax.mean[subDF2$Position==i]-
            subDF1$Vcmax.mean[subDF1$Position==i]
        
        plotDF4$Jmax.diff.mean[plotDF4$Position==i] <- subDF2$Jmax.mean[subDF2$Position==i]-
            subDF1$Jmax.mean[subDF1$Position==i]
        
        plotDF4$JVratio.diff.mean[plotDF4$Position==i] <- subDF2$JVratio.mean[subDF2$Position==i]-
            subDF1$JVratio.mean[subDF1$Position==i]
        
        plotDF4$Ci_transition_Ac_Aj.diff.mean[plotDF4$Position==i] <- subDF2$Ci_transition_Ac_Aj.mean[subDF2$Position==i]-
            subDF1$Ci_transition_Ac_Aj.mean[subDF1$Position==i]
        
        
        ### sd
        plotDF4$Vcmax.diff.sd[plotDF4$Position==i] <- sqrt((((subDF1$n[subDF1$Position==i]-1)*subDF1$Vcmax.sd[subDF1$Position==i]^2) +
                                                                ((subDF2$n[subDF2$Position==i]-1)*subDF2$Vcmax.sd[subDF2$Position==i]^2)) / (subDF1$n[subDF1$Position==i] + subDF2$n[subDF2$Position==i] - 2))
        
        plotDF4$Jmax.diff.sd[plotDF4$Position==i] <- sqrt((((subDF1$n[subDF1$Position==i]-1)*subDF1$Jmax.sd[subDF1$Position==i]^2) +
                                                               ((subDF2$n[subDF2$Position==i]-1)*subDF2$Jmax.sd[subDF2$Position==i]^2)) / (subDF1$n[subDF1$Position==i] + subDF2$n[subDF2$Position==i] - 2) )
        
        plotDF4$JVratio.diff.sd[plotDF4$Position==i] <- sqrt((((subDF1$n[subDF1$Position==i]-1)*subDF1$JVratio.sd[subDF1$Position==i]^2) +
                                                                  ((subDF2$n[subDF2$Position==i]-1)*subDF2$JVratio.sd[subDF2$Position==i]^2)) / (subDF1$n[subDF1$Position==i] + subDF2$n[subDF2$Position==i] - 2) )
        
        plotDF4$Ci_transition_Ac_Aj.diff.sd[plotDF4$Position==i] <- sqrt((((subDF1$n[subDF1$Position==i]-1)*subDF1$Ci_transition_Ac_Aj.sd[subDF1$Position==i]^2) +
                                                                              ((subDF2$n[subDF2$Position==i]-1)*subDF2$Ci_transition_Ac_Aj.sd[subDF2$Position==i]^2)) / (subDF1$n[subDF1$Position==i] + subDF2$n[subDF2$Position==i] - 2) )
        
    }
    
    
    ### convert into ratio
    for (i in unique(plotDF4$Position)) {
        ### means
        plotDF4$Vcmax.ratio.mean[plotDF4$Position==i] <- plotDF4$Vcmax.diff.mean[plotDF4$Position==i] / 
            plotDF4$Vcmax.mean[plotDF4$Position==i]
        
        plotDF4$Jmax.ratio.mean[plotDF4$Position==i] <- plotDF4$Jmax.diff.mean[plotDF4$Position==i] / 
            plotDF4$Jmax.mean[plotDF4$Position==i]
        
        plotDF4$JVratio.ratio.mean[plotDF4$Position==i] <- plotDF4$JVratio.diff.mean[plotDF4$Position==i] / 
            plotDF4$JVratio.mean[plotDF4$Position==i]
        
        plotDF4$Ci_transition_Ac_Aj.ratio.mean[plotDF4$Position==i] <- plotDF4$Ci_transition_Ac_Aj.diff.mean[plotDF4$Position==i] / 
            plotDF4$Ci_transition_Ac_Aj.mean[plotDF4$Position==i]
        
    }
    
    
    ### prepare plotting DF
    subDF3 <- plotDF4[,c("Position", "Type", "Vcmax.ratio.mean")]
    subDF4 <- plotDF4[,c("Position", "Type", "Jmax.ratio.mean")]
    subDF3$lab <- "Vcmax"
    subDF4$lab <- "Jmax"
    colnames(subDF3) <- colnames(subDF4) <- c("Position", "Type", "ratio", "lab")
    
    plotDF5 <- rbind(subDF3, subDF4)
    
    ### plotting
    p1 <- ggplot(plotDF5, aes(Position, ratio*100, group=lab)) +
        geom_bar(stat = "identity", position="dodge", aes(fill=lab))+
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
        ylab(expression(paste(CO[2], " effect (%)")))+
        scale_fill_manual(name="",
                          limits=c("Jmax", "Vcmax"),
                          values=c("grey", "black"),
                          labels=c(expression(J[max]),
                                   expression(V[cmax])))+
        scale_x_discrete(name="", 
                         breaks=c("5_Full", "4_TM", "3_Top", "2_low", "1_up"), 
                         labels=c("Full", "T+M", "Top", "Low", "Up"))
    
    
    pdf("output/biochemical_parameters/CO2_effect_on_Jmax_Vcmax.pdf", width=4, height=4)
    plot(p1)
    dev.off()  
    
}