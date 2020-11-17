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
    #stDF <- subset(stDF, CO2_treatment=="aCO2")

    #### A sens ratio
    stDF$Asens <- with(stDF, (ALEAF_600-ALEAF_400)/ALEAF_400)
    stDF$Ac_sens <- with(stDF, (Ac_600-Ac_400)/Ac_400)
    stDF$Aj_sens <- with(stDF, (Aj_600-Aj_400)/Aj_400)
    
    ### subset
    subDF1 <- stDF[,c("Asens", "CO2_treatment", "Chamber", "Position", "Type")]
    subDF2 <- stDF[,c("Ac_sens", "CO2_treatment", "Chamber", "Position", "Type")]
    subDF3 <- stDF[,c("Aj_sens", "CO2_treatment", "Chamber", "Position", "Type")]
    
    subDF1$lab <- "A"
    subDF2$lab <- "Ac"
    subDF3$lab <- "Aj"
    
    colnames(subDF1) <- colnames(subDF2) <- colnames(subDF3) <- c("value", "CO2_treatment", 
                                                                  "Chamber", "Position", "Type", "Limiting")
    
    myDF <- rbind(subDF1, subDF2, subDF3)
    
    ### summary By
    wtcDF <- summaryBy(value~Position+Limiting+CO2_treatment, 
                       FUN=c(mean, se), data=myDF, keep.names=T)
    
    ############################ end WTC data Ac vs. Aj ################################
    ####################################################################################

    
    ####################################################################################
    ####################################### MAAT #######################################
    ### read in MATE simulation results
    maatDF <- read.csv("output/MAAT/MAAT_Asens_output_table.csv")
    
    maatDF$Ac_sens <- as.numeric(maatDF$Ac_sens)
    
    maatLong <- melt(setDT(maatDF), id.vars = c("CO2_treatment", "Model", "LAI"), 
                     measure.vars = 10:12, variable.name = "Limiting")
    
    maatDF <- summaryBy(value~CO2_treatment+Model+Limiting, FUN=c(mean,se),
                        data=maatLong, na.rm=T, keep.names=T)
    
    ################################ end MAAT Ac vs. Aj ################################
    ####################################################################################
    
    
    ####################################################################################
    ############################ start two-leaf model ##################################
    
    ### read in simulated files
    myDF <- read.csv("output/simulated/two_leaf_result_summary.csv")
    
    ### melt
    twoDF <- melt(setDT(myDF), id.vars = c("CO2_treatment", "canopy"), 
                    measure.vars = 9:11, variable.name = "Limiting")
    
    ################################ end two-leaf model ###########3####################
    ####################################################################################

    ## process all three dataset and merge
    twoDF$value.se <- NA
    colnames(twoDF) <- c("CO2_treatment", "Position", "Limiting",
                         "value.mean", "value.se")
    twoDF$Model <- "two-leaf"
    twoDF <- twoDF[,c("CO2_treatment", "Model", "Position", "Limiting", 
                      "value.mean", "value.se")]
    
    wtcDF$Model <- "1_WTC"
    wtcDF <- wtcDF[,c("CO2_treatment", "Model", "Position", "Limiting",
                      "value.mean", "value.se")]
    
    maatDF$Position <- 12345
    maatDF <- maatDF[,c("CO2_treatment", "Model", "Position", "Limiting",
                        "value.mean", "value.se")]
    
    ### merge
    plotDF <- rbind(maatDF, wtcDF, twoDF)
    
    ## consistent labelling
    plotDF$Limiting <- gsub("Ac_sens", "Ac", plotDF$Limiting)
    plotDF$Limiting <- gsub("Aj_sens", "Aj", plotDF$Limiting)
    plotDF$Limiting <- gsub("Asens", "A", plotDF$Limiting)
    plotDF$Limiting <- gsub("Acan", "A", plotDF$Limiting)
    
    plotDF$Model <- gsub("Bigleaf", "2_bigleaf", plotDF$Model)
    plotDF$Model <- gsub("two-leaf", "3_twoleaf", plotDF$Model)
    plotDF$Model <- gsub("Multilayer", "4_multilayer", plotDF$Model)
    
    plotDF$Position <- gsub("12345", "3_Full", plotDF$Position)
    plotDF$Position <- gsub("up", "1_up", plotDF$Position)
    plotDF$Position <- gsub("low", "2_low", plotDF$Position)
    
    ### remove some points
    plotDF <- plotDF[plotDF$Position%in%c("1_up", "2_low", "3_Full"),]
    
    ### add label column
    plotDF$Lab <- plotDF$Model
    plotDF$Lab <- ifelse(plotDF$Model=="1_WTC", plotDF$Position, plotDF$Model)
    plotDF$Lab <- gsub("2_bigleaf", "4_bigleaf", plotDF$Lab)
    plotDF$Lab <- gsub("3_twoleaf", "5_twoleaf", plotDF$Lab)
    plotDF$Lab <- gsub("4_multilayer", "6_multilayer", plotDF$Lab)
    
    ### plotting
    p1 <- ggplot(data=plotDF[plotDF$CO2_treatment == "aCO2",], 
                 aes(Lab, value.mean, group=Limiting)) +
        geom_rect(aes(xmin = 0, xmax = 2.5, 
                      ymin = 0.0, ymax = 1.6),
            alpha = 0.2, fill = "lightgray")+
        geom_bar(stat = "identity", aes(fill=Limiting), 
                 position="dodge") +
        geom_errorbar(aes(x=Lab, ymin=value.mean-value.se, 
                          ymax=value.mean+value.se), 
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
        ylab(expression(paste(Delta * A * " / " * A[400])))+
        scale_x_discrete(breaks=c("1_up", "2_low", "3_Full", "4_bigleaf", "5_twoleaf", 
                                  "6_multilayer"),
                          labels=c("Up", 
                                   "Low",
                                   "Full",
                                   "Big leaf",
                                   "Two leaf",
                                   "Multi-layer"))+
        scale_fill_manual(name="",
                          breaks=c("A", "Ac", "Aj", "Asun", "Asha"),
                          labels=c("A", expression(paste(A[c])),
                                   expression(paste(A[j])),
                                   expression(paste(A[sun])),
                                   expression(paste(A[sha]))),
                          values = colorblind_pal()(5 + 1)[-1])+
        xlab("")+
        coord_cartesian(ylim = c(0, 0.5)) 
    
    
    pdf("output/simulated/relative_contribution_Ac_Aj_all_model.pdf", 
        width=6, height=4)
    plot(p1)
    dev.off()  
    
    

}