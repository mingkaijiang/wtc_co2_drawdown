
leaf_ACI_processing <- function() {
    #### individual leaf ACi curve measurement processing
    #### Fit A-CI curve for each chamber
    #### datasets are two: the first is upper canopy, and the second is lower canopy
    
    ### read in datasets
    myDF1 <- read.csv("data/ACi_curves/HFE_Aci_2008-2009.csv",stringsAsFactors=FALSE)
    myDF2  <- read.csv("data/ACi_curves/HFE_Aci_lowcanopy_2008-2009.csv",stringsAsFactors=FALSE)
    
    ### combine the datasets
    ### multiple factors: CO2 treatment
    ###                   chamber
    ###                   water treatment
    ###                   canopy location
    ###                   time
    myDF <- rbind(myDF1, myDF2)
    
    
    #### Fitting ACI curve at the finest resolution
    fits.all <- fitacis(myDF, group="Identity", fitmethod="bilinear")
    
    ### plot all fittings on the same graph, looks messy
    #plot(fits.all, how="oneplot")
    #plot(fits.all[[1]], col="black", add=T)
    
    ### assign factors onto the dataframe
    coefDF <- coef(fits.all)
    id.list <- unique(coefDF$Identity)
    
    for (i in id.list) {
        coefDF[coefDF$Identity==i, "Date"] <- unique(myDF[myDF$Identity==i, "Date"])
        coefDF[coefDF$Identity==i, "chamber"] <- unique(myDF[myDF$Identity==i, "chamber"])
        coefDF[coefDF$Identity==i, "Height"] <- unique(myDF[myDF$Identity==i, "Height"])
        coefDF[coefDF$Identity==i, "CO2_treatment"] <- unique(myDF[myDF$Identity==i, "CO2_treatment"])
        coefDF[coefDF$Identity==i, "Water_treatment"] <- unique(myDF[myDF$Identity==i, "Water_treatment"])
        coefDF[coefDF$Identity==i, "inside_or_outside_WTC"] <- unique(myDF[myDF$Identity==i, "inside_or_outside_WTC"])
    }
    
    
    ### add vcmax to jmax ratio
    coefDF$JVratio <- coefDF$Jmax/coefDF$Vcmax
    
    coefDF$Date <- as.Date(coefDF$Date)
    
    ### visually check whether Jmax, Vcmax, and JVratio change with multiple treatments
    
    ### plot a time series of Vcmax and Jmax and JV ratio
    p1 <- ggplot(coefDF) +
        geom_point(aes(Date, Vcmax, col=CO2_treatment, 
                       pch=Water_treatment, size=Height))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)+
        ylab(expression(Vc[max]))+
        scale_color_manual(limits=c("ambient", "elevated"),
                           values=c("blue2", "red3"))
    
    p2 <- ggplot(coefDF) +
        geom_point(aes(Date, Jmax, col=CO2_treatment, 
                       pch=Water_treatment, size=Height))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)+
        ylab(expression(J[max]))+
        scale_color_manual(limits=c("ambient", "elevated"),
                           values=c("blue2", "red3"))
    
    p3 <- ggplot(coefDF) +
        geom_point(aes(Date, JVratio, col=CO2_treatment, 
                       pch=Water_treatment, size=Height))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="bottom",
              legend.text.align=0)+
        ylab("JV ratio")+
        scale_color_manual(name=expression(paste(CO[2], " treatment")),
                           limits=c("ambient", "elevated"),
                           values=c("blue2", "red3"))+
        scale_size_discrete(name="Position")+
        scale_shape_discrete(name="Water treatment")+
        theme(legend.direction = "vertical", legend.box = "horizontal")


    pdf("output/leaf_flux_all_treatment_time_series_parameters.pdf", width=6, height=12)
    plot_grid(p1, p2, p3, rel_heights=c(1,1,1.5),
              labels="AUTO", ncol=1, align="v", axis = "l")
    dev.off()
    
    
    
    ### look at box plot of the groups 
    p4 <- ggplot(coefDF,
                 aes(CO2_treatment,Vcmax,
                     fill=Water_treatment))+
        facet_grid(.~Height,scale="free_x",space="free",
                   labeller=label_both)+
        guides(alpha = guide_legend(override.aes = list(fill = "darkgray")))+
        geom_boxplot()+
        xlab(expression(paste(CO[2], " treatment")))+
        ylab(expression(Vc[max]))+
        scale_fill_manual(name="Water treatment",
                          limits=c("wet", "dry"),
                          values=c("lightblue", "pink"))+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)
    
    p5 <- ggplot(coefDF,
                 aes(CO2_treatment,Jmax,
                     fill=Water_treatment))+
        facet_grid(.~Height,scale="free_x",space="free",
                   labeller=label_both)+
        guides(alpha = guide_legend(override.aes = list(fill = "darkgray")))+
        geom_boxplot()+
        xlab(expression(paste(CO[2], " treatment")))+
        ylab(expression(J[max]))+
        scale_fill_manual(name="Water treatment",
                          limits=c("wet", "dry"),
                          values=c("lightblue", "pink"))+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)
    
    p6 <- ggplot(coefDF,
                 aes(CO2_treatment,JVratio,
                     fill=Water_treatment))+
        facet_grid(.~Height,scale="free_x",space="free",
                   labeller=label_both)+
        guides(alpha = guide_legend(override.aes = list(fill = "darkgray")))+
        geom_boxplot()+
        xlab(expression(paste(CO[2], " treatment")))+
        ylab("JV ratio")+
        scale_fill_manual(name="Water treatment",
                          limits=c("wet", "dry"),
                          values=c("lightblue", "pink"))+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="bottom",
              legend.text.align=0)
    
    pdf("output/leaf_flux_all_treatment_boxplot_parameters.pdf", width=6, height=12)
    plot_grid(p4, p5, p6, rel_heights=c(1,1,1.5),
              labels="AUTO", ncol=1, align="v", axis = "l")
    dev.off()
    
    
    ### testing water by co2 interation
    ## vcmax
    mod1 <- lme(Vcmax ~ Water_treatment * CO2_treatment, random=~1|chamber, 
                data=coefDF, 
                method="REML")
    anova.lme(mod1, 
              type="sequential", 
              adjustSigma = FALSE)
    
    ## Jmax
    mod2 <- lme(Jmax ~ Water_treatment * CO2_treatment, random=~1|chamber, 
                data=coefDF, 
                method="REML")
    anova.lme(mod2, 
              type="sequential", 
              adjustSigma = FALSE)
    
    ## JV ratio
    mod3 <- lme(JVratio ~ Water_treatment * CO2_treatment, random=~1|chamber, 
                data=coefDF, 
                method="REML")
    anova.lme(mod3, 
              type="sequential", 
              adjustSigma = FALSE)
    
    ## conclusion: no co2 by water effect and interaction effect on all variables
    
    ### testing height effect
    ## vcmax
    mod4 <- lme(Vcmax ~ Height * Water_treatment * CO2_treatment, random=~1|chamber, 
                data=coefDF, 
                method="REML")
    anova.lme(mod4, 
              type="sequential", 
              adjustSigma = FALSE)
    
    ## jmax
    mod5 <- lme(Jmax ~ Height * Water_treatment * CO2_treatment, random=~1|chamber, 
                data=coefDF, 
                method="REML")
    anova.lme(mod5, 
              type="sequential", 
              adjustSigma = FALSE)
    
    ## jv ratio
    mod6 <- lme(JVratio ~ Height * Water_treatment * CO2_treatment, random=~1|chamber, 
                data=coefDF, 
                method="REML")
    anova.lme(mod6, 
              type="sequential", 
              adjustSigma = FALSE)
    
    ## conclusion: height effect on Vcmax and Jmax, but not JV ratio

    
    ### investigate jsut a subset of the data, close to canopy drawdown dates
    coefDF$year <- year(coefDF$Date)
    coefDF.sub <- subset(coefDF, year == 2009)
    coefDF.sub <- coefDF.sub[!(coefDF.sub$chamber%in%c("ch15","ch16","ch17")),]
    
    ### ignore the last entry as it is simply a duplicated
    subDF <- coefDF.sub[1:24,] 
    
    ### make a list of identify
    id.list <- unique(subDF$Identity)
    
    ### prepare an output df
    outDF <- data.frame(id.list, NA, NA, NA, NA, NA, NA,
                        NA, NA, NA, NA, NA, NA,
                        NA, NA, NA, NA, NA, NA,
                        NA, NA, NA, NA, NA, NA,
                        NA, NA, NA, NA, NA)
    colnames(outDF) <- c("Identity", "Chamber", "CO2_treatment", "Water_treatment", "Height",
                         "RMSE", "Vcmax", "Vcmax.se", "Jmax", "Jmax.se", "Rd", "Rd.se",
                         "Ci", "ALEAF", "GS", "ELEAF", "Ac", "Aj", "Ap", "Rd2", "VPD",
                         "Tleaf", "Ca", "Cc", "PPFD", "Ci_transition_Ac_Aj",
                         "curve.fitting", "Patm", "GammaStar", "Km")

    ### prepare an output list
    outlist <- list()

    ### the for loop
    for (i in 1:length(id.list)) {
        ## subset each data
        test <- subset(myDF, Identity == id.list[i])
        
        ## fit
        fit1 <- fitaci(test, fitmethod="bilinear")
        
        ## assign to list
        outlist[[i]] <- fit1
        
        ## get information on identity
        outDF[outDF$Identity == id.list[i], "CO2_treatment"] <- unique(test$CO2_treatment)
        outDF[outDF$Identity == id.list[i], "Water_treatment"] <- unique(test$Water_treatment)
        outDF[outDF$Identity == id.list[i], "Chamber"] <- unique(test$chamber)
        outDF[outDF$Identity == id.list[i], "Height"] <- unique(test$Height)
        outDF[outDF$Identity == id.list[i], "curve.fitting"] <- fit1$fitmethod
        
        ## assign fitted values
        outDF[outDF$Identity == id.list[i], "RMSE"] <- fit1$RMSE
        outDF[outDF$Identity == id.list[i], "Vcmax"] <- fit1$pars[1,1]
        outDF[outDF$Identity == id.list[i], "Vcmax.se"] <- fit1$pars[1,2]
        outDF[outDF$Identity == id.list[i], "Jmax"] <- fit1$pars[2,1]
        outDF[outDF$Identity == id.list[i], "Jmax.se"] <- fit1$pars[2,2]
        outDF[outDF$Identity == id.list[i], "Rd"] <- fit1$pars[3,1]
        outDF[outDF$Identity == id.list[i], "Rd.se"] <- fit1$pars[3,2]
        
        outDF[outDF$Identity == id.list[i], "Ci"] <- fit1$Photosyn()[1]
        outDF[outDF$Identity == id.list[i], "ALEAF"] <- fit1$Photosyn()[2]
        outDF[outDF$Identity == id.list[i], "GS"] <- fit1$Photosyn()[3]
        outDF[outDF$Identity == id.list[i], "ELEAF"] <- fit1$Photosyn()[4]
        outDF[outDF$Identity == id.list[i], "Ac"] <- fit1$Photosyn()[5]
        outDF[outDF$Identity == id.list[i], "Aj"] <- fit1$Photosyn()[6]
        outDF[outDF$Identity == id.list[i], "Ap"] <- fit1$Photosyn()[7]
        outDF[outDF$Identity == id.list[i], "Rd2"] <- fit1$Photosyn()[8]
        outDF[outDF$Identity == id.list[i], "VPD"] <- fit1$Photosyn()[9]
        outDF[outDF$Identity == id.list[i], "Tleaf"] <- fit1$Photosyn()[10]
        outDF[outDF$Identity == id.list[i], "Ca"] <- fit1$Photosyn()[11]
        outDF[outDF$Identity == id.list[i], "Cc"] <- fit1$Photosyn()[12]
        outDF[outDF$Identity == id.list[i], "PPFD"] <- fit1$Photosyn()[13]
        outDF[outDF$Identity == id.list[i], "Patm"] <- fit1$Photosyn()[14]
        
        outDF[outDF$Identity == id.list[i], "Ci_transition_Ac_Aj"] <- fit1$Ci_transition
        outDF[outDF$Identity == id.list[i], "GammaStar"] <- fit1$GammaStar
        outDF[outDF$Identity == id.list[i], "Km"] <- fit1$Km
    }
    
    ### save
    write.csv(outDF, "output/leaf_scale_parameters.csv")
    

    ### create pdf
    pdf("output/leaf_level_individual_chamber_result.pdf", height=24, width=20)
    par(mfrow=c(6,4))
    
    ## make plot
    plot.sequence <- c(1,9,12,15,2,11,14,17,6,4,21,19,10,24,16,13,5,8,20,22, 3,7,18)
    for (i in 1:length(plot.sequence)) {
        plot(outlist[[plot.sequence[i]]], main=paste0(outDF$Chamber[plot.sequence[i]], ", ", outDF$Height[plot.sequence[i]], ", ",
                                       outDF$CO2_treatment[plot.sequence[i]], ", ", outDF$Water_treatment[plot.sequence[i]]))
    }
    
    dev.off()
    
    
}
