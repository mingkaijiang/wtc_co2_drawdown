
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
    myDF <- subset(myDF, chamber %in% c("ch01", "ch02", "ch03", "ch04", "ch05", "ch06",
                                        "ch07", "ch08", "ch09", "ch10", "ch11", "ch12"))
    
    myDF$year <- year(myDF$Date)
    myDF <- subset(myDF, year == 2009)
    
    unique(myDF[c("chamber", "Height", "CO2_treatment", "Water_treatment")])
    
    #### Fitting ACI curve at the finest resolution
    fits.all <- fitacis(myDF, group="Identity", fitmethod="bilinear", Tcorrect=T)
    
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
    
    coefDF.sub <- coefDF
    
    ### investigate jsut a subset of the data, close to canopy drawdown dates
    coefDF.sub$year <- year(coefDF.sub$Date)
    coefDF.sub <- subset(coefDF.sub, year == 2009)
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
    
    ### visually check whether Jmax, Vcmax, and JVratio change with multiple treatments
    
    ### plot a time series of Vcmax and Jmax and JV ratio
    p1 <- ggplot(coefDF) +
        geom_point(aes(Date, Vcmax, col=CO2_treatment, 
                       pch=Water_treatment, size=as.factor(coefDF$Height)))+
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
                           values=c("blue2", "red3"))+
        scale_size_manual(name="Position",
                          values=c(1,4))
    
    
    p2 <- ggplot(coefDF) +
        geom_point(aes(Date, Jmax, col=CO2_treatment, 
                       pch=Water_treatment, size=as.factor(coefDF$Height)))+
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
                           values=c("blue2", "red3"))+
        scale_size_manual(name="Position",
                          values=c(1,4))
    
    p3 <- ggplot(coefDF) +
        geom_point(aes(Date, JVratio, col=CO2_treatment, 
                       pch=Water_treatment, size=as.factor(coefDF$Height)))+
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
        scale_size_manual(name="Position",
                          values=c(1,4))+
        scale_shape_discrete(name="Water treatment")+
        theme(legend.direction = "vertical", legend.box = "horizontal")

    pdf("output/leaf_flux_all_treatment_time_series_parameters.pdf", width=6, height=12)
    plot_grid(p1, p2, p3, rel_heights=c(1,1,1.5),
              labels="AUTO", ncol=1, align="v", axis = "l")
    dev.off()
    
    
    ### testing water by co2 interation
    ## vcmax
    mod1 <- lme(Vcmax ~ Water_treatment * CO2_treatment * Height, random=~1|chamber, 
                data=coefDF, 
                method="REML")
    anova.lme(mod1, 
              type="sequential", 
              adjustSigma = FALSE)
    
    ## Jmax
    mod2 <- lme(Jmax ~ Water_treatment * CO2_treatment * Height, random=~1|chamber, 
                data=coefDF, 
                method="REML")
    anova.lme(mod2, 
              type="sequential", 
              adjustSigma = FALSE)
    
    ## JV ratio
    mod3 <- lme(JVratio ~ Water_treatment * CO2_treatment * Height, random=~1|chamber, 
                data=coefDF, 
                method="REML")
    anova.lme(mod3, 
              type="sequential", 
              adjustSigma = FALSE)
    

    
    #### compute statistics on each individual treatment factor
    op <- par(mfrow = c(3, 1))
    with(coefDF, {
        interaction.plot(CO2_treatment, Water_treatment, Vcmax)
        interaction.plot(CO2_treatment, Height, Vcmax)
        interaction.plot(Height, Water_treatment, Vcmax)
    }
    )
    par(op)
    
    ##### check vcmax relationship
    ### 3-way anova 
    fm <- aov(Vcmax ~ CO2_treatment * Water_treatment * Height, data = coefDF)
    summary(fm)
    
    ## obtain r2 from the anova model
    lm <- lm(Vcmax ~ CO2_treatment * Water_treatment * Height, data = coefDF)
    anova(lm)
    summary(lm)
    
    
    ### to obtain the model without 3-way interaction
    fm1 <- update(fm, . ~ . -CO2_treatment:Water_treatment:Height)
    summary(fm1)
    
    ### remove all two-way interactions
    fm2 <- update(fm1, .~CO2_treatment+Water_treatment+Height)
    summary(fm2)
    
    ### check the two models
    anova(fm, fm2)
    
    ### the table of effects from the model is:
    model.tables(fm2,type="effects")
    model.tables(fm2,type="means")
    
    #test1 <- summaryBy(Vcmax~Water_treatment, data=coefDF, FUN=c(mean,se), keep.names=T)
    #test2 <- summaryBy(Vcmax~CO2_treatment, data=coefDF, FUN=c(mean,se), keep.names=T)
    #test3 <- summaryBy(Vcmax~Height, data=coefDF, FUN=c(mean,se), keep.names=T)
    
    ### we can remove the water effect
    fm.vcmax <- aov(Vcmax ~ CO2_treatment + Height, data = coefDF)
    summary(fm.vcmax)
    
    ### check residual of the three way anova
    op <-  par(mfrow = c(2, 2))
    plot(fm)
    par(op)
    
    coefDF %>% group_by(CO2_treatment, Height) %>% summarise(mean(Vcmax))
    model.tables(fm.vcmax, type="means")
    
    
    ##### check jmax relationship
    ### 3-way anova 
    fm <- aov(Jmax ~ CO2_treatment * Water_treatment * Height, data = coefDF)
    summary(fm)
    
    ## obtain r2 from the anova model
    lm <- lm(Jmax ~ CO2_treatment * Water_treatment * Height, data = coefDF)
    anova(lm)
    summary(lm)
    
    
    ##### check jv ratio relationship
    ### 3-way anova 
    fm <- aov(JVratio ~ CO2_treatment * Water_treatment * Height, data = coefDF)
    summary(fm)
    
    ## obtain r2 from the anova model
    lm <- lm(JVratio ~ CO2_treatment * Water_treatment * Height, data = coefDF)
    anova(lm)
    summary(lm)
    
    
    #### we can't group anything together
    myDF$Identity2 <- paste0(myDF$Height, "-", myDF$CO2_treatment, ".", myDF$Water_treatment)
    fits.gp <- fitacis(myDF, group="Identity2", fitmethod="bilinear", Tcorrect=T)
    
    
    ### assign factors onto the dataframe
    coefDF <- coef(fits.gp)
    coefDF$Height <- sub("-.*", "", coefDF$Identity2)
    coefDF$Water_treatment <- str_sub(coefDF$Identity2, start=-3)
    coefDF$CO2_treatment <- sub(".*-", "", coefDF$Identity2)
    coefDF$CO2_treatment <- sub(".dry", "", coefDF$CO2_treatment)
    coefDF$CO2_treatment <- sub(".wet", "", coefDF$CO2_treatment)
    
    ### add vcmax to jmax ratio
    coefDF$JVratio <- coefDF$Jmax/coefDF$Vcmax
    


    ### look at box plot of the groups 
    p4 <- ggplot(coefDF)+
        geom_errorbar(mapping=aes(Height,ymin=Vcmax-Vcmax_SE,ymax=Vcmax+Vcmax_SE,
                       color=CO2_treatment, lty=as.factor(coefDF$Water_treatment)), width=0.2,
                      position=position_dodge(width=0.5))+
        geom_point(aes(Height,Vcmax,
                       color=CO2_treatment, pch=as.factor(coefDF$Water_treatment)), size=4,
                   position=position_dodge(width=0.5))+
        xlab(expression(paste("Position")))+
        ylab(expression(Vc[max]*" (umol " * m^-2 * " " * s^-1 * ")"))+
        scale_color_manual(name=expression(paste(CO[2] * " treatment")),
                          limits=c("ambient", "elevated"),
                          values=c("blue3", "red2"))+
        scale_linetype_manual(name=expression(paste(H[2] * "O treatment")),
                           limits=c("wet", "dry"),
                           values=c(1, 1))+
        scale_shape_manual(name=expression(paste(H[2] * "O treatment")),
                              limits=c("wet", "dry"),
                              values=c(19, 17))+
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
        ylim(0,150)
    
    plot(p4)
    
    p5 <- ggplot(coefDF)+
        #geom_errorbar(mapping=aes(Height,ymin=Vcmax-Vcmax_SE,ymax=Vcmax+Vcmax_SE,
        #                          color=CO2_treatment), width=0.2,
        #              position=position_dodge(width=0.5))+
        geom_point(aes(Height,Jmax,
                       color=CO2_treatment, pch=as.factor(coefDF$Water_treatment)), size=4,
                   position=position_dodge(width=0.5))+
        xlab(expression(paste("Position")))+
        ylab(expression(J[max]*" (umol " * m^-2 * " " * s^-1 * ")"))+
        scale_color_manual(name=expression(paste(CO[2] * " treatment")),
                           limits=c("ambient", "elevated"),
                           values=c("blue3", "red2"))+
        scale_linetype_manual(name=expression(paste(H[2] * "O treatment")),
                              limits=c("wet", "dry"),
                              values=c(1, 1))+
        scale_shape_manual(name=expression(paste(H[2] * "O treatment")),
                           limits=c("wet", "dry"),
                           values=c(19, 17))+
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
        ylim(0,240)
    
    plot(p5)
    
    p6 <- ggplot(coefDF)+
        #geom_errorbar(mapping=aes(Height,ymin=Vcmax-Vcmax_SE,ymax=Vcmax+Vcmax_SE,
        #                          color=CO2_treatment), width=0.2,
        #              position=position_dodge(width=0.5))+
        geom_point(aes(Height,JVratio,
                       color=CO2_treatment, pch=as.factor(coefDF$Water_treatment)), size=4,
                   position=position_dodge(width=0.5))+
        xlab(expression(paste("Position")))+
        ylab("J/V ratio")+
        scale_color_manual(name=expression(paste(CO[2] * " treatment")),
                           limits=c("ambient", "elevated"),
                           values=c("blue3", "red2"))+
        scale_linetype_manual(name=expression(paste(H[2] * "O treatment")),
                              limits=c("wet", "dry"),
                              values=c(1, 1))+
        scale_shape_manual(name=expression(paste(H[2] * "O treatment")),
                           limits=c("wet", "dry"),
                           values=c(19, 17))+
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
        ylim(1,2)
    
    plot(p6)
    
    pdf("output/leaf_parameter_summary.pdf", width=8, height=14)
    plot_grid(p4, p5, p6, rel_heights=c(1,1,1.5),
              labels="AUTO", ncol=1, align="v", axis = "l")
    dev.off()
    
    


    

    
    
}
