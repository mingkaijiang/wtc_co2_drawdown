#### individual canopy ACi curve measurement processing
#### Fit A-CI curve for each chamber

canopy_ACI_processing <- function(cDF) {
    
    myDF.rename <- cDF
    colnames(myDF.rename) <- c("Chamber", "Canopy","Ca", "Tair", 
                               "date", "time", "datetime", "T",
                               "VPD", "DPLicorCh", "PARi", "slope2", 
                               "cmarea", "nslope2","k", "leak", 
                               "corrflux", "ncorrflux", "rh", "Photo",
                               "transpiration")
    
    
    ### clean the dataset to exclude na, negative values
    myDF.clean <- myDF.rename[complete.cases(myDF.rename$Photo), ]
    myDF <- myDF.clean[myDF.clean$transpiration > 0, ]
    
    
    ### get gs from transpiration
    myDF$gs <- myDF$transpiration / myDF$VPD
    
    ### get Ci from gs, A and Ca
    myDF$Ci <- with(myDF, Ca - (Photo/gs))
    
    ### create an identity list for each chamber and canopy
    myDF$identity <- paste0(myDF$Chamber, "-", myDF$Canopy)
    
    myDF <- myDF[myDF$identity!="7-45",]
    
    ### assign CO2, water treatment
    ## chambers 1, 3, 5, 7, 9, 11 are CO2 ambient
    ## chambers 1, 3, 4, 6, 8, 11 are wet 
    
    for (i in c(1, 3, 5, 7, 9, 11)) {
        myDF[myDF$Chamber == i, "CO2_treatment"] <- "ambient"
    }
    
    for (i in c(2, 4, 6, 8, 10, 12)) {
        myDF[myDF$Chamber == i, "CO2_treatment"] <- "elevated"
    }
    
    for (i in c(1, 3, 4, 6, 8, 11)) {
        myDF[myDF$Chamber == i, "Water_treatment"] <- "wet"
    }
    
    for (i in c(2, 5, 7, 9, 10, 12)) {
        myDF[myDF$Chamber == i, "Water_treatment"] <- "dry"
    }
    
    
    ### make a list of identify
    id.list <- unique(myDF$identity)
    
    ### prepare an output df
    outDF <- data.frame(id.list, NA, NA, NA, NA, NA, NA,
                        NA, NA, NA, NA, NA, NA,
                        NA, NA, NA, NA, NA, NA,
                        NA, NA, NA, NA, NA, NA,
                        NA, NA, NA, NA, NA)
    colnames(outDF) <- c("Identity", "Chamber", "CO2_treatment", "Water_treatment", "Canopy",
                         "RMSE", "Vcmax", "Vcmax.se", "Jmax", "Jmax.se", "Rd", "Rd.se",
                         "Ci", "ALEAF", "GS", "ELEAF", "Ac", "Aj", "Ap", "Rd2", "VPD",
                         "Tleaf", "Ca", "Cc", "PPFD", "Ci_transition_Ac_Aj",
                         "curve.fitting", "Patm", "GammaStar", "Km")
    
    ### prepare an output list
    outlist <- list()
    
    ### the for loop
    for (i in 1:length(id.list)) {
        ## subset each data
        test <- subset(myDF, identity == id.list[i])
        
        ## fit
        fit1 <- fitaci(test, fitmethod="bilinear")
        
        ## assign to list
        outlist[[i]] <- fit1
        
        ## get information on identity
        outDF[outDF$Identity == id.list[i], "CO2_treatment"] <- unique(test$CO2_treatment)
        outDF[outDF$Identity == id.list[i], "Water_treatment"] <- unique(test$Water_treatment)
        outDF[outDF$Identity == id.list[i], "Chamber"] <- unique(test$Chamber)
        outDF[outDF$Identity == id.list[i], "Canopy"] <- unique(test$Canopy)
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
    
    ### add vcmax to jmax ratio
    outDF$JVratio <- outDF$Jmax/outDF$Vcmax
    
    ### save
    write.csv(outDF, "output/canopy_scale_parameters.csv")
    
    
    
    #### remove 7-45 because can't be fitted
    #myDFnew <- myDF[myDF$identity!="7-45",]
    #
    #
    ##### Fitting ACI curve
    #fits <- fitacis(myDFnew, group="identity", fitmethod="bilinear", Tcorrect=T)
    #
    #### summary table between vcmax and jmax
    #coefDF <- coef(fits)
    #coefDF$Canopy <- sub(".*-", "", coefDF$identity)
    #coefDF$Chamber <- sub("-.*", "", coefDF$identity)
    #
    #### assign CO2, water treatment
    ### chambers 1, 3, 5, 7, 9, 11 are CO2 ambient
    ### chambers 1, 3, 4, 6, 8, 11 are wet 
    #
    #for (i in c(1, 3, 5, 7, 9, 11)) {
    #    coefDF[coefDF$Chamber == i, "CO2_treatment"] <- "ambient"
    #}
    #
    #for (i in c(2, 4, 6, 8, 10, 12)) {
    #    coefDF[coefDF$Chamber == i, "CO2_treatment"] <- "elevated"
    #}
    #
    #for (i in c(1, 3, 4, 6, 8, 11)) {
    #    coefDF[coefDF$Chamber == i, "Water_treatment"] <- "wet"
    #}
    #
    #for (i in c(2, 5, 7, 9, 10, 12)) {
    #    coefDF[coefDF$Chamber == i, "Water_treatment"] <- "dry"
    #}
    #
    #### add vcmax to jmax ratio
    #coefDF$JVratio <- coefDF$Jmax/coefDF$Vcmax
    #
    #
    #### plot some broad summary
    #pdf("output/canopy_scale_aci_fitting.pdf")
    #
    #par(mfrow=c(2, 2))
    #
    #### plot individual fitted data
    #plot(fits, how="oneplot")
    #plot(fits[[1]], col="black", add=T)
    #
    #### plot more individual level data
    #plot(fits, how="oneplot", what="data")
    #plot(fits, how="oneplot", add=T, what="model", lwd=c(1,1))
#
    #### look at other elements
    #rmses <- sapply(fits, "[[", "RMSE")
    #plot(rmses, type='h', ylab="RMSE", xlab="Curve nr", xaxt="n")
    #
    #dev.off()
    #
#
    ##### compute statistics on each individual treatment factor
    #op <- par(mfrow = c(3, 1))
    #with(coefDF, {
    #    interaction.plot(CO2_treatment, Water_treatment, Vcmax)
    #    interaction.plot(CO2_treatment, Canopy, Vcmax)
    #    interaction.plot(Canopy, Water_treatment, Vcmax)
    #}
    #)
    #par(op)
    #
    ###### check vcmax relationship
    #### 3-way anova 
    #fm <- aov(Vcmax ~ CO2_treatment * Water_treatment * Canopy, data = coefDF)
    #summary(fm)
    #
    ### obtain r2 from the anova model
    #lm <- lm(Vcmax ~ CO2_treatment * Water_treatment * Canopy, data = coefDF)
    #anova(lm)
    #summary(lm)
    #
    #
    ###### check jmax relationship
    #### 3-way anova 
    #fm <- aov(Jmax ~ CO2_treatment * Water_treatment * Canopy, data = coefDF)
    #summary(fm)
    #
    ### obtain r2 from the anova model
    #lm <- lm(Jmax ~ CO2_treatment * Water_treatment * Canopy, data = coefDF)
    #anova(lm)
    #summary(lm)
    #
    #
    ###### check jv ratio relationship
    #### 3-way anova 
    #fm <- aov(JVratio ~ CO2_treatment * Water_treatment * Canopy, data = coefDF)
    #summary(fm)
    #
    ### obtain r2 from the anova model
    #lm <- lm(JVratio ~ CO2_treatment * Water_treatment * Canopy, data = coefDF)
    #anova(lm)
    #summary(lm)
    
    
    #### because the water treatment was unbalanced 
    #### i.e. wet = chambers 1, 3, 4, 8, 11, and dry = 2, 7, 12,
    #### it is possible that the experimental design intentionally ignored water treatment.
    #### This is partially proven by leaf-scale data (including all time points), 
    #### as there was no water treatment effect
    #### hence, below I can ignore water treatment and group data with CO2 treatment and canopy positions
    #### and check the fit ACI results thereafter. 
    #for (i in c(1, 3, 5, 7, 9, 11)) {
    #    myDF[myDF$Chamber == i, "CO2_treatment"] <- "ambient"
    #}
    #
    #for (i in c(2, 4, 6, 8, 10, 12)) {
    #    myDF[myDF$Chamber == i, "CO2_treatment"] <- "elevated"
    #}
    
    ### create an identity list for each chamber and canopy
    myDF2 <- subset(myDF, Water_treatment == "wet")

    ### make a list of identify
    id.list <- unique(myDF2$identity)
    
    ### prepare an output df
    outDF <- data.frame(id.list, NA, NA, NA, NA, NA, NA,
                        NA, NA, NA, NA, NA, NA,
                        NA, NA, NA, NA, NA, NA,
                        NA, NA, NA, NA, NA, NA,
                        NA, NA, NA, NA, NA)
    colnames(outDF) <- c("Identity", "Chamber", "CO2_treatment", "Water_treatment", "Canopy",
                         "RMSE", "Vcmax", "Vcmax.se", "Jmax", "Jmax.se", "Rd", "Rd.se",
                         "Ci", "ALEAF", "GS", "ELEAF", "Ac", "Aj", "Ap", "Rd2", "VPD",
                         "Tleaf", "Ca", "Cc", "PPFD", "Ci_transition_Ac_Aj",
                         "curve.fitting", "Patm", "GammaStar", "Km")
    
    ### prepare an output list
    outlist <- list()
    
    ### the for loop
    for (i in 1:length(id.list)) {
        ## subset each data
        test <- subset(myDF2, identity == id.list[i])
        
        ## fit
        fit1 <- fitaci(test, fitmethod="bilinear")
        
        ## assign to list
        outlist[[i]] <- fit1
        
        ## get information on identity
        outDF[outDF$Identity == id.list[i], "CO2_treatment"] <- unique(test$CO2_treatment)
        outDF[outDF$Identity == id.list[i], "Water_treatment"] <- unique(test$Water_treatment)
        outDF[outDF$Identity == id.list[i], "Chamber"] <- unique(test$Chamber)
        outDF[outDF$Identity == id.list[i], "Canopy"] <- unique(test$Canopy)
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
    
    ### add vcmax to jmax ratio
    outDF$JVratio <- outDF$Jmax/outDF$Vcmax
    
    sumDF <- summaryBy(Vcmax + Jmax + Rd + ALEAF + GS + ELEAF + Ac + Aj +Ci_transition_Ac_Aj + GammaStar + Km + JVratio~ CO2_treatment+Canopy,
                       data=outDF, FUN = c(mean, se), keep.names=T)
    
    sumDF$Canopy <- gsub("12345", "Full", sumDF$Canopy)
    sumDF$Canopy <- gsub("345", "T+M", sumDF$Canopy)
    sumDF$Canopy <- gsub("45", "Top", sumDF$Canopy)
    
    p1 <- ggplot(sumDF) +
        geom_errorbar(aes(x=Canopy, ymin=(Vcmax.mean - Vcmax.se), 
                          ymax = (Vcmax.mean+Vcmax.se), color=CO2_treatment),
                      position=position_dodge(width=0.5),width=0.2)+
        geom_point(aes(Canopy, Vcmax.mean, fill=CO2_treatment), size = 10, shape=21, 
                   position=position_dodge(width=0.5))+
        xlab("")+
        ylab(expression(V[cmax]*" (umol " * m^-2 * " " * s^-1 * ")"))+
        scale_fill_manual(name=expression(paste(CO[2] * " treatment")),
                          limits=c("ambient", "elevated"),
                          values=c("grey", "black"))+
        scale_color_manual(name=expression(paste(CO[2] * " treatment")),
                           limits=c("ambient", "elevated"),
                           values=c("black", "black"))+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=18),
              axis.title.y=element_text(size=18),
              legend.text=element_text(size=18),
              legend.title=element_text(size=18),
              panel.grid.major=element_blank(),
              legend.position="none",
              plot.title = element_text(size = 18, face = "bold", hjust=0.1))+
        scale_x_discrete(breaks=c("Full", "T+M", "Top"),
                         labels=c("Full", "T+M", "Top"))+
        #ylim(5, 40)+
        ggtitle("a")
    
    p2 <- ggplot(sumDF) +
        geom_errorbar(aes(x=Canopy, ymin=(Jmax.mean - Jmax.se), 
                          ymax = (Jmax.mean+Jmax.se), color=CO2_treatment),
                      position=position_dodge(width=0.5),width=0.3)+
        geom_point(aes(Canopy, Jmax.mean, fill=CO2_treatment), size = 10, shape=21, 
                   position=position_dodge(width=0.5))+
        xlab("")+
        ylab(expression(J[max]*" (umol " * m^-2 * " " * s^-1 * ")"))+
        scale_fill_manual(name=expression(paste(CO[2] * " treatment")),
                          limits=c("ambient", "elevated"),
                          values=c("grey", "black"))+
        scale_color_manual(name=expression(paste(CO[2] * " treatment")),
                           limits=c("ambient", "elevated"),
                           values=c("black", "black"))+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=18),
              axis.title.y=element_text(size=18),
              legend.text=element_text(size=18),
              legend.title=element_text(size=18),
              panel.grid.major=element_blank(),
              legend.position="none",
              plot.title = element_text(size = 18, face = "bold", hjust=0.1))+
        scale_x_discrete(breaks=c("Full", "T+M", "Top"),
                         labels=c("Full", "T+M", "Top"))+
        #ylim(10,120)+
        ggtitle("b")
    
    p3 <- ggplot(sumDF) +
        geom_errorbar(aes(x=Canopy, ymin=(JVratio.mean - JVratio.se), 
                          ymax = (JVratio.mean+JVratio.se), color=CO2_treatment),
                      position=position_dodge(width=0.5),width=0.2)+
        geom_point(aes(Canopy, JVratio.mean, fill=CO2_treatment), size = 10, shape=21, 
                   position=position_dodge(width=0.5))+
        xlab("")+
        ylab("JV ratio")+
        scale_fill_manual(name=expression(paste(CO[2] * " treatment")),
                          limits=c("ambient", "elevated"),
                          values=c("grey", "black"))+
        scale_color_manual(name=expression(paste(CO[2] * " treatment")),
                           limits=c("ambient", "elevated"),
                           values=c("black", "black"))+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=18),
              axis.title.y=element_text(size=18),
              legend.text=element_text(size=18),
              legend.title=element_text(size=18),
              panel.grid.major=element_blank(),
              legend.position="none",
              plot.title = element_text(size = 18, face = "bold", hjust=0.1))+
        scale_x_discrete(breaks=c("Full", "T+M", "Top"),
                         labels=c("Full", "T+M", "Top"))+
        #ylim(1,5)+
        ggtitle("c")
    
    p4 <- ggplot(sumDF) +
        geom_errorbar(aes(x=Canopy, ymin=(Ci_transition_Ac_Aj.mean - Ci_transition_Ac_Aj.se), 
                          ymax = (Ci_transition_Ac_Aj.mean+Ci_transition_Ac_Aj.se), color=CO2_treatment),
                      position=position_dodge(width=0.5),width=0.2)+
        geom_point(aes(Canopy, Ci_transition_Ac_Aj.mean, fill=CO2_treatment), size = 10, shape=21, 
                   position=position_dodge(width=0.5))+
        xlab("")+
        ylab(expression(C[i]*" transition (umol " * mol^-1 * ")"))+
        scale_fill_manual(name=expression(paste(CO[2] * " treatment")),
                          limits=c("ambient", "elevated"),
                          values=c("grey", "black"))+
        scale_color_manual(name=expression(paste(CO[2] * " treatment")),
                           limits=c("ambient", "elevated"),
                           values=c("black", "black"))+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=18),
              axis.title.y=element_text(size=18),
              legend.text=element_text(size=18),
              legend.title=element_text(size=18),
              panel.grid.major=element_blank(),
              legend.position="none",
              plot.title = element_text(size = 18, face = "bold", hjust=0.1))+        
        scale_x_discrete(breaks=c("Full", "T+M", "Top"),
                         labels=c("Full", "T+M", "Top"))+
        #ylim(0,1800)+
        ggtitle("d")
    
    
    p5 <- ggplot(sumDF) +
        geom_errorbar(aes(x=Canopy, ymin=(Ac.mean - Ac.se), 
                          ymax = (Ac.mean+Ac.se), color=CO2_treatment),
                      position=position_dodge(width=0.5),width=0.2)+
        geom_point(aes(Canopy, Ac.mean, fill=CO2_treatment), size = 10, shape=21, 
                   position=position_dodge(width=0.5))+
        xlab("")+
        ylab(expression(A[c]*" (umol " * m^-2 * " " * s^-1 * ")"))+
        scale_fill_manual(name=expression(paste(CO[2] * " treatment")),
                          limits=c("ambient", "elevated"),
                          values=c("grey", "black"))+
        scale_color_manual(name=expression(paste(CO[2] * " treatment")),
                           limits=c("ambient", "elevated"),
                           values=c("black", "black"))+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_text(size=18),
              axis.text.y=element_text(size=18), 
              axis.title.y=element_text(size=18), 
              legend.text=element_text(size=18),
              legend.title=element_text(size=18),
              panel.grid.major=element_blank(),
              legend.position="none",
              plot.title = element_text(size = 18, face = "bold", hjust=0.1))+
        #ylim(9,25)+
        scale_x_discrete(breaks=c("Full", "T+M", "Top"),
                         labels=c("Full", "T+M", "Top"))+
        guides(fill = guide_legend(title.position = "top"))+
        ggtitle("e")
    
    p6 <- ggplot(sumDF) +
        geom_errorbar(aes(x=Canopy, ymin=(Aj.mean - Aj.se), 
                          ymax = (Aj.mean+Aj.se), color=CO2_treatment),
                      position=position_dodge(width=0.5),width=0.3)+
        geom_point(aes(Canopy, Aj.mean, fill=CO2_treatment), size = 10, shape=21, 
                   position=position_dodge(width=0.5))+
        xlab("")+
        ylab(expression(A[j]*" (umol " * m^-2 * " " * s^-1 * ")"))+
        scale_fill_manual(name=expression(paste(CO[2] * " treatment")),
                          limits=c("ambient", "elevated"),
                          values=c("grey", "black"))+
        scale_color_manual(name=expression(paste(CO[2] * " treatment")),
                           limits=c("ambient", "elevated"),
                           values=c("black", "black"))+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_text(size=18),
              axis.text.y=element_text(size=18), 
              axis.title.y=element_text(size=18), 
              legend.text=element_text(size=18),
              legend.title=element_text(size=18),
              panel.grid.major=element_blank(),
              legend.position="none",
              plot.title = element_text(size = 18, face = "bold", hjust=0.1))+
        #ylim(9,25)+
        scale_x_discrete(breaks=c("Full", "T+M", "Top"),
                         labels=c("Full", "T+M", "Top"))+
        guides(fill = guide_legend(title.position = "top"))+
        ggtitle("f")
    
    
    p7 <- ggplot(sumDF) +
        geom_errorbar(aes(x=Canopy, ymin=(ALEAF.mean - ALEAF.se), 
                          ymax = (ALEAF.mean+ALEAF.se), color=CO2_treatment),
                      position=position_dodge(width=0.5),width=0.2)+
        geom_point(aes(Canopy, ALEAF.mean, fill=CO2_treatment), size = 10, shape=21, 
                   position=position_dodge(width=0.5))+
        xlab("Canopy position")+
        ylab(expression(A[leaf]*" (umol " * m^-2 * " " * s^-1 * ")"))+
        scale_fill_manual(name=expression(paste(CO[2] * " treatment")),
                          limits=c("ambient", "elevated"),
                          values=c("grey", "black"))+
        scale_color_manual(name=expression(paste(CO[2] * " treatment")),
                           limits=c("ambient", "elevated"),
                           values=c("black", "black"))+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=20), 
              axis.text.x = element_text(size=18),
              axis.text.y=element_text(size=18), 
              axis.title.y=element_text(size=18), 
              legend.text=element_text(size=18),
              legend.title=element_text(size=18),
              panel.grid.major=element_blank(),
              legend.position="bottom",
              plot.title = element_text(size = 18, face = "bold", hjust=0.05))+
        #ylim(9,25)+
        scale_x_discrete(breaks=c("Full", "T+M", "Top"),
                         labels=c("Full", "T+M", "Top"))+
        guides(fill = guide_legend(title.position = "top"))+
        ggtitle("g")
    

    
    pdf("output/canopy_parameter_summary_break_into_groups_exclude_water.pdf", width=12, height=14)
    grid.arrange(p1, p2, p3, p4, p5, p6, 
                 p7,  
                 heights=c(1, 1, 1.2, 1.8),
                 layout_matrix = rbind(c(1,2),
                                       c(3,4),
                                       c(5,6),
                                       c(7,7)))
    
    dev.off()
    
    
    ### Note:
    ### some values don't make any sense. So raw data must be wrong.
    ### canopy gas exchange fluxes were normalized to a per leaf area basis
    ### with each harvest having its own leaf area.
    
    return(fits)
}
