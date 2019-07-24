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
    
    ### remove 7-45 because can't be fitted
    myDFnew <- myDF[myDF$identity!="7-45",]
    
    
    #### Fitting ACI curve
    fits <- fitacis(myDFnew, group="identity", fitmethod="bilinear", Tcorrect=T)
    
    ### summary table between vcmax and jmax
    coefDF <- coef(fits)
    coefDF$Canopy <- sub(".*-", "", coefDF$identity)
    coefDF$Chamber <- sub("-.*", "", coefDF$identity)
    
    ### assign CO2, water treatment
    ## chambers 1, 3, 5, 7, 9, 11 are CO2 ambient
    ## chambers 1, 3, 4, 6, 8, 11 are wet 
    
    for (i in c(1, 3, 5, 7, 9, 11)) {
        coefDF[coefDF$Chamber == i, "CO2_treatment"] <- "ambient"
    }
    
    for (i in c(2, 4, 6, 8, 10, 12)) {
        coefDF[coefDF$Chamber == i, "CO2_treatment"] <- "elevated"
    }
    
    for (i in c(1, 3, 4, 6, 8, 11)) {
        coefDF[coefDF$Chamber == i, "Water_treatment"] <- "wet"
    }
    
    for (i in c(2, 5, 7, 9, 10, 12)) {
        coefDF[coefDF$Chamber == i, "Water_treatment"] <- "dry"
    }
    
    ### add vcmax to jmax ratio
    coefDF$JVratio <- coefDF$Jmax/coefDF$Vcmax
    
    
    ### plot some broad summary
    pdf("output/canopy_scale_aci_fitting.pdf")
    
    par(mfrow=c(2, 2))
    
    ### plot individual fitted data
    plot(fits, how="oneplot")
    plot(fits[[1]], col="black", add=T)
    
    ### plot more individual level data
    plot(fits, how="oneplot", what="data")
    plot(fits, how="oneplot", add=T, what="model", lwd=c(1,1))

    ### look at other elements
    rmses <- sapply(fits, "[[", "RMSE")
    plot(rmses, type='h', ylab="RMSE", xlab="Curve nr", xaxt="n")
    
    dev.off()
    

    #### compute statistics on each individual treatment factor
    op <- par(mfrow = c(3, 1))
    with(coefDF, {
        interaction.plot(CO2_treatment, Water_treatment, Vcmax)
        interaction.plot(CO2_treatment, Canopy, Vcmax)
        interaction.plot(Canopy, Water_treatment, Vcmax)
    }
    )
    par(op)
    
    ##### check vcmax relationship
    ### 3-way anova 
    fm <- aov(Vcmax ~ CO2_treatment * Water_treatment * Canopy, data = coefDF)
    summary(fm)
    
    ## obtain r2 from the anova model
    lm <- lm(Vcmax ~ CO2_treatment * Water_treatment * Canopy, data = coefDF)
    anova(lm)
    summary(lm)
    
    
    ##### check jmax relationship
    ### 3-way anova 
    fm <- aov(Jmax ~ CO2_treatment * Water_treatment * Canopy, data = coefDF)
    summary(fm)
    
    ## obtain r2 from the anova model
    lm <- lm(Jmax ~ CO2_treatment * Water_treatment * Canopy, data = coefDF)
    anova(lm)
    summary(lm)
    
    
    ##### check jv ratio relationship
    ### 3-way anova 
    fm <- aov(JVratio ~ CO2_treatment * Water_treatment * Canopy, data = coefDF)
    summary(fm)
    
    ## obtain r2 from the anova model
    lm <- lm(JVratio ~ CO2_treatment * Water_treatment * Canopy, data = coefDF)
    anova(lm)
    summary(lm)
    
    
    #### because the water treatment was unbalanced 
    #### i.e. wet = chambers 1, 3, 4, 8, 11, and dry = 2, 7, 12,
    #### it is possible that the experimental design intentionally ignored water treatment.
    #### This is partially proven by leaf-scale data (including all time points), 
    #### as there was no water treatment effect
    #### hence, below I can ignore water treatment and group data with CO2 treatment and canopy positions
    #### and check the fit ACI results thereafter. 
    for (i in c(1, 3, 5, 7, 9, 11)) {
        myDF[myDF$Chamber == i, "CO2_treatment"] <- "ambient"
    }
    
    for (i in c(2, 4, 6, 8, 10, 12)) {
        myDF[myDF$Chamber == i, "CO2_treatment"] <- "elevated"
    }
    
    ### create an identity list for each chamber and canopy
    myDF$identity2 <- paste0(myDF$CO2_treatment, "-", myDF$Canopy)

    
    ######## Note: here you need to fit by chamber and group by treatment!
    #### Fitting ACI curve
    fits.sub <- fitacis(myDF, group="identity", fitmethod="bilinear", Tcorrect=T)
    ### problems noted: 1. missing chamber 12-345, 2. chamber 7-45 could not be fitted by default method
    ### need to go through these data and check what's wrong - can it be fitted individually?
    
    
    ### summary table between vcmax and jmax
    coefDF <- coef(fits.sub)
    coefDF$Canopy <- sub(".*-", "", coefDF$identity2)
    coefDF$CO2_treatment <- sub("-.*", "", coefDF$identity2)
    
    ### add vcmax to jmax ratio
    coefDF$JVratio <- coefDF$Jmax/coefDF$Vcmax
    
    ### assign CO2 treatment
    for (i in c(1, 3, 5, 7, 9, 11)) {
        coefDF[coefDF$Chamber == i, "CO2_treatment"] <- "ambient"
    }
    
    for (i in c(2, 4, 6, 8, 10, 12)) {
        coefDF[coefDF$Chamber == i, "CO2_treatment"] <- "elevated"
    }
    
    ### look at box plot of the groups 
    p4 <- ggplot(coefDF)+
        geom_errorbar(mapping=aes(Canopy,ymin=Vcmax-Vcmax_SE,ymax=Vcmax+Vcmax_SE,
                                  color=CO2_treatment), width=0.2,
                      position=position_dodge(width=0.5))+
        geom_point(aes(Canopy,Vcmax,
                       color=CO2_treatment), size=4,
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
        ylim(0,50)
    
    plot(p4)
    
    p5 <- ggplot(coefDF)+
        geom_point(aes(Canopy,Jmax,
                       color=CO2_treatment), size=4,
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
        ylim(0,80)
    
    plot(p5)
    
    p6 <- ggplot(coefDF)+
        geom_point(aes(Canopy,JVratio,
                       color=CO2_treatment), size=4,
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
        ylim(1,4)+
        scale_x_discrete(name="Canopy presence", breaks=c("12345", "345", "45"), 
                         labels=c("Whole", "Middle+Bottom", "Bottom"))
    
    plot(p6)
    
    pdf("output/canopy_parameter_summary.pdf", width=8, height=14)
    plot_grid(p4, p5, p6, rel_heights=c(1,1,1.5),
              labels="AUTO", ncol=1, align="v", axis = "l")
    dev.off()
    
    
    return(fits)
}
