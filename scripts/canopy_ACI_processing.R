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
    myDF <- myDF[myDF$identity!="7-45",]
    
    
    #### Fitting ACI curve
    fits <- fitacis(myDF, group="identity", fitmethod="bilinear", Tcorrect=T)
    
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
    


    
    
    return(fits)
}
