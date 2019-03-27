#### individual leaf ACi curve measurement processing
#### Fit A-CI curve for each chamber

#### Notes from David
#### Heroult dataset – I have the dataset with predawn. Needs checking
#### WTC 1 – I have A-Ci data from March 2009 for chambers 1-4 and chambers 8 and 11 (so, 6 A-Ci curves all up). 

leaf_ACI_processing <- function() {
    #### read in datasets
    myDF1 <- read.csv("data/HFE_Esal_ACI-Mar2009.csv")
    
    #### Fitting ACI curve
    fits <- fitacis(myDF1, group="Chamber", fitmethod="bilinear")
    
    #test <- subset(myDF1, Chamber=="11")
    #fit <- fitaci(test, fitmethod="bilinear")
    #plot(fit)
    
    
    pdf("output/leaf_scale_aci_fitting.pdf")

    par(mfrow=c(2, 2))
    
    ### plot individual fitted data
    plot(fits, how="oneplot")
    plot(fits[[1]], col="black", add=T)
    
    ### plot more individual level data
    plot(fits, how="oneplot", what="data")
    plot(fits, how="oneplot", add=T, what="model", lwd=c(1,1))
    
    
    ### plot relationship between vcmax and jmax
    coefDF <- coef(fits)
    with(coefDF, plot(Vcmax, Jmax, col=Chamber, pch=19))
    legend("topright", legend=coefDF$Chamber, col=coefDF$Chamber, pch=19)
    
    
    ### look at other elements
    rmses <- sapply(fits, "[[", "RMSE")
    plot(rmses, type='h', ylab="RMSE", xlab="Curve nr", xaxt="n")
    axis(side=1, at=c(1:6), labels=c(1,2,3,4,8,11))
    
    ## And plot the worst-fitting curve:
    #plot(fits[[which.max(rmses)]])
    #text(500,28, "Worst fitting curve", font=2, cex=1.5)
    
    dev.off()
    
    
    return(fits)
}
