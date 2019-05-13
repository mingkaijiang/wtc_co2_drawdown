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
    
    #### Fitting ACI curve
    fits <- fitacis(myDF, group="Chamber", fitmethod="bilinear")
    
    pdf("output/canopy_scale_aci_fitting.pdf")
    
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
    legend("bottomright", legend=coefDF$Chamber, col=coefDF$Chamber, pch=19)
    
    
    ### look at other elements
    rmses <- sapply(fits, "[[", "RMSE")
    plot(rmses, type='h', ylab="RMSE", xlab="Curve nr", xaxt="n")
    axis(side=1, at=c(1:8), labels=c(1,2,3,4,7,8,11,12))
    
    ## And plot the worst-fitting curve:
    #plot(fits[[which.max(rmses)]])
    #text(500,28, "Worst fitting curve", font=2, cex=1.5)
    
    dev.off()
    
    #
    ## It is very straightforward to summarize the coefficients by a factor variable
    ## that was contained in the original data. In manyacidat, there is a factor variable
    ## 'treatment'.
    ## We first have to refit the curves, using the 'id' argument:
    #fits <- fitacis(myDF.clean, "Canopy", fitmethod="bilinear", id="Chamber")
    #
    ## And now use this to plot Vcmax by treatment.
    #boxplot(Vcmax ~ Canopy, data=coef(fits), ylim=c(0,130))
    #
    ## As of package version 1.4-2, you can also use the id variable for colouring curves,
    ## when plotting all fitted curves in one plot.
    ## Set colours to be used. Also note that the 'id' variable has to be a factor,
    ## colours will be set in order of the levels of the factor.
    ## Set palette of colours:
    #palette(rainbow(8))
    #
    ## Use colours, add legend.
    #plot(fits, how="oneplot", colour_by_id = TRUE, id_legend=TRUE)
    
    
    return(fits)
}
