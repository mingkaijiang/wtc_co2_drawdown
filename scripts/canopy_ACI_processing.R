#### individual canopy ACi curve measurement processing
#### Fit A-CI curve for each chamber

canopy_ACI_processing <- function(myDF2) {
    
    myDF.rename <- myDF2
    colnames(myDF.rename) <- c("Chamber", "Canopy", "Datetime", "vtime",
                         "vCO2", "Ci", "CO2centralCh", "Tleaf", "Tair",
                         "DPLicorCh", "CondWater", "Photo", "SumOfarea_fully_exp",
                         "time", "date", "time_elapsed", "co2_flux")
    
    #### Fitting ACI curve
    myDF.clean <- myDF.rename[complete.cases(myDF.rename$Photo), ]
    
    fits <- fitacis(myDF.clean, group="Chamber", fitmethod="bilinear")
    
    ### plot relationship between vcmax and jmax
    with(coef(fits), plot(Vcmax, Jmax))
    
    ### plot Ac and Aj
    plot(fits[[1]])
    plot(fits, how="oneplot")
    plot(fits, how="oneplot", add=T, what="model", lwd=c(1,1))
    
    ### look at other elements
    rmses <- sapply(fits, "[[", "RMSE")
    plot(rmses, type='h', ylab="RMSE", xlab="Curve nr")
    
    # And plot the worst-fitting curve:
    plot(fits[[which.max(rmses)]])
    
    # It is very straightforward to summarize the coefficients by a factor variable
    # that was contained in the original data. In manyacidat, there is a factor variable
    # 'treatment'.
    # We first have to refit the curves, using the 'id' argument:
    fits <- fitacis(myDF.clean, "Chamber", fitmethod="bilinear", id="Canopy")
    
    # And now use this to plot Vcmax by treatment.
    boxplot(Vcmax ~ Canopy, data=coef(fits), ylim=c(0,130))
    
    # As of package version 1.4-2, you can also use the id variable for colouring curves,
    # when plotting all fitted curves in one plot.
    # Set colours to be used. Also note that the 'id' variable has to be a factor,
    # colours will be set in order of the levels of the factor.
    # Set palette of colours:
    palette(rainbow(8))
    
    # Use colours, add legend.
    plot(fits, how="oneplot", colour_by_id = TRUE, id_legend=TRUE)
    
    
    return(fits)
}
