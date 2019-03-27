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
    
    ### plot relationship between vcmax and jmax
    #with(coef(fits), plot(Vcmax, Jmax))
    #
    #### plot Ac and Aj
    #plot(fits[[1]])
    #plot(fits, how="oneplot")
    #plot(fits, how="oneplot", what="data", colour_by_id=T)
    #plot(fits, how="oneplot", add=T, what="model", lwd=c(1,1))
    #
    #### look at other elements
    #rmses <- sapply(fits, "[[", "RMSE")
    #plot(rmses, type='h', ylab="RMSE", xlab="Curve nr")
    #
    ## And plot the worst-fitting curve:
    #plot(fits[[which.max(rmses)]])
    #
    ## It is very straightforward to summarize the coefficients by a factor variable
    ## that was contained in the original data. In manyacidat, there is a factor variable
    ## 'treatment'.
    ## We first have to refit the curves, using the 'id' argument:
    #fits <- fitacis(myDF1, "Chamber", fitmethod="bilinear", id="C.treat")
    #
    ## And now use this to plot Vcmax by treatment.
    #boxplot(Vcmax ~ C.treat, data=coef(fits), ylim=c(0,130))
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
