
leaf_ACI_processing <- function() {
    #### individual leaf ACi curve measurement processing
    #### Fit A-CI curve for each chamber
    #### two datasets: the first is upper canopy, and the second is lower canopy
    
    ### read in datasets
    myDF1 <- read.csv("data/ACi_curves/HFE_Aci_2008-2009.csv",stringsAsFactors=FALSE)
    myDF2 <- read.csv("data/ACi_curves/HFE_Aci_lowcanopy_2008-2009.csv",stringsAsFactors=FALSE)
    
    ### combine the datasets
    ### multiple factors: CO2 treatment
    ###                   chamber
    ###                   water treatment
    ###                   canopy location
    ###                   time
    myDF <- rbind(myDF1, myDF2)
    myDF <- subset(myDF, chamber %in% c("ch01", "ch02", "ch03", "ch04", #"ch05", "ch06",
                                        "ch07", "ch08", #"ch09", "ch10", 
                                        "ch11", "ch12"))
    
    myDF$year <- year(myDF$Date)
    #myDF <- subset(myDF, year == 2009)
    
    ### I think we don't want to subset the dataset,
    ### because summer 2008-9 has a drought treatment.
    ### Plus, we would have less sample size. 
    #myDF$Date <- as.Date(myDF$Date)
    #myDF <- subset(myDF, Date >= "2008-10-01")
    
    #### Fitting ACI curve at the finest resolution
    fits.all <- fitacis(myDF, group="Identity", fitmethod="bilinear",Tcorrect=T, fitTPU=T)
    
    ### fit g1 value
    names(myDF)[names(myDF) == "RH_S"] <- "RH"
    fits.bb <- fitBBs(myDF, group="Identity")
    
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
        #coefDF[coefDF$Identity==i, "Water_treatment"] <- unique(myDF[myDF$Identity==i, "Water_treatment"])
        #coefDF[coefDF$Identity==i, "inside_or_outside_WTC"] <- unique(myDF[myDF$Identity==i, "inside_or_outside_WTC"])
    }
    
    
    ### add vcmax to jmax ratio
    coefDF$JVratio <- coefDF$Jmax/coefDF$Vcmax
    
    coefDF$Date <- as.Date(coefDF$Date)

    ### calculate Ac_Aj transition Ci point
    for (i in unique(coefDF$Identity)) {
        testDF <- subset(myDF, Identity==i)
        fit.i <- fitaci(testDF,Tcorrect=T, fitTPU=T)
        out <- findCiTransition(fit.i)
        coefDF[coefDF$Identity==i,"Ac_Aj"] <- out[1]
    }
    
    ### exclude outliers
    #coefDF.sub <- coefDF[coefDF$JVratio <= 3, ]
    coefDF.sub <- coefDF
    
    ### make a list of identify
    id.list <- unique(coefDF.sub$Identity)
    
    ### prepare an output df
    outDF <- data.frame(id.list, NA, NA, NA, NA, NA, NA, 
                        NA, NA, NA, NA, NA, NA,
                        NA, NA, NA, NA, NA, NA,
                        NA, NA, NA, NA, NA, NA,
                        NA, NA, NA, NA, NA, NA)
    colnames(outDF) <- c("Identity", "Chamber", "CO2_treatment", "Height", "Date",
                         "RMSE", "Vcmax", "Vcmax.se", "Jmax", "Jmax.se", "Rd", "Rd.se",
                         "Ci", "ALEAF", "GS", "ELEAF", "Ac", "Aj", "Ap", "Rd2", "VPD",
                         "Tleaf", "Ca", "Cc", "PPFD", "Ci_transition_Ac_Aj",
                         "curve.fitting", "Patm", "GammaStar", "Km", "G1")
    
    ### the for loop
    for (i in 1:length(id.list)) {
        ## subset each data
        test <- subset(myDF, Identity == id.list[i])
        
        ## fit
        fit1 <- fitaci(test, fitmethod="bilinear", Tcorrect=T, fitTPU=T)
        fit2 <- fitBB(test, gsmodel="BBOpti")
        
        
        ## get information on identity
        outDF[outDF$Identity == id.list[i], "CO2_treatment"] <- unique(test$CO2_treatment)
        outDF[outDF$Identity == id.list[i], "Chamber"] <- unique(test$chamber)
        outDF[outDF$Identity == id.list[i], "Height"] <- unique(test$Height)
        outDF[outDF$Identity == id.list[i], "curve.fitting"] <- fit1$fitmethod
        outDF[outDF$Identity == id.list[i], "Date"] <- unique(test$Date)
        
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
        # G1
        outDF[outDF$Identity == id.list[i], "G1"] <- coef(fit2)[2]
        
    }
    
    outDF$JVratio <- outDF$Jmax / outDF$Vcmax
    
    ### save
    write.csv(outDF, "output/leaf/leaf_scale_parameters.csv", row.names=F)
    
    
    ### return 
    return(outDF)
    
 }

