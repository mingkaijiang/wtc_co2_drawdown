
leaf_ACI_processing <- function(plot.option) {
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
    ## chambers 1, 3, 5, 7, 9, 11 are CO2 ambient
    ## chambers 1, 3, 4, 6, 8, 11 are wet 
    myDF <- rbind(myDF1, myDF2)
    myDF <- subset(myDF, chamber %in% c("ch01",  # aCO2, wet
                                        "ch03",  # aCO2, wet
                                        "ch04",  # eCO2, wet
                                        "ch08",  # eCO2, wet
                                        "ch11")) # aCO2, wet
    
    myDF$year <- year(myDF$Date)
    
    ### replace chamber names to keep it consistent with canopy labeling
    myDF$chamber <- gsub("ch01", "1", myDF$chamber)
    myDF$chamber <- gsub("ch03", "3", myDF$chamber)
    myDF$chamber <- gsub("ch04", "4", myDF$chamber)
    myDF$chamber <- gsub("ch08", "8", myDF$chamber)
    myDF$chamber <- gsub("ch11", "11", myDF$chamber)
    
    ### replace names
    names(myDF)[names(myDF) == "RH_S"] <- "RH"
    
    ### ID list
    id.list <- unique(myDF$Identity)
    
    ### prepare an output df
    outDF <- data.frame(id.list, 
                        NA, NA, NA, NA, NA, NA, 
                        NA, NA, NA, NA, NA, NA,
                        NA, NA, NA, NA, NA, NA,
                        NA, NA, NA, NA, NA, NA,
                        NA, NA, NA, NA, NA, NA, 
                        NA, NA, NA, NA, NA, NA)
    colnames(outDF) <- c("Identity", "Chamber", "CO2_treatment", "Position", "Date",
                         "RMSE", "Vcmax", "Vcmax.se", "Jmax", "Jmax.se", "Rd", "Rd.se",
                         "Ci_400", "ALEAF_400", "GS_400", "ELEAF_400", 
                         "Ac_400", "Aj_400", "Ap_400", 
                         "Ci_600", "ALEAF_600", "GS_600", "ELEAF_600", 
                         "Ac_600", "Aj_600", "Ap_600", 
                         "Ci_transition_Ac_Aj",
                         "Tleaf", "Ca", "Cc", "PPFD", "Patm", "VPD", 
                         "curve.fitting", 
                         "GammaStar", "Km", "G1")
    
    ### the for loop
    for (i in 1:length(id.list)) {
        ## subset each data
        test <- subset(myDF, Identity == id.list[i])
        
        ## fit
        fit1 <- fitaci(test, fitmethod="bilinear", Tcorrect=T, fitTPU=F,
                       EaV = 73412.98, EdVC = 2e+05, delsC = 643.955,
                       EaJ = 101017.38, EdVJ = 2e+05, delsJ = 655.345)
        fit2 <- fitBB(test, gsmodel="BBOpti")
        
        
        ## get information on identity
        outDF[outDF$Identity == id.list[i], "CO2_treatment"] <- unique(test$CO2_treatment)
        outDF[outDF$Identity == id.list[i], "Chamber"] <- unique(test$chamber)
        outDF[outDF$Identity == id.list[i], "Position"] <- unique(test$Height)
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
        
        outDF[outDF$Identity == id.list[i], "Ci_400"] <- fit1$Photosyn(Ca=400)[1]
        outDF[outDF$Identity == id.list[i], "ALEAF_400"] <- fit1$Photosyn(Ca=400)[2]
        outDF[outDF$Identity == id.list[i], "GS_400"] <- fit1$Photosyn(Ca=400)[3]
        outDF[outDF$Identity == id.list[i], "ELEAF_400"] <- fit1$Photosyn(Ca=400)[4]
        outDF[outDF$Identity == id.list[i], "Ac_400"] <- fit1$Photosyn(Ca=400)[5]
        outDF[outDF$Identity == id.list[i], "Aj_400"] <- fit1$Photosyn(Ca=400)[6]
        outDF[outDF$Identity == id.list[i], "Ap_400"] <- fit1$Photosyn(Ca=400)[7]
        
        
        outDF[outDF$Identity == id.list[i], "Ci_600"] <- fit1$Photosyn(Ca=600)[1]
        outDF[outDF$Identity == id.list[i], "ALEAF_600"] <- fit1$Photosyn(Ca=600)[2]
        outDF[outDF$Identity == id.list[i], "GS_600"] <- fit1$Photosyn(Ca=600)[3]
        outDF[outDF$Identity == id.list[i], "ELEAF_600"] <- fit1$Photosyn(Ca=600)[4]
        outDF[outDF$Identity == id.list[i], "Ac_600"] <- fit1$Photosyn(Ca=600)[5]
        outDF[outDF$Identity == id.list[i], "Aj_600"] <- fit1$Photosyn(Ca=600)[6]
        outDF[outDF$Identity == id.list[i], "Ap_600"] <- fit1$Photosyn(Ca=600)[7]
        
        outDF[outDF$Identity == id.list[i], "VPD"] <- fit1$Photosyn(Ca=400)[9]
        outDF[outDF$Identity == id.list[i], "Tleaf"] <- fit1$Photosyn(Ca=400)[10]
        outDF[outDF$Identity == id.list[i], "Ca"] <- fit1$Photosyn(Ca=400)[11]
        outDF[outDF$Identity == id.list[i], "Cc"] <- fit1$Photosyn(Ca=400)[12]
        outDF[outDF$Identity == id.list[i], "PPFD"] <- fit1$Photosyn(Ca=400)[13]
        outDF[outDF$Identity == id.list[i], "Patm"] <- fit1$Photosyn(Ca=400)[14]
        
        
        outDF[outDF$Identity == id.list[i], "Ci_transition_Ac_Aj"] <- fit1$Ci_transition
        outDF[outDF$Identity == id.list[i], "GammaStar"] <- fit1$GammaStar
        outDF[outDF$Identity == id.list[i], "Km"] <- fit1$Km
        # G1
        outDF[outDF$Identity == id.list[i], "G1"] <- coef(fit2)[2]
        
    }
    
    outDF$JVratio <- outDF$Jmax / outDF$Vcmax
    
    ### save
    write.csv(outDF, "output/leaf/leaf_scale_parameters.csv", row.names=F)
    
    
    
    ################################### Plotting script ################################
    if (plot.option == T) {
        
        #### Fitting ACI curve at the finest resolution
        fits.all <- fitacis(myDF, group="Identity", 
                            fitmethod="bilinear",Tcorrect=T, fitTPU=F,
                            EaV = 73412.98, EdVC = 2e+05, delsC = 643.955,
                            EaJ = 101017.38, EdVJ = 2e+05, delsJ = 655.345)
        
        ### fit g1 value
        #fits.bb <- fitBBs(myDF, group="Identity")
        #
        #### assign factors onto the dataframe
        #coefDF <- coef(fits.all)
        #id.list <- unique(coefDF$Identity)
        #
        #for (i in id.list) {
        #    coefDF[coefDF$Identity==i, "Date"] <- unique(myDF[myDF$Identity==i, "Date"])
        #    coefDF[coefDF$Identity==i, "chamber"] <- unique(myDF[myDF$Identity==i, "chamber"])
        #    coefDF[coefDF$Identity==i, "Height"] <- unique(myDF[myDF$Identity==i, "Height"])
        #    coefDF[coefDF$Identity==i, "CO2_treatment"] <- unique(myDF[myDF$Identity==i, "CO2_treatment"])
        #}
        #
        #
        #### add vcmax to jmax ratio
        #coefDF$JVratio <- coefDF$Jmax/coefDF$Vcmax
        #
        #coefDF$Date <- as.Date(coefDF$Date)
        #
        #### calculate Ac_Aj transition Ci point
        #for (i in unique(coefDF$Identity)) {
        #    testDF <- subset(myDF, Identity==i)
        #    fit.i <- fitaci(testDF,fitmethod="bilinear",Tcorrect=T, fitTPU=F,
        #                    EaV = 73412.98, EdVC = 2e+05, delsC = 643.955,
        #                    EaJ = 101017.38, EdVJ = 2e+05, delsJ = 655.345)
        #    out <- findCiTransition(fit.i)
        #    coefDF[coefDF$Identity==i,"Ac_Aj"] <- out[1]
        #}
        
        ### create pdf
        pdf("output/leaf/leaf_level_individual_chamber_result.pdf", height=24, width=20)
        par(mfrow=c(8,5))
        #1,3,11, 4, 8
        
        ### make plot
        for (i in 1:40) {
            plot(fits.all[[i]], main=paste0(outDF$Chamber[i], ", ", outDF$Height[i], ", ",
                                            outDF$CO2_treatment[i]))
            abline(v=c(320), lwd=2, lty=3)
        }
        
        dev.off()
    }
    ################################### end plotting script #################################
    
    
    ### return 
    return(outDF)
    
 }

