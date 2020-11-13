fit_canopy_ACa <- function(plot.option) {
    
    ### read csv
    rtDF <- read.csv("output/canopy_scale_processed_ACa_curves.csv")
    
    
    ###################################  Fit ACi ##########################################
    #### Fitting ACI curve at the finest resolution
    myDF <- rtDF[,c("Identity", "Chamber", "Canopy", "date", "RH",
                    "Norm_corr_CO2_flux", "WTC_CO2", "Norm_H2O_flux", 
                    "Ci", "WTC_T", "Tleaf", "WTC_PAR", "VPD")]
    
    
    colnames(myDF) <- c("Identity", "Chamber", "Position", "Date", "RH",
                        "Photo", "Ca", "Cond", 
                        "Ci", "Tair", "Tleaf", "PAR", "VPD")
    
    ### create DF for fit aci parameters
    id.list <- c(111:125)
    
    ### theta and alpha values
    thetaDF <- read.csv("output/leaf/leaf_scale_alpha_and_theta.csv")
    
    ### prepare an output df
    outDF <- data.frame(id.list, 
                        NA, NA, NA, NA, NA, NA, 
                        NA, NA, NA, NA, NA, NA,
                        NA, NA, NA, NA, NA, NA,
                        NA, NA, NA, NA, NA, NA,
                        NA, NA, NA, NA, NA, NA, 
                        NA, NA, NA, NA, NA, NA,
                        NA, NA, NA, NA, NA, NA, NA)
    colnames(outDF) <- c("Identity", "Chamber", "CO2_treatment", "Height", "Date",
                         "RMSE", "Vcmax", "Vcmax.se", "Jmax", "Jmax.se", "Rd", "Rd.se",
                         "Ci_400", "ALEAF_400", "GS_400", "ELEAF_400", 
                         "Ac_400", "Aj_400", "Ap_400", 
                         "Ci_600", "ALEAF_600", "GS_600", "ELEAF_600", 
                         "Ac_600", "Aj_600", "Ap_600", 
                         "Ci_280", "ALEAF_600", "GS_280", "ELEAF_280", 
                         "Ac_280", "Aj_280", "Ap_280", 
                         "Ci_transition_Ac_Aj",
                         "Tleaf", "Ca", "Cc", "PPFD", "Patm", "VPD", 
                         "curve.fitting", 
                         "GammaStar", "Km", "G1")
    
    ### the for loop
    for (i in id.list) {
        ## subset each data
        test <- subset(myDF, Identity == i)
        
        if (test$Chamber[1]%in%c(1,3,8)) {
            alpha <- thetaDF$alpha.j[thetaDF$Ca_Trt=="a" & thetaDF$Position=="all"]
            theta <- thetaDF$theta[thetaDF$Ca_Trt=="a" & thetaDF$Position=="all"]
            
        } else if (test$Chamber[1]%in%c(4,11)) {
            alpha <- thetaDF$alpha.j[thetaDF$Ca_Trt=="e" & thetaDF$Position=="all"]
            theta <- thetaDF$theta[thetaDF$Ca_Trt=="e" & thetaDF$Position=="all"]
            
        } else {
            #default
            alpha = 0.24
            theta = 0.85
        }
        
        ## fit
        fit1 <- fitaci(test, fitmethod="bilinear", varnames = list(ALEAF="Photo",
                                                                   Tleaf="Tleaf", 
                                                                   Ci = "Ci",
                                                                   PPFD="PAR"),
                       Tcorrect=T, fitTPU=F,
                       EaV = 73412.98, EdVC = 2e+05, delsC = 643.955,
                       EaJ = 101017.38, EdVJ = 2e+05, delsJ = 655.345,
                       alpha=alpha, theta=theta)
        
        fit2 <- fitBB(test, varnames = list(ALEAF = "Photo", GS = "Cond", VPD = "VPD",
                                            Ca = "Ca", RH = "RH"),
                      gsmodel="BBOpti")
        
        ### list parameters for photosyn function input
        g1 <- coef(fit2)[2]
        
        
        ## get information on identity
        outDF[outDF$Identity == i, "Chamber"] <- unique(test$Chamber)
        outDF[outDF$Identity == i, "Position"] <- unique(test$Position)
        outDF[outDF$Identity == i, "curve.fitting"] <- fit1$fitmethod
        
        ## assign fitted values
        outDF[outDF$Identity == i, "RMSE"] <- fit1$RMSE
        outDF[outDF$Identity == i, "Vcmax"] <- fit1$pars[1,1]
        outDF[outDF$Identity == i, "Vcmax.se"] <- fit1$pars[1,2]
        outDF[outDF$Identity == i, "Jmax"] <- fit1$pars[2,1]
        outDF[outDF$Identity == i, "Jmax.se"] <- fit1$pars[2,2]
        outDF[outDF$Identity == i, "Rd"] <- fit1$pars[3,1]
        outDF[outDF$Identity == i, "Rd.se"] <- fit1$pars[3,2]
        
        outDF[outDF$Identity == i, "Ci_400"] <- fit1$Photosyn(Ca=400, g1=g1)[1]
        outDF[outDF$Identity == i, "ALEAF_400"] <- fit1$Photosyn(Ca=400, g1=g1)[2]
        outDF[outDF$Identity == i, "GS_400"] <- fit1$Photosyn(Ca=400, g1=g1)[3]
        outDF[outDF$Identity == i, "ELEAF_400"] <- fit1$Photosyn(Ca=400, g1=g1)[4]
        outDF[outDF$Identity == i, "Ac_400"] <- fit1$Photosyn(Ca=400, g1=g1)[5]
        outDF[outDF$Identity == i, "Aj_400"] <- fit1$Photosyn(Ca=400, g1=g1)[6]
        outDF[outDF$Identity == i, "Ap_400"] <- fit1$Photosyn(Ca=400, g1=g1)[7]
        
        
        outDF[outDF$Identity == i, "Ci_600"] <- fit1$Photosyn(Ca=600, g1=g1)[1]
        outDF[outDF$Identity == i, "ALEAF_600"] <- fit1$Photosyn(Ca=600, g1=g1)[2]
        outDF[outDF$Identity == i, "GS_600"] <- fit1$Photosyn(Ca=600, g1=g1)[3]
        outDF[outDF$Identity == i, "ELEAF_600"] <- fit1$Photosyn(Ca=600, g1=g1)[4]
        outDF[outDF$Identity == i, "Ac_600"] <- fit1$Photosyn(Ca=600, g1=g1)[5]
        outDF[outDF$Identity == i, "Aj_600"] <- fit1$Photosyn(Ca=600, g1=g1)[6]
        outDF[outDF$Identity == i, "Ap_600"] <- fit1$Photosyn(Ca=600, g1=g1)[7]
        
        
        outDF[outDF$Identity == i, "Ci_280"] <- fit1$Photosyn(Ca=280, g1=g1)[1]
        outDF[outDF$Identity == i, "ALEAF_280"] <- fit1$Photosyn(Ca=280, g1=g1)[2]
        outDF[outDF$Identity == i, "GS_280"] <- fit1$Photosyn(Ca=280, g1=g1)[3]
        outDF[outDF$Identity == i, "ELEAF_280"] <- fit1$Photosyn(Ca=280, g1=g1)[4]
        outDF[outDF$Identity == i, "Ac_280"] <- fit1$Photosyn(Ca=280, g1=g1)[5]
        outDF[outDF$Identity == i, "Aj_280"] <- fit1$Photosyn(Ca=280, g1=g1)[6]
        outDF[outDF$Identity == i, "Ap_280"] <- fit1$Photosyn(Ca=280, g1=g1)[7]
        
        outDF[outDF$Identity == i, "VPD"] <- fit1$Photosyn(Ca=400, g1=g1)[9]
        outDF[outDF$Identity == i, "Tleaf"] <- fit1$Photosyn(Ca=400, g1=g1)[10]
        outDF[outDF$Identity == i, "Ca"] <- fit1$Photosyn(Ca=400, g1=g1)[11]
        outDF[outDF$Identity == i, "Cc"] <- fit1$Photosyn(Ca=400, g1=g1)[12]
        outDF[outDF$Identity == i, "PPFD"] <- fit1$Photosyn(Ca=400, g1=g1)[13]
        outDF[outDF$Identity == i, "Patm"] <- fit1$Photosyn(Ca=400, g1=g1)[14]
        
        outDF[outDF$Identity == i, "Ci_transition_Ac_Aj"] <- fit1$Ci_transition
        outDF[outDF$Identity == i, "GammaStar"] <- fit1$GammaStar
        outDF[outDF$Identity == i, "Km"] <- fit1$Km
        # G1
        outDF[outDF$Identity == i, "G1"] <- coef(fit2)[2]
        
    }
    
    outDF$JVratio <- outDF$Jmax / outDF$Vcmax
    
    outDF$CO2_treatment <- "aCO2"
    outDF$CO2_treatment[outDF$Chamber%in%c(4,8)] <- "eCO2"
    
    ### save
    write.csv(outDF, "output/canopy/canopy_scale_parameters.csv", row.names=F)
    
    
    ################################# Plotting fit Aci params #################################
    if (plot.option == T) {
        fits.all <- fitacis(myDF, group="Identity", 
                            fitmethod="bilinear", varnames = list(ALEAF="Photo",
                                                                  Tleaf="Tleaf", 
                                                                  Ci = "Ci",
                                                                  PPFD="PAR"),
                            Tcorrect=T, fitTPU=F,
                            EaV = 73412.98, EdVC = 2e+05, delsC = 643.955,
                            EaJ = 101017.38, EdVJ = 2e+05, delsJ = 655.345,
                            theta=0.47, alpha=0.25)
        
        ### fit g1 value
        fits.bb <- fitBBs(myDF, varnames = list(ALEAF = "Photo", GS = "Cond", VPD = "VPD",
                                                Ca = "Ca", RH = "RH"),
                          group="Identity")
        ### create pdf
        pdf("output/canopy/canopy_level_individual_chamber_result.pdf", height=24, width=20)
        par(mfrow=c(5,3))
        #1,3,11, 4, 8
        
        ### make plot
        for (i in 1:15) {
            plot(fits.all[[i]], main=paste0(outDF$Chamber[i], ", ", outDF$Position[i], ", ",
                                            outDF$CO2_treatment[i]),
                 xlim=c(0, 1200))
            abline(v=c(320), lwd=2, lty=3)
        }
        
        dev.off()
    } ## end plot
}