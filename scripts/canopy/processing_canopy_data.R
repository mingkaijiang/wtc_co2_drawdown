processing_canopy_data <- function(leafDF) {
    #### There are two possible datasets,
    #### they should be almost identical.
    #### The dataset "mergeall.text" is a potentially processed file,
    #### but it does not have canopy of 0, and PAR information, 
    #### and, it has less number of data entries (not much less).
    #### In comparison, the "drawdownanalysis9Sepb.csv" contains
    #### canopy of 0, the un-normalized data, PAR information.
    ### Here I am using the drawdownalaysis9Sepb.csv file
    
    #                                    Descriptions	
    # values calculated using SAS prog 	
    # chamber	chamber number
    # canopy	canopy layers present during drawdown
    # vCo2	CO2 concentration in chamber
    # vT	Air temp (by vaisala not shielded)
    # vtime2	date and time 
    # Tair	Air temp from WTC system
    # DPLicorCh	Dew point in chamber updated every 14 mins
    # PAR	PAR as measured by WTC system
    # slope2	dCO2/dt umol tree-1 s-1
    # cmarea	leaf area in chamber m2
    # nslope2	normalised flux umol m-2 leaf s-1
    # k	leak coefficient
    # time	time of day
    # leak	leak =(vco2-380)*k
    # corrflux	leak corrected flux umol tree-1 s-1
    # ncorrflux	normalised corrected flux umol m-2 leaf s-1#
    
    #### note: corrected for different sizes of trees,
    ####       by Drake's method (Drake et al. 2018),
    ####       i.e. express raw canopy flux on a leaf area basis by dividing by total canopy leaf area
    

    ########################  set-up variable names ###########################
    ### read in raw data
    myDF <- read.csv("data/canopy_drawdown/drawdownanalysis9Sepb.csv")
    
    ### set up dataset
    myDF$canopy <- as.character(myDF$canopy)
    
    colnames(myDF) <- c("Chamber", "Canopy","vCo2", 
                        "vT", "datetime", "Tair",
                        "DPLicorCh", "PARi", "slope2", 
                        "cmarea", "nslope2","k", "time", 
                        "leak", "corrflux", "ncorrflux")
    
    ### set up data and time
    myDF$date <- sub(" .*", "", myDF$datetime)
    myDF$date <- as.Date(as.character(myDF$date), format="%m/%d/%y")
    myDF$time1 <- sub(".+? ", "", myDF$datetime)
    myDF$time2 <- sub(" .*", "", myDF$time)
    myDF$time3 <- str_sub(myDF$time2, start=-3)
    myDF$time <- paste0(myDF$time1, ":00")#myDF$time3)
    
    myDF$datetime <- as.POSIXct(paste(as.character(myDF$date), myDF$time), format="%Y-%m-%d %H:%M:%S")
    myDF$time <- strftime(myDF$datetime, format="%H:%M:%S")
    myDF$time <- as.POSIXct(myDF$time, format="%H:%M:%S")
    
    myDF <- myDF[,!(colnames(myDF)%in% c("time1", "time2", "time3"))]
    
    ### update names to improve readability
    names(myDF)[names(myDF) == "vCo2"] <- "WTC_CO2"
    names(myDF)[names(myDF) == "vT"] <- "WTC_T"
    names(myDF)[names(myDF) == "DPLicorCh"] <- "WTC_dew_point"
    names(myDF)[names(myDF) == "PARi"] <- "WTC_PAR"
    names(myDF)[names(myDF) == "slope2"] <- "CO2_flux"
    names(myDF)[names(myDF) == "nslope2"] <- "Norm_CO2_flux"
    names(myDF)[names(myDF) == "cmarea"] <- "Leaf_area"
    names(myDF)[names(myDF) == "k"] <- "Leak_coef"
    names(myDF)[names(myDF) == "leak"] <- "Leak"
    names(myDF)[names(myDF) == "corrflux"] <- "Corr_CO2_flux"
    names(myDF)[names(myDF) == "ncorrflux"] <- "Norm_corr_CO2_flux"
    
    myDF <- myDF[,c("Chamber", "Canopy", "datetime", "date", "time", 
                    "WTC_CO2", "WTC_T", "WTC_dew_point", "WTC_PAR", 
                    "Tair", "Leaf_area", "Leak_coef", "Leak", 
                    "CO2_flux", "Norm_CO2_flux", "Corr_CO2_flux", 
                    "Norm_corr_CO2_flux")]
    
    ### add VPD 
    ## Saturation Vapor Pressure (es) = 0.6108 * exp(17.27 * T / (T + 237.3))
    ## kPa
    myDF$ES <- 0.6106 * exp(17.27 * myDF$WTC_T / (myDF$WTC_T + 237.3))
    
    ## calculate RH
    myDF$RH <- 100 - 5 * (myDF$WTC_T - myDF$WTC_dew_point)
    
    myDF$RH <- ifelse(myDF$RH > 100, 100, myDF$RH)
    
    ## Actual Vapor Pressure (ea) = RH / 100 * es 
    ## kPa
    myDF$EA <- myDF$ES * myDF$RH / 100 
    
    ##  VPD = ea - es
    #myDF$VPD <- myDF$ea - myDF$es
    ## kPa
    myDF$VPD <- myDF$ES * (100 - myDF$RH)/100
    
    ### ignore VPD = 0 and NA
    myDF <- subset(myDF, VPD > 0)
    myDF <- myDF[complete.cases(myDF$VPD), ]
    
    ## calculate based on planteophys
    #require(plantecophys)
    #myDF$VPD2 <- RHtoVPD(myDF$RH, myDF$Tair, Pa = 101)
    
    ### merge with H2O flux dataset
    myDF <- merge_with_H2O_flux_dataset(myDF)
    
    ### ignore unreasonable data
    myDF <- subset(myDF, Norm_corr_CO2_flux >= -2)
    
    ########################  quality control ###########################
    
    ### check canopy data structure
    #canopy_data_check_and_plot(myDF)
    
    ### only delete outliers, keep all date information
    myDF <- canopy_data_control_basic_2(myDF)
    
    ### continue cleaning data
    myDF$Norm_corr_CO2_flux <- as.numeric(myDF$Norm_corr_CO2_flux)
    
    ########################  add transpiration to get Ci ###########################
    ### add H2O flux
    outDF <- calculate_transpiration_flux(myDF)

    ### calculate gs
    outDF$gs1 <- outDF$Norm_H2O_flux / outDF$VPD

    ### read in leaf-scale g1 value to represent canopy g1
    ### i.e. assuming same g1 value for leaf and canopy
    outDF <- add_leaf_g1_to_canopy_data(leafDF=leafDF, canopyDF=outDF)
    
    ### alterantive way of calculating gs
    outDF$gs <- (1+(outDF$G1/sqrt(outDF$VPD))) * (outDF$Norm_corr_CO2_flux/outDF$WTC_CO2)
    outDF <- subset(outDF, gs > 0)
    
    ### alternative way of calculating water transpiration flux
    outDF$Norm_H2O_flux2 <- outDF$Norm_corr_CO2_flux * (outDF$G1*sqrt(outDF$VPD) + outDF$VPD) / outDF$WTC_CO2
    
    ### calculate Ci
    outDF$Ci <- outDF$WTC_CO2 - (outDF$Norm_corr_CO2_flux/outDF$gs)
    
    ### plotting CO2 and H2O flux at per second rate for each chamber
    canopy_data_per_second_check_and_plot(inDF=outDF)
    
    
    ### filter according to PAR
    outDF <- subset(outDF, WTC_PAR >= 1000)

    ### add identity information, starting from 110 so that it doesn't repeat leaf identity
    outDF$ID <- paste0(outDF$Chamber, "-", outDF$Canopy)
    idDF <- data.frame(c(111:125), unique(outDF$ID))
    colnames(idDF) <- c("Identity", "ID")
    
    outDF <- merge(outDF, idDF, by="ID", all=T)
    
    outDF$Tleaf <- outDF$WTC_T 
    
    
    ### return canopyDF
    rtDF <- outDF[,c("Identity", "Chamber", "Canopy", "datetime",
                     "date", "time", "WTC_CO2", "WTC_T", "WTC_dew_point",
                     "WTC_PAR", "Tair", "ES", "RH", "EA", "VPD",
                     "Leaf_area", "Leak_coef", "Leak", "CO2_flux",
                     "Norm_CO2_flux", "Corr_CO2_flux", "Norm_corr_CO2_flux",
                     "Cond_water", "WTC_volume_m3", "Norm_H2O_flux",
                     "gs1", "G1", "gs", "Norm_H2O_flux2", "Ci", "Tleaf")]
    
    
    
    #### Fitting ACI curve at the finest resolution
    myDF <- rtDF[,c("Identity", "Chamber", "Canopy", "date", "RH",
                      "Norm_corr_CO2_flux", "WTC_CO2", "Norm_H2O_flux", 
                      "Ci", "WTC_T", "Tleaf", "WTC_PAR", "VPD")]
    
    
    colnames(myDF) <- c("Identity", "Chamber", "Position", "Date", "RH",
                        "Photo", "Ca", "Cond", 
                        "Ci", "Tair", "Tleaf", "PAR", "VPD")
    
    fits.all <- fitacis(myDF, group="Identity", 
                        fitmethod="bilinear", varnames = list(ALEAF="Photo",
                                                              Tleaf="Tleaf", 
                                                              Ci = "Ci",
                                                              PPFD="PAR"),
                        Tcorrect=T, fitTPU=F,
                        EaV = 73412.98, EdVC = 2e+05, delsC = 643.955,
                        EaJ = 101017.38, EdVJ = 2e+05, delsJ = 655.345)
    
    ### fit g1 value
    fits.bb <- fitBBs(myDF, varnames = list(ALEAF = "Photo", GS = "Cond", VPD = "VPD",
                                            Ca = "Ca", RH = "RH"),
                      group="Identity")
    
    ### create DF for fit aci parameters
    id.list <- c(111:125)
    
    ### prepare an output df
    outDF <- data.frame(id.list, 
                        NA, NA, NA, NA, NA, NA, 
                        NA, NA, NA, NA, NA, NA,
                        NA, NA, NA, NA, NA, NA,
                        NA, NA, NA, NA, NA, NA,
                        NA, NA, NA, NA, NA, NA, 
                        NA, NA, NA, NA, NA, NA)
    colnames(outDF) <- c("Identity", "Chamber", "CO2_treatment", "Height", "Date",
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
    for (i in id.list) {
        ## subset each data
        test <- subset(myDF, Identity == i)
        
        ## fit
        fit1 <- fitaci(test, fitmethod="bilinear", varnames = list(ALEAF="Photo",
                                                                   Tleaf="Tleaf", 
                                                                   Ci = "Ci",
                                                                   PPFD="PAR"),
                       Tcorrect=T, fitTPU=F,
                       EaV = 73412.98, EdVC = 2e+05, delsC = 643.955,
                       EaJ = 101017.38, EdVJ = 2e+05, delsJ = 655.345)
        
        fit2 <- fitBB(test, varnames = list(ALEAF = "Photo", GS = "Cond", VPD = "VPD",
                                            Ca = "Ca", RH = "RH"),
                      gsmodel="BBOpti")
        
        
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
        
        outDF[outDF$Identity == i, "Ci_400"] <- fit1$Photosyn(Ca=400)[1]
        outDF[outDF$Identity == i, "ALEAF_400"] <- fit1$Photosyn(Ca=400)[2]
        outDF[outDF$Identity == i, "GS_400"] <- fit1$Photosyn(Ca=400)[3]
        outDF[outDF$Identity == i, "ELEAF_400"] <- fit1$Photosyn(Ca=400)[4]
        outDF[outDF$Identity == i, "Ac_400"] <- fit1$Photosyn(Ca=400)[5]
        outDF[outDF$Identity == i, "Aj_400"] <- fit1$Photosyn(Ca=400)[6]
        outDF[outDF$Identity == i, "Ap_400"] <- fit1$Photosyn(Ca=400)[7]
        
        
        outDF[outDF$Identity == i, "Ci_600"] <- fit1$Photosyn(Ca=600)[1]
        outDF[outDF$Identity == i, "ALEAF_600"] <- fit1$Photosyn(Ca=600)[2]
        outDF[outDF$Identity == i, "GS_600"] <- fit1$Photosyn(Ca=600)[3]
        outDF[outDF$Identity == i, "ELEAF_600"] <- fit1$Photosyn(Ca=600)[4]
        outDF[outDF$Identity == i, "Ac_600"] <- fit1$Photosyn(Ca=600)[5]
        outDF[outDF$Identity == i, "Aj_600"] <- fit1$Photosyn(Ca=600)[6]
        outDF[outDF$Identity == i, "Ap_600"] <- fit1$Photosyn(Ca=600)[7]
        
        outDF[outDF$Identity == i, "VPD"] <- fit1$Photosyn(Ca=400)[9]
        outDF[outDF$Identity == i, "Tleaf"] <- fit1$Photosyn(Ca=400)[10]
        outDF[outDF$Identity == i, "Ca"] <- fit1$Photosyn(Ca=400)[11]
        outDF[outDF$Identity == i, "Cc"] <- fit1$Photosyn(Ca=400)[12]
        outDF[outDF$Identity == i, "PPFD"] <- fit1$Photosyn(Ca=400)[13]
        outDF[outDF$Identity == i, "Patm"] <- fit1$Photosyn(Ca=400)[14]
        
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
    
    
    ### return output
    return(rtDF)

    
}
