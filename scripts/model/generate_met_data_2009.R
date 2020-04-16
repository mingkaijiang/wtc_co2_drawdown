generate_met_data_2009 <- function() {
    #### this script generates hourly met data for two-leaf model
    #### it reads year 2009 met data from WTC
    #### which is not chamber-specific
    #### then it combines with chamber-specific flux data
    #### to generate met data for each chamber
    
    
    #### need the following variables
    ### DateTime
    ### doy
    ### hod
    ### tair
    ### par
    ### vpd
    ### wind
    ### pressure
    ### Ca
    ### leafArea
    ### Vcmax25
    ### Jmax25
    ### FluxCO2
    ### FluxH2O
    ### chamber
    ### T_treatment
    ### Water_treatment
    
    
    ########################  read in WTC met data ###########################
    metDF <- read.csv("data/met/WTC_met_2009/HFE 30min Metdata 2009.csv")

    ### check which date do we run out of data
    subDF <- metDF[complete.cases(metDF$PAR),]
    subDF$DateTime <- as.POSIXct(as.character(subDF$DateTime), format="%Y-%m-%d %H:%M:%S")
    subDF$doy <- subDF$DOY
    
    ### subset only hourly data, ignore half hourly timesteps
    subDF2 <- subDF#subset(subDF, minute == "0")
    subDF2$hod <- subDF2$hour
    
    ### get the easy stuff ready
    subDF2$tair <- subDF2$TairRef
    subDF2$par <- subDF2$PAR
    subDF2$vpd <- subDF2$VPDdem
    subDF2$wind <- subDF2$winddem
    subDF2$pressure <- subDF2$AirPress
    
    subDF3 <- subDF2[,c("DateTime","doy", "hod", "tair", "par", "vpd", "wind", "pressure")]
    subDF4 <- subDF2[,c("DateTime","doy", "hod", "vpd", "wind", "pressure")]
    
    
    ########################  using drawdownanalysis9Sepb.csv file ###########################
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
    #myDF$time <- paste0(myDF$time1, myDF$time3)
    myDF$time <- paste0(myDF$time1, ":00")
    
    myDF$datetime <- as.POSIXct(paste(myDF$date, myDF$time), format="%Y-%m-%d %H:%M:%S")
    myDF$time <- strftime(myDF$datetime, format="%H:%M:%S")
    myDF$time <- as.POSIXct(myDF$time, format="%H:%M:%S")
    
    myDF <- myDF[,!(colnames(myDF)%in% c("time1", "time2", "time3"))]
    myDF <- myDF[myDF$Chamber%in%c(1,2,3,4,5,6,7,8,9,10,11,12),]  
    hDF <- myDF[complete.cases(myDF$cmarea),]
    hDF$DateTime <- hDF$datetime
    
    ### calculate hourly met data
    #myDF$time1 <- sub(".+? ", "", myDF$datetime)
    #myDF$hod <- as.numeric(sub(":.*", "", myDF$time1))
    
    #hDF <- summaryBy(vCo2+vT+Tair+DPLicorCh+PARi+cmarea+corrflux+ncorrflux~Chamber+Canopy+date+hod,
    #                 data=myDF, FUN=mean, na.rm=T, keep.names=T)
    #hDF <- hDF[complete.cases(hDF$cmarea),]
    #hDF$time <- paste0(hDF$hod, ":00:00")
    #hDF$DateTime <- as.POSIXct(paste(hDF$date, hDF$time), format="%Y-%m-%d %H:%M:%S")
    
    
    ########################  create chamber-specific met ###########################
    ch01 <- subset(hDF, Chamber==1)
    ch02 <- subset(hDF, Chamber==2)
    ch03 <- subset(hDF, Chamber==3)
    ch04 <- subset(hDF, Chamber==4)
    ch07 <- subset(hDF, Chamber==7)
    ch08 <- subset(hDF, Chamber==8)
    ch11 <- subset(hDF, Chamber==11)
    ch12 <- subset(hDF, Chamber==12)

    ## merge
    ch01.m <- merge(ch01, subDF4, by.x=c("DateTime"), by.y=c("DateTime"))
    ch02.m <- merge(ch02, subDF4, by.x=c("DateTime"), by.y=c("DateTime"))
    ch03.m <- merge(ch03, subDF4, by.x=c("DateTime"), by.y=c("DateTime"))
    ch04.m <- merge(ch04, subDF4, by.x=c("DateTime"), by.y=c("DateTime"))
    ch07.m <- merge(ch07, subDF4, by.x=c("DateTime"), by.y=c("DateTime"))
    ch08.m <- merge(ch08, subDF4, by.x=c("DateTime"), by.y=c("DateTime"))
    ch11.m <- merge(ch11, subDF4, by.x=c("DateTime"), by.y=c("DateTime"))
    ch12.m <- merge(ch12, subDF4, by.x=c("DateTime"), by.y=c("DateTime"))
    
    ## select output variable
    ch01.o <- ch01.m[,c("DateTime", "Chamber", "Canopy", "doy", "hod",
                        "vCo2", "vT", "PARi", "cmarea", "corrflux", "ncorrflux",
                        "vpd", "wind", "pressure")]
    ch02.o <- ch02.m[,c("DateTime", "Chamber", "Canopy", "doy", "hod",
                        "vCo2", "vT", "PARi", "cmarea", "corrflux","ncorrflux",
                        "vpd", "wind", "pressure")]
    ch03.o <- ch03.m[,c("DateTime", "Chamber", "Canopy", "doy", "hod",
                        "vCo2", "vT", "PARi", "cmarea", "corrflux","ncorrflux",
                        "vpd", "wind", "pressure")]    
    ch04.o <- ch04.m[,c("DateTime", "Chamber", "Canopy", "doy", "hod",
                        "vCo2", "vT", "PARi", "cmarea", "corrflux","ncorrflux",
                        "vpd", "wind", "pressure")]
    ch07.o <- ch07.m[,c("DateTime", "Chamber", "Canopy", "doy", "hod",
                        "vCo2", "vT", "PARi", "cmarea", "corrflux","ncorrflux",
                        "vpd", "wind", "pressure")]
    ch08.o <- ch08.m[,c("DateTime", "Chamber", "Canopy", "doy", "hod",
                        "vCo2", "vT", "PARi", "cmarea", "corrflux","ncorrflux",
                        "vpd", "wind", "pressure")]
    ch11.o <- ch11.m[,c("DateTime", "Chamber", "Canopy", "doy", "hod",
                        "vCo2", "vT", "PARi", "cmarea", "corrflux","ncorrflux",
                        "vpd", "wind", "pressure")]
    ch12.o <- ch12.m[,c("DateTime", "Chamber", "Canopy", "doy", "hod",
                        "vCo2", "vT", "PARi", "cmarea", "corrflux","ncorrflux",
                        "vpd", "wind", "pressure")]
    
    ## reassign names
    colnames(ch01.o) <- c("DateTime", "chamber", "Canopy", "doy", "hod",
                          "Ca", "tair", "par", "leafArea", "FluxCO2", "NfluxCO2",
                          "vpd", "wind", "pressure")
    colnames(ch02.o) <- c("DateTime", "chamber", "Canopy", "doy", "hod",
                          "Ca", "tair", "par", "leafArea", "FluxCO2", "NfluxCO2",
                          "vpd", "wind", "pressure")
    colnames(ch03.o) <- c("DateTime", "chamber", "Canopy", "doy", "hod",
                          "Ca", "tair", "par", "leafArea", "FluxCO2", "NfluxCO2",
                          "vpd", "wind", "pressure")
    colnames(ch04.o) <- c("DateTime", "chamber", "Canopy", "doy", "hod",
                          "Ca", "tair", "par", "leafArea", "FluxCO2", "NfluxCO2",
                          "vpd", "wind", "pressure")
    colnames(ch07.o) <- c("DateTime", "chamber", "Canopy", "doy", "hod",
                          "Ca", "tair", "par", "leafArea", "FluxCO2", "NfluxCO2",
                          "vpd", "wind", "pressure")
    colnames(ch08.o) <- c("DateTime", "chamber", "Canopy", "doy", "hod",
                          "Ca", "tair", "par", "leafArea", "FluxCO2", "NfluxCO2",
                          "vpd", "wind", "pressure")
    colnames(ch11.o) <- c("DateTime", "chamber", "Canopy", "doy", "hod",
                          "Ca", "tair", "par", "leafArea", "FluxCO2", "NfluxCO2",
                          "vpd", "wind", "pressure")
    colnames(ch12.o) <- c("DateTime", "chamber", "Canopy", "doy", "hod",
                          "Ca", "tair", "par", "leafArea", "FluxCO2", "NfluxCO2",
                          "vpd", "wind", "pressure")
    
    ch01.o$T_treatment <- "ambient"
    ch02.o$T_treatment <- "ambient"
    ch03.o$T_treatment <- "ambient"
    ch04.o$T_treatment <- "ambient"
    ch07.o$T_treatment <- "ambient"
    ch08.o$T_treatment <- "ambient"
    ch11.o$T_treatment <- "ambient"
    ch12.o$T_treatment <- "ambient"
    
    ch01.o$Water_treatment <- "control"
    ch02.o$Water_treatment <- "control"
    ch03.o$Water_treatment <- "control"
    ch04.o$Water_treatment <- "control"
    ch07.o$Water_treatment <- "control"
    ch08.o$Water_treatment <- "control"
    ch11.o$Water_treatment <- "control"
    ch12.o$Water_treatment <- "control"
    
    ch01.o$year <- "2009"
    ch02.o$year <- "2009"
    ch03.o$year <- "2009"
    ch04.o$year <- "2009"
    ch07.o$year <- "2009"
    ch08.o$year <- "2009"
    ch11.o$year <- "2009"
    ch12.o$year <- "2009"
    
    ch01.o$pressure <- ch01.o$pressure * 1000.0
    ch02.o$pressure <- ch02.o$pressure * 1000.0
    ch03.o$pressure <- ch03.o$pressure * 1000.0
    ch04.o$pressure <- ch04.o$pressure * 1000.0
    ch07.o$pressure <- ch07.o$pressure * 1000.0
    ch08.o$pressure <- ch08.o$pressure * 1000.0
    ch11.o$pressure <- ch11.o$pressure * 1000.0
    ch12.o$pressure <- ch12.o$pressure * 1000.0
    
    
    ### Add chamber-specific VPD data from cDF
    cDF.sub <- subset(cDF, Chamber == "1")
    ch01.m <- merge(ch01.o, cDF.sub, by.x=c("DateTime"), by.y=c("datetime"))
    
    cDF.sub <- subset(cDF, Chamber == "2")
    ch02.m <- merge(ch02.o, cDF.sub, by.x=c("DateTime"), by.y=c("datetime"))
    
    cDF.sub <- subset(cDF, Chamber == "3")
    ch03.m <- merge(ch03.o, cDF.sub, by.x=c("DateTime"), by.y=c("datetime"))
    
    cDF.sub <- subset(cDF, Chamber == "4")
    ch04.m <- merge(ch04.o, cDF.sub, by.x=c("DateTime"), by.y=c("datetime"))
    
    cDF.sub <- subset(cDF, Chamber == "7")
    ch07.m <- merge(ch07.o, cDF.sub, by.x=c("DateTime"), by.y=c("datetime"))
    
    cDF.sub <- subset(cDF, Chamber == "8")
    ch08.m <- merge(ch08.o, cDF.sub, by.x=c("DateTime"), by.y=c("datetime"))
    
    cDF.sub <- subset(cDF, Chamber == "11")
    ch11.m <- merge(ch11.o, cDF.sub, by.x=c("DateTime"), by.y=c("datetime"))
    
    cDF.sub <- subset(cDF, Chamber == "12")
    ch12.m <- merge(ch12.o, cDF.sub, by.x=c("DateTime"), by.y=c("datetime"))
    
    ch01.m$FluxH2O <- ch01.m$H2O_flux_normalized * ch01.m$leafArea
    ch02.m$FluxH2O <- ch02.m$H2O_flux_normalized * ch02.m$leafArea
    ch03.m$FluxH2O <- ch03.m$H2O_flux_normalized * ch03.m$leafArea
    ch04.m$FluxH2O <- ch04.m$H2O_flux_normalized * ch04.m$leafArea
    ch07.m$FluxH2O <- ch07.m$H2O_flux_normalized * ch07.m$leafArea
    ch08.m$FluxH2O <- ch08.m$H2O_flux_normalized * ch08.m$leafArea
    ch11.m$FluxH2O <- ch11.m$H2O_flux_normalized * ch11.m$leafArea
    ch12.m$FluxH2O <- ch12.m$H2O_flux_normalized * ch12.m$leafArea
    
    
    ## select output variable
    ch01.o <- ch01.m[,c("DateTime", "Chamber", "Canopy.x", "doy", "hod", "year", 
                        "Ca", "tair", "par", "leafArea", "FluxCO2", "NfluxCO2", "FluxH2O", "H2O_flux_normalized",
                        "vpd", "wind", "pressure", "T_treatment", "Water_treatment", "Tair", "VPD",)]
    
    ch02.o <- ch02.m[,c("DateTime", "Chamber", "Canopy.x", "doy", "hod","year", 
                        "Ca", "tair", "par", "leafArea", "FluxCO2", "NfluxCO2","FluxH2O", "H2O_flux_normalized",
                        "vpd", "wind", "pressure", "T_treatment", "Water_treatment", "Tair", "VPD")]
    
    ch03.o <- ch03.m[,c("DateTime", "Chamber", "Canopy.x", "doy", "hod","year", 
                        "Ca", "tair", "par", "leafArea", "FluxCO2", "NfluxCO2","FluxH2O", "H2O_flux_normalized",
                        "vpd", "wind", "pressure", "T_treatment", "Water_treatment", "Tair", "VPD")]
    
    ch04.o <- ch04.m[,c("DateTime", "Chamber", "Canopy.x", "doy", "hod","year", 
                        "Ca", "tair", "par", "leafArea", "FluxCO2", "NfluxCO2","FluxH2O", "H2O_flux_normalized",
                        "vpd", "wind", "pressure", "T_treatment", "Water_treatment", "Tair", "VPD")]
    
    ch07.o <- ch07.m[,c("DateTime", "Chamber", "Canopy.x", "doy", "hod","year", 
                        "Ca", "tair", "par", "leafArea", "FluxCO2", "NfluxCO2","FluxH2O", "H2O_flux_normalized",
                        "vpd", "wind", "pressure", "T_treatment", "Water_treatment", "Tair", "VPD")]
    
    ch08.o <- ch08.m[,c("DateTime", "Chamber", "Canopy.x", "doy", "hod","year", 
                        "Ca", "tair", "par", "leafArea", "FluxCO2", "NfluxCO2","FluxH2O", "H2O_flux_normalized",
                        "vpd", "wind", "pressure", "T_treatment", "Water_treatment", "Tair", "VPD")]
    
    ch11.o <- ch11.m[,c("DateTime", "Chamber", "Canopy.x", "doy", "hod","year", 
                        "Ca", "tair", "par", "leafArea", "FluxCO2", "NfluxCO2","FluxH2O", "H2O_flux_normalized",
                        "vpd", "wind", "pressure", "T_treatment", "Water_treatment", "Tair", "VPD")]
    
    ch12.o <- ch12.m[,c("DateTime", "Chamber", "Canopy.x", "doy", "hod","year", 
                        "Ca", "tair", "par", "leafArea", "FluxCO2", "NfluxCO2","FluxH2O", "H2O_flux_normalized",
                        "vpd", "wind", "pressure", "T_treatment", "Water_treatment", "Tair", "VPD")]
    
    
    colnames(ch01.o) <- c("DateTime", "chamber", "Canopy", "doy", "hod","year", 
                          "Ca", "tair.amb", "par", "leafArea", "FluxCO2", "NfluxCO2", "FluxH2O", "NfluxH2O",
                          "vpd.amb", "wind", "pressure", "T_treatment", "Water_treatment", "tair", "vpd")
    
    colnames(ch02.o) <- c("DateTime", "chamber", "Canopy", "doy", "hod","year", 
                          "Ca", "tair.amb", "par", "leafArea", "FluxCO2", "NfluxCO2","FluxH2O", "NfluxH2O",
                          "vpd.amb", "wind", "pressure", "T_treatment", "Water_treatment", "tair", "vpd")
    
    colnames(ch03.o) <- c("DateTime", "chamber", "Canopy", "doy", "hod","year", 
                          "Ca", "tair.amb", "par", "leafArea", "FluxCO2", "NfluxCO2","FluxH2O", "NfluxH2O",
                          "vpd.amb", "wind", "pressure", "T_treatment", "Water_treatment", "tair", "vpd")
    
    colnames(ch04.o) <- c("DateTime", "chamber", "Canopy", "doy", "hod","year", 
                          "Ca", "tair.amb", "par", "leafArea", "FluxCO2", "NfluxCO2","FluxH2O", "NfluxH2O",
                          "vpd.amb", "wind", "pressure", "T_treatment", "Water_treatment", "tair", "vpd")
    
    colnames(ch07.o) <- c("DateTime", "chamber", "Canopy", "doy", "hod","year", 
                          "Ca", "tair.amb", "par", "leafArea", "FluxCO2", "NfluxCO2","FluxH2O", "NfluxH2O",
                          "vpd.amb", "wind", "pressure", "T_treatment", "Water_treatment", "tair", "vpd")
    
    colnames(ch08.o) <- c("DateTime", "chamber", "Canopy", "doy", "hod","year", 
                          "Ca", "tair.amb", "par", "leafArea", "FluxCO2", "NfluxCO2","FluxH2O", "NfluxH2O",
                          "vpd.amb", "wind", "pressure", "T_treatment", "Water_treatment", "tair", "vpd")
    
    colnames(ch11.o) <- c("DateTime", "chamber", "Canopy", "doy", "hod","year", 
                          "Ca", "tair.amb", "par", "leafArea", "FluxCO2", "NfluxCO2","FluxH2O", "NfluxH2O",
                          "vpd.amb", "wind", "pressure", "T_treatment", "Water_treatment", "tair", "vpd")
    
    colnames(ch12.o) <- c("DateTime", "chamber", "Canopy", "doy", "hod","year", 
                          "Ca", "tair.amb", "par", "leafArea", "FluxCO2", "NfluxCO2","FluxH2O", "NfluxH2O",
                          "vpd.amb", "wind", "pressure", "T_treatment", "Water_treatment", "tair", "vpd")
    
    
    
    write.csv(ch01.o, "/Users/mingkaijiang/Documents/Research/Projects/WCT1_CO2_drawdown/Two_leaf_model/met_data/met_drawdownperiod_ch01.csv",
              row.names=F)
    write.csv(ch02.o, "/Users/mingkaijiang/Documents/Research/Projects/WCT1_CO2_drawdown/Two_leaf_model/met_data/met_drawdownperiod_ch02.csv",
              row.names=F)
    write.csv(ch03.o, "/Users/mingkaijiang/Documents/Research/Projects/WCT1_CO2_drawdown/Two_leaf_model/met_data/met_drawdownperiod_ch03.csv",
              row.names=F)
    write.csv(ch04.o, "/Users/mingkaijiang/Documents/Research/Projects/WCT1_CO2_drawdown/Two_leaf_model/met_data/met_drawdownperiod_ch04.csv",
              row.names=F)
    write.csv(ch07.o, "/Users/mingkaijiang/Documents/Research/Projects/WCT1_CO2_drawdown/Two_leaf_model/met_data/met_drawdownperiod_ch07.csv",
              row.names=F)
    write.csv(ch08.o, "/Users/mingkaijiang/Documents/Research/Projects/WCT1_CO2_drawdown/Two_leaf_model/met_data/met_drawdownperiod_ch08.csv",
              row.names=F)
    write.csv(ch11.o, "/Users/mingkaijiang/Documents/Research/Projects/WCT1_CO2_drawdown/Two_leaf_model/met_data/met_drawdownperiod_ch11.csv",
              row.names=F)
    write.csv(ch12.o, "/Users/mingkaijiang/Documents/Research/Projects/WCT1_CO2_drawdown/Two_leaf_model/met_data/met_drawdownperiod_ch12.csv",
              row.names=F)
   
}
