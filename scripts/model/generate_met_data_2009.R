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
    myDF$time <- paste0(myDF$time1, ":00")
    
    myDF$datetime <- as.POSIXct(paste(myDF$date, myDF$time), format="%Y-%m-%d %H:%M:%S")
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
    
    ################################## add H2O flux #################################
    ########################  using mergeall.text file ###########################
    ##### read in raw data - this dataset does not have PAR 
    myDF2 <- read.csv("data/canopy_drawdown/mergeall.csv")
    
    myDF2$date <- sub(" .*", "", myDF2$datetime)
    myDF2$date <- as.Date(as.character(myDF2$date), format="%m/%d/%y")
    myDF2$time1 <- sub(".+? ", "", myDF2$datetime)
    myDF2$time <- paste0(myDF2$time1, ":00")
    
    myDF2$datetime <- as.POSIXct(paste(myDF2$date, myDF2$time), format="%Y-%m-%d %H:%M:%S")
    myDF2$time <- strftime(myDF2$datetime, format="%H:%M:%S")
    myDF2$time <- as.POSIXct(myDF2$time, format="%H:%M:%S")
    
    myDF2 <- myDF2[,!(colnames(myDF2)%in% c("time1"))]
    
    
    ### merge two datasets
    mgDF <- merge(myDF, myDF2, by.x=c("Chamber", "Canopy", "datetime"),
                  by.y=c("chamber", "canopy", "datetime"),all=T)
    
    mgDF <- mgDF[,c("Chamber", "Canopy", "datetime", "date.x", "time.x", 
                    "WTC_CO2", "WTC_T", "WTC_dew_point", "WTC_PAR", 
                    "Tair.x", "Leaf_area", "Leak_coef", "Leak", 
                    "CO2_flux", "Norm_CO2_flux", "Corr_CO2_flux", 
                    "Norm_corr_CO2_flux", "CondWater")]
    
    colnames(mgDF) <- c("Chamber", "Canopy", "datetime", "date", "time", 
                        "WTC_CO2", "WTC_T", "WTC_dew_point", "WTC_PAR", 
                        "Tair", "Leaf_area", "Leak_coef", "Leak", 
                        "CO2_flux", "Norm_CO2_flux", "Corr_CO2_flux", 
                        "Norm_corr_CO2_flux", "FluxH2O")
    
    ### add volumn information - does not include the effect of cone top
    ## note that chamber 11 = 49800 L
    ## all else = 52800 L
    ## output unit m3
    mgDF$WTC_volume_m3 <- 52800 / 1000
    mgDF$WTC_volume_m3[mgDF$Chamber == "11"] <- 49800 / 1000
    
    ### select 
    myDF <- mgDF[mgDF$Chamber%in%c(1,2,3,4,5,6,7,8,9,10,11,12),]  
    hDF <- myDF[complete.cases(myDF$Leaf_area),]
    hDF$DateTime <- hDF$datetime
    
    
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
                        "WTC_CO2", "WTC_T", "WTC_PAR", "Leaf_area", "CO2_flux", "Norm_CO2_flux",
                        "FluxH2O", "vpd", "wind", "pressure")]
    ch02.o <- ch02.m[,c("DateTime", "Chamber", "Canopy", "doy", "hod",
                        "WTC_CO2", "WTC_T", "WTC_PAR", "Leaf_area", "CO2_flux", "Norm_CO2_flux",
                        "FluxH2O", "vpd", "wind", "pressure")]
    ch03.o <- ch03.m[,c("DateTime", "Chamber", "Canopy", "doy", "hod",
                        "WTC_CO2", "WTC_T", "WTC_PAR", "Leaf_area", "CO2_flux", "Norm_CO2_flux",
                        "FluxH2O", "vpd", "wind", "pressure")]    
    ch04.o <- ch04.m[,c("DateTime", "Chamber", "Canopy", "doy", "hod",
                        "WTC_CO2", "WTC_T", "WTC_PAR", "Leaf_area", "CO2_flux", "Norm_CO2_flux",
                        "FluxH2O", "vpd", "wind", "pressure")]
    ch07.o <- ch07.m[,c("DateTime", "Chamber", "Canopy", "doy", "hod",
                        "WTC_CO2", "WTC_T", "WTC_PAR", "Leaf_area", "CO2_flux", "Norm_CO2_flux",
                        "FluxH2O", "vpd", "wind", "pressure")]
    ch08.o <- ch08.m[,c("DateTime", "Chamber", "Canopy", "doy", "hod",
                        "WTC_CO2", "WTC_T", "WTC_PAR", "Leaf_area", "CO2_flux", "Norm_CO2_flux",
                        "FluxH2O", "vpd", "wind", "pressure")]
    ch11.o <- ch11.m[,c("DateTime", "Chamber", "Canopy", "doy", "hod",
                        "WTC_CO2", "WTC_T", "WTC_PAR", "Leaf_area", "CO2_flux", "Norm_CO2_flux",
                        "FluxH2O", "vpd", "wind", "pressure")]
    ch12.o <- ch12.m[,c("DateTime", "Chamber", "Canopy", "doy", "hod",
                        "WTC_CO2", "WTC_T", "WTC_PAR", "Leaf_area", "CO2_flux", "Norm_CO2_flux",
                        "FluxH2O", "vpd", "wind", "pressure")]
    
    ## reassign names
    colnames(ch01.o) <- c("DateTime", "chamber", "Canopy", "doy", "hod",
                          "Ca", "tair", "par", "leafArea", "FluxCO2", "NfluxCO2","FluxH2O", 
                          "vpd", "wind", "pressure")
    colnames(ch02.o) <- c("DateTime", "chamber", "Canopy", "doy", "hod",
                          "Ca", "tair", "par", "leafArea", "FluxCO2", "NfluxCO2","FluxH2O", 
                          "vpd", "wind", "pressure")
    colnames(ch03.o) <- c("DateTime", "chamber", "Canopy", "doy", "hod",
                          "Ca", "tair", "par", "leafArea", "FluxCO2", "NfluxCO2","FluxH2O", 
                          "vpd", "wind", "pressure")
    colnames(ch04.o) <- c("DateTime", "chamber", "Canopy", "doy", "hod",
                          "Ca", "tair", "par", "leafArea", "FluxCO2", "NfluxCO2","FluxH2O", 
                          "vpd", "wind", "pressure")
    colnames(ch07.o) <- c("DateTime", "chamber", "Canopy", "doy", "hod",
                          "Ca", "tair", "par", "leafArea", "FluxCO2", "NfluxCO2","FluxH2O", 
                          "vpd", "wind", "pressure")
    colnames(ch08.o) <- c("DateTime", "chamber", "Canopy", "doy", "hod",
                          "Ca", "tair", "par", "leafArea", "FluxCO2", "NfluxCO2","FluxH2O", 
                          "vpd", "wind", "pressure")
    colnames(ch11.o) <- c("DateTime", "chamber", "Canopy", "doy", "hod",
                          "Ca", "tair", "par", "leafArea", "FluxCO2", "NfluxCO2","FluxH2O", 
                          "vpd", "wind", "pressure")
    colnames(ch12.o) <- c("DateTime", "chamber", "Canopy", "doy", "hod",
                          "Ca", "tair", "par", "leafArea", "FluxCO2", "NfluxCO2","FluxH2O", 
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
    
    
    ch01.o$NfluxH2O <- ch01.o$FluxH2O / ch01.o$leafArea
    ch02.o$NfluxH2O <- ch02.o$FluxH2O / ch02.o$leafArea
    ch03.o$NfluxH2O <- ch03.o$FluxH2O / ch03.o$leafArea
    ch04.o$NfluxH2O <- ch04.o$FluxH2O / ch04.o$leafArea
    ch07.o$NfluxH2O <- ch07.o$FluxH2O / ch07.o$leafArea
    ch08.o$NfluxH2O <- ch08.o$FluxH2O / ch08.o$leafArea
    ch11.o$NfluxH2O <- ch11.o$FluxH2O / ch11.o$leafArea
    ch12.o$NfluxH2O <- ch12.o$FluxH2O / ch12.o$leafArea
    
    
    ## select output variable
    ch01.o <- ch01.o[,c("DateTime", "chamber", "Canopy", "doy", "hod", "year", 
                        "Ca", "tair", "par", "leafArea", "FluxCO2", "NfluxCO2", "FluxH2O", "NfluxH2O",
                        "vpd", "wind", "pressure", "T_treatment", "Water_treatment")]
    
    ch02.o <- ch02.o[,c("DateTime", "chamber", "Canopy", "doy", "hod","year", 
                        "Ca", "tair", "par", "leafArea", "FluxCO2", "NfluxCO2","FluxH2O", "NfluxH2O",
                        "vpd", "wind", "pressure", "T_treatment", "Water_treatment")]
    
    ch03.o <- ch03.o[,c("DateTime", "chamber", "Canopy", "doy", "hod","year", 
                        "Ca", "tair", "par", "leafArea", "FluxCO2", "NfluxCO2","FluxH2O", "NfluxH2O",
                        "vpd", "wind", "pressure", "T_treatment", "Water_treatment")]
    
    ch04.o <- ch04.o[,c("DateTime", "chamber", "Canopy", "doy", "hod","year", 
                        "Ca", "tair", "par", "leafArea", "FluxCO2", "NfluxCO2","FluxH2O", "NfluxH2O",
                        "vpd", "wind", "pressure", "T_treatment", "Water_treatment")]
    
    ch07.o <- ch07.o[,c("DateTime", "chamber", "Canopy", "doy", "hod","year", 
                        "Ca", "tair", "par", "leafArea", "FluxCO2", "NfluxCO2","FluxH2O", "NfluxH2O",
                        "vpd", "wind", "pressure", "T_treatment", "Water_treatment")]
    
    ch08.o <- ch08.o[,c("DateTime", "chamber", "Canopy", "doy", "hod","year", 
                        "Ca", "tair", "par", "leafArea", "FluxCO2", "NfluxCO2","FluxH2O", "NfluxH2O",
                        "vpd", "wind", "pressure", "T_treatment", "Water_treatment")]
    
    ch11.o <- ch11.o[,c("DateTime", "chamber", "Canopy", "doy", "hod","year", 
                        "Ca", "tair", "par", "leafArea", "FluxCO2", "NfluxCO2","FluxH2O", "NfluxH2O",
                        "vpd", "wind", "pressure", "T_treatment", "Water_treatment")]
    
    ch12.o <- ch12.o[,c("DateTime", "chamber", "Canopy", "doy", "hod","year", 
                        "Ca", "tair", "par", "leafArea", "FluxCO2", "NfluxCO2","FluxH2O", "NfluxH2O",
                        "vpd", "wind", "pressure", "T_treatment", "Water_treatment")]
    
    
    
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
