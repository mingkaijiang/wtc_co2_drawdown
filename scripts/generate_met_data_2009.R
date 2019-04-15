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
    subDF2 <- subset(subDF, minute == "0")
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
    myDF$time <- paste0(myDF$time1, myDF$time3)
    
    myDF$datetime <- as.POSIXct(paste(myDF$date, myDF$time), format="%Y-%m-%d %H:%M:%S")
    myDF$time <- strftime(myDF$datetime, format="%H:%M:%S")
    myDF$time <- as.POSIXct(myDF$time, format="%H:%M:%S")
    
    myDF <- myDF[,!(colnames(myDF)%in% c("time1", "time2", "time3"))]
    myDF <- myDF[myDF$Chamber%in%c(1,2,3,4,5,6,7,8,9,10,11,12),]  
    
    ### calculate hourly met data
    myDF$time1 <- sub(".+? ", "", myDF$datetime)
    myDF$hod <- as.numeric(sub(":.*", "", myDF$time1))
    
    hDF <- summaryBy(vCo2+vT+Tair+DPLicorCh+PARi+cmarea+corrflux+ncorrflux~Chamber+Canopy+date+hod,
                     data=myDF, FUN=mean, na.rm=T, keep.names=T)
    hDF <- hDF[complete.cases(hDF$cmarea),]
    hDF$time <- paste0(hDF$hod, ":00:00")
    hDF$DateTime <- as.POSIXct(paste(hDF$date, hDF$time), format="%Y-%m-%d %H:%M:%S")
    
    
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
    ch01.o <- ch01.m[,c("DateTime", "Chamber", "Canopy", "doy", "hod.y",
                        "vCo2", "vT", "PARi", "cmarea", "corrflux",
                        "vpd", "wind", "pressure")]
    ch02.o <- ch02.m[,c("DateTime", "Chamber", "Canopy", "doy", "hod.y",
                        "vCo2", "vT", "PARi", "cmarea", "corrflux",
                        "vpd", "wind", "pressure")]
    ch03.o <- ch03.m[,c("DateTime", "Chamber", "Canopy", "doy", "hod.y",
                        "vCo2", "vT", "PARi", "cmarea", "corrflux",
                        "vpd", "wind", "pressure")]    
    ch04.o <- ch04.m[,c("DateTime", "Chamber", "Canopy", "doy", "hod.y",
                        "vCo2", "vT", "PARi", "cmarea", "corrflux",
                        "vpd", "wind", "pressure")]
    ch07.o <- ch07.m[,c("DateTime", "Chamber", "Canopy", "doy", "hod.y",
                        "vCo2", "vT", "PARi", "cmarea", "corrflux",
                        "vpd", "wind", "pressure")]
    ch08.o <- ch08.m[,c("DateTime", "Chamber", "Canopy", "doy", "hod.y",
                        "vCo2", "vT", "PARi", "cmarea", "corrflux",
                        "vpd", "wind", "pressure")]
    ch11.o <- ch11.m[,c("DateTime", "Chamber", "Canopy", "doy", "hod.y",
                        "vCo2", "vT", "PARi", "cmarea", "corrflux",
                        "vpd", "wind", "pressure")]
    ch12.o <- ch12.m[,c("DateTime", "Chamber", "Canopy", "doy", "hod.y",
                        "vCo2", "vT", "PARi", "cmarea", "corrflux",
                        "vpd", "wind", "pressure")]
    
    ## reassign names
    colnames(ch01.o) <- c("DateTime", "chamber", "Canopy", "doy", "hod",
                          "Ca", "tair", "par", "leafArea", "FluxCO2",
                          "vpd", "wind", "pressure")
    colnames(ch02.o) <- c("DateTime", "chamber", "Canopy", "doy", "hod",
                          "Ca", "tair", "par", "leafArea", "FluxCO2",
                          "vpd", "wind", "pressure")
    colnames(ch03.o) <- c("DateTime", "chamber", "Canopy", "doy", "hod",
                          "Ca", "tair", "par", "leafArea", "FluxCO2",
                          "vpd", "wind", "pressure")
    colnames(ch04.o) <- c("DateTime", "chamber", "Canopy", "doy", "hod",
                          "Ca", "tair", "par", "leafArea", "FluxCO2",
                          "vpd", "wind", "pressure")
    colnames(ch07.o) <- c("DateTime", "chamber", "Canopy", "doy", "hod",
                          "Ca", "tair", "par", "leafArea", "FluxCO2",
                          "vpd", "wind", "pressure")
    colnames(ch08.o) <- c("DateTime", "chamber", "Canopy", "doy", "hod",
                          "Ca", "tair", "par", "leafArea", "FluxCO2",
                          "vpd", "wind", "pressure")
    colnames(ch11.o) <- c("DateTime", "chamber", "Canopy", "doy", "hod",
                          "Ca", "tair", "par", "leafArea", "FluxCO2",
                          "vpd", "wind", "pressure")
    colnames(ch12.o) <- c("DateTime", "chamber", "Canopy", "doy", "hod",
                          "Ca", "tair", "par", "leafArea", "FluxCO2",
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
    
    ### what's wrong with the met files
    ##  1. year and doy in output file are all 0,
    ##     need to check DateTime format.
    ##  2. output unit are GPP in g m-2 d-1, should be h-1 at least.
    ##  3. check whether it is correct to just run for several hours of drawdown period.
    ##     we are comparing canopy drawdown photosynthesis rate, so should be,
    ##     but then there is the problem if different leaf area,
    ##     and even more important, the problem of reducing rate of photo as time goes,
    ##     because Ca is dropping. This is not reflected in modelled result,
    ##     mainly because Ca is fixed in the two-leaf model code.
    ##  4. check which met files to use for the canopy drawdown period.
    ##  5. what to do with the unfilled parameters.
    ##  6. what about T-treatment and water-treatment. Do they have any effect in the code?
    ##  
    
    
}
