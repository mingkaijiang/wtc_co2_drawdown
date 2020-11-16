process_MAAT_output <- function() {
    
    ### this is code to process MAAT output and generate figures
    ### currently MAAT outputs single bigleaf and multi-layer model results
    
    ### read input
    aDF <- read.csv("data/MAAT/out_aco2co2.csv")
    eDF <- read.csv("data/MAAT/out_eco2co2.csv")
    
    aDF$CO2_treatment <- "aCO2"
    eDF$CO2_treatment <- "eCO2"
    
    myDF <- rbind(aDF, eDF)
    
    ### split according to big model and multi-layer model
    bigDF <- myDF[myDF$canopy.sys=="f_sys_bigleaf_s1992",]
    mulDF <- myDF[myDF$canopy.sys=="f_sys_multilayer",]
    
    ### data structure:
    ### Each model simulation set contains A-Ca responses under 
    ### real LAI (6.2 and 5) and 
    ### a list of LAI (1 - 10),
    ### and long-term CO2 treatment (aCO2 and eCO2)
    
    
    
    
    ### plot basic check
    p1 <- ggplot(aDF) +
        geom_point(aes(x=canopy.par, y=A, 
                       fill=as.character(canopy.ca_conc), 
                       pch=as.factor(canopy.lai)))+
        scale_fill_manual(values=c("red", "black"))+
        scale_shape_manual(values=c(1,19))
    
    plot(p1)
    
    
    ### check delta A / A400 ratio at all light levels
    aDF1 <- subset(aDF, canopy.ca_conc == 400)
    aDF2 <- subset(aDF, canopy.ca_conc == 600)
    
    newaDF <- merge(aDF1, aDF2, by=c("canopy.par", "canopy.lai"), all=T)
    newaDF$Aratio <- with(newaDF, (A.y - A.x)/A.x)
    
    plotDF <- subset(newaDF, Aratio > 0)
    
    ### plot
    p1 <- ggplot(plotDF) +
        geom_point(aes(x=canopy.par, y=Aratio, 
                       fill=as.character(canopy.lai)), pch = 21)+
        scale_fill_manual(breaks = c("5.01", "6.59"), 
                          values=c("red", "black"))
    
    plot(p1)
    
    
    ### read in A-Ca curve
    myDF1 <- read.csv("data/MAAT/out_aco2co2.csv")

    ### plot
    p1 <- ggplot(myDF1) +
        geom_point(aes(x=canopy.ca_conc, y=A/canopy.lai, 
                       fill=as.character(canopy.lai)), pch = 21)+
        scale_fill_manual(breaks = c("5.01", "6.59"), 
                          values=c("red", "black"))
    
    plot(p1)
    
    
    ### investigate canopy fraction parameter
    myDF1 <- read.csv("data/MAAT/out_laicut.csv")
    
    ### plot
    p1 <- ggplot(myDF1) +
        geom_point(aes(x=canopy.ca_conc, y=A, 
                       fill=as.character(canopy.can_lai_upper)), pch = 21)
    
    plot(p1)
    
    
}