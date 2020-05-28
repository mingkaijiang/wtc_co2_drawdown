biomass_data_processing <- function() {
    
    ### variable names:
    ### Wleaf: leaf biomass (wet)
    ### Wbrlt1: branch biomass (< 1cm)
    ### Wbrgt1: branch biomass (> 1cm)
    ### Wbuds: bud biomass
    ### Wdead: dead biomass
    ### Ws: stem biomass
    ### SLA: cm g-1
    ### LA: m2
    ### BA: cm2
    
    ### read in DF
    inDF <- read.csv("data/canopy_drawdown/HFE final harvest biomass by layer.csv")
    
    ### select chambers 1, 3, 4, 8, 11
    subDF <- inDF[inDF$chamber%in%c("ch01", "ch03", "ch04", "ch08", "ch11"),]
    
    ### add other
    subDF$Other <- with(subDF, Wbuds+Wdead)
    
    ### add branch together
    subDF$Branch <- with(subDF, Wbrlt1+Wbrgt1)
    
    ### change chamber names
    subDF$Chamber <- gsub("ch", "", subDF$chamber)
    subDF$Chamber <- as.numeric(subDF$Chamber)
    
    ### prepare subsets based on leaf layer
    subDF2 <- subset(subDF, layerno%in%c(3,4,5))
    subDF3 <- subset(subDF, layerno%in%c(4,5))
    
    ### merge layers
    outDF <- data.frame(rep(c(1,3,4,8,11), each=3), 
                        rep(c(12345, 345, 45), 5), 
                        NA, NA, NA, NA, NA)
    colnames(outDF) <- c("Chamber", "Position", "Leaf", "Stem",
                         "Branch", "SLA", "LA")
    
    for (i in unique(outDF$Chamber)) {
        
        ### position 12345
        outDF$Leaf[outDF$Chamber==i&outDF$Position=="12345"] <- sum(subDF$Wleaf[subDF$Chamber==i], na.rm=T)
        outDF$Stem[outDF$Chamber==i&outDF$Position=="12345"] <- sum(subDF$Ws[subDF$Chamber==i], na.rm=T)
        outDF$Branch[outDF$Chamber==i&outDF$Position=="12345"] <- sum(subDF$Branch[subDF$Chamber==i], na.rm=T)
        outDF$LA[outDF$Chamber==i&outDF$Position=="12345"] <- sum(subDF$LA[subDF$Chamber==i], na.rm=T)
        outDF$SLA[outDF$Chamber==i&outDF$Position=="12345"] <- mean(subDF$SLA[subDF$Chamber==i], na.rm=T)

        
        ### position 345
        outDF$Leaf[outDF$Chamber==i&outDF$Position=="345"] <- sum(subDF2$Wleaf[subDF2$Chamber==i], na.rm=T)
        outDF$Stem[outDF$Chamber==i&outDF$Position=="345"] <- sum(subDF2$Ws[subDF2$Chamber==i], na.rm=T)
        outDF$Branch[outDF$Chamber==i&outDF$Position=="345"] <- sum(subDF2$Branch[subDF2$Chamber==i], na.rm=T)
        outDF$LA[outDF$Chamber==i&outDF$Position=="345"] <- sum(subDF2$LA[subDF2$Chamber==i], na.rm=T)
        outDF$SLA[outDF$Chamber==i&outDF$Position=="345"] <- mean(subDF2$SLA[subDF2$Chamber==i], na.rm=T)
        
        
        ### position 45
        outDF$Leaf[outDF$Chamber==i&outDF$Position=="45"] <- sum(subDF3$Wleaf[subDF3$Chamber==i], na.rm=T)
        outDF$Stem[outDF$Chamber==i&outDF$Position=="45"] <- sum(subDF3$Ws[subDF3$Chamber==i], na.rm=T)
        outDF$Branch[outDF$Chamber==i&outDF$Position=="45"] <- sum(subDF3$Branch[subDF3$Chamber==i], na.rm=T)
        outDF$LA[outDF$Chamber==i&outDF$Position=="45"] <- sum(subDF3$LA[subDF3$Chamber==i], na.rm=T)
        outDF$SLA[outDF$Chamber==i&outDF$Position=="45"] <- mean(subDF3$SLA[subDF3$Chamber==i], na.rm=T)
        
    }
    
    ### summarize
    outDF$CO2_treatment <- "aCO2"
    outDF$CO2_treatment[outDF$Chamber%in%c(4,8)] <- "eCO2"
    
    return(outDF)
    
    
}