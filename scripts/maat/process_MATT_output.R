process_MATT_output <- function() {
    
    ## this is code to process MATT output and generate figures
    
    ### read input
    aDF <- read.csv("data/MATT/out_aco2.csv")
    
    p1 <- ggplot(aDF) +
        geom_point(aes(x=canopy.par, y=A, 
                       fill=as.character(canopy.ca_conc), 
                       pch=as.factor(canopy.lai)))+
        scale_fill_manual(values=c("red", "black"))+
        scale_shape_manual(values=c(1,19))
    
    plot(p1)
    
    eDF <- read.csv("data/MATT/out_eco2.csv")
    
    p2 <- ggplot(eDF) +
        geom_point(aes(x=canopy.par, y=A, 
                       fill=as.character(canopy.ca_conc), 
                       pch=as.factor(canopy.lai)))+
        scale_fill_manual(values=c("red", "black"))+
        scale_shape_manual(values=c(1,19))
    
    plot(p2)
    
    
    ### check r database
    
    myDF1 <- readRDS("data/MATT/out_aco2_var.RDS")
    
}