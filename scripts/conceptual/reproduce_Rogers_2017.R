reproduce_Rogers_2017 <- function() {
    
    ### read input
    myDF1 <- read.csv("data/concept/Rogers_Bethy.csv")
    myDF2 <- read.csv("data/concept/Rogers_CLM.csv")
    myDF3 <- read.csv("data/concept/Rogers_ED2.csv")
    myDF4 <- read.csv("data/concept/Rogers_GDAY.csv")
    myDF5 <- read.csv("data/concept/Rogers_OCN.csv")
    myDF6 <- read.csv("data/concept/Rogers_JULES.csv")
    myDF7 <- read.csv("data/concept/Rogers_JSBACH.csv")
    colnames(myDF1) <- colnames(myDF7) <- colnames(myDF5) <- colnames(myDF4) <- colnames(myDF2) <- c("Ca", "leaf45", "leaf60", "canopy45", "canopy60")
    colnames(myDF6) <- c("Ca", "canopy60", "canopy45", "leaf60", "leaf45")
    colnames(myDF3) <- c("Ca", "leaf45", "canopy45", "leaf60", "canopy60")
    
    myDF6 <- myDF6[,c("Ca", "leaf45", "leaf60", "canopy45", "canopy60")]
    myDF3 <- myDF3[,c("Ca", "leaf45", "leaf60", "canopy45", "canopy60")]
    
    myDF1$model <- "Bethy"
    myDF2$model <- "CLM"
    myDF3$model <- "ED2"
    myDF4$model <- "GDAY"
    myDF5$model <- "OCN"
    myDF6$model <- "JULES"
    myDF7$model <- "JSBACH"

    
    plotDF <- do.call("rbind", list(myDF1, myDF2, myDF3, myDF4,
                                    myDF5, myDF6, myDF7))
    
    ## normalize to per leaf area
    plotDF$canopy45 <- plotDF$canopy45/3
    plotDF$canopy60 <- plotDF$canopy60/3
    
    
    ### convert into long format
    plotDF1 <- plotDF[,c("Ca", "leaf60", "model")]
    plotDF2 <- plotDF[,c("Ca", "canopy60", "model")]
    
    colnames(plotDF1) <- colnames(plotDF2) <- c("Ca", "Anet", "Model")
    
    plotDF1$Scale <- "Leaf"
    plotDF2$Scale <- "Canopy"
    
    plotDF3 <- rbind(plotDF1, plotDF2)
    
    ### plot
    p1 <- ggplot(plotDF3) +
        geom_line(aes(Ca, Anet, col=Model, lty = Scale))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=8),
              legend.title=element_text(size=10),
              panel.grid.major=element_blank(),
              legend.position="right",
              legend.text.align=0)+
        xlab(expression(paste(C[a]* " (" * mu * "mol " * mol^-1 * ")")))+
        ylab(expression(paste(A[n] * " (" * mu * "mol "* CO[2] * " ", m^-2 * " " * s^-1, ")")))+
        xlim(200,1000)+
        ylim(0,25)+
        theme(legend.direction = "horizontal", legend.box = "horizontal",
              legend.position = c(0.5, 0.14))+
        scale_colour_colorblind(name="Model",
                                guide = guide_legend(nrow=3))+
        scale_linetype_manual(name="Scale",
                              values=c(2,1),
                              labels=c("Canopy", "Leaf"),
                              guide = guide_legend(nrow=2))
    
    #plot(p1)
    
    pdf("output/concept/Rogers_Figure3_reproduction.pdf", width=5, height=4)
    plot(p1)
    dev.off()
    
    
}
