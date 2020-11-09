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
    
    ### plot
    p1 <- ggplot(plotDF) +
        geom_point(aes(Ca, leaf60, pch=model),color="red3",
                   size=1)+
        #geom_smooth(aes(Ca, leaf60),color="red3",
        #            formula = 'y ~ log(x)', method = 'glm')+
        geom_point(aes(Ca, canopy60, pch=model), color="blue",
                   size=1)+
        #geom_smooth(aes(Ca, canopy60),color="blue2",
        #            formula = 'y ~ log(x)', method = 'glm')+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="right",
              legend.text.align=0)+
        xlab(expression(paste(C[a]* " (umol ", " ", mol^-1, ")")))+
        ylab(expression(paste("A (umol "* CO[2], " ", m^-1, s^-1, ")")))+
        scale_fill_discrete(name="Method")+
        xlim(0,1200)+
        ylim(0,30)+
        theme(legend.direction = "vertical", legend.box = "horizontal")+
        scale_color_manual(name="Method",
                           values=c("blue2", "red3"),
                           labels=c("Observed", "Modeled"))
    
    #plot(p1)
    
    pdf("output/conceptual_figure_Rogers_Figure3_reproduction.pdf", width=8, height=6)
    plot(p1)
    dev.off()
    
    
}