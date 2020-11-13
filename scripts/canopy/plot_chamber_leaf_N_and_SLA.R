plot_chamber_leaf_N_and_SLA <- function() {
    
    ### read input
    myDF <- read.csv("data/SLA/HFE preharvest leaf samples NCSLA.csv")
    
    ### subset
    myDF <- myDF[myDF$chamber%in%c("ch01", "ch02", "ch03", "ch04",
                                   "ch05", "ch06", "ch07", "ch08",
                                   "ch09", "ch10", "ch11", "ch12"),]
    
    ### assign treatment
    myDF$CO2_treatment <- "aCO2"
    myDF$CO2_treatment[myDF$chamber%in%c("ch02", "ch04", "ch06", "ch08",
                                         "ch10", "ch12")] <- "eCO2"
    
    myDF$Water_treatment <- "wet"
    myDF$Water_treatment[myDF$chamber%in%c("ch02", "ch05", "ch07", "ch09",
                                           "ch10", "ch12")] <- "dry"
    
    myDF <- myDF[myDF$Water_treatment=="wet",]
    myDF <- myDF[myDF$layer <=5, ]
    
    
    ### summarize
    sumDF <- summaryBy(leafarea+SLA+nperc+cperc~CO2_treatment+position+layer,
                       FUN=c(mean, se), data=myDF, keep.names=T, na.rm=T)
    
    ### split by CO2
    plotDF1 <- subset(sumDF, CO2_treatment == "aCO2")
    plotDF2 <- subset(sumDF, CO2_treatment == "eCO2")
    
    
    ### plot leaf area, aCO2
    p1 <- ggplot(data=plotDF1, 
                 aes(layer, leafarea.mean, group=position)) +
        geom_bar(stat = "identity", aes(alpha=position), 
                 position="dodge", fill="navyblue") +
        geom_errorbar(aes(x=layer, ymin=leafarea.mean-leafarea.se, 
                          ymax=leafarea.mean+leafarea.se),
                      position=position_dodge(0.9), 
                      width=0.2) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_blank(),
              axis.title.x=element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.box = 'vertical',
              legend.box.just = 'left',
              legend.position = "none")+
        ylab(expression(paste("Leaf area (" * cm^2 * ")")))+
        xlab("Canopy layer")+
        scale_alpha_manual(name="Position",
                           breaks=c("Inner", "Outer"),
                           labels=c("Inner", "Outer"),
                           values=c(0.5, 1))+
        guides(fill=FALSE)+
        ylim(0.0, 400)
    
    ### plot leaf area, eCO2
    p2 <- ggplot(data=plotDF2, 
                 aes(layer, leafarea.mean, group=position)) +
        geom_bar(stat = "identity", aes(alpha=position), 
                 position="dodge", fill="navyblue") +
        geom_errorbar(aes(x=layer, ymin=leafarea.mean-leafarea.se, 
                          ymax=leafarea.mean+leafarea.se),
                      position=position_dodge(0.9), 
                      width=0.2) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_blank(),
              axis.title.x=element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.box = 'vertical',
              legend.box.just = 'left',
              legend.position = "none")+
        ylab(expression(paste("Leaf area (" * cm^2 * ")")))+
        xlab("Canopy layer")+
        scale_alpha_manual(name="Position",
                           breaks=c("Inner", "Outer"),
                           labels=c("Inner", "Outer"),
                           values=c(0.5, 1))+
        guides(fill=FALSE)+
        ylim(0.0, 400)
    
    ### plot SLA, aCO2
    p3 <- ggplot(data=plotDF1, 
                 aes(layer, SLA.mean, group=position)) +
        geom_bar(stat = "identity", aes(alpha=position), 
                 position="dodge", fill="navyblue") +
        geom_errorbar(aes(x=layer, ymin=SLA.mean-SLA.se, 
                          ymax=SLA.mean+SLA.se),
                      position=position_dodge(0.9), 
                      width=0.2) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_blank(),
              axis.title.x=element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.box = 'vertical',
              legend.box.just = 'left',
              legend.position = "none")+
        ylab(expression(paste("Specific leaf area (cm " * g^1 * ")")))+
        xlab("Canopy layer")+
        scale_alpha_manual(name="Position",
                           breaks=c("Inner", "Outer"),
                           labels=c("Inner", "Outer"),
                           values=c(0.5, 1))+
        guides(fill=FALSE)+
        ylim(0.0, 140)
    
    
    ### plot SLA, eCO2
    p4 <- ggplot(data=plotDF2, 
                 aes(layer, SLA.mean, group=position)) +
        geom_bar(stat = "identity", aes(alpha=position), 
                 position="dodge", fill="navyblue") +
        geom_errorbar(aes(x=layer, ymin=SLA.mean-SLA.se, 
                          ymax=SLA.mean+SLA.se),
                      position=position_dodge(0.9), 
                      width=0.2) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_blank(),
              axis.title.x=element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.box = 'vertical',
              legend.box.just = 'left',
              legend.position = "none")+
        ylab(expression(paste("Specific leaf area (cm " * g^1 * ")")))+
        xlab("Canopy layer")+
        scale_alpha_manual(name="Position",
                           breaks=c("Inner", "Outer"),
                           labels=c("Inner", "Outer"),
                           values=c(0.5, 1))+
        guides(fill=FALSE)+
        ylim(0.0, 140)
    
    
    ### plot leaf N, aCO2
    p5 <- ggplot(data=plotDF1, 
                 aes(layer, nperc.mean, group=position)) +
        geom_bar(stat = "identity", aes(alpha=position), 
                 position="dodge", fill="navyblue") +
        geom_errorbar(aes(x=layer, ymin=nperc.mean-nperc.se, 
                          ymax=nperc.mean+nperc.se),
                      position=position_dodge(0.9), 
                      width=0.2) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.box = 'vertical',
              legend.box.just = 'left',
              legend.position = "none")+
        ylab(expression(paste("Leaf nitrogen (%)")))+
        xlab("Canopy layer")+
        scale_alpha_manual(name="Position",
                           breaks=c("Inner", "Outer"),
                           labels=c("Inner", "Outer"),
                           values=c(0.5, 1))+
        guides(fill=FALSE)+
        ylim(0.0, 2.2)
    
    
    ### plot leaf N, eCO2
    p6 <- ggplot(data=plotDF2, 
                 aes(layer, nperc.mean, group=position)) +
        geom_bar(stat = "identity", aes(alpha=position), 
                 position="dodge", fill="navyblue") +
        geom_errorbar(aes(x=layer, ymin=nperc.mean-nperc.se, 
                          ymax=nperc.mean+nperc.se),
                      position=position_dodge(0.9), 
                      width=0.2) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=14),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.box = 'vertical',
              legend.box.just = 'left',
              legend.position = "none")+
        ylab(expression(paste("Leaf nitrogen (%)")))+
        xlab("Canopy layer")+
        scale_alpha_manual(name="Position",
                           breaks=c("Inner", "Outer"),
                           labels=c("Inner", "Outer"),
                           values=c(0.5, 1))+
        guides(fill=FALSE)+
        ylim(0.0, 2.2)
    
    
    ### legend
    legend_shared <- get_legend(p1 + theme(legend.position="bottom",
                                           legend.box = 'vertical',
                                           legend.box.just = 'left'))
    
    combined_plots <- plot_grid(p1, p2, p3, p4, p5, p6,
                                labels=c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)"),
                                ncol=2, align="vh", axis = "l",
                                label_x=0.25, label_y=0.95, #rel_widths = c(1.2, 1),
                                label_size = 14)
    
    
    ### save pdf
    pdf("output/canopy/canopy_leafN_SLA.pdf", width=6, height=8)
    plot_grid(combined_plots, legend_shared, ncol=1, rel_heights=c(1,0.1))
    dev.off()  
}
