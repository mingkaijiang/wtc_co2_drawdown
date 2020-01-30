canopy_data_per_second_check_and_plot <- function(myDF2){
    
    ### plot CO2 flux over CO2 concentration
    p1 <- ggplot(myDF2, aes(vCo2))+
        geom_point(aes(y=co2_flux, shape=factor(canopy), color=factor(chamber)), size=2.0)+
        labs(x=expression(paste(CO[2], " concentration (ppm)")),
             y=expression(paste(CO[2], " flux (ppm ", CO[2], " ", min^-1, ")")))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="right")+
        scale_colour_manual(name="Chamber", values = c("1" = "brown", "2" = "red", "3" = "pink",
                                                       "4" = "orange", "7" = "yellow", "8" = "green",
                                                       "11" = "cyan", "12" = "blue"))+
        scale_shape_manual(name="Canopy", values = c("12345" = 16, "345" = 17, "45" = 15),
                           labels=c("full", "top + middle", "top"))
    
    
    ### plot CO2 flux over time elapsed
    p2 <- ggplot(myDF2, aes(time_elapsed))+
        geom_point(aes(y=co2_flux, shape=factor(canopy), color=factor(chamber)), size=2.0)+
        labs(x="Time Elapsed (s)", y=expression(paste(CO[2], " flux (ppm ", CO[2], " ", min^-1, ")")))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="right")+
        scale_colour_manual(name="Chamber", values = c("1" = "brown", "2" = "red", "3" = "pink",
                                                       "4" = "orange", "7" = "yellow", "8" = "green",
                                                       "11" = "cyan", "12" = "blue"))+
        scale_shape_manual(name="Canopy", values = c("12345" = 16, "345" = 17, "45" = 15),
                           labels=c("full", "top + middle", "top"))
    
    
    ### chamber 1 example
    p3 <- ggplot(myDF2[myDF2$chamber==1,], aes(x=time_elapsed, y=vCo2, color=factor(canopy)))+
        geom_point(size=2.0)+
        geom_smooth(se=TRUE, method="gam", formula = y~s(x))+
        labs(x="Time", y=expression(paste(CO[2], " concentration (ppm)")))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="right")+
        scale_colour_manual(name="Canopy", values = c("12345" = "red", "345" = "green",
                                                      "45" = "blue"),
                            labels=c("full", "top + middle", "top"))+
        ggtitle("Chamber 1 example")
    
    ### chamber 2 example
    p4 <- ggplot(myDF2[myDF2$chamber==2,], aes(x=time_elapsed, y=vCo2, color=factor(canopy)))+
        geom_point(size=2.0)+
        geom_smooth(se=TRUE, method="gam", formula = y~s(x))+
        labs(x="Time", y=expression(paste(CO[2], " concentration (ppm)")))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="right")+
        scale_colour_manual(name="Canopy", values = c("12345" = "red", "345" = "green",
                                                      "45" = "blue"),
                            labels=c("full", "top + middle", "top"))+
        ggtitle("Chamber 2 example")
    
    ### chamber 3 example
    p5 <- ggplot(myDF2[myDF2$chamber==3,], aes(x=time_elapsed, y=vCo2, color=factor(canopy)))+
        geom_point(size=2.0)+
        geom_smooth(se=TRUE, method="gam", formula = y~s(x))+
        labs(x="Time", y=expression(paste(CO[2], " concentration (ppm)")))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="right")+
        scale_colour_manual(name="Canopy", values = c("12345" = "red", "345" = "green",
                                                      "45" = "blue"),
                            labels=c("full", "top + middle", "top"))+
        ggtitle("Chamber 3 example")
    
    
    ### chamber 4 example
    p6 <- ggplot(myDF2[myDF2$chamber==4,], aes(x=time_elapsed, y=vCo2, color=factor(canopy)))+
        geom_point(size=2.0)+
        geom_smooth(se=TRUE, method="gam", formula = y~s(x))+
        labs(x="Time", y=expression(paste(CO[2], " concentration (ppm)")))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="right")+
        scale_colour_manual(name="Canopy", values = c("12345" = "red", "345" = "green",
                                                      "45" = "blue"),
                            labels=c("full", "top + middle", "top"))+
        ggtitle("Chamber 4 example")
    
    ### chamber 7 example
    p7 <- ggplot(myDF2[myDF2$chamber==7,], aes(x=time_elapsed, y=vCo2, color=factor(canopy)))+
        geom_point(size=2.0)+
        geom_smooth(se=TRUE, method="gam", formula = y~s(x))+
        labs(x="Time", y=expression(paste(CO[2], " concentration (ppm)")))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="right")+
        scale_colour_manual(name="Canopy", values = c("12345" = "red", "345" = "green",
                                                      "45" = "blue"),
                            labels=c("full", "top + middle", "top"))+
        ggtitle("Chamber 7 example")
    
    ### chamber 8 example
    p8 <- ggplot(myDF2[myDF2$chamber==8,], aes(x=time_elapsed, y=vCo2, color=factor(canopy)))+
        geom_point(size=2.0)+
        geom_smooth(se=TRUE, method="gam", formula = y~s(x))+
        labs(x="Time", y=expression(paste(CO[2], " concentration (ppm)")))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="right")+
        scale_colour_manual(name="Canopy", values = c("12345" = "red", "345" = "green",
                                                      "45" = "blue"),
                            labels=c("full", "top + middle", "top"))+
        ggtitle("Chamber 8 example")
    
    ### chamber 11 example
    p9 <- ggplot(myDF2[myDF2$chamber==11,], aes(x=time_elapsed, y=vCo2, color=factor(canopy)))+
        geom_point(size=2.0)+
        geom_smooth(se=TRUE, method="gam", formula = y~s(x))+
        labs(x="Time", y=expression(paste(CO[2], " concentration (ppm)")))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="right")+
        scale_colour_manual(name="Canopy", values = c("12345" = "red", "345" = "green",
                                                      "45" = "blue"),
                            labels=c("full", "top + middle", "top"))+
        ggtitle("Chamber 11 example")
    
    ### chamber 12 example
    p10 <- ggplot(myDF2[myDF2$chamber==12,], aes(x=time_elapsed, y=vCo2, color=factor(canopy)))+
        geom_point(size=2.0)+
        geom_smooth(se=TRUE, method="gam", formula = y~s(x))+
        labs(x="Time", y=expression(paste(CO[2], " concentration (ppm)")))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="right")+
        scale_colour_manual(name="Canopy", values = c("12345" = "red", "345" = "green",
                                                      "45" = "blue"),
                            labels=c("full", "top + middle", "top"))+
        ggtitle("Chamber 12 example")
    
    
    ### output
    pdf("output/overall_data_fluxes.pdf", width=12, height=6)
    plot(p1)
    plot(p2)
    plot(p3)
    plot(p4)
    plot(p5)
    plot(p6)
    plot(p7)
    plot(p8)
    plot(p9)
    plot(p10)
    dev.off()
}