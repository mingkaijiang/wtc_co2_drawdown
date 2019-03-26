canopy_data_check_and_plot <- function(myDF) {
    
    
    ### plot overall data - CO2 concentration over time
    p1 <- ggplot(myDF, aes(datetime))+
        geom_point(aes(y=CO2Local, shape=factor(canopy), color=factor(chamber)), size=2.0)+
        labs(x="Date Time", y=expression(paste(CO[2], " concentration (ppm)")))+
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
                           labels=c("full", "top + middle", "top"))+
        scale_x_datetime(labels = date_format("%b %d %H:%M:%S"))
    
    ### CO2 against leaf area
    p2 <- ggplot(myDF, aes(SumOfarea_fully_exp))+
        geom_point(aes(y=CO2Local, shape=factor(canopy), color=factor(chamber)), size=2.0)+
        labs(x="Sum of leaf area", y=expression(paste(CO[2], " concentration (ppm)")))+
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
    
    ### plot all responses onto the same day
    p3 <- ggplot(myDF, aes(time))+
        geom_point(aes(y=CO2Local, shape=factor(canopy), color=factor(chamber)), size=2.0)+
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
        scale_colour_manual(name="Chamber", values = c("1" = "brown", "2" = "red", "3" = "pink",
                                                       "4" = "orange", "7" = "yellow", "8" = "green",
                                                       "11" = "cyan", "12" = "blue"))+
        scale_shape_manual(name="Canopy", values = c("12345" = 16, "345" = 17, "45" = 15),
                           labels=c("full", "top + middle", "top"))+
        scale_x_datetime(limits = as.POSIXct(c('2019-01-23 07:00:00','2019-01-23 16:59:59')))
    
    
    ### CO2 concentration agains slope
    p4 <- ggplot(myDF, aes(CO2Local))+
        geom_point(aes(y=slope, shape=factor(canopy), color=factor(chamber)), size=2.0)+
        labs(x=expression(paste(CO[2], " concentration (ppm)")),
             y=expression(paste(Delta, CO[2], " slope")))+
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
    
    ### output
    pdf("output/overall_data.pdf", width=12, height=6)
    plot(p1)
    plot(p2)
    plot(p3)
    plot(p4)
    dev.off()
    
}