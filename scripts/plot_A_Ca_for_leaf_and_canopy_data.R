plot_A_Ca_for_leaf_and_canopy_data <- function(cDF) {
    #### This script plots A-CA curve for leaf and canopy data on to the same plot
    #### and compare the shapes
    #### which can be related to Rogers et al. 2017 conceptual figure
    
    #### read in leaf-scale data
    lDF1 <- read.csv("data/ACi_curves/HFE_Aci_2008-2009.csv",stringsAsFactors=FALSE)
    lDF2  <- read.csv("data/ACi_curves/HFE_Aci_lowcanopy_2008-2009.csv",stringsAsFactors=FALSE)
    
    ### combine the datasets
    lDF <- rbind(lDF1, lDF2)
    
    ### a function to combine leaf and canopy data together at each chamber level
    leaf_canopy_combined <- function(lDF, cDF, ch.l, ch.c) {
        
        ch01.l <- subset(lDF, chamber==ch.l)
        ch01.c <- subset(cDF, Chamber==ch.c)
        
        ch01.l <- ch01.l[,c("Photo", "CO2S", "Height")]
        ch01.c <- ch01.c[,c("Norm_corr_CO2_flux", "WTC_CO2", "Canopy")]
        colnames(ch01.l) <- colnames(ch01.c) <- c("Photo", "Ca", "Position")
        ch01.l$Source <- "leaf"
        ch01.c$Source <- "canopy"
        
        outDF <- rbind(ch01.l, ch01.c)
        
        return(outDF)
    }
    
    ch01DF <- leaf_canopy_combined(lDF, cDF, 
                                   ch.l="ch01", ch.c="1")

    ch02DF <- leaf_canopy_combined(lDF, cDF, 
                                   ch.l="ch02", ch.c="2")
    
    ch03DF <- leaf_canopy_combined(lDF, cDF, 
                                   ch.l="ch03", ch.c="3")
    
    ch04DF <- leaf_canopy_combined(lDF, cDF, 
                                   ch.l="ch04", ch.c="4")
    
    ch07DF <- leaf_canopy_combined(lDF, cDF, 
                                   ch.l="ch07", ch.c="7")
    
    ch08DF <- leaf_canopy_combined(lDF, cDF, 
                                   ch.l="ch08", ch.c="8")
    
    ch11DF <- leaf_canopy_combined(lDF, cDF, 
                                   ch.l="ch11", ch.c="11")
    
    ch12DF <- leaf_canopy_combined(lDF, cDF,
                                   ch.l="ch12", ch.c="12")
    
    ## plot
    p1 <- ggplot() +
        geom_smooth(data=ch01DF, aes(Ca, Photo, group=ch01DF$Source,
                                     col=as.factor(ch01DF$Source)),
                    method = "lm", formula = y ~ splines::bs(x, 3))+
        geom_point(data=ch01DF, aes(Ca, Photo, 
                                    fill=as.factor(ch01DF$Position), 
                                    pch = as.factor(ch01DF$Source)))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'vertical',
              legend.box.just = 'left')+
        xlab(expression(paste(C[a]* " (umol ", m^-2, s^-1, ")")))+
        ylab(expression(paste(A* " (umol "* CO[2], " ", m^-2, s^-1, ")")))+
        scale_fill_manual(name="Position",
                           limits=c("12345", "345", "45", "up", "low"),
                           values=c("blue2", "red3", "purple", "orange", "green"),
                           labels=c("whole", "middle+bottom", "bottom", "up", "low"))+
        scale_shape_manual(name="Measurements",
                           values=c(21, 24),
                           labels=c("Canopy", "Leaf"))+
        xlim(0,2000)+
        ylim(-5,60)+
        ggtitle("Chamber 01")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21, 21, 24, 24))))+
        scale_color_manual(name="Predictions",
                          limits=c("canopy", "leaf"),
                          values=c("slateblue", "yellowgreen"),
                          labels=c("Canopy", "Leaf"))
    
    plot(p1)
    
    p2 <- ggplot() +
        geom_smooth(data=ch02DF, aes(Ca, Photo, group=ch02DF$Source,
                                     col=as.factor(ch02DF$Source)),
                    method = "lm", formula = y ~ splines::bs(x, 3))+
        geom_point(data=ch02DF, aes(Ca, Photo, 
                                    fill=as.factor(ch02DF$Position), 
                                    pch = as.factor(ch02DF$Source)))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'vertical',
              legend.box.just = 'left')+
        xlab(expression(paste(C[a]* " (umol ", m^-2, s^-1, ")")))+
        ylab(expression(paste(A* " (umol "* CO[2], " ", m^-2, s^-1, ")")))+
        scale_fill_manual(name="Position",
                          limits=c("12345", "345", "45", "up", "low"),
                          values=c("blue2", "red3", "purple", "orange", "green"),
                          labels=c("whole", "middle+bottom", "bottom", "up", "low"))+
        scale_shape_manual(name="Measurements",
                           values=c(21, 24),
                           labels=c("Canopy", "Leaf"))+
        xlim(0,2000)+
        ylim(-5,60)+
        ggtitle("Chamber 02")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21, 21, 24, 24))))+
        scale_color_manual(name="Predictions",
                           limits=c("canopy", "leaf"),
                           values=c("slateblue", "yellowgreen"),
                           labels=c("Canopy", "Leaf"))
    
    p3 <- ggplot() +
        geom_smooth(data=ch03DF, aes(Ca, Photo, group=ch03DF$Source,
                                     col=as.factor(ch03DF$Source)),
                    method = "lm", formula = y ~ splines::bs(x, 3))+
        geom_point(data=ch03DF, aes(Ca, Photo, 
                                    fill=as.factor(ch03DF$Position), 
                                    pch = as.factor(ch03DF$Source)))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'vertical',
              legend.box.just = 'left')+
        xlab(expression(paste(C[a]* " (umol ", m^-2, s^-1, ")")))+
        ylab(expression(paste(A* " (umol "* CO[2], " ", m^-2, s^-1, ")")))+
        scale_fill_manual(name="Position",
                          limits=c("12345", "345", "45", "up", "low"),
                          values=c("blue2", "red3", "purple", "orange", "green"),
                          labels=c("whole", "middle+bottom", "bottom", "up", "low"))+
        scale_shape_manual(name="Measurements",
                           values=c(21, 24),
                           labels=c("Canopy", "Leaf"))+
        xlim(0,2000)+
        ylim(-5,60)+
        ggtitle("Chamber 03")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21, 21, 24, 24))))+
        scale_color_manual(name="Predictions",
                           limits=c("canopy", "leaf"),
                           values=c("slateblue", "yellowgreen"),
                           labels=c("Canopy", "Leaf"))
    
    p4 <- ggplot() +
        geom_smooth(data=ch04DF, aes(Ca, Photo, group=ch04DF$Source,
                                     col=as.factor(ch04DF$Source)),
                    method = "lm", formula = y ~ splines::bs(x, 3))+
        geom_point(data=ch04DF, aes(Ca, Photo, 
                                    fill=as.factor(ch04DF$Position), 
                                    pch = as.factor(ch04DF$Source)))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'vertical',
              legend.box.just = 'left')+
        xlab(expression(paste(C[a]* " (umol ", m^-2, s^-1, ")")))+
        ylab(expression(paste(A* " (umol "* CO[2], " ", m^-2, s^-1, ")")))+
        scale_fill_manual(name="Position",
                          limits=c("12345", "345", "45", "up", "low"),
                          values=c("blue2", "red3", "purple", "orange", "green"),
                          labels=c("whole", "middle+bottom", "bottom", "up", "low"))+
        scale_shape_manual(name="Measurements",
                           values=c(21, 24),
                           labels=c("Canopy", "Leaf"))+
        xlim(0,2000)+
        ylim(-5,60)+
        ggtitle("Chamber 04")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21, 21, 24, 24))))+
        scale_color_manual(name="Predictions",
                           limits=c("canopy", "leaf"),
                           values=c("slateblue", "yellowgreen"),
                           labels=c("Canopy", "Leaf"))
    
    p5 <- ggplot() +
        geom_smooth(data=ch07DF, aes(Ca, Photo, group=ch07DF$Source,
                                     col=as.factor(ch07DF$Source)),
                    method = "lm", formula = y ~ splines::bs(x, 3))+
        geom_point(data=ch07DF, aes(Ca, Photo, 
                                    fill=as.factor(ch07DF$Position), 
                                    pch = as.factor(ch07DF$Source)))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'vertical',
              legend.box.just = 'left')+
        xlab(expression(paste(C[a]* " (umol ", m^-2, s^-1, ")")))+
        ylab(expression(paste(A* " (umol "* CO[2], " ", m^-2, s^-1, ")")))+
        scale_fill_manual(name="Position",
                          limits=c("12345", "345", "45", "up", "low"),
                          values=c("blue2", "red3", "purple", "orange", "green"),
                          labels=c("whole", "middle+bottom", "bottom", "up", "low"))+
        scale_shape_manual(name="Measurements",
                           values=c(21, 24),
                           labels=c("Canopy", "Leaf"))+
        xlim(0,2000)+
        ylim(-5,60)+
        ggtitle("Chamber 07")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21, 21, 24, 24))))+
        scale_color_manual(name="Predictions",
                           limits=c("canopy", "leaf"),
                           values=c("slateblue", "yellowgreen"),
                           labels=c("Canopy", "Leaf"))
    
    p6 <- ggplot() +
        geom_smooth(data=ch08DF, aes(Ca, Photo, group=ch08DF$Source,
                                     col=as.factor(ch08DF$Source)),
                    method = "lm", formula = y ~ splines::bs(x, 3))+
        geom_point(data=ch08DF, aes(Ca, Photo, 
                                    fill=as.factor(ch08DF$Position), 
                                    pch = as.factor(ch08DF$Source)))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'vertical',
              legend.box.just = 'left')+
        xlab(expression(paste(C[a]* " (umol ", m^-2, s^-1, ")")))+
        ylab(expression(paste(A* " (umol "* CO[2], " ", m^-2, s^-1, ")")))+
        scale_fill_manual(name="Position",
                          limits=c("12345", "345", "45", "up", "low"),
                          values=c("blue2", "red3", "purple", "orange", "green"),
                          labels=c("whole", "middle+bottom", "bottom", "up", "low"))+
        scale_shape_manual(name="Measurements",
                           values=c(21, 24),
                           labels=c("Canopy", "Leaf"))+
        xlim(0,2000)+
        ylim(-5,60)+
        ggtitle("Chamber 08")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21, 21, 24, 24))))+
        scale_color_manual(name="Predictions",
                           limits=c("canopy", "leaf"),
                           values=c("slateblue", "yellowgreen"),
                           labels=c("Canopy", "Leaf"))
    
    p7 <- ggplot() +
        geom_smooth(data=ch11DF, aes(Ca, Photo, group=ch11DF$Source,
                                     col=as.factor(ch11DF$Source)),
                    method = "lm", formula = y ~ splines::bs(x, 3))+
        geom_point(data=ch11DF, aes(Ca, Photo, 
                                    fill=as.factor(ch11DF$Position), 
                                    pch = as.factor(ch11DF$Source)))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'vertical',
              legend.box.just = 'left')+
        xlab(expression(paste(C[a]* " (umol ", m^-2, s^-1, ")")))+
        ylab(expression(paste(A* " (umol "* CO[2], " ", m^-2, s^-1, ")")))+
        scale_fill_manual(name="Position",
                          limits=c("12345", "345", "45", "up", "low"),
                          values=c("blue2", "red3", "purple", "orange", "green"),
                          labels=c("whole", "middle+bottom", "bottom", "up", "low"))+
        scale_shape_manual(name="Measurements",
                           values=c(21, 24),
                           labels=c("Canopy", "Leaf"))+
        xlim(0,2000)+
        ylim(-5,60)+
        ggtitle("Chamber 11")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21, 21, 24, 24))))+
        scale_color_manual(name="Predictions",
                           limits=c("canopy", "leaf"),
                           values=c("slateblue", "yellowgreen"),
                           labels=c("Canopy", "Leaf"))
    
    p8 <- ggplot() +
        geom_smooth(data=ch12DF, aes(Ca, Photo, group=ch12DF$Source,
                                     col=as.factor(ch12DF$Source)),
                    method = "lm", formula = y ~ splines::bs(x, 3))+
        geom_point(data=ch12DF, aes(Ca, Photo, 
                                    fill=as.factor(ch12DF$Position), 
                                    pch = as.factor(ch12DF$Source)))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'vertical',
              legend.box.just = 'left')+
        xlab(expression(paste(C[a]* " (umol ", m^-2, s^-1, ")")))+
        ylab(expression(paste(A* " (umol "* CO[2], " ", m^-2, s^-1, ")")))+
        scale_fill_manual(name="Position",
                          limits=c("12345", "345", "45", "up", "low"),
                          values=c("blue2", "red3", "purple", "orange", "green"),
                          labels=c("whole", "middle+bottom", "bottom", "up", "low"))+
        scale_shape_manual(name="Measurements",
                           values=c(21, 24),
                           labels=c("Canopy", "Leaf"))+
        xlim(0,2000)+
        ylim(-5,60)+
        ggtitle("Chamber 12")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21, 21, 24, 24))))+
        scale_color_manual(name="Predictions",
                           limits=c("canopy", "leaf"),
                           values=c("slateblue", "yellowgreen"),
                           labels=c("Canopy", "Leaf"))
    
    
    legend_shared <- get_legend(p1 + theme(legend.position="bottom",
                                           legend.box = 'vertical',
                                           legend.box.just = 'left'))
    
    combined_plots <- plot_grid(p1, p2, p3, p4, p5, p6, p7, p8, 
                                labels="AUTO", ncol=2, align="vh", axis = "l")
    
    #plot(p1)
    
    pdf("output/chamber_result_comparison_A_vs_Ca_flux_no_scaling.pdf", width=10, height=20)
    plot_grid(combined_plots, legend_shared, ncol=1, rel_heights=c(1,0.1))
    dev.off()    
    
  }
