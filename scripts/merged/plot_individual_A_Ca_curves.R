plot_individual_A_Ca_curves <- function(mgDF) {
    #### This script plots A-CA curve for leaf and canopy data on to the same plot
    #### and compare the shapes
    #### which can be related to Rogers et al. 2017 conceptual figure
   
    ### individual chambers
    ch01DF <- subset(mgDF, Chamber == 1)
    ch03DF <- subset(mgDF, Chamber == 3)
    ch11DF <- subset(mgDF, Chamber == 11)
    ch04DF <- subset(mgDF, Chamber == 4)
    ch08DF <- subset(mgDF, Chamber == 8)
    
    ## plot
    p1 <- ggplot() +
        geom_smooth(data=ch01DF, aes(Ca, Photo, group=Type,
                                     col=as.factor(Type)),
                    method = "lm", formula = y ~ splines::bs(x, 3))+
        geom_point(data=ch01DF, aes(Ca, Photo, 
                                    fill=as.factor(Position), 
                                    pch = as.factor(Type)))+
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
        xlab(expression(paste(C[a], " (", mu, "mol ", m^-2, s^-1, ")")))+
        ylab(expression(paste(A, " (", mu, "mol "* CO[2], " ", m^-2, s^-1, ")")))+
        scale_fill_manual(name="Position",
                           limits=c("12345", "345", "45", "up", "low"),
                           values=c("blue2", "red3", "purple", "orange", "green"),
                           labels=c("Full", "T+M", "Top", "Up", "Low"))+
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
    
    
    p2 <- ggplot() +
        geom_smooth(data=ch03DF, aes(Ca, Photo, group=Type,
                                     col=as.factor(Type)),
                    method = "lm", formula = y ~ splines::bs(x, 3))+
        geom_point(data=ch03DF, aes(Ca, Photo, 
                                    fill=as.factor(Position), 
                                    pch = as.factor(Type)))+
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
        xlab(expression(paste(C[a], " (", mu, "mol ", m^-2, s^-1, ")")))+
        ylab(expression(paste(A, " (", mu, "mol "* CO[2], " ", m^-2, s^-1, ")")))+
        scale_fill_manual(name="Position",
                          limits=c("12345", "345", "45", "up", "low"),
                          values=c("blue2", "red3", "purple", "orange", "green"),
                          labels=c("Full", "T+M", "Top", "Up", "Low"))+
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
    
    p3 <- ggplot() +
        geom_smooth(data=ch11DF, aes(Ca, Photo, group=Type,
                                     col=as.factor(Type)),
                    method = "lm", formula = y ~ splines::bs(x, 3))+
        geom_point(data=ch11DF, aes(Ca, Photo, 
                                    fill=as.factor(Position), 
                                    pch = as.factor(Type)))+
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
        xlab(expression(paste(C[a], " (", mu, "mol ", m^-2, s^-1, ")")))+
        ylab(expression(paste(A, " (", mu, "mol "* CO[2], " ", m^-2, s^-1, ")")))+
        scale_fill_manual(name="Position",
                          limits=c("12345", "345", "45", "up", "low"),
                          values=c("blue2", "red3", "purple", "orange", "green"),
                          labels=c("Full", "T+M", "Top", "up", "Low"))+
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
    
    p4 <- ggplot() +
        geom_smooth(data=ch04DF, aes(Ca, Photo, group=Type,
                                     col=as.factor(Type)),
                    method = "lm", formula = y ~ splines::bs(x, 3))+
        geom_point(data=ch04DF, aes(Ca, Photo, 
                                    fill=as.factor(Position), 
                                    pch = as.factor(Type)))+
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
        xlab(expression(paste(C[a], " (", mu, "mol ", m^-2, s^-1, ")")))+
        ylab(expression(paste(A, " (", mu, "mol "* CO[2], " ", m^-2, s^-1, ")")))+
        scale_fill_manual(name="Position",
                          limits=c("12345", "345", "45", "up", "low"),
                          values=c("blue2", "red3", "purple", "orange", "green"),
                          labels=c("Full", "T+M", "Top", "Up", "Low"))+
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
        geom_smooth(data=ch08DF, aes(Ca, Photo, group=Type,
                                     col=as.factor(Type)),
                    method = "lm", formula = y ~ splines::bs(x, 3))+
        geom_point(data=ch08DF, aes(Ca, Photo, 
                                    fill=as.factor(Position), 
                                    pch = as.factor(Type)))+
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
        xlab(expression(paste(C[a], " (", mu, "mol ", m^-2, s^-1, ")")))+
        ylab(expression(paste(A, " (", mu, "mol "* CO[2], " ", m^-2, s^-1, ")")))+
        scale_fill_manual(name="Position",
                          limits=c("12345", "345", "45", "up", "low"),
                          values=c("blue2", "red3", "purple", "orange", "green"),
                          labels=c("Full", "T+M", "Top", "Up", "Low"))+
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

    
    
    legend_shared <- get_legend(p1 + theme(legend.position="bottom",
                                           legend.box = 'vertical',
                                           legend.box.just = 'left'))
    
    combined_plots <- plot_grid(p1, p4, p2,  p5, p3,
                                labels="AUTO", ncol=2, align="vh", axis = "l")
    
    #plot(p1)
    
    pdf("output/A-Ca/individual_A-Ca_plots.pdf", width=10, height=14)
    plot_grid(combined_plots, legend_shared, ncol=1, rel_heights=c(1,0.1))
    dev.off()    
    
}
