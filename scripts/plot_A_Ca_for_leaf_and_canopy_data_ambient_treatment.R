plot_A_Ca_for_leaf_and_canopy_data_ambient_treatment <- function(cDF) {
    #### This script plots A-CA curve for leaf and canopy data on to the same plot
    #### and compare the shapes
    #### which can be related to Rogers et al. 2017 conceptual figure
    #### Only based on a subset of data that is amient CO2 and well-watered treatment
    
    #### read in leaf-scale data
    lDF1 <- read.csv("data/ACi_curves/HFE_Aci_2008-2009.csv",stringsAsFactors=FALSE)
    lDF2  <- read.csv("data/ACi_curves/HFE_Aci_lowcanopy_2008-2009.csv",stringsAsFactors=FALSE)
    
    ### combine the datasets
    lDF <- rbind(lDF1, lDF2)
    
    #### subset year 2009
    lDF$year <- year(lDF$Date)
    lDF <- subset(lDF, year == 2009)
    
    ### remove duplicated ch03 measurements
    lDF <- subset(lDF, Date!="2009-01-10")
    
    ### rename canopy DF
    ### note the Tleaf definition is not real!
    colnames(cDF) <- c("Chamber", "Canopy","Ca", "Tair", 
                       "date", "time", "datetime", "Tleaf",
                       "VPD", "DPLicorCh", "PARi", "slope2", 
                       "cmarea", "nslope2","k", "leak", 
                       "corrflux", "ncorrflux", "rh", "Photo",
                       "transpiration")
    
    ### clean the dataset to exclude na, negative values
    cDF <- cDF[complete.cases(cDF$Photo), ]
    cDF <- cDF[cDF$transpiration > 0, ]
    
    
    ### get gs from transpiration
    cDF$gs <- cDF$transpiration / cDF$VPD
    
    ### get Ci from gs, A and Ca
    cDF$Ci <- with(cDF, Ca - (Photo/gs))
    
    ### assign CO2, water treatment
    ## chambers 1, 3, 5, 7, 9, 11 are CO2 ambient
    ## chambers 1, 3, 4, 6, 8, 11 are wet 
    
    ch01DF <- leaf_canopy_combined_for_ACA_ACI_curves(lDF, cDF, 
                                   ch.l="ch01", ch.c="1")

    #ch02DF <- leaf_canopy_combined(lDF, cDF, 
    #                               ch.l="ch02", ch.c="2")
    
    ch03DF <- leaf_canopy_combined_for_ACA_ACI_curves(lDF, cDF, 
                                   ch.l="ch03", ch.c="3")
    
    #ch04DF <- leaf_canopy_combined(lDF, cDF, 
    #                               ch.l="ch04", ch.c="4")
    
    #ch07DF <- leaf_canopy_combined(lDF, cDF, 
    #                               ch.l="ch07", ch.c="7")
    
    #ch08DF <- leaf_canopy_combined(lDF, cDF, 
    #                               ch.l="ch08", ch.c="8")
    
    ch11DF <- leaf_canopy_combined_for_ACA_ACI_curves(lDF, cDF, 
                                   ch.l="ch11", ch.c="11")
    
    #ch12DF <- leaf_canopy_combined(lDF, cDF,
    #                               ch.l="ch12", ch.c="12")
    
    
    #### clearly some data points are problematic, so removing them
    #ch01DF[ch01DF$Source=="canopy"&ch01DF$Position=="345", "Photo"] <- ifelse(ch01DF[ch01DF$Source=="canopy"&ch01DF$Position=="345", "Photo"] > 12, 
    #                                                                                    NA, ch01DF[ch01DF$Source=="canopy"&ch01DF$Position=="345", "Photo"])
    #ch01DF[ch01DF$Source=="canopy"&ch01DF$Position=="12345", "Photo"] <- ifelse(ch01DF[ch01DF$Source=="canopy"&ch01DF$Position=="12345", "Photo"] > 18, 
    #                                                                                    NA, ch01DF[ch01DF$Source=="canopy"&ch01DF$Position=="12345", "Photo"])
    #ch01DF[ch01DF$Source=="canopy"&ch01DF$Position=="45", "Photo"] <- ifelse(ch01DF[ch01DF$Source=="canopy"&ch01DF$Position=="45", "Photo"] > 20, 
    #                                                                            NA, ch01DF[ch01DF$Source=="canopy"&ch01DF$Position=="45", "Photo"])
    #
    #
    #
    #ch03DF[ch03DF$Source=="canopy"&ch03DF$Position=="345", "Photo"] <- ifelse(ch03DF[ch03DF$Source=="canopy"&ch03DF$Position=="345", "Photo"] > 12, 
    #                                                                          NA, ch03DF[ch03DF$Source=="canopy"&ch03DF$Position=="345", "Photo"])
    #ch03DF[ch03DF$Source=="canopy"&ch03DF$Position=="45", "Photo"] <- ifelse(ch03DF[ch03DF$Source=="canopy"&ch03DF$Position=="45", "Photo"] > 18, 
    #                                                                         NA, ch03DF[ch03DF$Source=="canopy"&ch03DF$Position=="45", "Photo"])
    #
    #
    #ch11DF[ch11DF$Source=="canopy"&ch11DF$Position=="12345", "Photo"] <- ifelse(ch11DF[ch11DF$Source=="canopy"&ch11DF$Position=="12345", "Photo"] > 16, 
    #                                                                            NA, ch11DF[ch11DF$Source=="canopy"&ch11DF$Position=="12345", "Photo"])
    
    
    ## plot
    p1 <- ggplot() +
      geom_point(data=ch01DF, aes(Ca, Photo, 
                                  fill=as.factor(ch01DF$Position), 
                                  pch = as.factor(ch01DF$Source)), alpha=0.9)+
      #geom_smooth(data=ch01DF, aes(Ca, Photo, group=ch01DF$Position,
      #                             col=as.factor(ch01DF$Position)),
      #            method = "lm", formula = y ~ splines::bs(x, 4), se=FALSE)+
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
      xlab(expression(paste(C[a]* " (umol ", m^-2, " ", s^-1, ")")))+
      ylab(expression(paste(A* " (umol "* CO[2], " ", m^-2, " ", s^-1, ")")))+
      scale_fill_manual(name="Position",
                        limits=c("12345", "345", "45", "up", "low"),
                        values=c("blue2", "red3", "purple", "orange", "green"),
                        labels=c("Whole", "T+M", "Top", "Up", "Low"))+
      scale_color_manual(name="Position",
                         limits=c("12345", "345", "45", "up", "low"),
                         values=c("blue2", "red3", "purple", "orange", "darkgreen"),
                         labels=c("Whole", "T+M", "Top", "Up", "Low"))+
      scale_shape_manual(name="Measurements",
                         values=c(21, 24),
                         labels=c("Canopy", "Leaf"))+
      xlim(0,2000)+
      ylim(-5,40)+
      ggtitle("Chamber 01")+
      guides(fill = guide_legend(override.aes = list(shape = c(21, 21, 21, 24, 24))))
    
    
    p2 <- ggplot() +
      geom_point(data=ch03DF, aes(Ca, Photo, 
                                  fill=as.factor(ch03DF$Position), 
                                  pch = as.factor(ch03DF$Source)), alpha=0.9)+
      #geom_smooth(data=ch03DF, aes(Ca, Photo, group=ch03DF$Position,
      #                             col=as.factor(ch03DF$Position)),
      #            method = "lm", formula = y ~ splines::bs(x, 5), se=F)+
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
      xlab(expression(paste(C[a]* " (umol ", m^-2," ", s^-1, ")")))+
      ylab(expression(paste(A* " (umol "* CO[2], " ", m^-2, " ", s^-1, ")")))+
      scale_fill_manual(name="Position",
                        limits=c("12345", "345", "45", "up", "low"),
                        values=c("blue2", "red3", "purple", "orange", "green"),
                        labels=c("Whole", "T+M", "Top", "Up", "Low"))+
      scale_color_manual(name="Position",
                         limits=c("12345", "345", "45", "up", "low"),
                         values=c("blue2", "red3", "purple", "orange", "darkgreen"),
                         labels=c("Whole", "T+M", "Top", "Up", "Low"))+
      scale_shape_manual(name="Measurements",
                         values=c(21, 24),
                         labels=c("Canopy", "Leaf"))+
      xlim(0,2000)+
      ylim(-5,40)+
      ggtitle("Chamber 03")+
      guides(fill = guide_legend(override.aes = list(shape = c(21, 21, 21, 24, 24))))
    

    p3 <- ggplot() +
      geom_point(data=ch11DF, aes(Ca, Photo, 
                                  fill=as.factor(ch11DF$Position), 
                                  pch = as.factor(ch11DF$Source)), alpha=0.9)+
      #geom_smooth(data=ch11DF, aes(Ca, Photo, group=ch11DF$Position,
      #                             col=as.factor(ch11DF$Position)),
      #            method = "lm", formula = y ~ splines::bs(x, 5), se=F)+
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
      xlab(expression(paste(C[a]* " (umol ", m^-2, " ",  s^-1, ")")))+
      ylab(expression(paste(A* " (umol "* CO[2], " ", m^-2, " ", s^-1, ")")))+
      scale_fill_manual(name="Position",
                        limits=c("12345", "345", "45", "up", "low"),
                        values=c("blue2", "red3", "purple", "orange", "green"),
                        labels=c("Whole", "T+M", "Top", "Up", "Low"))+
      scale_color_manual(name="Position",
                         limits=c("12345", "345", "45", "up", "low"),
                         values=c("blue2", "red3", "purple", "orange", "darkgreen"),
                         labels=c("Whole", "T+M", "Top", "Up", "Low"))+
      scale_shape_manual(name="Measurements",
                         values=c(21, 24),
                         labels=c("Canopy", "Leaf"))+
      xlim(0,2000)+
      ylim(-5,40)+
      ggtitle("Chamber 11")+
      guides(fill = guide_legend(override.aes = list(shape = c(21, 21, 21, 24, 24))))
    
    
    ### combined plots + shared legend
    legend_shared <- get_legend(p1 + theme(legend.position="bottom",
                                           legend.box = 'vertical',
                                           legend.box.just = 'left'))
    
    combined_plots <- plot_grid(p1, p2, p3, 
                                labels="AUTO", ncol=1, align="vh", axis = "l")
    

    ### output
    pdf("output/chamber_result_comparison_A_vs_Ca_flux_no_scaling_ambient.pdf", width=6, height=14)
    plot_grid(combined_plots, legend_shared, ncol=1, rel_heights=c(1,0.1))
    dev.off()    
    
    
    ##### preparing aci fit
    fits.ch01 <- fitacis(ch01DF, group="Position", fitmethod="bilinear", Tcorrect=T)
    
    #plot(fits.ch01, how="oneplot")
    #plot(fits.ch01[[1]], col=alpha("black",0.2), add=T)
    #plot(fits.ch01[[2]], col=alpha("blue2", 0.2), add=T)
    plot(fits.ch01, how="oneplot", what="data", 
         pch=c(21, 22, 23, 24, 25), col=T)
    plot(fits.ch01, how="oneplot", color="blue", 
         add=T, what="model", lwd=3, lty=c(1,2,3,4,5))
    abline(v=410, add=T, lwd = 2)
    dev.off()

    ##
    pdf("output/chamber_result_comparison_A_vs_Ci_flux_no_scaling_ambient.pdf", width=4, height=14)
    par(mfrow=c(5,1))
    
    plot(fits.ch01[[5]],lwd=3, col=alpha("black",0.6), pch=21, main="Up leaves")
    plot(fits.ch01[[4]],lwd=3, col=alpha("black",0.6), pch=21, main="Low leaves", addlegend=F)
    plot(fits.ch01[[1]],lwd=3, col=alpha("black",0.6), pch=21, main="Whole canopy", addlegend=F)
    plot(fits.ch01[[2]],lwd=3, col=alpha("black",0.6), pch=21, main="T+M canopy", addlegend=F)
    plot(fits.ch01[[3]],lwd=3, col=alpha("black",0.6), pch=21, main="Top canopy", addlegend=F)

    dev.off()
    
    
    
    
    #### ggplot test
    #plDF1 <- fits.ch01[[1]]$df
    #
    #p1 <- ggplot(plDF1)+
    #  geom_point(aes(Ci, Ameas), alpha=0.2)+
    #  geom_line(aes(Ci, Ac), col="red")+
    #  geom_point(aes(Ci, Aj), col="blue")+
    #  geom_smooth(aes(Ci, Ac), col="red",
    #              method = "lm", formula = y ~ splines::bs(x, 4), se=FALSE)+
    #  theme_linedraw() +
    #  theme(panel.grid.minor=element_blank(),
    #        axis.text.x=element_text(size=12),
    #        axis.title.x=element_text(size=14),
    #        axis.text.y=element_text(size=12),
    #        axis.title.y=element_text(size=14),
    #        legend.text=element_text(size=12),
    #        legend.title=element_text(size=14),
    #        panel.grid.major=element_blank(),
    #        legend.position="none",
    #        legend.box = 'vertical',
    #        legend.box.just = 'left')+
    #  xlab(expression(paste(C[i]* " (umol ", m^-2, " ", s^-1, ")")))+
    #  ylab(expression(paste(A[n]* " (umol "* CO[2], " ", m^-2, " ", s^-1, ")")))+
    #  scale_fill_manual(name="Position",
    #                    limits=c("12345", "345", "45", "up", "low"),
    #                    values=c("blue2", "red3", "purple", "orange", "green"),
    #                    labels=c("Whole", "T+M", "Top", "Up", "Low"))+
    #  scale_color_manual(name="Position",
    #                     limits=c("12345", "345", "45", "up", "low"),
    #                     values=c("blue2", "red3", "purple", "orange", "darkgreen"),
    #                     labels=c("Whole", "T+M", "Top", "Up", "Low"))+
    #  scale_shape_manual(name="Measurements",
    #                     values=c(21, 24),
    #                     labels=c("Canopy", "Leaf"))+
    #  xlim(0,2000)+
    #  ylim(-5,40)+
    #  ggtitle("Chamber 01")+
    #  guides(fill = guide_legend(override.aes = list(shape = c(21, 21, 21, 24, 24))))
    #
    #plot(p1)
    

  }
