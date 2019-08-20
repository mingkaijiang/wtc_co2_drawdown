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
    lDF$Date <- as.Date(lDF$Date)
    
    lDF <- subset(lDF, Identity %in% c("90", "91", "92", "93", "96", "97", "100", "101", ### low
                                     "78", "104", "103", "87", "81", "82", "84", "86"))  ### high
    
    #lDF <- subset(lDF, Date >= "2008-10-01")
    #lDF <- subset(lDF, Identity !="69")
    #lDF <- subset(lDF, Identity !="65")
    #lDF <- subset(lDF, Identity !="63")
    #lDF <- subset(lDF, Identity !="64")
    
    ### remove duplicated ch03 measurements
    #lDF <- subset(lDF, Date!="2009-01-10")
    
    ### rename canopy DF
    ### note the Tleaf definition is not real!
    colnames(cDF) <- c("Chamber", "Canopy","Ca", "Tair", 
                       "date", "time", "datetime", "Tleaf",
                       "VPD", "DPLicorCh", "PARi", "slope2", 
                       "cmarea", "nslope2","k", "leak", 
                       "corrflux", "Photo", "rh", "my_co2_flux",
                       "transpiration")
    
    ### clean the dataset to exclude na, negative values
    #cDF[cDF$Chamber == 2 & cDF$Photo > 15, "Photo"] <- NA
    #cDF[cDF$Chamber == 12 & cDF$Photo > 25, "Photo"] <- NA
    #cDF[cDF$Chamber == 7 & cDF$Photo > 16, "Photo"] <- NA
    
    cDF <- cDF[complete.cases(cDF$Photo), ]
    cDF <- cDF[cDF$transpiration > 0, ]
    cDF <- cDF[cDF$Photo > 0, ]

    ### get gs from transpiration
    cDF$gs <- cDF$transpiration / cDF$VPD
    
    ### get Ci from gs, A and Ca
    cDF$Ci <- with(cDF, Ca - (Photo/gs))
    
    #cDF$g1 <- with(cDF, ((Ca * (transpiration / 1000000) / Photo) - (VPD))/sqrt(VPD))
    #cDF$g1 <- with(cDF, (((Ca * (transpiration / 1000000)) / (1.6 * Photo)) - 1.0) * sqrt(VPD))
    
    cDF$Ci <- with(cDF, Ca - (Photo/g1))
    cDF$Ci_Ca <- with(cDF, Ci / Ca)
    cDF$Ci_2 <- cDF$Ca * 0.7
    cDF <- subset(cDF, Ci_Ca < 1 & Ci_Ca > 0)

    ### assign CO2, water treatment
    ## chambers 1, 3, 5, 7, 9, 11 are CO2 ambient
    ## chambers 1, 3, 4, 6, 8, 11 are wet 
    
    ### ambient CO2
    ch01DF <- leaf_canopy_combined_for_ACA_ACI_curves(lDF, cDF, 
                                                      ch.l="ch01", ch.c="1")
    
    ch03DF <- leaf_canopy_combined_for_ACA_ACI_curves(lDF, cDF, 
                                                      ch.l="ch03", ch.c="3")
    
    ch07DF <- leaf_canopy_combined_for_ACA_ACI_curves(lDF, cDF, 
                                                      ch.l="ch07", ch.c="7")
    
    ch11DF <- leaf_canopy_combined_for_ACA_ACI_curves(lDF, cDF, 
                                                      ch.l="ch11", ch.c="11")
    
    ### elevated CO2
    ch02DF <- leaf_canopy_combined_for_ACA_ACI_curves(lDF, cDF, 
                                                      ch.l="ch02", ch.c="2")
    
    ch04DF <- leaf_canopy_combined_for_ACA_ACI_curves(lDF, cDF, 
                                                      ch.l="ch04", ch.c="4")
    
    ch08DF <- leaf_canopy_combined_for_ACA_ACI_curves(lDF, cDF, 
                                                      ch.l="ch08", ch.c="8")
    
    ch12DF <- leaf_canopy_combined_for_ACA_ACI_curves(lDF, cDF,
                                                      ch.l="ch12", ch.c="12")
    
    
    
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
      geom_point(data=ch02DF, aes(Ca, Photo, 
                                  fill=as.factor(ch02DF$Position), 
                                  pch = as.factor(ch02DF$Source)), alpha=0.9)+
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
      ggtitle("Chamber 02")+
      guides(fill = guide_legend(override.aes = list(shape = c(21, 21, 21, 24, 24))))
    
    
    p3 <- ggplot() +
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
    
    
    p4 <- ggplot() +
      geom_point(data=ch04DF, aes(Ca, Photo, 
                                  fill=as.factor(ch04DF$Position), 
                                  pch = as.factor(ch04DF$Source)), alpha=0.9)+
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
      ggtitle("Chamber 04")+
      guides(fill = guide_legend(override.aes = list(shape = c(21, 21, 21, 24, 24))))
    
    
    p5 <- ggplot() +
      geom_point(data=ch07DF, aes(Ca, Photo, 
                                  fill=as.factor(ch07DF$Position), 
                                  pch = as.factor(ch07DF$Source)), alpha=0.9)+
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
      ggtitle("Chamber 07")+
      guides(fill = guide_legend(override.aes = list(shape = c(21, 21, 21, 24, 24))))
    
    
    p6 <- ggplot() +
      geom_point(data=ch08DF, aes(Ca, Photo, 
                                  fill=as.factor(ch08DF$Position), 
                                  pch = as.factor(ch08DF$Source)), alpha=0.9)+
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
      ggtitle("Chamber 08")+
      guides(fill = guide_legend(override.aes = list(shape = c(21, 21, 21, 24, 24))))
    

    p7 <- ggplot() +
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
    
    
    p8 <- ggplot() +
      geom_point(data=ch12DF, aes(Ca, Photo, 
                                  fill=as.factor(ch12DF$Position), 
                                  pch = as.factor(ch12DF$Source)), alpha=0.9)+
      #geom_smooth(data=ch12DF, aes(Ca, Photo, group=ch12DF$Position,
      #                             col=as.factor(ch12DF$Position)),
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
      ggtitle("Chamber 12")+
      guides(fill = guide_legend(override.aes = list(shape = c(21, 21, 21, 24, 24))))
    
    
    ### combined plots + shared legend
    legend_shared <- get_legend(p1 + theme(legend.position="bottom",
                                           legend.box = 'vertical',
                                           legend.box.just = 'left'))
    
    combined_plots <- plot_grid(p1, p2, p3, p4, p5, p6, p7, p8, 
                                labels="AUTO", ncol=2, align="vh", axis = "l")
    

    ### output
    pdf("output/chamber_result_comparison_A_vs_Ca_flux_no_scaling_ambient.pdf", width=6, height=14)
    plot_grid(combined_plots, legend_shared, ncol=1, rel_heights=c(1,0.1))
    dev.off()    
    
    #test <- subset(cDF, Chamber == "12" & Canopy == "345")
    
    ##### preparing acifit
    #### the fit TPU function makes it long to run!
    fits.ch01 <- fitacis(ch01DF, group="Position", fitmethod="bilinear", Tcorrect=T, fitTPU=T)
    fits.ch03 <- fitacis(ch03DF, group="Position", fitmethod="bilinear", Tcorrect=T, fitTPU=T)
    fits.ch07 <- fitacis(ch07DF, group="Position", fitmethod="bilinear", Tcorrect=T, fitTPU=T)
    fits.ch11 <- fitacis(ch11DF, group="Position", fitmethod="bilinear", Tcorrect=T, fitTPU=T)
    
    fits.ch02 <- fitacis(ch02DF, group="Position", fitmethod="bilinear", Tcorrect=T, fitTPU=T)
    fits.ch04 <- fitacis(ch04DF, group="Position", fitmethod="bilinear", Tcorrect=T, fitTPU=T)
    fits.ch08 <- fitacis(ch08DF, group="Position", fitmethod="bilinear", Tcorrect=T, fitTPU=T)
    fits.ch12 <- fitacis(ch12DF, group="Position", fitmethod="bilinear", Tcorrect=T, fitTPU=T)
    
    #test <- subset(cDF, Chamber == "12" & Canopy == "345")
    #with(test, plot(Photo~Ca))
    #test.fit <- fitaci(test, fitmethod="bilinear", Tcorrect=T, fitTPU = T)
    #coef(test.fit)
    

    ##
    pdf("output/chamber_result_comparison_A_vs_Ci_flux_no_scaling_ambient.pdf", width=20, height=14)
    par(mfrow=c(5,8),mar=c(2,2,4,1),oma = c(4, 6, 0, 0))
    
    ymin <- -2
    ymax <- 40
    title.size <-2
    
    ### first row
    plot(fits.ch01[[5]],lwd=3, col=alpha("black",0.6), pch=21, main="Up leaves", cex.main=title.size,
         xlim=c(0, 1600), ylim=c(ymin, ymax))
    abline(v=c(380, 620), lwd=2, lty=c(3,1))
    #mtext("Up leaves", side = 2, line = 1, cex = 1.2)
    
    plot(fits.ch03[[5]],lwd=3, col=alpha("black",0.6), pch=21, 
         xlim=c(0, 1600), ylim=c(ymin, ymax), addlegend=F)
    abline(v=c(380, 620), lwd=2, lty=c(3,1))
    
    plot(fits.ch07[[5]],lwd=3, col=alpha("black",0.6), pch=21, 
         xlim=c(0, 1600), ylim=c(ymin, ymax), addlegend=F)
    abline(v=c(380, 620), lwd=2, lty=c(3,1))
    
    plot(fits.ch11[[5]],lwd=3, col=alpha("black",0.6), pch=21, 
         xlim=c(0, 1600), ylim=c(ymin, ymax), addlegend=F)
    abline(v=c(380, 620), lwd=2, lty=c(3,1))
    
    plot(fits.ch02[[5]],lwd=3, col=alpha("black",0.6), pch=21, 
         xlim=c(0, 1600), ylim=c(ymin, ymax), addlegend=F)
    abline(v=c(380, 620), lwd=2, lty=c(3,1))
    
    plot(fits.ch04[[5]],lwd=3, col=alpha("black",0.6), pch=21, 
         xlim=c(0, 1600), ylim=c(ymin, ymax), addlegend=F)
    abline(v=c(380, 620), lwd=2, lty=c(3,1))
    
    plot(fits.ch08[[5]],lwd=3, col=alpha("black",0.6), pch=21, 
         xlim=c(0, 1600), ylim=c(ymin, ymax), addlegend=F)
    abline(v=c(380, 620), lwd=2, lty=c(3,1))
    
    plot(fits.ch12[[5]],lwd=3, col=alpha("black",0.6), pch=21, 
         xlim=c(0, 1600), ylim=c(ymin, ymax), addlegend=F)
    abline(v=c(380, 620), lwd=2, lty=c(3,1))
    
    ### second row
    plot(fits.ch01[[4]],lwd=3, col=alpha("black",0.6), pch=21, addlegend=F, main="Low leaves", cex.main=title.size,
         xlim=c(0, 1600), ylim=c(ymin, ymax))
    abline(v=c(380, 620), lwd=2, lty=c(3,1))
    
    plot(fits.ch03[[4]],lwd=3, col=alpha("black",0.6), pch=21, addlegend=F, 
         xlim=c(0, 1600), ylim=c(ymin, ymax))
    abline(v=c(380, 620), lwd=2, lty=c(3,1))
    
    plot(fits.ch07[[4]],lwd=3, col=alpha("black",0.6), pch=21, addlegend=F, 
         xlim=c(0, 1600), ylim=c(ymin, ymax))
    abline(v=c(380, 620), lwd=2, lty=c(3,1))
    
    plot(fits.ch11[[4]],lwd=3, col=alpha("black",0.6), pch=21, addlegend=F, 
         xlim=c(0, 1600), ylim=c(ymin, ymax))
    abline(v=c(380, 620), lwd=2, lty=c(3,1))
    
    plot(fits.ch02[[4]],lwd=3, col=alpha("black",0.6), pch=21, addlegend=F, 
         xlim=c(0, 1600), ylim=c(ymin, ymax))
    abline(v=c(380, 620), lwd=2, lty=c(3,1))
    
    plot(fits.ch04[[4]],lwd=3, col=alpha("black",0.6), pch=21, addlegend=F, 
         xlim=c(0, 1600), ylim=c(ymin, ymax))
    abline(v=c(380, 620), lwd=2, lty=c(3,1))
    
    plot(fits.ch08[[4]],lwd=3, col=alpha("black",0.6), pch=21, addlegend=F, 
         xlim=c(0, 1600), ylim=c(ymin, ymax))
    abline(v=c(380, 620), lwd=2, lty=c(3,1))
    
    plot(fits.ch12[[4]],lwd=3, col=alpha("black",0.6), pch=21, addlegend=F, 
         xlim=c(0, 1600), ylim=c(ymin, ymax))
    abline(v=c(380, 620), lwd=2, lty=c(3,1))
    
    
    ### third row
    plot(fits.ch01[[1]],lwd=3, col=alpha("black",0.6), pch=21, addlegend=F, main="Whole canopy", cex.main=title.size,
         xlim=c(0, 1600), ylim=c(ymin, ymax))
    abline(v=c(380, 620), lwd=2, lty=c(3,1))
    
    plot(fits.ch03[[1]],lwd=3, col=alpha("black",0.6), pch=21, addlegend=F,
         xlim=c(0, 1600), ylim=c(ymin, ymax))
    abline(v=c(380, 620), lwd=2, lty=c(3,1))
    
    plot(fits.ch07[[1]],lwd=3, col=alpha("black",0.6), pch=21, addlegend=F,
         xlim=c(0, 1600), ylim=c(ymin, ymax))
    abline(v=c(380, 620), lwd=2, lty=c(3,1))
    
    plot(fits.ch11[[1]],lwd=3, col=alpha("black",0.6), pch=21, addlegend=F, 
         xlim=c(0, 1600), ylim=c(ymin, ymax))
    abline(v=c(380, 620), lwd=2, lty=c(3,1))
    
    plot(fits.ch02[[1]],lwd=3, col=alpha("black",0.6), pch=21, addlegend=F,
         xlim=c(0, 1600), ylim=c(ymin, ymax))
    abline(v=c(380, 620), lwd=2, lty=c(3,1))
    
    plot(fits.ch04[[1]],lwd=3, col=alpha("black",0.6), pch=21, addlegend=F,
         xlim=c(0, 1600), ylim=c(ymin, ymax))
    abline(v=c(380, 620), lwd=2, lty=c(3,1))
    
    plot(fits.ch08[[1]],lwd=3, col=alpha("black",0.6), pch=21, addlegend=F,
         xlim=c(0, 1600), ylim=c(ymin, ymax))
    abline(v=c(380, 620), lwd=2, lty=c(3,1))
    
    plot(fits.ch12[[1]],lwd=3, col=alpha("black",0.6), pch=21, addlegend=F,
         xlim=c(0, 1600), ylim=c(ymin, ymax))
    abline(v=c(380, 620), lwd=2, lty=c(3,1))
    
    
    ### fourth row
    plot(fits.ch01[[2]],lwd=3, col=alpha("black",0.6), pch=21, addlegend=F,main="T+M canopy", cex.main=title.size,
         xlim=c(0, 1600), ylim=c(ymin, ymax))
    abline(v=c(380, 620), lwd=2, lty=c(3,1))
    
    plot(fits.ch03[[2]],lwd=3, col=alpha("black",0.6), pch=21, addlegend=F, 
         xlim=c(0, 1600), ylim=c(ymin, ymax))
    abline(v=c(380, 620), lwd=2, lty=c(3,1))
    
    plot(fits.ch07[[2]],lwd=3, col=alpha("black",0.6), pch=21, addlegend=F, 
         xlim=c(0, 1600), ylim=c(ymin, ymax))
    abline(v=c(380, 620), lwd=2, lty=c(3,1))
    
    plot(fits.ch11[[2]],lwd=3, col=alpha("black",0.6), pch=21, addlegend=F, 
         xlim=c(0, 1600), ylim=c(ymin, ymax))
    abline(v=c(380, 620), lwd=2, lty=c(3,1))
    
    plot(fits.ch02[[2]],lwd=3, col=alpha("black",0.6), pch=21, addlegend=F, 
         xlim=c(0, 1600), ylim=c(ymin, ymax))
    abline(v=c(380, 620), lwd=2, lty=c(3,1))
    
    plot(fits.ch04[[2]],lwd=3, col=alpha("black",0.6), pch=21, addlegend=F, 
         xlim=c(0, 1600), ylim=c(ymin, ymax))
    abline(v=c(380, 620), lwd=2, lty=c(3,1))
    
    plot(fits.ch08[[2]],lwd=3, col=alpha("black",0.6), pch=21, addlegend=F, 
         xlim=c(0, 1600), ylim=c(ymin, ymax))
    abline(v=c(380, 620), lwd=2, lty=c(3,1))
    
    plot(fits.ch12[[2]],lwd=3, col=alpha("black",0.6), pch=21, addlegend=F, 
         xlim=c(0, 1600), ylim=c(ymin, ymax))
    abline(v=c(380, 620), lwd=2, lty=c(3,1))
    
    ### fifth row
    plot(fits.ch01[[3]],lwd=3, col=alpha("black",0.6), pch=21, addlegend=F, main="Top canopy", cex.main=title.size,
         xlim=c(0, 1600), ylim=c(ymin, ymax))
    abline(v=c(380, 620), lwd=2, lty=c(3,1))

    plot(fits.ch03[[3]],lwd=3, col=alpha("black",0.6), pch=21, addlegend=F, 
         xlim=c(0, 1600), ylim=c(ymin, ymax))
    abline(v=c(380, 620), lwd=2, lty=c(3,1))
    
    plot(fits.ch07[[3]],lwd=3, col=alpha("black",0.6), pch=21, addlegend=F, 
         xlim=c(0, 1600), ylim=c(ymin, ymax))
    abline(v=c(380, 620), lwd=2, lty=c(3,1))
    
    plot(fits.ch11[[3]],lwd=3, col=alpha("black",0.6), pch=21, addlegend=F,
         xlim=c(0, 1600), ylim=c(ymin, ymax))
    abline(v=c(380, 620), lwd=2, lty=c(3,1))
    
    plot(fits.ch02[[3]],lwd=3, col=alpha("black",0.6), pch=21, addlegend=F, 
         xlim=c(0, 1600), ylim=c(ymin, ymax))
    abline(v=c(380, 620), lwd=2, lty=c(3,1))
    
    plot(fits.ch04[[3]],lwd=3, col=alpha("black",0.6), pch=21, addlegend=F, 
         xlim=c(0, 1600), ylim=c(ymin, ymax))
    abline(v=c(380, 620), lwd=2, lty=c(3,1))
    
    plot(fits.ch08[[3]],lwd=3, col=alpha("black",0.6), pch=21, addlegend=F, 
         xlim=c(0, 1600), ylim=c(ymin, ymax))
    abline(v=c(380, 620), lwd=2, lty=c(3,1))
    
    plot(fits.ch12[[3]],lwd=3, col=alpha("black",0.6), pch=21, addlegend=F, 
         xlim=c(0, 1600), ylim=c(ymin, ymax))
    abline(v=c(380, 620), lwd=2, lty=c(3,1))
    
    # print the overall labels
    mtext(expression(C[i] * " (ppm)"), side = 1, outer = TRUE, line = 2, cex=2)
    mtext(expression(A[n] * " (" * mu * "mol " * m^-2 * " " * s^-1 * ")"), side = 2, outer = TRUE, line = 2, cex=2)
    
    dev.off()
    
    
    #### export biochemical parameter summary table
    ### make a list of identify
    id.list <- rep(c("up", "low", "12345", "345", "45"), each=8)
    chamber.list <- rep(c(1, 3, 7, 11, 2, 4, 8, 12), by = 5)
    ### prepare an output df
    outDF <- data.frame(id.list, chamber.list, NA, 
                        NA, NA, NA, NA, NA, NA,
                        NA, NA, NA, NA, NA, NA,
                        NA, NA, NA, NA, NA, NA,
                        NA, NA, NA, NA, NA)
    colnames(outDF) <- c("Position", "Chamber", 
                         "RMSE", "Vcmax", "Vcmax.se", "Jmax", "Jmax.se", "Rd", "Rd.se",
                         "Ci", "ALEAF", "GS", "ELEAF", "Ac", "Aj", "Ap", "VPD",
                         "Tleaf", "Ca", "Cc", "PPFD", "Ci_transition_Ac_Aj",
                         "curve.fitting", "Patm", "GammaStar", "Km")
    
    id.list <- unique(id.list)
    
    ### the for loop
    ### ch01
    for (i in 1:length(id.list)) {
      ## subset each data
      test <- subset(ch01DF, Position == id.list[i])
      
      ## fit
      fit1 <- fitaci(test, fitmethod="bilinear", Tcorrect=T, fitTPU = T)
      
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "1", "curve.fitting"] <- fit1$fitmethod
      
      ## assign fitted values
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "1", "RMSE"] <- fit1$RMSE
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "1", "Vcmax"] <- fit1$pars[1,1]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "1", "Vcmax.se"] <- fit1$pars[1,2]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "1", "Jmax"] <- fit1$pars[2,1]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "1", "Jmax.se"] <- fit1$pars[2,2]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "1", "Rd"] <- fit1$pars[3,1]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "1", "Rd.se"] <- fit1$pars[3,2]
      
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "1", "Ci"] <- fit1$Photosyn()[1]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "1", "ALEAF"] <- fit1$Photosyn()[2]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "1", "GS"] <- fit1$Photosyn()[3]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "1", "ELEAF"] <- fit1$Photosyn()[4]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "1", "Ac"] <- fit1$Photosyn()[5]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "1", "Aj"] <- fit1$Photosyn()[6]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "1", "Ap"] <- fit1$Photosyn()[7]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "1", "VPD"] <- fit1$Photosyn()[9]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "1", "Tleaf"] <- fit1$Photosyn()[10]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "1", "Ca"] <- fit1$Photosyn()[11]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "1", "Cc"] <- fit1$Photosyn()[12]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "1", "PPFD"] <- fit1$Photosyn()[13]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "1", "Patm"] <- fit1$Photosyn()[14]
      
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "1", "Ci_transition_Ac_Aj"] <- fit1$Ci_transition
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "1", "GammaStar"] <- fit1$GammaStar
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "1", "Km"] <- fit1$Km   
         
    }
    
    ### ch03
    for (i in 1:length(id.list)) {
      ## subset each data
      test <- subset(ch03DF, Position == id.list[i])
      
      ## fit
      fit1 <- fitaci(test, fitmethod="bilinear", Tcorrect=T)
      
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "3", "curve.fitting"] <- fit1$fitmethod
      
      ## assign fitted values
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "3", "RMSE"] <- fit1$RMSE
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "3", "Vcmax"] <- fit1$pars[1,1]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "3", "Vcmax.se"] <- fit1$pars[1,2]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "3", "Jmax"] <- fit1$pars[2,1]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "3", "Jmax.se"] <- fit1$pars[2,2]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "3", "Rd"] <- fit1$pars[3,1]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "3", "Rd.se"] <- fit1$pars[3,2]
      
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "3", "Ci"] <- fit1$Photosyn()[1]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "3", "ALEAF"] <- fit1$Photosyn()[2]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "3", "GS"] <- fit1$Photosyn()[3]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "3", "ELEAF"] <- fit1$Photosyn()[4]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "3", "Ac"] <- fit1$Photosyn()[5]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "3", "Aj"] <- fit1$Photosyn()[6]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "3", "Ap"] <- fit1$Photosyn()[7]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "3", "Rd2"] <- fit1$Photosyn()[8]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "3", "VPD"] <- fit1$Photosyn()[9]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "3", "Tleaf"] <- fit1$Photosyn()[10]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "3", "Ca"] <- fit1$Photosyn()[11]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "3", "Cc"] <- fit1$Photosyn()[12]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "3", "PPFD"] <- fit1$Photosyn()[13]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "3", "Patm"] <- fit1$Photosyn()[14]
      
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "3", "Ci_transition_Ac_Aj"] <- fit1$Ci_transition
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "3", "GammaStar"] <- fit1$GammaStar
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "3", "Km"] <- fit1$Km   
      
    }
    
    ### ch07
    for (i in 1:length(id.list)) {
      ## subset each data
      test <- subset(ch07DF, Position == id.list[i])
      
      ## fit
      fit1 <- fitaci(test, fitmethod="bilinear", Tcorrect=T)
      
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "7", "curve.fitting"] <- fit1$fitmethod
      
      ## assign fitted values
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "7", "RMSE"] <- fit1$RMSE
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "7", "Vcmax"] <- fit1$pars[1,1]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "7", "Vcmax.se"] <- fit1$pars[1,2]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "7", "Jmax"] <- fit1$pars[2,1]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "7", "Jmax.se"] <- fit1$pars[2,2]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "7", "Rd"] <- fit1$pars[3,1]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "7", "Rd.se"] <- fit1$pars[3,2]
      
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "7", "Ci"] <- fit1$Photosyn()[1]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "7", "ALEAF"] <- fit1$Photosyn()[2]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "7", "GS"] <- fit1$Photosyn()[3]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "7", "ELEAF"] <- fit1$Photosyn()[4]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "7", "Ac"] <- fit1$Photosyn()[5]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "7", "Aj"] <- fit1$Photosyn()[6]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "7", "Ap"] <- fit1$Photosyn()[7]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "7", "Rd2"] <- fit1$Photosyn()[8]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "7", "VPD"] <- fit1$Photosyn()[9]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "7", "Tleaf"] <- fit1$Photosyn()[10]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "7", "Ca"] <- fit1$Photosyn()[11]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "7", "Cc"] <- fit1$Photosyn()[12]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "7", "PPFD"] <- fit1$Photosyn()[13]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "7", "Patm"] <- fit1$Photosyn()[14]
      
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "7", "Ci_transition_Ac_Aj"] <- fit1$Ci_transition
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "7", "GammaStar"] <- fit1$GammaStar
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "7", "Km"] <- fit1$Km   
      
    }
    
    ### ch11
    for (i in 1:length(id.list)) {
      ## subset each data
      test <- subset(ch11DF, Position == id.list[i])
      
      ## fit
      fit1 <- fitaci(test, fitmethod="bilinear", Tcorrect=T)
      
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "11", "curve.fitting"] <- fit1$fitmethod
      
      ## assign fitted values
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "11", "RMSE"] <- fit1$RMSE
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "11", "Vcmax"] <- fit1$pars[1,1]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "11", "Vcmax.se"] <- fit1$pars[1,2]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "11", "Jmax"] <- fit1$pars[2,1]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "11", "Jmax.se"] <- fit1$pars[2,2]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "11", "Rd"] <- fit1$pars[3,1]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "11", "Rd.se"] <- fit1$pars[3,2]
      
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "11", "Ci"] <- fit1$Photosyn()[1]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "11", "ALEAF"] <- fit1$Photosyn()[2]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "11", "GS"] <- fit1$Photosyn()[3]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "11", "ELEAF"] <- fit1$Photosyn()[4]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "11", "Ac"] <- fit1$Photosyn()[5]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "11", "Aj"] <- fit1$Photosyn()[6]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "11", "Ap"] <- fit1$Photosyn()[7]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "11", "Rd2"] <- fit1$Photosyn()[8]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "11", "VPD"] <- fit1$Photosyn()[9]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "11", "Tleaf"] <- fit1$Photosyn()[10]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "11", "Ca"] <- fit1$Photosyn()[11]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "11", "Cc"] <- fit1$Photosyn()[12]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "11", "PPFD"] <- fit1$Photosyn()[13]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "11", "Patm"] <- fit1$Photosyn()[14]
      
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "11", "Ci_transition_Ac_Aj"] <- fit1$Ci_transition
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "11", "GammaStar"] <- fit1$GammaStar
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "11", "Km"] <- fit1$Km   
      
    }
    
    
    ### ch02
    for (i in 1:length(id.list)) {
      ## subset each data
      test <- subset(ch02DF, Position == id.list[i])
      
      ## fit
      fit1 <- fitaci(test, fitmethod="bilinear", Tcorrect=T, fitTPU = T)
      
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "2", "curve.fitting"] <- fit1$fitmethod
      
      ## assign fitted values
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "2", "RMSE"] <- fit1$RMSE
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "2", "Vcmax"] <- fit1$pars[1,1]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "2", "Vcmax.se"] <- fit1$pars[1,2]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "2", "Jmax"] <- fit1$pars[2,1]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "2", "Jmax.se"] <- fit1$pars[2,2]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "2", "Rd"] <- fit1$pars[3,1]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "2", "Rd.se"] <- fit1$pars[3,2]
    
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "2", "Ci"] <- fit1$Photosyn()[1]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "2", "ALEAF"] <- fit1$Photosyn()[2]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "2", "GS"] <- fit1$Photosyn()[3]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "2", "ELEAF"] <- fit1$Photosyn()[4]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "2", "Ac"] <- fit1$Photosyn()[5]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "2", "Aj"] <- fit1$Photosyn()[6]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "2", "Ap"] <- fit1$Photosyn()[7]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "2", "VPD"] <- fit1$Photosyn()[9]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "2", "Tleaf"] <- fit1$Photosyn()[10]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "2", "Ca"] <- fit1$Photosyn()[11]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "2", "Cc"] <- fit1$Photosyn()[12]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "2", "PPFD"] <- fit1$Photosyn()[13]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "2", "Patm"] <- fit1$Photosyn()[14]
      
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "2", "Ci_transition_Ac_Aj"] <- fit1$Ci_transition
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "2", "GammaStar"] <- fit1$GammaStar
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "2", "Km"] <- fit1$Km   
      
    }
    
    ### ch04
    for (i in 1:length(id.list)) {
      ## subset each data
      test <- subset(ch04DF, Position == id.list[i])
      
      ## fit
      fit1 <- fitaci(test, fitmethod="bilinear", Tcorrect=T)
      
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "4", "curve.fitting"] <- fit1$fitmethod
      
      ## assign fitted values
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "4", "RMSE"] <- fit1$RMSE
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "4", "Vcmax"] <- fit1$pars[1,1]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "4", "Vcmax.se"] <- fit1$pars[1,2]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "4", "Jmax"] <- fit1$pars[2,1]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "4", "Jmax.se"] <- fit1$pars[2,2]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "4", "Rd"] <- fit1$pars[3,1]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "4", "Rd.se"] <- fit1$pars[3,2]
      
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "4", "Ci"] <- fit1$Photosyn()[1]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "4", "ALEAF"] <- fit1$Photosyn()[2]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "4", "GS"] <- fit1$Photosyn()[3]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "4", "ELEAF"] <- fit1$Photosyn()[4]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "4", "Ac"] <- fit1$Photosyn()[5]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "4", "Aj"] <- fit1$Photosyn()[6]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "4", "Ap"] <- fit1$Photosyn()[7]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "4", "Rd2"] <- fit1$Photosyn()[8]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "4", "VPD"] <- fit1$Photosyn()[9]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "4", "Tleaf"] <- fit1$Photosyn()[10]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "4", "Ca"] <- fit1$Photosyn()[11]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "4", "Cc"] <- fit1$Photosyn()[12]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "4", "PPFD"] <- fit1$Photosyn()[13]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "4", "Patm"] <- fit1$Photosyn()[14]
      
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "4", "Ci_transition_Ac_Aj"] <- fit1$Ci_transition
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "4", "GammaStar"] <- fit1$GammaStar
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "4", "Km"] <- fit1$Km   
      
    }
    
    ### ch08
    for (i in 1:length(id.list)) {
      ## subset each data
      test <- subset(ch08DF, Position == id.list[i])
      
      ## fit
      fit1 <- fitaci(test, fitmethod="bilinear", Tcorrect=T)
      
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "8", "curve.fitting"] <- fit1$fitmethod
      
      ## assign fitted values
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "8", "RMSE"] <- fit1$RMSE
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "8", "Vcmax"] <- fit1$pars[1,1]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "8", "Vcmax.se"] <- fit1$pars[1,2]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "8", "Jmax"] <- fit1$pars[2,1]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "8", "Jmax.se"] <- fit1$pars[2,2]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "8", "Rd"] <- fit1$pars[3,1]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "8", "Rd.se"] <- fit1$pars[3,2]
      
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "8", "Ci"] <- fit1$Photosyn()[1]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "8", "ALEAF"] <- fit1$Photosyn()[2]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "8", "GS"] <- fit1$Photosyn()[3]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "8", "ELEAF"] <- fit1$Photosyn()[4]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "8", "Ac"] <- fit1$Photosyn()[5]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "8", "Aj"] <- fit1$Photosyn()[6]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "8", "Ap"] <- fit1$Photosyn()[7]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "8", "Rd2"] <- fit1$Photosyn()[8]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "8", "VPD"] <- fit1$Photosyn()[9]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "8", "Tleaf"] <- fit1$Photosyn()[10]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "8", "Ca"] <- fit1$Photosyn()[11]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "8", "Cc"] <- fit1$Photosyn()[12]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "8", "PPFD"] <- fit1$Photosyn()[13]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "8", "Patm"] <- fit1$Photosyn()[14]
      
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "8", "Ci_transition_Ac_Aj"] <- fit1$Ci_transition
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "8", "GammaStar"] <- fit1$GammaStar
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "8", "Km"] <- fit1$Km   
      
    }
    
    ### ch12
    for (i in 1:length(id.list)) {
      ## subset each data
      test <- subset(ch12DF, Position == id.list[i])
      
      ## fit
      fit1 <- fitaci(test, fitmethod="bilinear", Tcorrect=T)
      
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "12", "curve.fitting"] <- fit1$fitmethod
      
      ## assign fitted values
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "12", "RMSE"] <- fit1$RMSE
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "12", "Vcmax"] <- fit1$pars[1,1]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "12", "Vcmax.se"] <- fit1$pars[1,2]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "12", "Jmax"] <- fit1$pars[2,1]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "12", "Jmax.se"] <- fit1$pars[2,2]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "12", "Rd"] <- fit1$pars[3,1]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "12", "Rd.se"] <- fit1$pars[3,2]
      
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "12", "Ci"] <- fit1$Photosyn()[1]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "12", "ALEAF"] <- fit1$Photosyn()[2]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "12", "GS"] <- fit1$Photosyn()[3]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "12", "ELEAF"] <- fit1$Photosyn()[4]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "12", "Ac"] <- fit1$Photosyn()[5]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "12", "Aj"] <- fit1$Photosyn()[6]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "12", "Ap"] <- fit1$Photosyn()[7]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "12", "Rd2"] <- fit1$Photosyn()[8]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "12", "VPD"] <- fit1$Photosyn()[9]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "12", "Tleaf"] <- fit1$Photosyn()[10]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "12", "Ca"] <- fit1$Photosyn()[11]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "12", "Cc"] <- fit1$Photosyn()[12]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "12", "PPFD"] <- fit1$Photosyn()[13]
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "12", "Patm"] <- fit1$Photosyn()[14]
      
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "12", "Ci_transition_Ac_Aj"] <- fit1$Ci_transition
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "12", "GammaStar"] <- fit1$GammaStar
      outDF[outDF$Position == id.list[i] & outDF$Chamber == "12", "Km"] <- fit1$Km   
      
    }
    
    outDF$JV_ratio <- outDF$Jmax / outDF$Vcmax
    
    ### save
    write.csv(outDF, "output/ambient_chambers_biochemical_parameter_summary_table.csv", row.names=F)
    
    
    
    ################################# Plot delta A at the Ca = 400 to 600 range and see the slope
    #### export biochemical parameter summary table
    ### make a list of identify
    id.list <- rep(c("up", "low", "12345", "345", "45"), each=8)
    chamber.list <- rep(c(1, 3, 7, 11, 2, 4, 8, 12), by = 5)
    
    ### prepare an output df
    outDF2 <- data.frame(id.list, chamber.list, 
                        NA, NA, NA, NA, NA, NA)
    colnames(outDF2) <- c("Position", "Chamber", 
                         "ALEAF_400","ALEAF_600",
                         "Ac_400", "Ac_600",
                         "Aj_400", "Aj_600")
    
    id.list <- unique(id.list)
    
    ### the for loop
    ### ch01
    for (i in 1:length(id.list)) {
      ## subset each data
      test <- subset(ch01DF, Position == id.list[i])
      
      ## fit
      fit1 <- fitaci(test, fitmethod="bilinear", Tcorrect=T, fitTPU = T)
      
      outDF2[outDF2$Position == id.list[i] & outDF2$Chamber == "1", "ALEAF_400"] <- fit1$Photosyn(Ci=400)$ALEAF
      outDF2[outDF2$Position == id.list[i] & outDF2$Chamber == "1", "ALEAF_600"] <- fit1$Photosyn(Ci=600)$ALEAF
      outDF2[outDF2$Position == id.list[i] & outDF2$Chamber == "1", "Ac_400"] <- fit1$Photosyn(Ci=400)$Ac
      outDF2[outDF2$Position == id.list[i] & outDF2$Chamber == "1", "Ac_600"] <- fit1$Photosyn(Ci=600)$Ac
      outDF2[outDF2$Position == id.list[i] & outDF2$Chamber == "1", "Aj_400"] <- fit1$Photosyn(Ci=400)$Aj
      outDF2[outDF2$Position == id.list[i] & outDF2$Chamber == "1", "Aj_600"] <- fit1$Photosyn(Ci=600)$Aj
    }
    
    ### ch02
    for (i in 1:length(id.list)) {
      ## subset each data
      test <- subset(ch02DF, Position == id.list[i])
      
      ## fit
      fit1 <- fitaci(test, fitmethod="bilinear", Tcorrect=T, fitTPU = T)
      
      outDF2[outDF2$Position == id.list[i] & outDF2$Chamber == "2", "ALEAF_400"] <- fit1$Photosyn(Ci=400)$ALEAF
      outDF2[outDF2$Position == id.list[i] & outDF2$Chamber == "2", "ALEAF_600"] <- fit1$Photosyn(Ci=600)$ALEAF
      outDF2[outDF2$Position == id.list[i] & outDF2$Chamber == "2", "Ac_400"] <- fit1$Photosyn(Ci=400)$Ac
      outDF2[outDF2$Position == id.list[i] & outDF2$Chamber == "2", "Ac_600"] <- fit1$Photosyn(Ci=600)$Ac
      outDF2[outDF2$Position == id.list[i] & outDF2$Chamber == "2", "Aj_400"] <- fit1$Photosyn(Ci=400)$Aj
      outDF2[outDF2$Position == id.list[i] & outDF2$Chamber == "2", "Aj_600"] <- fit1$Photosyn(Ci=600)$Aj
    }
    
    ### ch03
    for (i in 1:length(id.list)) {
      ## subset each data
      test <- subset(ch03DF, Position == id.list[i])
      
      ## fit
      fit1 <- fitaci(test, fitmethod="bilinear", Tcorrect=T, fitTPU = T)
      
      outDF2[outDF2$Position == id.list[i] & outDF2$Chamber == "3", "ALEAF_400"] <- fit1$Photosyn(Ci=400)$ALEAF
      outDF2[outDF2$Position == id.list[i] & outDF2$Chamber == "3", "ALEAF_600"] <- fit1$Photosyn(Ci=600)$ALEAF
      outDF2[outDF2$Position == id.list[i] & outDF2$Chamber == "3", "Ac_400"] <- fit1$Photosyn(Ci=400)$Ac
      outDF2[outDF2$Position == id.list[i] & outDF2$Chamber == "3", "Ac_600"] <- fit1$Photosyn(Ci=600)$Ac
      outDF2[outDF2$Position == id.list[i] & outDF2$Chamber == "3", "Aj_400"] <- fit1$Photosyn(Ci=400)$Aj
      outDF2[outDF2$Position == id.list[i] & outDF2$Chamber == "3", "Aj_600"] <- fit1$Photosyn(Ci=600)$Aj
    }
    
    ### ch04
    for (i in 1:length(id.list)) {
      ## subset each data
      test <- subset(ch04DF, Position == id.list[i])
      
      ## fit
      fit1 <- fitaci(test, fitmethod="bilinear", Tcorrect=T, fitTPU = T)
      
      outDF2[outDF2$Position == id.list[i] & outDF2$Chamber == "4", "ALEAF_400"] <- fit1$Photosyn(Ci=400)$ALEAF
      outDF2[outDF2$Position == id.list[i] & outDF2$Chamber == "4", "ALEAF_600"] <- fit1$Photosyn(Ci=600)$ALEAF
      outDF2[outDF2$Position == id.list[i] & outDF2$Chamber == "4", "Ac_400"] <- fit1$Photosyn(Ci=400)$Ac
      outDF2[outDF2$Position == id.list[i] & outDF2$Chamber == "4", "Ac_600"] <- fit1$Photosyn(Ci=600)$Ac
      outDF2[outDF2$Position == id.list[i] & outDF2$Chamber == "4", "Aj_400"] <- fit1$Photosyn(Ci=400)$Aj
      outDF2[outDF2$Position == id.list[i] & outDF2$Chamber == "4", "Aj_600"] <- fit1$Photosyn(Ci=600)$Aj
    }
    
    ### ch07
    for (i in 1:length(id.list)) {
      ## subset each data
      test <- subset(ch07DF, Position == id.list[i])
      
      ## fit
      fit1 <- fitaci(test, fitmethod="bilinear", Tcorrect=T, fitTPU = T)
      
      outDF2[outDF2$Position == id.list[i] & outDF2$Chamber == "7", "ALEAF_400"] <- fit1$Photosyn(Ci=400)$ALEAF
      outDF2[outDF2$Position == id.list[i] & outDF2$Chamber == "7", "ALEAF_600"] <- fit1$Photosyn(Ci=600)$ALEAF
      outDF2[outDF2$Position == id.list[i] & outDF2$Chamber == "7", "Ac_400"] <- fit1$Photosyn(Ci=400)$Ac
      outDF2[outDF2$Position == id.list[i] & outDF2$Chamber == "7", "Ac_600"] <- fit1$Photosyn(Ci=600)$Ac
      outDF2[outDF2$Position == id.list[i] & outDF2$Chamber == "7", "Aj_400"] <- fit1$Photosyn(Ci=400)$Aj
      outDF2[outDF2$Position == id.list[i] & outDF2$Chamber == "7", "Aj_600"] <- fit1$Photosyn(Ci=600)$Aj
    }
    
    ### ch08
    for (i in 1:length(id.list)) {
      ## subset each data
      test <- subset(ch08DF, Position == id.list[i])
      
      ## fit
      fit1 <- fitaci(test, fitmethod="bilinear", Tcorrect=T, fitTPU = T)
      
      outDF2[outDF2$Position == id.list[i] & outDF2$Chamber == "8", "ALEAF_400"] <- fit1$Photosyn(Ci=400)$ALEAF
      outDF2[outDF2$Position == id.list[i] & outDF2$Chamber == "8", "ALEAF_600"] <- fit1$Photosyn(Ci=600)$ALEAF
      outDF2[outDF2$Position == id.list[i] & outDF2$Chamber == "8", "Ac_400"] <- fit1$Photosyn(Ci=400)$Ac
      outDF2[outDF2$Position == id.list[i] & outDF2$Chamber == "8", "Ac_600"] <- fit1$Photosyn(Ci=600)$Ac
      outDF2[outDF2$Position == id.list[i] & outDF2$Chamber == "8", "Aj_400"] <- fit1$Photosyn(Ci=400)$Aj
      outDF2[outDF2$Position == id.list[i] & outDF2$Chamber == "8", "Aj_600"] <- fit1$Photosyn(Ci=600)$Aj
    }
    
    ### ch11
    for (i in 1:length(id.list)) {
      ## subset each data
      test <- subset(ch11DF, Position == id.list[i])
      
      ## fit
      fit1 <- fitaci(test, fitmethod="bilinear", Tcorrect=T, fitTPU = T)
      
      outDF2[outDF2$Position == id.list[i] & outDF2$Chamber == "11", "ALEAF_400"] <- fit1$Photosyn(Ci=400)$ALEAF
      outDF2[outDF2$Position == id.list[i] & outDF2$Chamber == "11", "ALEAF_600"] <- fit1$Photosyn(Ci=600)$ALEAF
      outDF2[outDF2$Position == id.list[i] & outDF2$Chamber == "11", "Ac_400"] <- fit1$Photosyn(Ci=400)$Ac
      outDF2[outDF2$Position == id.list[i] & outDF2$Chamber == "11", "Ac_600"] <- fit1$Photosyn(Ci=600)$Ac
      outDF2[outDF2$Position == id.list[i] & outDF2$Chamber == "11", "Aj_400"] <- fit1$Photosyn(Ci=400)$Aj
      outDF2[outDF2$Position == id.list[i] & outDF2$Chamber == "11", "Aj_600"] <- fit1$Photosyn(Ci=600)$Aj
    }
    
    ### ch12
    for (i in 1:length(id.list)) {
      ## subset each data
      test <- subset(ch12DF, Position == id.list[i])
      
      ## fit
      fit1 <- fitaci(test, fitmethod="bilinear", Tcorrect=T, fitTPU = T)
      
      outDF2[outDF2$Position == id.list[i] & outDF2$Chamber == "12", "ALEAF_400"] <- fit1$Photosyn(Ci=400)$ALEAF
      outDF2[outDF2$Position == id.list[i] & outDF2$Chamber == "12", "ALEAF_600"] <- fit1$Photosyn(Ci=600)$ALEAF
      outDF2[outDF2$Position == id.list[i] & outDF2$Chamber == "12", "Ac_400"] <- fit1$Photosyn(Ci=400)$Ac
      outDF2[outDF2$Position == id.list[i] & outDF2$Chamber == "12", "Ac_600"] <- fit1$Photosyn(Ci=600)$Ac
      outDF2[outDF2$Position == id.list[i] & outDF2$Chamber == "12", "Aj_400"] <- fit1$Photosyn(Ci=400)$Aj
      outDF2[outDF2$Position == id.list[i] & outDF2$Chamber == "12", "Aj_600"] <- fit1$Photosyn(Ci=600)$Aj
    }
    
    outDF2$A_sens <- with(outDF2, (ALEAF_600-ALEAF_400)/ALEAF_400)
    outDF2$Aj_sens <- with(outDF2, (Aj_600-Aj_400)/Aj_400)
    outDF2$Ac_sens <- with(outDF2, (Ac_600-Ac_400)/Ac_400)
    
    
    
    write.csv(outDF2, "output/predicted_A_values_at_Ci_in_400_600_ppm.csv", row.names=F)
    
    
    
}
