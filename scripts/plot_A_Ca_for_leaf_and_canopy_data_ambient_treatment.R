plot_A_Ca_for_leaf_and_canopy_data_ambient_treatment <- function(cDF) {
    #### This script plots A-CA curve for leaf and canopy data on to the same plot
    #### and compare the shapes
    #### which can be related to Rogers et al. 2017 conceptual figure
    #### Only based on a subset of data that is amient CO2 and well-watered treatment
    
  
    ################################# plot A-Ca for ambient #################################
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
    
    #lDF <- subset(lDF, Date>=as.Date("2009-01-01")&Date<=as.Date("2009-03-01"))
    
    ### rename canopy DF
    ### note the Tleaf definition is not real!
    names(cDF)[names(cDF) == "WTC_CO2"] <- "Ca"
    
    cDF$Ci_Ca <- with(cDF, Ci / Ca)
    
    ### assign CO2, water treatment
    ## chambers 1, 3, 5, 7, 9, 11 are CO2 ambient
    ## chambers 1, 3, 4, 6, 8, 11 are wet 
    
    ### ambient CO2
    ch01DF <- leaf_canopy_combined_for_ACA_ACI_curves(lDF, cDF, 
                                                      ch.l="ch01", ch.c="1")
    
    ch03DF <- leaf_canopy_combined_for_ACA_ACI_curves(lDF, cDF, 
                                                      ch.l="ch03", ch.c="3")
    
    ch11DF <- leaf_canopy_combined_for_ACA_ACI_curves(lDF, cDF, 
                                                      ch.l="ch11", ch.c="11")
    
    ch07DF <- leaf_canopy_combined_for_ACA_ACI_curves(lDF, cDF, 
                                                      ch.l="ch07", ch.c="7")
    
    ### elevated CO2
    ch02DF <- leaf_canopy_combined_for_ACA_ACI_curves(lDF, cDF, 
                                                      ch.l="ch02", ch.c="2")
    
    ch04DF <- leaf_canopy_combined_for_ACA_ACI_curves(lDF, cDF, 
                                                      ch.l="ch04", ch.c="4")
    
    ch08DF <- leaf_canopy_combined_for_ACA_ACI_curves(lDF, cDF, 
                                                      ch.l="ch08", ch.c="8")
    
    ch12DF <- leaf_canopy_combined_for_ACA_ACI_curves(lDF, cDF,
                                                      ch.l="ch12", ch.c="12")
    
    
    ### combine only aCO2 and wet treatment
    plotDF <- rbind(ch01DF, ch03DF, ch11DF)
    
    ## plot
    p1 <- ggplot(data=plotDF, aes(Ca, Photo, group=Position)) +
      geom_point(data=plotDF, aes(fill=as.factor(Position), 
                                  pch = as.factor(Source)), alpha=0.6)+
      #geom_smooth(data=plotDF, aes(Ca, Photo, group=Position,
      #                             col=as.factor(Position)),
      #            method = "lm", formula = y ~ splines::bs(x, 4), se=T)+
      geom_smooth(aes(col=as.factor(Position)), 
                  method="nls", 
                  formula=y~a*exp(b/x),
                  fullrange=T,
                  method.args = list(start=c(a=1,b=0.1)), 
                  se=F)+
      theme_linedraw() +
      theme(panel.grid.minor=element_blank(),
            axis.text.x=element_text(size=12),
            axis.title.x=element_text(size=14),
            axis.text.y=element_text(size=12),
            axis.title.y=element_text(size=14),
            legend.text=element_text(size=12),
            legend.title=element_text(size=14),
            panel.grid.major=element_blank(),
            legend.position="bottom",
            legend.box = 'vertical',
            legend.box.just = 'left')+
      xlab(expression(paste(C[a], " (", mu, "mol ", m^-2, " ", s^-1, ")")))+
      ylab(expression(paste(A, " (", mu, "mol "* CO[2], " ", m^-2, " ", s^-1, ")")))+
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
      xlim(0,1800)+
      ylim(-5,40)+
      guides(fill = guide_legend(override.aes = list(shape = c(21, 21, 21, 24, 24))))
    
    ## testing fitting outside geom_smooth
    #subDF <- as.data.frame(subset(plotDF, Position=="up"))
    #myModel <- nls(Photo~a*exp(b/Ca), data=subDF, start=list(a=1, b=0.1))
    #myPredict <- expand.grid(Ca = seq(10, 1600, by =10))  
    #myPredict$fit <- predict(myModel, newdata= myPredict) 
    #p1 <- ggplot(data=subDF, 
    #             aes(Ca, Photo)) +
    #  geom_point()+
    #  geom_line(data = myPredict, aes(x=Ca, y= fit))
    #plot(p1)
  
    ### output
    pdf("output/A-Ca/ambient_A-Ca_plot.pdf", width=8, height=8)
    plot(p1)
    dev.off()    
    
    
    ### plot a subset range
    ### i.e. Ca = 400 to 600
    subDF <- subset(plotDF, Ca>=350 & Ca <= 650)
    
    ## plot
    p2 <- ggplot() +
      geom_point(data=subDF, aes(Ca, Photo, 
                                  fill=as.factor(Position), 
                                  pch = as.factor(Source)), alpha=0.6)+
      geom_smooth(data=subDF, aes(Ca, Photo, group=Position,
                                   col=as.factor(Position)),
                  method = "lm", formula = y ~ x, se=T)+
      theme_linedraw() +
      theme(panel.grid.minor=element_blank(),
            axis.text.x=element_text(size=12),
            axis.title.x=element_text(size=14),
            axis.text.y=element_text(size=12),
            axis.title.y=element_text(size=14),
            legend.text=element_text(size=12),
            legend.title=element_text(size=14),
            panel.grid.major=element_blank(),
            legend.position="bottom",
            legend.box = 'vertical',
            legend.box.just = 'left')+
      xlab(expression(paste(C[a], " (", mu, "mol ", m^-2, " ", s^-1, ")")))+
      ylab(expression(paste(A, " (", mu, "mol "* CO[2], " ", m^-2, " ", s^-1, ")")))+
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
      xlim(350,650)+
      ylim(-5,40)+
      guides(fill = guide_legend(override.aes = list(shape = c(21, 21, 21, 24, 24))))
    
    
    ### output
    pdf("output/A-Ca/ambient_A-Ca_plot_400_600_ppm.pdf", width=8, height=8)
    plot(p2)
    dev.off()    
    
    
    
    ################################# plot A-Ci for ambient #################################
    ##### preparing acifit
    #### the fit TPU function makes it long to run!
    fits.ch01 <- fitacis(ch01DF, group="Position", fitmethod="bilinear", Tcorrect=T, fitTPU=F)
    fits.ch03 <- fitacis(ch03DF, group="Position", fitmethod="bilinear", Tcorrect=T, fitTPU=F)
    fits.ch11 <- fitacis(ch11DF, group="Position", fitmethod="bilinear", Tcorrect=T, fitTPU=F)


    ## pdf
    pdf("output/A-Ca/ambient_A-Ci_plots.pdf", width=14, height=14)
    par(mfrow=c(5,3),mar=c(2,2,4,1),oma = c(4, 6, 0, 0))
    
    ymin <- -2
    ymax <- 40
    title.size <- 1.5
    
    ### first row
    plot(fits.ch01[[5]],lwd=3, col=alpha("black",0.6), pch=21, main="Leaves: up", cex.main=title.size,
         xlim=c(0, 1600), ylim=c(ymin, ymax))
    abline(v=c(280), lwd=2, lty=c(3))

    plot(fits.ch03[[5]],lwd=3, col=alpha("black",0.6), pch=21, 
         xlim=c(0, 1600), ylim=c(ymin, ymax), addlegend=F)
    abline(v=c(280), lwd=2, lty=c(3))
    
    
    plot(fits.ch11[[5]],lwd=3, col=alpha("black",0.6), pch=21, 
         xlim=c(0, 1600), ylim=c(ymin, ymax), addlegend=F)
    abline(v=c(280), lwd=2, lty=c(3))
    
    
    ### second row
    plot(fits.ch01[[4]],lwd=3, col=alpha("black",0.6), pch=21, addlegend=F, main="Leaves: low", cex.main=title.size,
         xlim=c(0, 1600), ylim=c(ymin, ymax))
    abline(v=c(280), lwd=2, lty=c(3))
    
    plot(fits.ch03[[4]],lwd=3, col=alpha("black",0.6), pch=21, addlegend=F, 
         xlim=c(0, 1600), ylim=c(ymin, ymax))
    abline(v=c(280), lwd=2, lty=c(3))
    
    plot(fits.ch11[[4]],lwd=3, col=alpha("black",0.6), pch=21, addlegend=F, 
         xlim=c(0, 1600), ylim=c(ymin, ymax))
    abline(v=c(280), lwd=2, lty=c(3))
    

    ### third row
    ymax <- 20
    
    plot(fits.ch01[[1]],lwd=3, col=alpha("black",0.6), pch=21, addlegend=F, main="Canopy: whole", cex.main=title.size,
         xlim=c(0, 1600), ylim=c(ymin, ymax))
    abline(v=c(280), lwd=2, lty=c(3))
    
    plot(fits.ch03[[1]],lwd=3, col=alpha("black",0.6), pch=21, addlegend=F,
         xlim=c(0, 1600), ylim=c(ymin, ymax))
    abline(v=c(280), lwd=2, lty=c(3))
    
    plot(fits.ch11[[1]],lwd=3, col=alpha("black",0.6), pch=21, addlegend=F, 
         xlim=c(0, 1600), ylim=c(ymin, ymax))
    abline(v=c(280), lwd=2, lty=c(3))
    
    
    ### fourth row
    plot(fits.ch01[[2]],lwd=3, col=alpha("black",0.6), pch=21, addlegend=F,main="Canopy: T+M", cex.main=title.size,
         xlim=c(0, 1600), ylim=c(ymin, ymax))
    abline(v=c(280), lwd=2, lty=c(3))
    
    plot(fits.ch03[[2]],lwd=3, col=alpha("black",0.6), pch=21, addlegend=F, 
         xlim=c(0, 1600), ylim=c(ymin, ymax))
    abline(v=c(280), lwd=2, lty=c(3))
    
    plot(fits.ch11[[2]],lwd=3, col=alpha("black",0.6), pch=21, addlegend=F, 
         xlim=c(0, 1600), ylim=c(ymin, ymax))
    abline(v=c(280), lwd=2, lty=c(3))
    
    
    ### fifth row
    plot(fits.ch01[[3]],lwd=3, col=alpha("black",0.6), pch=21, addlegend=F, main="Canopy: top", cex.main=title.size,
         xlim=c(0, 1600), ylim=c(ymin, ymax))
    abline(v=c(280), lwd=2, lty=c(3))

    plot(fits.ch03[[3]],lwd=3, col=alpha("black",0.6), pch=21, addlegend=F, 
         xlim=c(0, 1600), ylim=c(ymin, ymax))
    abline(v=c(280), lwd=2, lty=c(3))
    
    
    plot(fits.ch11[[3]],lwd=3, col=alpha("black",0.6), pch=21, addlegend=F,
         xlim=c(0, 1600), ylim=c(ymin, ymax))
    abline(v=c(280), lwd=2, lty=c(3))
    
    # print the overall labels
    mtext(expression(C[i] * " (ppm)"), side = 1, outer = TRUE, line = 2, cex=2)
    mtext(expression(A[n] * " (" * mu * "mol " * m^-2 * " " * s^-1 * ")"), 
          side = 2, outer = TRUE, line = 2, cex=2)
    
    dev.off()
    
    
    ################################# plot statistics for ambient #################################
    #### read biochemical parameter summary table
    ### we have multiple dates in leaf-scale measurements
    stDF.c <- read.csv("output/canopy/canopy_scale_parameters.csv", header=T)
    stDF.l <- read.csv("output/leaf/leaf_scale_parameters.csv", header=T)
    
    ### subset leaf
    subDF.l <- subset(stDF.l, Chamber%in%c("ch01", "ch03", "ch11"))
    subDF.l$Date <- as.Date(as.character(subDF.l$Date))
    subDF.l <- subset(subDF.l, Date>=as.Date("2009-01-01")&Date<=as.Date("2009-03-01"))
    
    subDF.l <- subDF.l[,c("Chamber", "Height", "RMSE", "Vcmax", "Vcmax.se", "Jmax",
                          "Jmax.se", "Rd", "Rd.se", "Ci", "ALEAF", "GS", "ELEAF", "Ac",
                          "Aj", "Ap", "VPD", "Tleaf", "Ca", "Cc", "PPFD", 
                          "Ci_transition_Ac_Aj", "GammaStar", "Km", "G1", "JVratio")]
    
    subDF.l$Chamber <- as.character(subDF.l$Chamber)
    subDF.l$Chamber <- as.numeric(gsub("ch", "", subDF.l$Chamber))
    subDF.l$Height <- as.character(subDF.l$Height)
    subDF.l$Source <- "Leaf"
    
    ### subset canopy
    subDF.c <- subset(stDF.c, Chamber%in%c("1", "3", "11"))
    
    subDF.c <- subDF.c[,c("Chamber", "Canopy", "RMSE", "Vcmax", "Vcmax.se", "Jmax",
                          "Jmax.se", "Rd", "Rd.se", "Ci", "ALEAF", "GS", "ELEAF", "Ac",
                          "Aj", "Ap", "VPD", "Tleaf", "Ca", "Cc", "PPFD", 
                          "Ci_transition_Ac_Aj", "GammaStar", "Km", "G1", "JVratio")]
    
    ### change col names
    names(subDF.c)[names(subDF.c) == "Canopy"] <- "Height"
    
    subDF.c$Source <- "Canopy"

    ### combine    
    plotDF <- rbind(subDF.l, subDF.c)
    plotDF$Chamber <- as.factor(as.character(plotDF$Chamber))
    plotDF$Source <- as.factor(as.character(plotDF$Source))
    plotDF$Height <- as.factor(as.character(plotDF$Height))
    
    
    ### plotting
    p1 <- ggplot() +
      geom_point(data=plotDF, aes(Height, Vcmax, 
                                  fill=as.factor(Height), 
                                  pch = as.factor(Source)), alpha=1.0, size=4)+
      theme_linedraw() +
      theme(panel.grid.minor=element_blank(),
            axis.text.x=element_text(size=12),
            axis.title.x=element_text(size=14),
            axis.text.y=element_text(size=12),
            axis.title.y=element_text(size=14),
            legend.text=element_text(size=12),
            legend.title=element_text(size=14),
            panel.grid.major=element_blank(),
            legend.position="bone",
            legend.box = 'vertical',
            legend.box.just = 'left')+
      xlab("")+
      ylab(expression(paste(V[cmax], " (", mu, "mol "* CO[2], " ", m^-2, " ", s^-1, ")")))+
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
      scale_x_discrete(name="", 
                       breaks=c("12345", "345", "45", "up", "low"), 
                       labels=c("Whole", "T+M", "Top", "Up", "Low"))+
      guides(fill = guide_legend(override.aes = list(shape = c(21, 21, 21, 24, 24))))
    
    
    p2 <- ggplot() +
      geom_point(data=plotDF, aes(Height, Jmax, 
                                  fill=as.factor(Height), 
                                  pch = as.factor(Source)), alpha=1.0, size=4)+
      theme_linedraw() +
      theme(panel.grid.minor=element_blank(),
            axis.text.x=element_text(size=12),
            axis.title.x=element_text(size=14),
            axis.text.y=element_text(size=12),
            axis.title.y=element_text(size=14),
            legend.text=element_text(size=12),
            legend.title=element_text(size=14),
            panel.grid.major=element_blank(),
            legend.position="bone",
            legend.box = 'vertical',
            legend.box.just = 'left')+
      xlab("")+
      ylab(expression(paste(J[max], " (", mu, "mol "* CO[2], " ", m^-2, " ", s^-1, ")")))+
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
      scale_x_discrete(name="", 
                       breaks=c("12345", "345", "45", "up", "low"), 
                       labels=c("Whole", "T+M", "Top", "Up", "Low"))+
      guides(fill = guide_legend(override.aes = list(shape = c(21, 21, 21, 24, 24))))
    
    
    p3 <- ggplot() +
      geom_point(data=plotDF, aes(Height, JVratio, 
                                  fill=as.factor(Height), 
                                  pch = as.factor(Source)), alpha=1.0, size=4)+
      theme_linedraw() +
      theme(panel.grid.minor=element_blank(),
            axis.text.x=element_text(size=12),
            axis.title.x=element_text(size=14),
            axis.text.y=element_text(size=12),
            axis.title.y=element_text(size=14),
            legend.text=element_text(size=12),
            legend.title=element_text(size=14),
            panel.grid.major=element_blank(),
            legend.position="bone",
            legend.box = 'vertical',
            legend.box.just = 'left')+
      xlab("")+
      ylab("JV ratio")+
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
      scale_x_discrete(name="", 
                       breaks=c("12345", "345", "45", "up", "low"), 
                       labels=c("Whole", "T+M", "Top", "Up", "Low"))+
      guides(fill = guide_legend(override.aes = list(shape = c(21, 21, 21, 24, 24))))
    
    
    p4 <- ggplot() +
      geom_point(data=plotDF, aes(Height, Ci_transition_Ac_Aj, 
                                  fill=as.factor(Height), 
                                  pch = as.factor(Source)), alpha=1.0, size=4)+
      theme_linedraw() +
      theme(panel.grid.minor=element_blank(),
            axis.text.x=element_text(size=12),
            axis.title.x=element_text(size=14),
            axis.text.y=element_text(size=12),
            axis.title.y=element_text(size=14),
            legend.text=element_text(size=12),
            legend.title=element_text(size=14),
            panel.grid.major=element_blank(),
            legend.position="bone",
            legend.box = 'vertical',
            legend.box.just = 'left')+
      xlab("")+
      ylab("Transition Ci (ppm)")+
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
      scale_x_discrete(name="", 
                       breaks=c("12345", "345", "45", "up", "low"), 
                       labels=c("Whole", "T+M", "Top", "Up", "Low"))+
      guides(fill = guide_legend(override.aes = list(shape = c(21, 21, 21, 24, 24))))
    
    p5 <- ggplot() +
      geom_point(data=plotDF, aes(Height, Ac, 
                                  fill=as.factor(Height), 
                                  pch = as.factor(Source)), alpha=1.0, size=4)+
      theme_linedraw() +
      theme(panel.grid.minor=element_blank(),
            axis.text.x=element_text(size=12),
            axis.title.x=element_text(size=14),
            axis.text.y=element_text(size=12),
            axis.title.y=element_text(size=14),
            legend.text=element_text(size=12),
            legend.title=element_text(size=14),
            panel.grid.major=element_blank(),
            legend.position="bone",
            legend.box = 'vertical',
            legend.box.just = 'left')+
      xlab("")+
      ylab(expression(paste(A[c], " (", mu, "mol "* CO[2], " ", m^-2, " ", s^-1, ")")))+
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
      scale_x_discrete(name="", 
                       breaks=c("12345", "345", "45", "up", "low"), 
                       labels=c("Whole", "T+M", "Top", "Up", "Low"))+
      guides(fill = guide_legend(override.aes = list(shape = c(21, 21, 21, 24, 24))))
    
    
    p6 <- ggplot() +
      geom_point(data=plotDF, aes(Height, Aj, 
                                  fill=as.factor(Height), 
                                  pch = as.factor(Source)), alpha=1.0, size=4)+
      theme_linedraw() +
      theme(panel.grid.minor=element_blank(),
            axis.text.x=element_text(size=12),
            axis.title.x=element_text(size=14),
            axis.text.y=element_text(size=12),
            axis.title.y=element_text(size=14),
            legend.text=element_text(size=12),
            legend.title=element_text(size=14),
            panel.grid.major=element_blank(),
            legend.position="bone",
            legend.box = 'vertical',
            legend.box.just = 'left')+
      xlab("")+
      ylab(expression(paste(A[j], " (", mu, "mol "* CO[2], " ", m^-2, " ", s^-1, ")")))+
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
      scale_x_discrete(name="", 
                       breaks=c("12345", "345", "45", "up", "low"), 
                       labels=c("Whole", "T+M", "Top", "Up", "Low"))+
      guides(fill = guide_legend(override.aes = list(shape = c(21, 21, 21, 24, 24))))
    
    
    legend_shared <- get_legend(p1 + theme(legend.position="bottom",
                                           legend.box = 'vertical',
                                           legend.box.just = 'left'))
    
    combined_plots <- plot_grid(p1, p2, p3, p4, p5, p6, 
                                labels="AUTO", ncol=2, align="vh", axis = "l")
    
    #plot(p1)
    
    pdf("output/A-Ca/biochemical_parameter_stats_plot.pdf", width=10, height=16)
    plot_grid(combined_plots, legend_shared, ncol=1, rel_heights=c(1,0.1))
    dev.off()  
    
    
    
    #### perform statistics
    require(nlme)
    require(lme4)
    require(lmerTest)
    fit = aov(Vcmax ~ Source + Error(Height), data=plotDF)
    summary(fit)
    
    pf(q=13057/805,
       df1=1,
       df2=3,
       lower.tail=FALSE)
    
    pf(q=805/142.6,
       df1=3,
       df2=10,
       lower.tail=F)
    
    ### to be written
    
    
    
    
    ################################# plot delta A sensitivity #################################
    ###### Plot delta A at the Ca = 400 to 600 range and see the slope
    #### export biochemical parameter summary table
    ### make a list of identify
    id.list <- rep(c("up", "low", "12345", "345", "45"), each=3)
    chamber.list <- rep(c(1, 3, 11), by = 5)
    
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
      fit1 <- fitaci(test, fitmethod="bilinear", varnames = list(ALEAF = "Photo", 
                                                                Tleaf = "Tleaf", 
                                                                Ci = "Ci",
                                                                PPFD = "PAR", 
                                                                Rd = "Rd"),
                     Tcorrect=T, fitTPU = F)
      
      outDF2[outDF2$Position == id.list[i] & outDF2$Chamber == "1", "ALEAF_400"] <- fit1$Photosyn(Ci=400)$ALEAF
      outDF2[outDF2$Position == id.list[i] & outDF2$Chamber == "1", "ALEAF_600"] <- fit1$Photosyn(Ci=600)$ALEAF
      outDF2[outDF2$Position == id.list[i] & outDF2$Chamber == "1", "Ac_400"] <- fit1$Photosyn(Ci=400)$Ac
      outDF2[outDF2$Position == id.list[i] & outDF2$Chamber == "1", "Ac_600"] <- fit1$Photosyn(Ci=600)$Ac
      outDF2[outDF2$Position == id.list[i] & outDF2$Chamber == "1", "Aj_400"] <- fit1$Photosyn(Ci=400)$Aj
      outDF2[outDF2$Position == id.list[i] & outDF2$Chamber == "1", "Aj_600"] <- fit1$Photosyn(Ci=600)$Aj
    }
   
    
    ### ch03
    for (i in 1:length(id.list)) {
      ## subset each data
      test <- subset(ch03DF, Position == id.list[i])
      
      ## fit
      fit1 <- fitaci(test, fitmethod="bilinear", varnames = list(ALEAF = "Photo", 
                                                                 Tleaf = "Tleaf", 
                                                                 Ci = "Ci",
                                                                 PPFD = "PAR", 
                                                                 Rd = "Rd"),
                     Tcorrect=T, fitTPU = F)
      
      outDF2[outDF2$Position == id.list[i] & outDF2$Chamber == "3", "ALEAF_400"] <- fit1$Photosyn(Ci=400)$ALEAF
      outDF2[outDF2$Position == id.list[i] & outDF2$Chamber == "3", "ALEAF_600"] <- fit1$Photosyn(Ci=600)$ALEAF
      outDF2[outDF2$Position == id.list[i] & outDF2$Chamber == "3", "Ac_400"] <- fit1$Photosyn(Ci=400)$Ac
      outDF2[outDF2$Position == id.list[i] & outDF2$Chamber == "3", "Ac_600"] <- fit1$Photosyn(Ci=600)$Ac
      outDF2[outDF2$Position == id.list[i] & outDF2$Chamber == "3", "Aj_400"] <- fit1$Photosyn(Ci=400)$Aj
      outDF2[outDF2$Position == id.list[i] & outDF2$Chamber == "3", "Aj_600"] <- fit1$Photosyn(Ci=600)$Aj
    }

    ### ch11
    for (i in 1:length(id.list)) {
      ## subset each data
      test <- subset(ch11DF, Position == id.list[i])
      
      ## fit
      fit1 <- fitaci(test, fitmethod="bilinear", varnames = list(ALEAF = "Photo", 
                                                                 Tleaf = "Tleaf", 
                                                                 Ci = "Ci",
                                                                 PPFD = "PAR", 
                                                                 Rd = "Rd"),
                     Tcorrect=T, fitTPU = F)
      
      outDF2[outDF2$Position == id.list[i] & outDF2$Chamber == "11", "ALEAF_400"] <- fit1$Photosyn(Ci=400)$ALEAF
      outDF2[outDF2$Position == id.list[i] & outDF2$Chamber == "11", "ALEAF_600"] <- fit1$Photosyn(Ci=600)$ALEAF
      outDF2[outDF2$Position == id.list[i] & outDF2$Chamber == "11", "Ac_400"] <- fit1$Photosyn(Ci=400)$Ac
      outDF2[outDF2$Position == id.list[i] & outDF2$Chamber == "11", "Ac_600"] <- fit1$Photosyn(Ci=600)$Ac
      outDF2[outDF2$Position == id.list[i] & outDF2$Chamber == "11", "Aj_400"] <- fit1$Photosyn(Ci=400)$Aj
      outDF2[outDF2$Position == id.list[i] & outDF2$Chamber == "11", "Aj_600"] <- fit1$Photosyn(Ci=600)$Aj
    }
    
    ### unnormalized sensitivity
    outDF2$A_sens <- with(outDF2, (ALEAF_600-ALEAF_400)/(600-400))
    outDF2$Aj_sens <- with(outDF2, (Aj_600-Aj_400)/(600-400))
    outDF2$Ac_sens <- with(outDF2, (Ac_600-Ac_400)/(600-400))

    ### normalized sensitivity
    outDF2$A_sens_norm <- with(outDF2, (ALEAF_600-ALEAF_400)/ALEAF_400)
    outDF2$Aj_sens_norm <- with(outDF2, (Aj_600-Aj_400)/Aj_400)
    outDF2$Ac_sens_norm <- with(outDF2, (Ac_600-Ac_400)/Ac_400)
    
    ### Source
    outDF2$Source <- c(rep("Leaf", 6), rep("Canopy", 9))
    
    write.csv(outDF2, "output/A-Ca/predicted_A_at_Ci_400_600_ppm.csv", row.names=F)
    
    ### plotting
    p1 <- ggplot() +
      geom_point(data=outDF2, aes(Position, A_sens, 
                                  fill=as.factor(Position), 
                                  pch = as.factor(Source)), alpha=1.0, size=4)+
      theme_linedraw() +
      theme(panel.grid.minor=element_blank(),
            axis.text.x=element_text(size=12),
            axis.title.x=element_text(size=14),
            axis.text.y=element_text(size=12),
            axis.title.y=element_text(size=14),
            legend.text=element_text(size=12),
            legend.title=element_text(size=14),
            panel.grid.major=element_blank(),
            legend.position="bone",
            legend.box = 'vertical',
            legend.box.just = 'left')+
      xlab("")+
      ylab(expression(paste(delta,  "A/", delta, CO[2], " (", mu, "mol ", m^-2, " ", s^-1, " ", ppm^-1, ")")))+
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
      scale_x_discrete(name="", 
                       breaks=c("12345", "345", "45", "up", "low"), 
                       labels=c("Whole", "T+M", "Top", "Up", "Low"))+
      guides(fill = guide_legend(override.aes = list(shape = c(21, 21, 21, 24, 24))))
    
    

    p2 <- ggplot() +
      geom_point(data=outDF2, aes(Position, A_sens_norm, 
                                  fill=as.factor(Position), 
                                  pch = as.factor(Source)), alpha=1.0, size=4)+
      theme_linedraw() +
      theme(panel.grid.minor=element_blank(),
            axis.text.x=element_text(size=12),
            axis.title.x=element_text(size=14),
            axis.text.y=element_text(size=12),
            axis.title.y=element_text(size=14),
            legend.text=element_text(size=12),
            legend.title=element_text(size=14),
            panel.grid.major=element_blank(),
            legend.position="bone",
            legend.box = 'vertical',
            legend.box.just = 'left')+
      xlab("")+
      ylab(expression(paste(delta,  "A/", A[400], " (", mu, "mol ", m^-2, " ", s^-1, ")")))+
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
      scale_x_discrete(name="", 
                       breaks=c("12345", "345", "45", "up", "low"), 
                       labels=c("Whole", "T+M", "Top", "Up", "Low"))+
      guides(fill = guide_legend(override.aes = list(shape = c(21, 21, 21, 24, 24))))
    
    
    
    p3 <- ggplot() +
      geom_point(data=outDF2, aes(Position, Ac_sens, 
                                  fill=as.factor(Position), 
                                  pch = as.factor(Source)), alpha=1.0, size=4)+
      theme_linedraw() +
      theme(panel.grid.minor=element_blank(),
            axis.text.x=element_text(size=12),
            axis.title.x=element_text(size=14),
            axis.text.y=element_text(size=12),
            axis.title.y=element_text(size=14),
            legend.text=element_text(size=12),
            legend.title=element_text(size=14),
            panel.grid.major=element_blank(),
            legend.position="bone",
            legend.box = 'vertical',
            legend.box.just = 'left')+
      xlab("")+
      ylab(expression(paste(delta,  A[c], "/", delta, CO[2], " (", mu, "mol ", m^-2, " ", s^-1, " ", ppm^-1, ")")))+
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
      scale_x_discrete(name="", 
                       breaks=c("12345", "345", "45", "up", "low"), 
                       labels=c("Whole", "T+M", "Top", "Up", "Low"))+
      guides(fill = guide_legend(override.aes = list(shape = c(21, 21, 21, 24, 24))))
    
    
    
    p4 <- ggplot() +
      geom_point(data=outDF2, aes(Position, Ac_sens_norm, 
                                  fill=as.factor(Position), 
                                  pch = as.factor(Source)), alpha=1.0, size=4)+
      theme_linedraw() +
      theme(panel.grid.minor=element_blank(),
            axis.text.x=element_text(size=12),
            axis.title.x=element_text(size=14),
            axis.text.y=element_text(size=12),
            axis.title.y=element_text(size=14),
            legend.text=element_text(size=12),
            legend.title=element_text(size=14),
            panel.grid.major=element_blank(),
            legend.position="bone",
            legend.box = 'vertical',
            legend.box.just = 'left')+
      xlab("")+
      ylab(expression(paste(delta,  A[c], "/", A[c400], " (", mu, "mol ", m^-2, " ", s^-1, ")")))+
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
      scale_x_discrete(name="", 
                       breaks=c("12345", "345", "45", "up", "low"), 
                       labels=c("Whole", "T+M", "Top", "Up", "Low"))+
      guides(fill = guide_legend(override.aes = list(shape = c(21, 21, 21, 24, 24))))
    
    
    
    p5 <- ggplot() +
      geom_point(data=outDF2, aes(Position, Aj_sens, 
                                  fill=as.factor(Position), 
                                  pch = as.factor(Source)), alpha=1.0, size=4)+
      theme_linedraw() +
      theme(panel.grid.minor=element_blank(),
            axis.text.x=element_text(size=12),
            axis.title.x=element_text(size=14),
            axis.text.y=element_text(size=12),
            axis.title.y=element_text(size=14),
            legend.text=element_text(size=12),
            legend.title=element_text(size=14),
            panel.grid.major=element_blank(),
            legend.position="bone",
            legend.box = 'vertical',
            legend.box.just = 'left')+
      xlab("")+
      ylab(expression(paste(delta,  A[j], "/", delta, CO[2], " (", mu, "mol ", m^-2, " ", s^-1, " ", ppm^-1, ")")))+
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
      scale_x_discrete(name="", 
                       breaks=c("12345", "345", "45", "up", "low"), 
                       labels=c("Whole", "T+M", "Top", "Up", "Low"))+
      guides(fill = guide_legend(override.aes = list(shape = c(21, 21, 21, 24, 24))))
    
    
    
    p6 <- ggplot() +
      geom_point(data=outDF2, aes(Position, Aj_sens_norm, 
                                  fill=as.factor(Position), 
                                  pch = as.factor(Source)), alpha=1.0, size=4)+
      theme_linedraw() +
      theme(panel.grid.minor=element_blank(),
            axis.text.x=element_text(size=12),
            axis.title.x=element_text(size=14),
            axis.text.y=element_text(size=12),
            axis.title.y=element_text(size=14),
            legend.text=element_text(size=12),
            legend.title=element_text(size=14),
            panel.grid.major=element_blank(),
            legend.position="bone",
            legend.box = 'vertical',
            legend.box.just = 'left')+
      xlab("")+
      ylab(expression(paste(delta,  A[j], "/", A[j400], " (", mu, "mol ", m^-2, " ", s^-1, ")")))+
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
      scale_x_discrete(name="", 
                       breaks=c("12345", "345", "45", "up", "low"), 
                       labels=c("Whole", "T+M", "Top", "Up", "Low"))+
      guides(fill = guide_legend(override.aes = list(shape = c(21, 21, 21, 24, 24))))
    
    
    
    legend_shared <- get_legend(p1 + theme(legend.position="bottom",
                                           legend.box = 'vertical',
                                           legend.box.just = 'left'))
    
    combined_plots <- plot_grid(p1, p2, p3, p4, p5, p6, 
                                labels="AUTO", ncol=2, align="vh", axis = "l")
    
    #plot(p1)
    
    pdf("output/A-Ca/delta_A_sensitivity_plot.pdf", width=10, height=16)
    plot_grid(combined_plots, legend_shared, ncol=1, rel_heights=c(1,0.1))
    dev.off()  
    
    
}
