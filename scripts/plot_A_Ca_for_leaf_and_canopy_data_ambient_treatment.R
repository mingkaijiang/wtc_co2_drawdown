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
    
    lDF <- subset(lDF, Date>=as.Date("2009-01-01")&Date<=as.Date("2009-03-01"))
    
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
    p1 <- ggplot() +
      geom_point(data=plotDF, aes(Ca, Photo, 
                                  fill=as.factor(Position), 
                                  pch = as.factor(Source)), alpha=0.6)+
      geom_smooth(data=plotDF, aes(Ca, Photo, group=Position,
                                   col=as.factor(Position)),
                  method = "lm", formula = y ~ splines::bs(x, 4), se=T)+
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
      xlim(0,1800)+
      ylim(-5,40)+
      guides(fill = guide_legend(override.aes = list(shape = c(21, 21, 21, 24, 24))))
    
  
    ### output
    pdf("output/A-Ca/ambient_A-Ca_plot.pdf", width=8, height=8)
    plot(p1)
    dev.off()    
    
    
    ##### preparing acifit
    #### the fit TPU function makes it long to run!
    fits.ch01 <- fitacis(ch01DF, group="Position", fitmethod="bilinear", Tcorrect=T, fitTPU=F)
    fits.ch03 <- fitacis(ch03DF, group="Position", fitmethod="bilinear", Tcorrect=T, fitTPU=F)
    fits.ch11 <- fitacis(ch11DF, group="Position", fitmethod="bilinear", Tcorrect=T, fitTPU=F)


    ##
    pdf("output/A-Ca/ambient_A-Ci_plots.pdf", width=14, height=14)
    par(mfrow=c(5,3),mar=c(2,2,4,1),oma = c(4, 6, 0, 0))
    
    ymin <- -2
    ymax <- 40
    title.size <- 1.5
    
    ### first row
    plot(fits.ch01[[5]],lwd=3, col=alpha("black",0.6), pch=21, main="Leaves: up", cex.main=title.size,
         xlim=c(0, 1600), ylim=c(ymin, ymax))
    abline(v=c(380), lwd=2, lty=c(3))

    plot(fits.ch03[[5]],lwd=3, col=alpha("black",0.6), pch=21, 
         xlim=c(0, 1600), ylim=c(ymin, ymax), addlegend=F)
    abline(v=c(380), lwd=2, lty=c(3))
    
    
    plot(fits.ch11[[5]],lwd=3, col=alpha("black",0.6), pch=21, 
         xlim=c(0, 1600), ylim=c(ymin, ymax), addlegend=F)
    abline(v=c(380), lwd=2, lty=c(3))
    
    
    ### second row
    plot(fits.ch01[[4]],lwd=3, col=alpha("black",0.6), pch=21, addlegend=F, main="Leaves: low", cex.main=title.size,
         xlim=c(0, 1600), ylim=c(ymin, ymax))
    abline(v=c(380), lwd=2, lty=c(3))
    
    plot(fits.ch03[[4]],lwd=3, col=alpha("black",0.6), pch=21, addlegend=F, 
         xlim=c(0, 1600), ylim=c(ymin, ymax))
    abline(v=c(380), lwd=2, lty=c(3))
    
    plot(fits.ch11[[4]],lwd=3, col=alpha("black",0.6), pch=21, addlegend=F, 
         xlim=c(0, 1600), ylim=c(ymin, ymax))
    abline(v=c(380), lwd=2, lty=c(3))
    

    ### third row
    ymax <- 20
    
    plot(fits.ch01[[1]],lwd=3, col=alpha("black",0.6), pch=21, addlegend=F, main="Canopy: whole", cex.main=title.size,
         xlim=c(0, 1600), ylim=c(ymin, ymax))
    abline(v=c(380), lwd=2, lty=c(3))
    
    plot(fits.ch03[[1]],lwd=3, col=alpha("black",0.6), pch=21, addlegend=F,
         xlim=c(0, 1600), ylim=c(ymin, ymax))
    abline(v=c(380), lwd=2, lty=c(3))
    
    plot(fits.ch11[[1]],lwd=3, col=alpha("black",0.6), pch=21, addlegend=F, 
         xlim=c(0, 1600), ylim=c(ymin, ymax))
    abline(v=c(380), lwd=2, lty=c(3))
    
    
    ### fourth row
    plot(fits.ch01[[2]],lwd=3, col=alpha("black",0.6), pch=21, addlegend=F,main="Canopy: T+M", cex.main=title.size,
         xlim=c(0, 1600), ylim=c(ymin, ymax))
    abline(v=c(380), lwd=2, lty=c(3))
    
    plot(fits.ch03[[2]],lwd=3, col=alpha("black",0.6), pch=21, addlegend=F, 
         xlim=c(0, 1600), ylim=c(ymin, ymax))
    abline(v=c(380), lwd=2, lty=c(3))
    
    plot(fits.ch11[[2]],lwd=3, col=alpha("black",0.6), pch=21, addlegend=F, 
         xlim=c(0, 1600), ylim=c(ymin, ymax))
    abline(v=c(380), lwd=2, lty=c(3))
    
    
    ### fifth row
    plot(fits.ch01[[3]],lwd=3, col=alpha("black",0.6), pch=21, addlegend=F, main="Canopy: top", cex.main=title.size,
         xlim=c(0, 1600), ylim=c(ymin, ymax))
    abline(v=c(380), lwd=2, lty=c(3))

    plot(fits.ch03[[3]],lwd=3, col=alpha("black",0.6), pch=21, addlegend=F, 
         xlim=c(0, 1600), ylim=c(ymin, ymax))
    abline(v=c(380), lwd=2, lty=c(3))
    
    
    plot(fits.ch11[[3]],lwd=3, col=alpha("black",0.6), pch=21, addlegend=F,
         xlim=c(0, 1600), ylim=c(ymin, ymax))
    abline(v=c(380), lwd=2, lty=c(3))
    
    # print the overall labels
    mtext(expression(C[i] * " (ppm)"), side = 1, outer = TRUE, line = 2, cex=2)
    mtext(expression(A[n] * " (" * mu * "mol " * m^-2 * " " * s^-1 * ")"), 
          side = 2, outer = TRUE, line = 2, cex=2)
    
    dev.off()
    
    
    #### read biochemical parameter summary table
    ### we have multiple dates in leaf-scale measurements
    
    stDF.c <- read.csv("output/canopy/canopy_scale_parameters.csv", header=T)
    stDF.l <- read.csv("output/leaf/leaf_scale_parameters.csv", header=T)
    
    subDF.l <- subset(stDF.l, Chamber%in%c("ch01", "ch03", "ch11"))
    subDF.l$Date <- as.Date(as.character(subDF.l$Date))
    subDF.l <- subset(subDF.l, Date>=as.Date("2009-01-01")&Date<=as.Date("2009-03-01"))
    
    
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
    

    
    outDF2$A_sens <- with(outDF2, (ALEAF_600-ALEAF_400)/ALEAF_400)
    outDF2$Aj_sens <- with(outDF2, (Aj_600-Aj_400)/Aj_400)
    outDF2$Ac_sens <- with(outDF2, (Ac_600-Ac_400)/Ac_400)
    
    
    
    write.csv(outDF2, "output/A-Ca/predicted_A_values_at_Ci_in_400_600_ppm.csv", row.names=F)
    
    
    
}
