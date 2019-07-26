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
                       "corrflux", "Photo", "rh", "my_co2_flux",
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
    #ch03DF[ch03DF$Source=="canopy"&ch03DF$Position=="45", "Photo"] <- ifelse(ch03DF[ch03DF$Source=="canopy"&ch03DF$Position=="45", "Photo"] > 15, 
    #                                                                         NA, ch03DF[ch03DF$Source=="canopy"&ch03DF$Position=="45", "Photo"])
    #
    #
    #ch11DF[ch11DF$Source=="canopy", "Photo"] <- ifelse(ch11DF[ch11DF$Source=="canopy", "Photo"] > 16, 
    #                                                                            NA, ch11DF[ch11DF$Source=="canopy", "Photo"])
    
    #ch11DF <- ch11DF[complete.cases(ch11DF$Photo),]
    #ch03DF <- ch03DF[complete.cases(ch03DF$Photo),]
    
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
    fits.ch03 <- fitacis(ch03DF, group="Position", fitmethod="bilinear", Tcorrect=T)
    fits.ch11 <- fitacis(ch11DF, group="Position", fitmethod="bilinear", Tcorrect=T)
    
    #plot(fits.ch01, how="oneplot")
    #plot(fits.ch01[[1]], col=alpha("black",0.2), add=T)
    #plot(fits.ch01[[2]], col=alpha("blue2", 0.2), add=T)
    #plot(fits.ch01, how="oneplot", what="data", 
    #     pch=c(21, 22, 23, 24, 25), col=T)
    #plot(fits.ch01[[1]], how="oneplot", color="blue", 
    #     add=T, what="model", lwd=3, lty=c(1,2,3,4,5))
    #abline(v=410, add=T, lwd = 2)
    #dev.off()

    ##
    pdf("output/chamber_result_comparison_A_vs_Ci_flux_no_scaling_ambient.pdf", width=8, height=14)
    par(mfrow=c(5,3),mar=c(2,2,4,1),oma = c(4, 6, 0, 0))
    
    ymin <- -2
    ymax <- 30
    title.size <-2
    
    ### first row
    plot(fits.ch01[[5]],lwd=3, col=alpha("black",0.6), pch=21, 
         xlim=c(0, 1600), ylim=c(ymin, ymax))
    abline(v=c(380, 620), lwd=2, lty=c(3,1))
    #mtext("Up leaves", side = 2, line = 1, cex = 1.2)
    
    plot(fits.ch03[[5]],lwd=3, col=alpha("black",0.6), pch=21, main="Up leaves", cex.main=title.size,
         xlim=c(0, 1600), ylim=c(ymin, ymax), addlegend=F)
    abline(v=c(380, 620), lwd=2, lty=c(3,1))
    
    plot(fits.ch11[[5]],lwd=3, col=alpha("black",0.6), pch=21, 
         xlim=c(0, 1600), ylim=c(ymin, ymax), addlegend=F)
    abline(v=c(380, 620), lwd=2, lty=c(3,1))
    
    ### second row
    plot(fits.ch01[[4]],lwd=3, col=alpha("black",0.6), pch=21, addlegend=F, 
         xlim=c(0, 1600), ylim=c(ymin, ymax))
    abline(v=c(380, 620), lwd=2, lty=c(3,1))
    
    plot(fits.ch03[[4]],lwd=3, col=alpha("black",0.6), pch=21, main="Low leaves", cex.main=title.size,addlegend=F, 
         xlim=c(0, 1600), ylim=c(ymin, ymax))
    abline(v=c(380, 620), lwd=2, lty=c(3,1))
    
    plot(fits.ch11[[4]],lwd=3, col=alpha("black",0.6), pch=21, addlegend=F, 
         xlim=c(0, 1600), ylim=c(ymin, ymax))
    abline(v=c(380, 620), lwd=2, lty=c(3,1))
    
    
    ### third row
    plot(fits.ch01[[1]],lwd=3, col=alpha("black",0.6), pch=21, addlegend=F, 
         xlim=c(0, 1600), ylim=c(ymin, ymax))
    abline(v=c(380, 620), lwd=2, lty=c(3,1))
    
    plot(fits.ch03[[1]],lwd=3, col=alpha("black",0.6), pch=21, main="Whole canopy", cex.main=title.size,addlegend=F,
         xlim=c(0, 1600), ylim=c(ymin, ymax))
    abline(v=c(380, 620), lwd=2, lty=c(3,1))
    
    plot(fits.ch11[[1]],lwd=3, col=alpha("black",0.6), pch=21, addlegend=F, 
         xlim=c(0, 1600), ylim=c(ymin, ymax))
    abline(v=c(380, 620), lwd=2, lty=c(3,1))
    
    
    ### fourth row
    plot(fits.ch01[[2]],lwd=3, col=alpha("black",0.6), pch=21, addlegend=F,
         xlim=c(0, 1600), ylim=c(ymin, ymax))
    abline(v=c(380, 620), lwd=2, lty=c(3,1))
    
    plot(fits.ch03[[2]],lwd=3, col=alpha("black",0.6), pch=21, main="T+M canopy", cex.main=title.size,addlegend=F, 
         xlim=c(0, 1600), ylim=c(ymin, ymax))
    abline(v=c(380, 620), lwd=2, lty=c(3,1))
    
    plot(fits.ch11[[2]],lwd=3, col=alpha("black",0.6), pch=21, addlegend=F, 
         xlim=c(0, 1600), ylim=c(ymin, ymax))
    abline(v=c(380, 620), lwd=2, lty=c(3,1))
    
    ### fifth row
    plot(fits.ch01[[3]],lwd=3, col=alpha("black",0.6), pch=21, addlegend=F, 
         xlim=c(0, 1600), ylim=c(ymin, ymax))
    abline(v=c(380, 620), lwd=2, lty=c(3,1))

    plot(fits.ch03[[3]],lwd=3, col=alpha("black",0.6), pch=21, main="Top canopy", cex.main=title.size,addlegend=F, 
         xlim=c(0, 1600), ylim=c(ymin, ymax))
    abline(v=c(380, 620), lwd=2, lty=c(3,1))
    
    plot(fits.ch11[[3]],lwd=3, col=alpha("black",0.6), pch=21, addlegend=F,
         xlim=c(0, 1600), ylim=c(ymin, ymax))
    abline(v=c(380, 620), lwd=2, lty=c(3,1))
    
    # print the overall labels
    mtext(expression(C[i] * " (ppm)"), side = 1, outer = TRUE, line = 2, cex=2)
    mtext(expression(A[n] * " (" * mu * "mol " * m^-2 * " " * s^-1 * ")"), side = 2, outer = TRUE, line = 2, cex=2)
    
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
    
    
    #### export biochemical parameter summary table
    ### make a list of identify
    id.list <- rep(c("up", "low", "12345", "345", "45"), each=3)
    chamber.list <- rep(c(1, 3, 11), by = 5)
    ### prepare an output df
    outDF <- data.frame(id.list, chamber.list, NA, NA,
                        NA, NA, NA, NA, NA, NA,
                        NA, NA, NA, NA, NA, NA,
                        NA, NA, NA, NA, NA, NA,
                        NA, NA, NA, NA, NA)
    colnames(outDF) <- c("Position", "Chamber", 
                         "RMSE", "Vcmax", "Vcmax.se", "Jmax", "Jmax.se", "Rd", "Rd.se",
                         "Ci", "ALEAF", "GS", "ELEAF", "Ac", "Aj", "Ap", "VPD",
                         "Tleaf", "Ca", "Cc", "PPFD", "Ci_transition_Ac_Aj",
                         "curve.fitting", "Patm", "GammaStar", "Km")
    
    
    ### problems here: 
    ### canopy data has gaps and bad data points, so can't use default to fit aci curves.
    ### for example, cannot fit ch01 12345, ch03 345, ch11 45.
    ### Therefore have to use bilinear function,
    ### but some biochemical parameters don't make sense. 
    ### so I probably need to fill the data gaps with a gam function
    ### or simply smooth all the canopy data with gam function.
    ### but then it's not "data" any more, so plotting A-Ca is not real.
    ### 
    
 
    ### the for loop
    for (i in 1:length(id.list)) {
      ## subset each data
      test <- subset(ch01DF, Position == id.list[i])
      
      ## fit
      fit1 <- fitaci(test, fitmethod="bilinear", Tcorrect=T)
      
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
    
    outDF$JV_ratio <- outDF$Jmax / outDF$Vcmax
    
    ### save
    write.csv(outDF, "output/ambient_chambers_biochemical_parameter_summary_table.csv")
    
    
    
    
    #################3## fit the data as whole, i.e. ignore chamber
    combDF <- rbind(ch01DF, ch03DF, ch11DF)
    fits.all <- fitacis(combDF, group="Position", Tcorrect=T)
    
    
    ##
    pdf("output/A_vs_Ci_flux_no_scaling_ambient_ignore_chamber.pdf", width=4, height=14)
    par(mfrow=c(5,1),mar=c(2,2,4,1),oma = c(4, 6, 0, 0))
    
    ymin <- -2
    ymax <- 30
    title.size <-2
    
    ### first row
    plot(fits.all[[5]],lwd=3, col=alpha("black",0.6), pch=21, cex.main=title.size,
         xlim=c(0, 1600), ylim=c(ymin, ymax), main="up leaves")
    abline(v=c(380, 620), lwd=2, lty=c(3,1))
    #mtext("Up leaves", side = 2, line = 1, cex = 1.2)
    
    
    ### second row
    plot(fits.all[[4]],lwd=3, col=alpha("black",0.6), pch=21, main="Low leaves", cex.main=title.size,addlegend=F, 
         xlim=c(0, 1600), ylim=c(ymin, ymax))
    abline(v=c(380, 620), lwd=2, lty=c(3,1))
    
    
    ### third row
   plot(fits.all[[1]],lwd=3, col=alpha("black",0.6), pch=21, main="Whole canopy", cex.main=title.size,addlegend=F,
         xlim=c(0, 1600), ylim=c(ymin, ymax))
    abline(v=c(380, 620), lwd=2, lty=c(3,1))
    
    
    ### fourth row
    plot(fits.all[[2]],lwd=3, col=alpha("black",0.6), pch=21, main="T+M canopy", cex.main=title.size,addlegend=F, 
         xlim=c(0, 1600), ylim=c(ymin, ymax))
    abline(v=c(380, 620), lwd=2, lty=c(3,1))
    
    ### fifth row
    plot(fits.all[[3]],lwd=3, col=alpha("black",0.6), pch=21, main="Top canopy", cex.main=title.size,addlegend=F, 
         xlim=c(0, 1600), ylim=c(ymin, ymax))
    abline(v=c(380, 620), lwd=2, lty=c(3,1))
    
    
    # print the overall labels
    mtext(expression(C[i] * " (ppm)"), side = 1, outer = TRUE, line = 2, cex=2)
    mtext(expression(A[n] * " (" * mu * "mol " * m^-2 * " " * s^-1 * ")"), side = 2, outer = TRUE, line = 2, cex=2)
    
    dev.off()
    
    
  }
