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
    
    #### subset 2009
    lDF$year <- year(lDF$Date)
    lDF$Date <- as.Date(lDF$Date)
    
    ### add type information
    lDF$Type <- "leaf"
    cDF$Type <- "canopy"

    ### select columns
    lDF.sub <- lDF[,c("Identity", "chamber", "Height", "Date",
                     "Photo", "CO2S", "Cond", 
                      "Ci", "Tair", "Tleaf", "PARi", "VpdL", "Type")]
    
    cDF.sub <- cDF[,c("Identity", "Chamber", "Canopy", "date",
                      "Norm_corr_CO2_flux", "WTC_CO2", "Norm_H2O_flux", 
                      "Ci", "WTC_T", "WTC_T", "WTC_PAR", "VPD", 'Type')]
    
    
    colnames(lDF.sub) <- colnames(cDF.sub) <- c("Identity", "Chamber", "Position", "Date",
                                                "Photo", "Ca", "Cond", 
                                                "Ci", "Tair", "Tleaf", "PAR", "VPD", "Type")
    
    myDF <- rbind(lDF.sub, cDF.sub)
    
    myDF$Identity <- as.numeric(as.character(myDF$Identity))
    
    myDF$Chamber <- gsub("ch", "", myDF$Chamber)
    myDF$Chamber <- as.numeric(as.character(myDF$Chamber))
    
    ### assign CO2, water treatment
    ## chambers 1, 3, 5, 7, 9, 11 are CO2 ambient
    ## chambers 1, 3, 4, 6, 8, 11 are wet 
    ## do not include drought treatment chambers
    myDF <- myDF[myDF$Chamber%in%c(1,3,11,4,8),]
    myDF$CO2_treatment <- "aCO2"
    myDF$CO2_treatment[myDF$Chamber%in%c(4,8)] <- "eCO2"
    
    ### generate identity list
    idDF <- unique(myDF[,c("Identity", "Chamber", "Position", 
                           "Type", "CO2_treatment")])
    
    ### individual chambers
    ch01DF <- subset(myDF, Chamber == 1)
    ch03DF <- subset(myDF, Chamber == 3)
    ch11DF <- subset(myDF, Chamber == 11)
    ch04DF <- subset(myDF, Chamber == 4)
    ch08DF <- subset(myDF, Chamber == 8)

    ### combine only aCO2 and wet treatment
    plotDF1 <- rbind(ch01DF, ch03DF, ch11DF)
    plotDF2 <- rbind(ch04DF, ch08DF)
    
    ### plot a subset range
    ### i.e. Ca = 400 to 600
    subDF1 <- subset(plotDF1, Ca>=350 & Ca <= 650)
    subDF2 <- subset(plotDF2, Ca>=350 & Ca <= 650)
    
    ## plot
    p1 <- ggplot(data=plotDF1, aes(Ca, Photo, group=Position)) +
      geom_point(data=plotDF1, aes(fill=as.factor(Position), 
                                  pch = as.factor(Type)), alpha=0.6)+
      geom_smooth(aes(col=as.factor(Position)), 
                  method="nls", 
                  formula=y~a*exp(b/x),
                  fullrange=T,
                  method.args = list(start=c(a=1,b=0.1)), 
                  se=F)+
      theme_linedraw() +
      theme(panel.grid.minor=element_blank(),
            axis.text.x=element_text(size=12),
            axis.title.x=element_blank(),
            axis.text.y=element_text(size=12),
            axis.title.y=element_text(size=14),
            legend.text=element_text(size=12),
            legend.title=element_text(size=14),
            panel.grid.major=element_blank(),
            legend.position="none",
            legend.box = 'vertical',
            legend.box.just = 'left')+
      xlab(expression(paste(C[a], " (", mu, "mol ", mol^-1, ")")))+
      ylab(expression(paste(A, " (", mu, "mol "* CO[2], " ", m^-2, " ", s^-1, ")")))+
      scale_fill_manual(name="Position",
                        limits=c("12345", "345", "45", "up", "low"),
                        values=c("blue2", "red3", "purple", "orange", "green"),
                        labels=c("Whole", "T+M", "Top", "Up", "Low"))+
      scale_color_manual(name="Position",
                         limits=c("12345", "345", "45", "up", "low"),
                         values=c("blue2", "red3", "purple", "orange", "darkgreen"),
                         labels=c("Whole", "T+M", "Top", "Up", "Low"))+
      scale_shape_manual(name="Type",
                         values=c(21, 24),
                         labels=c("Canopy", "Leaf"))+
      xlim(0,1250)+
      ylim(-5,50)+
      guides(fill = guide_legend(override.aes = list(shape = c(21, 21, 21, 24, 24))))
    
    
    ## plot
    p2 <- ggplot() +
      geom_point(data=subDF1, aes(Ca, Photo, 
                                  fill=as.factor(Position), 
                                  pch = as.factor(Type)), alpha=0.6)+
      geom_smooth(data=subDF1, aes(Ca, Photo, group=Position,
                                   col=as.factor(Position)),
                  method = "lm", formula = y ~ x, se=T)+
      theme_linedraw() +
      theme(panel.grid.minor=element_blank(),
            axis.text.x=element_text(size=12),
            axis.title.x=element_blank(),
            axis.text.y=element_text(size=12),
            axis.title.y=element_blank(),
            legend.text=element_text(size=12),
            legend.title=element_text(size=14),
            panel.grid.major=element_blank(),
            legend.position="none",
            legend.box = 'vertical',
            legend.box.just = 'left')+
      xlab(expression(paste(C[a], " (", mu, "mol ", mol^-1, ")")))+
      ylab(expression(paste(A, " (", mu, "mol "* CO[2], " ", m^-2, " ", s^-1, ")")))+
      scale_fill_manual(name="Position",
                        limits=c("12345", "345", "45", "up", "low"),
                        values=c("blue2", "red3", "purple", "orange", "green"),
                        labels=c("Whole", "T+M", "Top", "Up", "Low"))+
      scale_color_manual(name="Position",
                         limits=c("12345", "345", "45", "up", "low"),
                         values=c("blue2", "red3", "purple", "orange", "darkgreen"),
                         labels=c("Whole", "T+M", "Top", "Up", "Low"))+
      scale_shape_manual(name="Type",
                         values=c(21, 24),
                         labels=c("Canopy", "Leaf"))+
      xlim(350,650)+
      ylim(-5,40)+
      guides(fill = guide_legend(override.aes = list(shape = c(21, 21, 21, 24, 24))))
    
    
    ### elevated CO2 treatment
    p3 <- ggplot(data=plotDF2, aes(Ca, Photo, group=Position)) +
      geom_point(data=plotDF2, aes(fill=as.factor(Position), 
                                   pch = as.factor(Type)), alpha=0.6)+
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
            legend.position="none",
            legend.box = 'vertical',
            legend.box.just = 'left')+
      xlab(expression(paste(C[a], " (", mu, "mol ", mol^-1, ")")))+
      ylab(expression(paste(A, " (", mu, "mol "* CO[2], " ", m^-2, " ", s^-1, ")")))+
      scale_fill_manual(name="Position",
                        limits=c("12345", "345", "45", "up", "low"),
                        values=c("blue2", "red3", "purple", "orange", "green"),
                        labels=c("Whole", "T+M", "Top", "Up", "Low"))+
      scale_color_manual(name="Position",
                         limits=c("12345", "345", "45", "up", "low"),
                         values=c("blue2", "red3", "purple", "orange", "darkgreen"),
                         labels=c("Whole", "T+M", "Top", "Up", "Low"))+
      scale_shape_manual(name="Type",
                         values=c(21, 24),
                         labels=c("Canopy", "Leaf"))+
      xlim(0,1250)+
      ylim(-5,50)+
      guides(fill = guide_legend(override.aes = list(shape = c(21, 21, 21, 24, 24))))
    
    
    ## plot
    p4 <- ggplot() +
      geom_point(data=subDF2, aes(Ca, Photo, 
                                  fill=as.factor(Position), 
                                  pch = as.factor(Type)), alpha=0.6)+
      geom_smooth(data=subDF2, aes(Ca, Photo, group=Position,
                                   col=as.factor(Position)),
                  method = "lm", formula = y ~ x, se=T)+
      theme_linedraw() +
      theme(panel.grid.minor=element_blank(),
            axis.text.x=element_text(size=12),
            axis.title.x=element_text(size=14),
            axis.text.y=element_text(size=12),
            axis.title.y=element_blank(),
            legend.text=element_text(size=12),
            legend.title=element_text(size=14),
            panel.grid.major=element_blank(),
            legend.position="none",
            legend.box = 'vertical',
            legend.box.just = 'left')+
      xlab(expression(paste(C[a], " (", mu, "mol ", mol^-1, ")")))+
      ylab(expression(paste(A, " (", mu, "mol "* CO[2], " ", m^-2, " ", s^-1, ")")))+
      scale_fill_manual(name="Position",
                        limits=c("12345", "345", "45", "up", "low"),
                        values=c("blue2", "red3", "purple", "orange", "green"),
                        labels=c("Whole", "T+M", "Top", "Up", "Low"))+
      scale_color_manual(name="Position",
                         limits=c("12345", "345", "45", "up", "low"),
                         values=c("blue2", "red3", "purple", "orange", "darkgreen"),
                         labels=c("Whole", "T+M", "Top", "Up", "Low"))+
      scale_shape_manual(name="Type",
                         values=c(21, 24),
                         labels=c("Canopy", "Leaf"))+
      xlim(350,650)+
      ylim(-5,40)+
      guides(fill = guide_legend(override.aes = list(shape = c(21, 21, 21, 24, 24))))
    
    
    ## plot A-Ci
    p5 <- ggplot() +
      geom_point(data=subDF1, aes(Ci, Photo, 
                                 fill=as.factor(Position), 
                                 pch = as.factor(Type)), alpha=0.6)+
      geom_smooth(data=subDF1, aes(Ci, Photo, group=Position,
                                  col=as.factor(Position)),
                  method = "lm", formula = y ~ x, se=T)+
      theme_linedraw() +
      theme(panel.grid.minor=element_blank(),
            axis.text.x=element_text(size=12),
            axis.title.x=element_blank(),
            axis.text.y=element_text(size=12),
            axis.title.y=element_blank(),
            legend.text=element_text(size=12),
            legend.title=element_text(size=14),
            panel.grid.major=element_blank(),
            legend.position="none",
            legend.box = 'vertical',
            legend.box.just = 'left')+
      xlab(expression(paste(C[i], " (", mu, "mol ", mol^-1, ")")))+
      ylab(expression(paste(A, " (", mu, "mol "* CO[2], " ", m^-2, " ", s^-1, ")")))+
      scale_fill_manual(name="Position",
                        limits=c("12345", "345", "45", "up", "low"),
                        values=c("blue2", "red3", "purple", "orange", "green"),
                        labels=c("Whole", "T+M", "Top", "Up", "Low"))+
      scale_color_manual(name="Position",
                         limits=c("12345", "345", "45", "up", "low"),
                         values=c("blue2", "red3", "purple", "orange", "darkgreen"),
                         labels=c("Whole", "T+M", "Top", "Up", "Low"))+
      scale_shape_manual(name="Type",
                         values=c(21, 24),
                         labels=c("Canopy", "Leaf"))+
      xlim(150,600)+
      ylim(-5,40)+
      guides(fill = guide_legend(override.aes = list(shape = c(21, 21, 21, 24, 24))))
    
    
    p6 <- ggplot() +
      geom_point(data=subDF2, aes(Ci, Photo, 
                                  fill=as.factor(Position), 
                                  pch = as.factor(Type)), alpha=0.6)+
      geom_smooth(data=subDF2, aes(Ci, Photo, group=Position,
                                   col=as.factor(Position)),
                  method = "lm", formula = y ~ x, se=T)+
      theme_linedraw() +
      theme(panel.grid.minor=element_blank(),
            axis.text.x=element_text(size=12),
            axis.title.x=element_text(size=14),
            axis.text.y=element_text(size=12),
            axis.title.y=element_blank(),
            legend.text=element_text(size=12),
            legend.title=element_text(size=14),
            panel.grid.major=element_blank(),
            legend.position="none",
            legend.box = 'vertical',
            legend.box.just = 'left')+
      xlab(expression(paste(C[i], " (", mu, "mol ", mol^-1, ")")))+
      ylab(expression(paste(A, " (", mu, "mol "* CO[2], " ", m^-2, " ", s^-1, ")")))+
      scale_fill_manual(name="Position",
                        limits=c("12345", "345", "45", "up", "low"),
                        values=c("blue2", "red3", "purple", "orange", "green"),
                        labels=c("Whole", "T+M", "Top", "Up", "Low"))+
      scale_color_manual(name="Position",
                         limits=c("12345", "345", "45", "up", "low"),
                         values=c("blue2", "red3", "purple", "orange", "darkgreen"),
                         labels=c("Whole", "T+M", "Top", "Up", "Low"))+
      scale_shape_manual(name="Type",
                         values=c(21, 24),
                         labels=c("Canopy", "Leaf"))+
      xlim(150,600)+
      ylim(-5,40)+
      guides(fill = guide_legend(override.aes = list(shape = c(21, 21, 21, 24, 24))))
    
    
    ### output
    legend_shared <- get_legend(p1 + theme(legend.position="bottom",
                                           legend.box = 'vertical',
                                           legend.box.just = 'left'))
    
    combined_plots <- plot_grid(p1, p2, p5, p3, p4, p6, 
                                labels="auto", ncol=3, align="vh", axis = "l")
    
    pdf("output/A-Ca/A-Ca_plots.pdf", width=12, height=10)
    plot_grid(combined_plots, legend_shared, ncol=1, rel_heights=c(1,0.1))
    dev.off() 
    
    
    
    ################################# Fit A-CI #################################
    ##### preparing acifit
    #### the fit TPU function makes it long to run!
    #fits <- fitacis(myDF, group="Identity", varnames = list(ALEAF="Photo",
    #                                                        Tleaf="Tleaf", 
    #                                                        Ci = "Ci",
    #                                                        PPFD="PAR"),
    #                fitmethod="bilinear", Tcorrect=T, fitTPU=F)
    #
    #coefDF <- coef(fits)
    #coefDF <- merge(coefDF, idDF, by="Identity", all=T)
    
    
    ## ambient A-Ci plots, leaf and canopy
    #pdf("output/A-Ca/ambient_A-Ci_plots.pdf", width=14, height=14)
    #par(mfrow=c(5,3),mar=c(2,2,4,1),oma = c(4, 6, 0, 0))
    #
    #ymin <- -2
    #ymax <- 40
    #title.size <- 1.5
    #
    #### first row
    #plot(fits.ch01[[5]],lwd=3, col=alpha("black",0.6), pch=21, main="Leaves: up", cex.main=title.size,
    #     xlim=c(0, 1200), ylim=c(ymin, ymax))
    #abline(v=c(320, 512), lwd=2, lty=c(1, 3))
#
    #plot(fits.ch03[[5]],lwd=3, col=alpha("black",0.6), pch=21, 
    #     xlim=c(0, 1200), ylim=c(ymin, ymax), addlegend=F)
    #abline(v=c(320, 512), lwd=2, lty=c(1, 3))
    #
    #
    #plot(fits.ch11[[5]],lwd=3, col=alpha("black",0.6), pch=21, 
    #     xlim=c(0, 1200), ylim=c(ymin, ymax), addlegend=F)
    #abline(v=c(320, 512), lwd=2, lty=c(1, 3))
    #
    #
    #### second row
    #plot(fits.ch01[[4]],lwd=3, col=alpha("black",0.6), pch=21, addlegend=F, main="Leaves: low", cex.main=title.size,
    #     xlim=c(0, 1200), ylim=c(ymin, ymax))
    #abline(v=c(320, 512), lwd=2, lty=c(1, 3))
    #
    #plot(fits.ch03[[4]],lwd=3, col=alpha("black",0.6), pch=21, addlegend=F, 
    #     xlim=c(0, 1200), ylim=c(ymin, ymax))
    #abline(v=c(320, 512), lwd=2, lty=c(1, 3))
    #
    #plot(fits.ch11[[4]],lwd=3, col=alpha("black",0.6), pch=21, addlegend=F, 
    #     xlim=c(0, 1200), ylim=c(ymin, ymax))
    #abline(v=c(320, 512), lwd=2, lty=c(1, 3))
    #
#
    #### third row
    #ymax <- 20
    #
    #plot(fits.ch01[[1]],lwd=3, col=alpha("black",0.6), pch=21, addlegend=F, main="Canopy: whole", cex.main=title.size,
    #     xlim=c(0, 1200), ylim=c(ymin, ymax))
    #abline(v=c(320, 512), lwd=2, lty=c(1, 3))
    #
    #plot(fits.ch03[[1]],lwd=3, col=alpha("black",0.6), pch=21, addlegend=F,
    #     xlim=c(0, 1200), ylim=c(ymin, ymax))
    #abline(v=c(320, 512), lwd=2, lty=c(1, 3))
    #
    #plot(fits.ch11[[1]],lwd=3, col=alpha("black",0.6), pch=21, addlegend=F, 
    #     xlim=c(0, 1200), ylim=c(ymin, ymax))
    #abline(v=c(320, 512), lwd=2, lty=c(1, 3))
    #
    #
    #### fourth row
    #plot(fits.ch01[[2]],lwd=3, col=alpha("black",0.6), pch=21, addlegend=F,main="Canopy: T+M", cex.main=title.size,
    #     xlim=c(0, 1200), ylim=c(ymin, ymax))
    #abline(v=c(320, 512), lwd=2, lty=c(1, 3))
    #
    #plot(fits.ch03[[2]],lwd=3, col=alpha("black",0.6), pch=21, addlegend=F, 
    #     xlim=c(0, 1200), ylim=c(ymin, ymax))
    #abline(v=c(320, 512), lwd=2, lty=c(1, 3))
    #
    #plot(fits.ch11[[2]],lwd=3, col=alpha("black",0.6), pch=21, addlegend=F, 
    #     xlim=c(0, 1200), ylim=c(ymin, ymax))
    #abline(v=c(320, 512), lwd=2, lty=c(1, 3))
    #
    #
    #### fifth row
    #plot(fits.ch01[[3]],lwd=3, col=alpha("black",0.6), pch=21, addlegend=F, main="Canopy: top", cex.main=title.size,
    #     xlim=c(0, 1200), ylim=c(ymin, ymax))
    #abline(v=c(320, 512), lwd=2, lty=c(1, 3))
    #
    #plot(fits.ch03[[3]],lwd=3, col=alpha("black",0.6), pch=21, addlegend=F, 
    #     xlim=c(0, 1200), ylim=c(ymin, ymax))
    #abline(v=c(320, 512), lwd=2, lty=c(1, 3))
    #
    #
    #plot(fits.ch11[[3]],lwd=3, col=alpha("black",0.6), pch=21, addlegend=F,
    #     xlim=c(0, 1200), ylim=c(ymin, ymax))
    #abline(v=c(320, 512), lwd=2, lty=c(1, 3))
    #
    ## print the overall labels
    #mtext(expression(C[i] * " (" * mu * "mol " * mol^-1 * ")"), side = 1, outer = TRUE, line = 2, cex=2)
    #mtext(expression(A * " (" * mu * "mol " * CO[2] * m^-2 * " " * s^-1 * ")"), 
    #      side = 2, outer = TRUE, line = 2, cex=2)
    #
    #dev.off()
    
    
    ################################# plot statistics #################################
    #### read biochemical parameter summary table
    ### we have multiple dates in leaf-scale measurements
    stDF.c <- read.csv("output/canopy/canopy_scale_parameters.csv", header=T)
    stDF.l <- read.csv("output/leaf/leaf_scale_parameters.csv", header=T)
    
    ### subset chambers
    subDF.l <- subset(stDF.l, Chamber%in%c("ch01", "ch03", "ch11", "ch04", "ch08"))
    subDF.c <- subset(stDF.c, Chamber%in%c("1", "3", "11", "4", "8"))
    
    ### subset columns
    subDF.l <- subDF.l[,c("Identity", "RMSE", "Vcmax", "Vcmax.se", "Jmax",
                          "Jmax.se", "Rd", "Rd.se", "Ci", "ALEAF", "GS", "ELEAF", "Ac",
                          "Aj", "Ap", "VPD", "Tleaf", "Ca", "Cc", "PPFD", 
                          "Ci_transition_Ac_Aj", "GammaStar", "Km", "G1", "JVratio")]
    
    subDF.c <- subDF.c[,c("Identity", "RMSE", "Vcmax", "Vcmax.se", "Jmax",
                          "Jmax.se", "Rd", "Rd.se", "Ci", "ALEAF", "GS", "ELEAF", "Ac",
                          "Aj", "Ap", "VPD", "Tleaf", "Ca", "Cc", "PPFD", 
                          "Ci_transition_Ac_Aj", "GammaStar", "Km", "G1", "JVratio")]
    
    ### change col names
    stDF <- rbind(subDF.l, subDF.c)
    stDF <- merge(stDF, idDF, by="Identity", all=T)
    
    ### separate into aCO2 and eCO2 DF
    plotDF1 <- subset(stDF, CO2_treatment == "aCO2")
    plotDF2 <- subset(stDF, CO2_treatment == "eCO2")
    
    ### convert into factors
    stDF$Type <- as.factor(stDF$Type)
    stDF$CO2_treatment <- as.factor(stDF$CO2_treatment)
    plotDF1$Type <- as.factor(plotDF1$Type)
    plotDF2$Type <- as.factor(plotDF2$Type)
    
    ### build a linear model to compare position only
    #mod <- gls(Jmax ~ Position,
    #           data=plotDF1)
    #
    #coef(summary(mod)) 
    #anova(mod, type = "marginal")
    #
    #library(emmeans)
    #pairs(emmeans(mod, "Position"))
    
    ### nested anova
    #summary(aov(Jmax ~ Type + Position, plotDF1))
    #contrasts(plotDF1$Position) <- contr.sum
    #library(car)
    #Anova(aov(Jmax ~ Type/Position, plotDF1))
    
    
    ### perform linear mixed effect model statistics
    ## including all factors
    mod <- lmer(Jmax~ CO2_treatment + Type + (1|Position), data=stDF)
    anova(mod)
    rand(mod)
    # result: 
    #         CO2 treatment is not signfiicant
    #         Type is marginally significant
    #         random effect (position) is significant
    
    ## aCO2
    mod1 <- lmer(Jmax~ Type + (1|Position), data=plotDF1)
    anova(mod1)
    rand(mod1)
    # result:
    #        Type is marginally significant
    #        random effect (position) is significant
    
    mod2 <- lmer(Jmax~ Type + (1|Position), data=plotDF2)
    anova(mod2)
    rand(mod2)
    # result:
    #        Type is marginally significant
    #        random effect (position) is not significant
    
    data.lme <- lme(Jmax ~ CO2_treatment + Type, random = ~1 | Position, stDF)
    summary(data.lme)
    anova(data.lme)
    summary(glht(data.lme, linfct = mcp(Type = "Tukey")))
    summary(glht(data.lme, linfct = mcp(CO2_treatment = "Tukey")))

    
    ### plotting
    p1 <- ggplot() +
      geom_point(data=plotDF1, aes(Position, Vcmax, 
                                  fill=as.factor(Position), 
                                  pch = as.factor(Type)), alpha=1.0, size=4)+
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
      scale_shape_manual(name="Type",
                         values=c(21, 24),
                         labels=c("Canopy", "Leaf"))+
      scale_x_discrete(name="", 
                       breaks=c("12345", "345", "45", "up", "low"), 
                       labels=c("Whole", "T+M", "Top", "Up", "Low"))+
      guides(fill = guide_legend(override.aes = list(shape = c(21, 21, 21, 24, 24))))
    
    
    p2 <- ggplot() +
      geom_point(data=plotDF1, aes(Position, Jmax, 
                                  fill=as.factor(Position), 
                                  pch = as.factor(Type)), alpha=1.0, size=4)+
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
      scale_shape_manual(name="Type",
                         values=c(21, 24),
                         labels=c("Canopy", "Leaf"))+
      scale_x_discrete(name="", 
                       breaks=c("12345", "345", "45", "up", "low"), 
                       labels=c("Whole", "T+M", "Top", "Up", "Low"))+
      guides(fill = guide_legend(override.aes = list(shape = c(21, 21, 21, 24, 24))))
    
    
    p3 <- ggplot() +
      geom_point(data=plotDF1, aes(Position, JVratio, 
                                  fill=as.factor(Position), 
                                  pch = as.factor(Type)), alpha=1.0, size=4)+
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
      xlab("")+
      ylab(expression(paste(J[max] * "/" * V[cmax] * " ratio")))+
      scale_fill_manual(name="Position",
                        limits=c("12345", "345", "45", "up", "low"),
                        values=c("blue2", "red3", "purple", "orange", "green"),
                        labels=c("Whole", "T+M", "Top", "Up", "Low"))+
      scale_color_manual(name="Position",
                         limits=c("12345", "345", "45", "up", "low"),
                         values=c("blue2", "red3", "purple", "orange", "darkgreen"),
                         labels=c("Whole", "T+M", "Top", "Up", "Low"))+
      scale_shape_manual(name="Type",
                         values=c(21, 24),
                         labels=c("Canopy", "Leaf"))+
      scale_x_discrete(name="", 
                       breaks=c("12345", "345", "45", "up", "low"), 
                       labels=c("Whole", "T+M", "Top", "Up", "Low"))+
      guides(fill = guide_legend(override.aes = list(shape = c(21, 21, 21, 24, 24))))
    
    
    p4 <- ggplot() +
      geom_point(data=plotDF1, aes(Position, Ci_transition_Ac_Aj, 
                                  fill=as.factor(Position), 
                                  pch = as.factor(Type)), alpha=1.0, size=4)+
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
      xlab("")+
      ylab(expression(paste("Transition " * C[i] * " (" * mu * "mol" * " " * mol^-1 * ")")))+
      scale_fill_manual(name="Position",
                        limits=c("12345", "345", "45", "up", "low"),
                        values=c("blue2", "red3", "purple", "orange", "green"),
                        labels=c("Whole", "T+M", "Top", "Up", "Low"))+
      scale_color_manual(name="Position",
                         limits=c("12345", "345", "45", "up", "low"),
                         values=c("blue2", "red3", "purple", "orange", "darkgreen"),
                         labels=c("Whole", "T+M", "Top", "Up", "Low"))+
      scale_shape_manual(name="Type",
                         values=c(21, 24),
                         labels=c("Canopy", "Leaf"))+
      scale_x_discrete(name="", 
                       breaks=c("12345", "345", "45", "up", "low"), 
                       labels=c("Whole", "T+M", "Top", "Up", "Low"))+
      guides(fill = guide_legend(override.aes = list(shape = c(21, 21, 21, 24, 24))))
    

    legend_shared <- get_legend(p1 + theme(legend.position="bottom",
                                           legend.box = 'vertical',
                                           legend.box.just = 'left'))
    
    combined_plots <- plot_grid(p1, p2, p3, p4, 
                                labels="auto", ncol=2, align="vh", axis = "l")
    
    pdf("output/A-Ca/ambient_biochemical_parameter_plot.pdf", width=10, height=10)
    plot_grid(combined_plots, legend_shared, ncol=1, rel_heights=c(1,0.1))
    dev.off()  
    
    
    
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
      
      outDF2[outDF2$Position == id.list[i] & outDF2$Chamber == "1", "ALEAF_400"] <- fit1$Photosyn(Ca=400)$ALEAF
      outDF2[outDF2$Position == id.list[i] & outDF2$Chamber == "1", "ALEAF_600"] <- fit1$Photosyn(Ca=600)$ALEAF
      outDF2[outDF2$Position == id.list[i] & outDF2$Chamber == "1", "Ac_400"] <- fit1$Photosyn(Ca=400)$Ac
      outDF2[outDF2$Position == id.list[i] & outDF2$Chamber == "1", "Ac_600"] <- fit1$Photosyn(Ca=600)$Ac
      outDF2[outDF2$Position == id.list[i] & outDF2$Chamber == "1", "Aj_400"] <- fit1$Photosyn(Ca=400)$Aj
      outDF2[outDF2$Position == id.list[i] & outDF2$Chamber == "1", "Aj_600"] <- fit1$Photosyn(Ca=600)$Aj
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
      
      outDF2[outDF2$Position == id.list[i] & outDF2$Chamber == "3", "ALEAF_400"] <- fit1$Photosyn(Ca=400)$ALEAF
      outDF2[outDF2$Position == id.list[i] & outDF2$Chamber == "3", "ALEAF_600"] <- fit1$Photosyn(Ca=600)$ALEAF
      outDF2[outDF2$Position == id.list[i] & outDF2$Chamber == "3", "Ac_400"] <- fit1$Photosyn(Ca=400)$Ac
      outDF2[outDF2$Position == id.list[i] & outDF2$Chamber == "3", "Ac_600"] <- fit1$Photosyn(Ca=600)$Ac
      outDF2[outDF2$Position == id.list[i] & outDF2$Chamber == "3", "Aj_400"] <- fit1$Photosyn(Ca=400)$Aj
      outDF2[outDF2$Position == id.list[i] & outDF2$Chamber == "3", "Aj_600"] <- fit1$Photosyn(Ca=600)$Aj
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
      
      outDF2[outDF2$Position == id.list[i] & outDF2$Chamber == "11", "ALEAF_400"] <- fit1$Photosyn(Ca=400)$ALEAF
      outDF2[outDF2$Position == id.list[i] & outDF2$Chamber == "11", "ALEAF_600"] <- fit1$Photosyn(Ca=600)$ALEAF
      outDF2[outDF2$Position == id.list[i] & outDF2$Chamber == "11", "Ac_400"] <- fit1$Photosyn(Ca=400)$Ac
      outDF2[outDF2$Position == id.list[i] & outDF2$Chamber == "11", "Ac_600"] <- fit1$Photosyn(Ca=600)$Ac
      outDF2[outDF2$Position == id.list[i] & outDF2$Chamber == "11", "Aj_400"] <- fit1$Photosyn(Ca=400)$Aj
      outDF2[outDF2$Position == id.list[i] & outDF2$Chamber == "11", "Aj_600"] <- fit1$Photosyn(Ca=600)$Aj
    }
    
    ### unnormalized sensitivity
    outDF2$A_sens <- with(outDF2, (ALEAF_600-ALEAF_400)/(600-400))
    outDF2$Aj_sens <- with(outDF2, (Aj_600-Aj_400)/(600-400))
    outDF2$Ac_sens <- with(outDF2, (Ac_600-Ac_400)/(600-400))

    ### normalized sensitivity
    outDF2$A_sens_norm <- with(outDF2, (ALEAF_600-ALEAF_400)/ALEAF_400)
    outDF2$Aj_sens_norm <- with(outDF2, (Aj_600-Aj_400)/Aj_400)
    outDF2$Ac_sens_norm <- with(outDF2, (Ac_600-Ac_400)/Ac_400)
    
    ### Type
    outDF2$Type <- c(rep("Leaf", 6), rep("Canopy", 9))
    
    write.csv(outDF2, "output/A-Ca/predicted_A_at_Ci_400_600_ppm.csv", row.names=F)
    
    ### plotting
    p1 <- ggplot() +
      geom_point(data=outDF2, aes(Position, A_sens, 
                                  fill=as.factor(Position), 
                                  pch = as.factor(Type)), alpha=1.0, size=4)+
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
      xlab("")+
      ylab(expression(paste(delta,  "A / ", delta, CO[2], " (", mu, "mol ", m^-2, " ", s^-1, " ", ppm^-1, ")")))+
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
                                  pch = as.factor(Type)), alpha=1.0, size=4)+
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
      xlab("")+
      ylab(expression(paste(delta,  "A / ", A[400])))+
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
                                  pch = as.factor(Type)), alpha=1.0, size=4)+
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
      xlab("")+
      ylab(expression(paste(delta,  A[c], " / ", delta, CO[2], " (", mu, "mol ", m^-2, " ", s^-1, " ", ppm^-1, ")")))+
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
                                  pch = as.factor(Type)), alpha=1.0, size=4)+
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
      xlab("")+
      ylab(expression(paste(delta,  A[c], " / ", A[c400])))+
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
                                  pch = as.factor(Type)), alpha=1.0, size=4)+
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
      xlab("")+
      ylab(expression(paste(delta,  A[j], " / ", delta, CO[2], " (", mu, "mol ", m^-2, " ", s^-1, " ", ppm^-1, ")")))+
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
                                  pch = as.factor(Type)), alpha=1.0, size=4)+
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
      xlab("")+
      ylab(expression(paste(delta,  A[j], " / ", A[j400])))+
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
    
    pdf("output/A-Ca/ambient_A_sensitivity_plot.pdf", width=10, height=14)
    plot_grid(combined_plots, legend_shared, ncol=1, rel_heights=c(1,0.1))
    dev.off()  
    
    
}
