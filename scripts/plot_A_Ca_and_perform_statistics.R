plot_A_Ca_and_perform_statistics <- function(cDF) {
    #### This script plots A-CA curve for leaf and canopy data on to the same plot
    #### and compare the shapes
    #### which can be related to Rogers et al. 2017 conceptual figure
    #### Only based on a subset of data that is well-watered treatment
    
  
    ################################# plot A-Ca #################################
    #### read in leaf-scale data
    lDF1 <- read.csv("data/ACi_curves/HFE_Aci_2008-2009.csv",stringsAsFactors=FALSE)
    lDF2  <- read.csv("data/ACi_curves/HFE_Aci_lowcanopy_2008-2009.csv",stringsAsFactors=FALSE)
    
    ### combine the datasets
    lDF <- rbind(lDF1, lDF2)
    
    #### subset 2009
    lDF$year <- year(lDF$Date)
    lDF$Date <- as.Date(lDF$Date)
    
    ### plot leaf Tleaf and Tair
    #with(lDF, plot(Tleaf~Tair, xlim=c(20, 35), ylim=c(20,35)))
    #mod <- lm(Tleaf~Tair, data=lDF)
    #abline(a=coef(mod)[1], b=coef(mod)[2])
    
    ### add type information
    lDF$Type <- "leaf"
    cDF$Type <- "canopy"

    ### select columns
    lDF.sub <- lDF[,c("Identity", "chamber", "Height", "Date",
                     "Photo", "CO2S", "Cond", 
                      "Ci", "Tair", "Tleaf", "PARi", "VpdL", "Type")]
    
    cDF.sub <- cDF[,c("Identity", "Chamber", "Canopy", "date",
                      "Norm_corr_CO2_flux", "WTC_CO2", "Norm_H2O_flux", 
                      "Ci", "WTC_T", "Tleaf", "WTC_PAR", "VPD", 'Type')]
    
    
    colnames(lDF.sub) <- colnames(cDF.sub) <- c("Identity", "Chamber", "Position", "Date",
                                                "Photo", "Ca", "Cond", 
                                                "Ci", "Tair", "Tleaf", "PAR", "VPD", "Type")
    
    ### adjust WTC Tleaf
    #cDF.sub$Tleaf <- cDF.sub$Tair * coef(mod)[2] + coef(mod)[1]
    
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
    
    
    ### create slope DF to store the slops
    slpDF1 <- data.frame(unique(subDF1$Identity), NA, NA, NA, NA, NA)
    slpDF2 <- data.frame(unique(subDF2$Identity), NA, NA, NA, NA, NA)
    colnames(slpDF1) <- colnames(slpDF2) <- c("Identity", "slope", "intercept", 
                                              "A400", "A600", "sens")

    slpDF1 <- merge(slpDF1, idDF, by="Identity")
    slpDF2 <- merge(slpDF2, idDF, by="Identity")
    
    ### calculate linear slope and intercept
    for (i in unique(slpDF1$Identity)) {
      subDF <- subset(subDF1, Identity == i)
      
      ## linear fit
      lm.mod <- lm(Photo~Ca, data=subDF)
      
      ## coefficients
      slpDF1$slope[slpDF1$Identity==i] <- coef(lm.mod)[2]
      slpDF1$intercept[slpDF1$Identity==i] <- coef(lm.mod)[1]
      slpDF1$A400[slpDF1$Identity==i] <- coef(lm.mod)[2] * 400 + coef(lm.mod)[1]
      slpDF1$A600[slpDF1$Identity==i] <- coef(lm.mod)[2] * 600 + coef(lm.mod)[1]
    }
    
    for (i in unique(slpDF2$Identity)) {
      subDF <- subset(subDF2, Identity == i)
      
      ## linear fit
      lm.mod <- lm(Photo~Ca, data=subDF)
      
      ## coefficients
      slpDF2$slope[slpDF2$Identity==i] <- coef(lm.mod)[2]
      slpDF2$intercept[slpDF2$Identity==i] <- coef(lm.mod)[1]
      slpDF2$A400[slpDF2$Identity==i] <- coef(lm.mod)[2] * 400 + coef(lm.mod)[1]
      slpDF2$A600[slpDF2$Identity==i] <- coef(lm.mod)[2] * 600 + coef(lm.mod)[1]
      
    }
    
    slpDF1$sens <- (slpDF1$A600 - slpDF1$A400) / slpDF1$A400
    slpDF2$sens <- (slpDF2$A600 - slpDF2$A400) / slpDF2$A400
    
    slpDF <- rbind(slpDF1, slpDF2)
    
    ### test statistics of the slope
    mod1 <- lmer(slope~ Position * CO2_treatment + (1|Chamber), data=slpDF)
    out1 <- anova(mod1)
    lab1 <- summary(glht(mod1, linfct = mcp(Position = "Tukey")))
    
    
    ### summarize slope
    outDF1 <- summaryBy(slope+intercept+A400+A600+sens~Position+Type+CO2_treatment,
                        FUN=c(mean, se), data=slpDF, keep.names=T, na.rm=T)
    
    write.csv(outDF1, "output/A-Ca/linear_fit_summary_table.csv", row.names=F)
    
    ### predict A-Ca based on linear fit form Ca = 350 to 650
    ftDF1 <- data.frame(rep(unique(slpDF1$Identity), each=301),
                        rep(c(350:650), length(unique(slpDF1$Identity))),
                        NA)
    ftDF2 <- data.frame(rep(unique(slpDF2$Identity), each=301),
                        rep(c(350:650), length(unique(slpDF2$Identity))),
                        NA)
    colnames(ftDF1) <- colnames(ftDF2) <- c("Identity", "Ca", "Photo")
    ftDF1 <- merge(ftDF1, idDF, by = "Identity")
    ftDF2 <- merge(ftDF2, idDF, by = "Identity")
    
    for (i in unique(slpDF1$Identity)) {
      ftDF1$Photo[ftDF1$Identity==i] <- ftDF1$Ca[ftDF1$Identity==i] * slpDF1$slope[slpDF1$Identity==i] + slpDF1$intercept[slpDF1$Identity==i]
    }
    
    for (i in unique(slpDF2$Identity)) {
      ftDF2$Photo[ftDF2$Identity==i] <- ftDF2$Ca[ftDF2$Identity==i] * slpDF2$slope[slpDF2$Identity==i] + slpDF2$intercept[slpDF2$Identity==i]
    }
    
    ### 
    smDF1 <- summaryBy(Photo~Position+Type+CO2_treatment+Ca, 
                       FUN=c(mean, se), keep.names=T, na.rm=T,
                       data=ftDF1)
    smDF2 <- summaryBy(Photo~Position+Type+CO2_treatment+Ca, 
                       FUN=c(mean, se), keep.names=T, na.rm=T,
                       data=ftDF2)
    
    
    ### plot
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
                        labels=c("Full", "T+M", "Top", "Up", "Low"))+
      scale_color_manual(name="Position",
                         limits=c("12345", "345", "45", "up", "low"),
                         values=c("blue2", "red3", "purple", "orange", "darkgreen"),
                         labels=c("Full", "T+M", "Top", "Up", "Low"))+
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
      geom_line(data=smDF1, aes(Ca, Photo.mean, group=Position,
                                   col=as.factor(Position)), size=1.5)+
      geom_ribbon(data=smDF1, 
                  aes(x=Ca,group=Position,
                      ymin = Photo.mean-Photo.se, 
                      ymax = Photo.mean+Photo.se,
                      fill=as.factor(Position)), alpha=0.2) +
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
                        labels=c("Full", "T+M", "Top", "Up", "Low"))+
      scale_color_manual(name="Position",
                         limits=c("12345", "345", "45", "up", "low"),
                         values=c("blue2", "red3", "purple", "orange", "darkgreen"),
                         labels=c("Full", "T+M", "Top", "Up", "Low"))+
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
                        labels=c("Full", "T+M", "Top", "Up", "Low"))+
      scale_color_manual(name="Position",
                         limits=c("12345", "345", "45", "up", "low"),
                         values=c("blue2", "red3", "purple", "orange", "darkgreen"),
                         labels=c("Full", "T+M", "Top", "Up", "Low"))+
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
      geom_line(data=smDF2, aes(Ca, Photo.mean, group=Position,
                                col=as.factor(Position)), size=1.5)+
      geom_ribbon(data=smDF2, 
                  aes(x=Ca,group=Position,
                      ymin = Photo.mean-Photo.se, 
                      ymax = Photo.mean+Photo.se,
                      fill=as.factor(Position)), alpha=0.2) +
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
                        labels=c("Full", "T+M", "Top", "Up", "Low"))+
      scale_color_manual(name="Position",
                         limits=c("12345", "345", "45", "up", "low"),
                         values=c("blue2", "red3", "purple", "orange", "darkgreen"),
                         labels=c("Full", "T+M", "Top", "Up", "Low"))+
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
                        labels=c("Full", "T+M", "Top", "Up", "Low"))+
      scale_color_manual(name="Position",
                         limits=c("12345", "345", "45", "up", "low"),
                         values=c("blue2", "red3", "purple", "orange", "darkgreen"),
                         labels=c("Full", "T+M", "Top", "Up", "Low"))+
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
                        labels=c("Full", "T+M", "Top", "Up", "Low"))+
      scale_color_manual(name="Position",
                         limits=c("12345", "345", "45", "up", "low"),
                         values=c("blue2", "red3", "purple", "orange", "darkgreen"),
                         labels=c("Full", "T+M", "Top", "Up", "Low"))+
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
    
    combined_plots <- plot_grid(p1, p2, p3, p4,  
                                labels=c("(a)", "(b)", "(c)", "(d)"),
                                ncol=2, align="vh", axis = "l",
                                label_x=0.16, label_y=0.95,
                                label_size = 18)
    
    pdf("output/A-Ca/A-Ca_plots.pdf", width=8, height=10)
    plot_grid(combined_plots, legend_shared, ncol=1, rel_heights=c(1,0.1))
    dev.off() 
    
    
    ################################# plot delta A sensitivity #################################
    ###### Plot delta A at the Ca = 400 to 600 range and see the slope
    #### export biochemical parameter summary table
    mgDF <- rbind(ftDF1, ftDF2)
    subDF1 <- subset(mgDF, Ca == "400")
    subDF2 <- subset(mgDF, Ca == "600")
    mgDF2 <- merge(subDF1, subDF2, by="Identity", keep.all=T)
    mgDF3 <- mgDF2[,c("Identity", "Ca.x", "Ca.y", "Photo.x", "Photo.y", "Chamber.x", 
                      "Position.x", "Type.x", "CO2_treatment.x")]
    colnames(mgDF3) <- c("Identity", "Ca400", "Ca600", "A400", "A600",
                         "Chamber", "Position", "Type", "CO2_treatment")
    
    ### normalized sensitivity to per CO2 concentration
    mgDF3$A_sens <- with(mgDF3, (A600-A400)/(600-400))
    
    ### normalized sensitivity to A400
    mgDF3$A_sens_norm <- with(mgDF3, (A600-A400)/A400)
    
    write.csv(mgDF3, "output/A-Ca/predicted_A_at_400_600_ppm.csv", row.names=F)
    
    plotDF2 <- summaryBy(A_sens+A_sens_norm~Position+Type+CO2_treatment,
                         FUN=c(mean,se), keep.names=T, data=mgDF3)
    
    plotDF2$Position <- gsub("12345", "5_Full", plotDF2$Position)
    plotDF2$Position <- gsub("345", "4_TM", plotDF2$Position)
    plotDF2$Position <- gsub("45", "3_Top", plotDF2$Position)
    plotDF2$Position <- gsub("low", "2_low", plotDF2$Position)
    plotDF2$Position <- gsub("up", "1_up", plotDF2$Position)
    
    
    ### test statistics
    mod1 <- lmer(A_sens_norm ~ Position * CO2_treatment + (1|Chamber), data=mgDF3)
    out1 <- anova(mod1)
    lab1 <- summary(glht(mod1, linfct = mcp(Position = "Tukey")))
    
    
    ################################# plotting
   # p1 <- ggplot(plotDF2, aes(Position, A_sens.mean, fill=CO2_treatment)) +
   #   geom_errorbar(aes(x=Position, ymin=A_sens.mean-A_sens.se,
   #                     ymax=A_sens.mean+A_sens.se,
   #                     col=CO2_treatment), 
   #                 position=position_dodge(width=0.2),
   #                 width=0.2)+
   #   geom_point(aes(Position, A_sens.mean, 
   #                  fill=CO2_treatment), 
   #              position=position_dodge(width=0.2),
   #              pch=21, size=4)+
   #   theme_linedraw() +
   #   theme(panel.grid.minor=element_blank(),
   #         axis.text.x=element_text(size=12),
   #         axis.title.x=element_text(size=14),
   #         axis.text.y=element_text(size=12),
   #         axis.title.y=element_text(size=14),
   #         legend.text=element_text(size=12),
   #         legend.title=element_text(size=14),
   #         panel.grid.major=element_blank(),
   #         legend.position="none",
   #         legend.box = 'vertical',
   #         legend.box.just = 'left')+
   #   xlab("")+
   #   ylab(expression(paste(delta,  "A / ", delta, CO[2], " (", mu, "mol ", m^-2, " ", s^-1, " ", ppm^-1, ")")))+
   #   scale_fill_manual(name="",
   #                     limits=c("aCO2", "eCO2"),
   #                     values=c("blue2", "red3"),
   #                     labels=c(expression(paste(aCO[2])),
   #                              expression(paste(eCO[2]))))+
   #   scale_color_manual(name="",
   #                     limits=c("aCO2", "eCO2"),
   #                     values=c("blue2", "red3"),
   #                     labels=c(expression(paste(aCO[2])),
   #                              expression(paste(eCO[2]))))+
   #   scale_x_discrete(name="", 
   #                    breaks=c("5_Full", "4_TM", "3_Top", "2_low", "1_up"), 
   #                    labels=c("Full", "T+M", "Top", "Low", "Up"))
    
    p2 <- ggplot(plotDF2, aes(Position, A_sens_norm.mean, fill=CO2_treatment)) +
      geom_errorbar(aes(x=Position, ymin=A_sens_norm.mean-A_sens_norm.se,
                        ymax=A_sens_norm.mean+A_sens_norm.se,
                        col=CO2_treatment), 
                    position=position_dodge(width=0.2),
                    width=0.2)+
      geom_point(aes(Position, A_sens_norm.mean, 
                     fill=CO2_treatment), 
                 position=position_dodge(width=0.2),
                 pch=21, size=4)+
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
      xlab("")+
      ylab(expression(paste(delta,  "A / ", A[400])))+
      scale_fill_manual(name="",
                        limits=c("aCO2", "eCO2"),
                        values=c("blue2", "red3"),
                        labels=c(expression(paste(aCO[2])),
                                 expression(paste(eCO[2]))))+
      scale_color_manual(name="",
                         limits=c("aCO2", "eCO2"),
                         values=c("blue2", "red3"),
                         labels=c(expression(paste(aCO[2])),
                                  expression(paste(eCO[2]))))+
      scale_x_discrete(name="", 
                       breaks=c("5_Full", "4_TM", "3_Top", "2_low", "1_up"), 
                       labels=c("Full", "T+M", "Top", "Low", "Up"))
    
  
    
    #legend_shared <- get_legend(p1 + theme(legend.position="bottom",
    #                                       legend.box = 'vertical',
    #                                       legend.box.just = 'left'))
    #
    #combined_plots <- plot_grid(p1, p2, 
    #                            labels=c("(a)", "(b)"),
    #                            ncol=1, align="vh", axis = "l",
    #                            label_x=0.88, label_y=0.95,
    #                            label_size = 18)
    

    pdf("output/A-Ca/predicted_A_sensitivity_plot.pdf", width=4, height=4)
    #plot_grid(combined_plots, legend_shared, ncol=1, rel_heights=c(1,0.1))
    plot(p2)
    dev.off()  
    
    
    
    
    
    
    
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
    plotDF1$Position <- as.factor(plotDF1$Position)
    plotDF2$Position <- as.factor(plotDF2$Position)
    
    
    ### perform linear mixed effect model statistics
    ### check type effect, ignoring CO2 and position effect
    mod1 <- lmer(Vcmax~ Type + (1|Chamber), data=stDF)
    out1 <- anova(mod1)
    lab1 <- summary(glht(mod1, linfct = mcp(Type = "Tukey")))
    #eff.size1 <- round(lab1$test$coefficients[1], 1)
    #eff.sig1 <- round(lab1$test$pvalues[1], 3)
    #eff.error1 <- round(lab1$test$sigma[1], 1)
    

    mod2 <- lmer(Jmax~ Type + (1|Chamber), data=stDF)
    out2 <- anova(mod2)
    lab2 <- summary(glht(mod2, linfct = mcp(Type = "Tukey")))
    
    
    mod3 <- lmer(JVratio~ Type + (1|Chamber), data=stDF)
    out3 <- anova(mod3)
    lab3 <- summary(glht(mod3, linfct = mcp(Type = "Tukey")))
    
    mod4 <- lmer(Ci_transition_Ac_Aj~ Type + (1|Chamber), data=stDF)
    out4 <- anova(mod4)
    lab4 <- summary(glht(mod4, linfct = mcp(Type = "Tukey")))
    
    
    ### fix jitter
    set.seed(123)
    
    ### plotting
    p1 <- ggplot(data=stDF) +
      geom_boxplot(aes(Type, Vcmax),
                   outlier.fill = "white", outlier.color = "white",
                   outlier.size = 0.0, outlier.alpha = 0.0,
                   fill="grey")+
      geom_jitter(aes(Type, Vcmax, 
                      fill=Position, 
                      pch = Type), 
                  size=4, width = 0.3)+
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
                        labels=c("Full", "T+M", "Top", "Up", "Low"))+
      scale_shape_manual(name="Scale",
                         values=c(21, 24),
                         labels=c("Canopy", "Leaf"),
                         guide=F)+
      scale_x_discrete(name="", 
                       breaks=c("canopy", "leaf"), 
                       labels=c("Canopy", "Leaf"))+
      guides(fill = guide_legend(override.aes = list(shape = c(21, 21, 21, 24, 24),
                                                     fill = c("blue2","red3", "purple",
                                                              "orange", "darkgreen"),
                                                     alpha=1.0),
                                 nrow=2, byrow = T))+
      ylim(0, 200)
    
    
    p2 <- ggplot(data=stDF) +
      geom_boxplot(aes(Type, Jmax),
                   outlier.fill = "white", outlier.color = "white",
                   outlier.size = 0.0, outlier.alpha = 0.0,
                   fill="grey")+
      geom_jitter(aes(Type, Jmax, 
                      fill=Position, 
                      pch = Type), 
                  size=4, width = 0.3)+
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
                        labels=c("Full", "T+M", "Top", "Up", "Low"))+
      scale_shape_manual(name="Scale",
                         values=c(21, 24),
                         labels=c("Canopy", "Leaf"),
                         guide=F)+
      scale_x_discrete(name="", 
                       breaks=c("canopy", "leaf"), 
                       labels=c("Canopy", "Leaf"))+
      guides(fill = guide_legend(override.aes = list(shape = c(21, 21, 21, 24, 24),
                                                     fill = c("blue2","red3", "purple",
                                                              "orange", "darkgreen"),
                                                     alpha=1.0),
                                 nrow=2, byrow = T))+
      ylim(0, 300)
    
    p3 <- ggplot(data=stDF) +
      geom_boxplot(aes(Type, JVratio),
                   outlier.fill = "white", outlier.color = "white",
                   outlier.size = 0.0, outlier.alpha = 0.0,
                   fill="grey")+
      geom_jitter(aes(Type, JVratio, 
                      fill=Position, 
                      pch = Type), 
                  size=4, width = 0.3)+
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
      ylab(expression(paste(J[max] * " / " * V[cmax])))+
      scale_fill_manual(name="Position",
                        limits=c("12345", "345", "45", "up", "low"),
                        values=c("blue2", "red3", "purple", "orange", "green"),
                        labels=c("Full", "T+M", "Top", "Up", "Low"))+
      scale_shape_manual(name="Scale",
                         values=c(21, 24),
                         labels=c("Canopy", "Leaf"),
                         guide=F)+
      scale_x_discrete(name="", 
                       breaks=c("canopy", "leaf"), 
                       labels=c("Canopy", "Leaf"))+
      guides(fill = guide_legend(override.aes = list(shape = c(21, 21, 21, 24, 24),
                                                     fill = c("blue2","red3", "purple",
                                                              "orange", "darkgreen"),
                                                     alpha=1.0),
                                 nrow=2, byrow = T))+
      ylim(0, 3)
    
    p4 <- ggplot(data=stDF) +
      geom_boxplot(aes(Type, Ci_transition_Ac_Aj),
                   outlier.fill = "white", outlier.color = "white",
                   outlier.size = 0.0, outlier.alpha = 0.0,
                   fill="grey")+
      geom_jitter(aes(Type, Ci_transition_Ac_Aj, 
                      fill=Position, 
                      pch = Type), 
                  size=4, width = 0.3)+
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
                        labels=c("Full", "T+M", "Top", "Up", "Low"))+
      scale_shape_manual(name="Scale",
                         values=c(21, 24),
                         labels=c("Canopy", "Leaf"),
                         guide=F)+
      scale_x_discrete(name="", 
                       breaks=c("canopy", "leaf"), 
                       labels=c("Canopy", "Leaf"))+
      guides(fill = guide_legend(override.aes = list(shape = c(21, 21, 21, 24, 24),
                                                     fill = c("blue2","red3", "purple",
                                                              "orange", "darkgreen"),
                                                     alpha=1.0),
                                 nrow=2, byrow = T))+
      ylim(0, 600)
    
    legend_shared <- get_legend(p1 + theme(legend.position="bottom",
                                           legend.box = 'vertical',
                                           legend.box.just = 'left'))
    
    combined_plots <- plot_grid(p1, p2, p3, p4, 
                                labels=c("(a)", "(b)", "(c)", "(d)"),
                                ncol=2, align="vh", axis = "l",
                                label_x=0.16, label_y=0.95,
                                label_size = 18)
    
    pdf("output/A-Ca/biochemical_parameter_plot_by_scale.pdf", width=10, height=10)
    plot_grid(combined_plots, legend_shared, ncol=1, rel_heights=c(1,0.1))
    dev.off()  
    
    
    #####################
    ## aCO2 only: scale effect on Vcmax, Jmax, JV ratio, and Ci transition
    mod1 <- lmer(Vcmax~ Type + (1|Chamber), data=plotDF1)
    #mod1 <- lmer(Vcmax~ Type + (1|Position/Chamber), data=plotDF1)
    anova(mod1)
    rand(mod1)
    summary(glht(mod1, linfct = mcp(Type = "Tukey")))


    mod2 <- lmer(Jmax~ Type + (1|Chamber), data=plotDF1)
    anova(mod2)
    rand(mod2)
    summary(glht(mod2, linfct = mcp(Type = "Tukey")))
    
    mod3 <- lmer(JVratio~ Type + (1|Chamber), data=plotDF1)
    anova(mod3)
    rand(mod3)
    summary(glht(mod3, linfct = mcp(Type = "Tukey")))
    
    mod4 <- lmer(Ci_transition_Ac_Aj~ Type + (1|Chamber), data=plotDF1)
    anova(mod4)
    rand(mod4)
    summary(glht(mod4, linfct = mcp(Type = "Tukey")))
    
    
    ## aCO2 and leaf only: position effect on Vcmax, Jmax, JV ratio, and Ci transition
    mod1 <- lmer(Vcmax~ Position + (1|Chamber), data=plotDF1[plotDF1$Type=="leaf",])
    anova(mod1)
    rand(mod1)
    summary(glht(mod1, linfct = mcp(Position = "Tukey")))
    
    mod2 <- lmer(Jmax~ Position + (1|Chamber), data=plotDF1[plotDF1$Type=="leaf",])
    anova(mod2)
    rand(mod2)
    summary(glht(mod2, linfct = mcp(Position = "Tukey")))
    
    mod3 <- lmer(JVratio~ Position + (1|Chamber), data=plotDF1[plotDF1$Type=="leaf",])
    anova(mod3)
    rand(mod3)
    summary(glht(mod3, linfct = mcp(Position = "Tukey")))
    
    
    mod4 <- lmer(Ci_transition_Ac_Aj~ Position + (1|Chamber), data=plotDF1[plotDF1$Type=="leaf",])
    anova(mod4)
    rand(mod4)
    summary(glht(mod4, linfct = mcp(Position = "Tukey")))
    
    
    ## aCO2 and canopy only: position effect on Vcmax, Jmax, JV ratio, and Ci transition
    mod1 <- lmer(Vcmax~ Position + (1|Chamber), data=plotDF1[plotDF1$Type=="canopy",])
    anova(mod1)
    rand(mod1)
    summary(glht(mod1, linfct = mcp(Position = "Tukey")))
    
    mod2 <- lmer(Jmax~ Position + (1|Chamber), data=plotDF1[plotDF1$Type=="canopy",])
    anova(mod2)
    rand(mod2)
    summary(glht(mod2, linfct = mcp(Position = "Tukey")))
    
    mod3 <- lmer(JVratio~ Position + (1|Chamber), data=plotDF1[plotDF1$Type=="canopy",])
    anova(mod3)
    rand(mod3)
    summary(glht(mod3, linfct = mcp(Position = "Tukey")))
    
    
    mod4 <- lmer(Ci_transition_Ac_Aj~ Position + (1|Chamber), data=plotDF1[plotDF1$Type=="canopy",])
    anova(mod4)
    rand(mod4)
    summary(glht(mod4, linfct = mcp(Position = "Tukey")))
    
    
    ## eCO2 only: scale effect on Vcmax, Jmax, JV ratio, and Ci transition
    mod1 <- lmer(Vcmax~ Type + (1|Chamber), data=plotDF2)
    anova(mod1)
    rand(mod1)
    summary(glht(mod1, linfct = mcp(Type = "Tukey")))
    
    
    mod2 <- lmer(Jmax~ Type + (1|Chamber), data=plotDF2)
    anova(mod2)
    rand(mod2)
    summary(glht(mod2, linfct = mcp(Type = "Tukey")))
    
    mod3 <- lmer(JVratio~ Type + (1|Chamber), data=plotDF2)
    anova(mod3)
    rand(mod3)
    summary(glht(mod3, linfct = mcp(Type = "Tukey")))
    
    mod4 <- lmer(Ci_transition_Ac_Aj~ Type + (1|Chamber), data=plotDF2)
    anova(mod4)
    rand(mod4)
    summary(glht(mod4, linfct = mcp(Type = "Tukey")))
    
    
    ## eCO2 and leaf only: position effect on Vcmax, Jmax, JV ratio, and Ci transition
    mod1 <- lmer(Vcmax~ Position + (1|Chamber), data=plotDF2[plotDF2$Type=="leaf",])
    anova(mod1)
    rand(mod1)
    summary(glht(mod1, linfct = mcp(Position = "Tukey")))
    
    mod2 <- lmer(Jmax~ Position + (1|Chamber), data=plotDF2[plotDF2$Type=="leaf",])
    anova(mod2)
    rand(mod2)
    summary(glht(mod2, linfct = mcp(Position = "Tukey")))
    
    mod3 <- lmer(JVratio~ Position + (1|Chamber), data=plotDF2[plotDF2$Type=="leaf",])
    anova(mod3)
    rand(mod3)
    summary(glht(mod3, linfct = mcp(Position = "Tukey")))
    
    
    mod4 <- lmer(Ci_transition_Ac_Aj~ Position + (1|Chamber), data=plotDF2[plotDF2$Type=="leaf",])
    anova(mod4)
    rand(mod4)
    summary(glht(mod4, linfct = mcp(Position = "Tukey")))
    
    
    ## eCO2 and canopy only: position effect on Vcmax, Jmax, JV ratio, and Ci transition
    mod1 <- lmer(Vcmax~ Position + (1|Chamber), data=plotDF2[plotDF2$Type=="canopy",])
    anova(mod1)
    rand(mod1)
    summary(glht(mod1, linfct = mcp(Position = "Tukey")))
    
    mod2 <- lmer(Jmax~ Position + (1|Chamber), data=plotDF2[plotDF2$Type=="canopy",])
    anova(mod2)
    rand(mod2)
    summary(glht(mod2, linfct = mcp(Position = "Tukey")))
    
    mod3 <- lmer(JVratio~ Position + (1|Chamber), data=plotDF2[plotDF2$Type=="canopy",])
    anova(mod3)
    rand(mod3)
    summary(glht(mod3, linfct = mcp(Position = "Tukey")))
    
    
    mod4 <- lmer(Ci_transition_Ac_Aj~ Position + (1|Chamber), data=plotDF2[plotDF2$Type=="canopy",])
    anova(mod4)
    rand(mod4)
    summary(glht(mod4, linfct = mcp(Position = "Tukey")))
    
    
    
    ################################ plotting
    #p1 <- ggplot() +
    #  geom_point(data=plotDF1, aes(Position, Vcmax, 
    #                              fill=as.factor(Position), 
    #                              pch = as.factor(Type)), alpha=1.0, size=4)+
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
    #  xlab("")+
    #  ylab(expression(paste(V[cmax], " (", mu, "mol "* CO[2], " ", m^-2, " ", s^-1, ")")))+
    #  scale_fill_manual(name="Position",
    #                    limits=c("12345", "345", "45", "up", "low"),
    #                    values=c("blue2", "red3", "purple", "orange", "green"),
    #                    labels=c("Full", "T+M", "Top", "Up", "Low"))+
    #  scale_color_manual(name="Position",
    #                     limits=c("12345", "345", "45", "up", "low"),
    #                     values=c("blue2", "red3", "purple", "orange", "darkgreen"),
    #                     labels=c("Full", "T+M", "Top", "Up", "Low"))+
    #  scale_shape_manual(name="Type",
    #                     values=c(21, 24),
    #                     labels=c("Canopy", "Leaf"))+
    #  scale_x_discrete(name="", 
    #                   breaks=c("12345", "345", "45", "up", "low"), 
    #                   labels=c("Full", "T+M", "Top", "Up", "Low"))+
    #  guides(fill = guide_legend(override.aes = list(shape = c(21, 21, 21, 24, 24))))
    
    ### fix jitter
    set.seed(123)
    
    ### plotting
    p1 <- ggplot() +
      geom_boxplot(data=plotDF1, aes(Type, Vcmax),
                   outlier.fill = "white", outlier.color = "white",
                   outlier.size = 0.0,
                   outlier.alpha = 0.0, fill="grey")+
      geom_jitter(data=plotDF1, aes(Type, Vcmax, 
                                   fill=as.factor(Position), 
                                   pch = as.factor(Type)), 
                 alpha=0.8, col="black", size=4, width = 0.2)+
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
                        labels=c("Full", "T+M", "Top", "Up", "Low"))+
      scale_shape_manual(name="Scale",
                         values=c(21, 24),
                         labels=c("Canopy", "Leaf"),
                         guide=F)+
      scale_x_discrete(name="", 
                       breaks=c("canopy", "leaf"), 
                       labels=c("Canopy", "Leaf"))+
      guides(fill = guide_legend(override.aes = list(shape = c(21, 21, 21, 24, 24),
                                                     fill = c("blue2","red3", "purple",
                                                              "orange", "darkgreen"),
                                                     alpha=1.0),
                                 nrow=2, byrow = T))+
      ylim(0, 200)
  
    
    p2 <- ggplot() +
      geom_boxplot(data=plotDF1, aes(Type, Jmax),
                   outlier.fill = "white", outlier.color = "white",
                   outlier.size = 0.0,
                   outlier.alpha = 0.0, fill="grey")+
      geom_jitter(data=plotDF1, aes(Type, Jmax, 
                                    fill=as.factor(Position), 
                                    pch = as.factor(Type)), 
                  alpha=0.8, col="black", size=4, width = 0.2)+
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
                        labels=c("Full", "T+M", "Top", "Up", "Low"))+
      scale_shape_manual(name="Scale",
                         values=c(21, 24),
                         labels=c("Canopy", "Leaf"),
                         guide=F)+
      scale_x_discrete(name="", 
                       breaks=c("canopy", "leaf"), 
                       labels=c("Canopy", "Leaf"))+
      guides(fill = guide_legend(override.aes = list(shape = c(21, 21, 21, 24, 24),
                                                     fill = c("blue2","red3", "purple",
                                                              "orange", "darkgreen"),
                                                     alpha=1.0),
                                 nrow=2, byrow = T))+
      ylim(0, 300)
    
    
    p3 <- ggplot() +
      geom_boxplot(data=plotDF1, aes(Type, JVratio),
                   outlier.fill = "white", outlier.color = "white",
                   outlier.size = 0.0,
                   outlier.alpha = 0.0, fill="grey")+
      geom_jitter(data=plotDF1, aes(Type, JVratio, 
                                    fill=as.factor(Position), 
                                    pch = as.factor(Type)), 
                  alpha=0.8, col="black", size=4, width = 0.2)+
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
                        labels=c("Full", "T+M", "Top", "Up", "Low"))+
      scale_shape_manual(name="Scale",
                         values=c(21, 24),
                         labels=c("Canopy", "Leaf"),
                         guide=F)+
      scale_x_discrete(name="", 
                       breaks=c("canopy", "leaf"), 
                       labels=c("Canopy", "Leaf"))+
      guides(fill = guide_legend(override.aes = list(shape = c(21, 21, 21, 24, 24),
                                                     fill = c("blue2","red3", "purple",
                                                              "orange", "darkgreen"),
                                                     alpha=1.0),
                                 nrow=2, byrow = T))+
      ylim(0, 3)
    
    
    p4 <- ggplot() +
      geom_boxplot(data=plotDF1, aes(Type, Ci_transition_Ac_Aj),
                   outlier.fill = "white", outlier.color = "white",
                   outlier.size = 0.0,
                   outlier.alpha = 0.0, fill="grey")+
      geom_jitter(data=plotDF1, aes(Type, Ci_transition_Ac_Aj, 
                                    fill=as.factor(Position), 
                                    pch = as.factor(Type)), 
                  alpha=0.8, col="black", size=4, width = 0.2)+
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
                        labels=c("Full", "T+M", "Top", "Up", "Low"))+
      scale_shape_manual(name="Scale",
                         values=c(21, 24),
                         labels=c("Canopy", "Leaf"),
                         guide=F)+
      scale_x_discrete(name="", 
                       breaks=c("canopy", "leaf"), 
                       labels=c("Canopy", "Leaf"))+
      guides(fill = guide_legend(override.aes = list(shape = c(21, 21, 21, 24, 24),
                                                     fill = c("blue2","red3", "purple",
                                                              "orange", "darkgreen"),
                                                     alpha=1.0),
                                 nrow=2, byrow = T))+
      ylim(0, 600)
    

    legend_shared <- get_legend(p1 + theme(legend.position="bottom",
                                           legend.box = 'vertical',
                                           legend.box.just = 'left'))
    
    combined_plots <- plot_grid(p1, p2, p3, p4, 
                                labels=c("(a)", "(b)", "(c)", "(d)"),
                                ncol=2, align="vh", axis = "l",
                                label_x=0.16, label_y=0.95,
                                label_size = 18)
    
    pdf("output/A-Ca/ambient_biochemical_parameter_plot.pdf", width=10, height=10)
    plot_grid(combined_plots, legend_shared, ncol=1, rel_heights=c(1,0.1))
    dev.off()  
    
    
    
    ### plotting elevated
    p1 <- ggplot() +
      geom_boxplot(data=plotDF2, aes(Type, Vcmax),
                   outlier.fill = "white", outlier.color = "white",
                   outlier.size = 0.0,
                   outlier.alpha = 0.0, fill="grey")+
      geom_jitter(data=plotDF2, aes(Type, Vcmax, 
                                    fill=as.factor(Position), 
                                    pch = as.factor(Type)), 
                  alpha=0.8, col="black", size=4, width = 0.2)+
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
                        labels=c("Full", "T+M", "Top", "Up", "Low"))+
      scale_shape_manual(name="Scale",
                         values=c(21, 24),
                         labels=c("Canopy", "Leaf"),
                         guide=F)+
      scale_x_discrete(name="", 
                       breaks=c("canopy", "leaf"), 
                       labels=c("Canopy", "Leaf"))+
      guides(fill = guide_legend(override.aes = list(shape = c(21, 21, 21, 24, 24),
                                                     fill = c("blue2","red3", "purple",
                                                              "orange", "darkgreen"),
                                                     alpha=1.0),
                                 nrow=2, byrow = T))+
      ylim(0, 200)
    
    
    p2 <- ggplot() +
      geom_boxplot(data=plotDF2, aes(Type, Jmax),
                   outlier.fill = "white", outlier.color = "white",
                   outlier.size = 0.0,
                   outlier.alpha = 0.0, fill="grey")+
      geom_jitter(data=plotDF2, aes(Type, Jmax, 
                                    fill=as.factor(Position), 
                                    pch = as.factor(Type)), 
                  alpha=0.8, col="black", size=4, width = 0.2)+
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
                        labels=c("Full", "T+M", "Top", "Up", "Low"))+
      scale_shape_manual(name="Scale",
                         values=c(21, 24),
                         labels=c("Canopy", "Leaf"),
                         guide=F)+
      scale_x_discrete(name="", 
                       breaks=c("canopy", "leaf"), 
                       labels=c("Canopy", "Leaf"))+
      guides(fill = guide_legend(override.aes = list(shape = c(21, 21, 21, 24, 24),
                                                     fill = c("blue2","red3", "purple",
                                                              "orange", "darkgreen"),
                                                     alpha=1.0),
                                 nrow=2, byrow = T))+
      ylim(0, 300)
    
    
    p3 <- ggplot() +
      geom_boxplot(data=plotDF2, aes(Type, JVratio),
                   outlier.fill = "white", outlier.color = "white",
                   outlier.size = 0.0,
                   outlier.alpha = 0.0, fill="grey")+
      geom_jitter(data=plotDF2, aes(Type, JVratio, 
                                    fill=as.factor(Position), 
                                    pch = as.factor(Type)), 
                  alpha=0.8, col="black", size=4, width = 0.2)+
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
                        labels=c("Full", "T+M", "Top", "Up", "Low"))+
      scale_shape_manual(name="Scale",
                         values=c(21, 24),
                         labels=c("Canopy", "Leaf"),
                         guide=F)+
      scale_x_discrete(name="", 
                       breaks=c("canopy", "leaf"), 
                       labels=c("Canopy", "Leaf"))+
      guides(fill = guide_legend(override.aes = list(shape = c(21, 21, 21, 24, 24),
                                                     fill = c("blue2","red3", "purple",
                                                              "orange", "darkgreen"),
                                                     alpha=1.0),
                                 nrow=2, byrow = T))+
      ylim(0, 3)
    
    
    p4 <- ggplot() +
      geom_boxplot(data=plotDF2, aes(Type, Ci_transition_Ac_Aj),
                   outlier.fill = "white", outlier.color = "white",
                   outlier.size = 0.0,
                   outlier.alpha = 0.0, fill="grey")+
      geom_jitter(data=plotDF2, aes(Type, Ci_transition_Ac_Aj, 
                                    fill=as.factor(Position), 
                                    pch = as.factor(Type)), 
                  alpha=0.8, col="black", size=4, width = 0.2)+
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
                        labels=c("Full", "T+M", "Top", "Up", "Low"))+
      scale_shape_manual(name="Scale",
                         values=c(21, 24),
                         labels=c("Canopy", "Leaf"),
                         guide=F)+
      scale_x_discrete(name="", 
                       breaks=c("canopy", "leaf"), 
                       labels=c("Canopy", "Leaf"))+
      guides(fill = guide_legend(override.aes = list(shape = c(21, 21, 21, 24, 24),
                                                     fill = c("blue2","red3", "purple",
                                                              "orange", "darkgreen"),
                                                     alpha=1.0),
                                 nrow=2, byrow = T))+
      ylim(0, 600)
    
    
    legend_shared <- get_legend(p1 + theme(legend.position="bottom",
                                           legend.box = 'vertical',
                                           legend.box.just = 'left'))
    
    combined_plots <- plot_grid(p1, p2, p3, p4, 
                                labels=c("(a)", "(b)", "(c)", "(d)"),
                                ncol=2, align="vh", axis = "l",
                                label_x=0.16, label_y=0.95,
                                label_size = 18)
    
    pdf("output/A-Ca/elevated_biochemical_parameter_plot.pdf", width=10, height=10)
    plot_grid(combined_plots, legend_shared, ncol=1, rel_heights=c(1,0.1))
    dev.off()  
    
    
    
    ### plotting CO2 comparison
    p1 <- ggplot() +
      geom_boxplot(data=stDF, aes(Type, Vcmax,
                                  fill=CO2_treatment))+
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
      scale_fill_manual(name="",
                        limits=c("aCO2", "eCO2"),
                        values=c("blue2", "red3"),
                        labels=c(expression(paste(aCO[2])),
                                 expression(paste(eCO[2]))))+
      scale_x_discrete(name="", 
                       breaks=c("canopy", "leaf"), 
                       labels=c("Canopy", "Leaf"))+
      ylim(0, 200)
    
    
    p2 <- ggplot() +
      geom_boxplot(data=stDF, aes(Type, Jmax,
                                  fill=CO2_treatment))+
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
      scale_fill_manual(name="",
                        limits=c("aCO2", "eCO2"),
                        values=c("blue2", "red3"),
                        labels=c(expression(paste(aCO[2])),
                                 expression(paste(eCO[2]))))+
      scale_x_discrete(name="", 
                       breaks=c("canopy", "leaf"), 
                       labels=c("Canopy", "Leaf"))+
      ylim(0, 300)
    
    
    p3 <- ggplot() +
      geom_boxplot(data=stDF, aes(Type, JVratio,
                                  fill=CO2_treatment))+
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
      scale_fill_manual(name="",
                        limits=c("aCO2", "eCO2"),
                        values=c("blue2", "red3"),
                        labels=c(expression(paste(aCO[2])),
                                 expression(paste(eCO[2]))))+
      scale_x_discrete(name="", 
                       breaks=c("canopy", "leaf"), 
                       labels=c("Canopy", "Leaf"))+
      ylim(0, 3)
    
    
    p4 <- ggplot() +
      geom_boxplot(data=stDF, aes(Type, Ci_transition_Ac_Aj,
                                  fill=CO2_treatment))+
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
      scale_fill_manual(name="",
                        limits=c("aCO2", "eCO2"),
                        values=c("blue2", "red3"),
                        labels=c(expression(paste(aCO[2])),
                                 expression(paste(eCO[2]))))+
      scale_x_discrete(name="", 
                       breaks=c("canopy", "leaf"), 
                       labels=c("Canopy", "Leaf"))+
      ylim(0, 600)
    
    
    legend_shared <- get_legend(p1 + theme(legend.position="bottom",
                                           legend.box = 'vertical',
                                           legend.box.just = 'left'))
    
    combined_plots <- plot_grid(p1, p2, p3, p4, 
                                labels=c("(a)", "(b)", "(c)", "(d)"),
                                ncol=2, align="vh", axis = "l",
                                label_x=0.16, label_y=0.95,
                                label_size = 18)
    
    pdf("output/A-Ca/biochemical_parameter_plot.pdf", width=10, height=10)
    plot_grid(combined_plots, legend_shared, ncol=1, rel_heights=c(1,0.1))
    dev.off()  
    
    
    
    ################################# #################################
    ### check relationships between Ci and Vcmax, Jmax and JVratio
    lm1 <- lm(Vcmax~Ci_transition_Ac_Aj, data=plotDF1)
    lm2 <- lm(Jmax~Ci_transition_Ac_Aj, data=plotDF1)
    lm3 <- lm(JVratio~Ci_transition_Ac_Aj, data=plotDF1)
    lm4 <- lm(Vcmax~Ci_transition_Ac_Aj, data=plotDF2)
    lm5 <- lm(Jmax~Ci_transition_Ac_Aj, data=plotDF2)
    lm6 <- lm(JVratio~Ci_transition_Ac_Aj, data=plotDF2)
    
    
    
    p1 <- ggplot(plotDF1, aes(Ci_transition_Ac_Aj, Vcmax)) +
      geom_point(data=plotDF1, aes(Ci_transition_Ac_Aj, Vcmax,
                                   fill=as.factor(Position), 
                                   pch = as.factor(Type)), alpha=1.0, size=4)+
      geom_smooth(method='lm', se=T, col="black")+
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
            legend.box.just = 'left',
            plot.title = element_text(size=16, face="bold.italic", 
                                      hjust = 0.5))+
      ylab(expression(paste(V[cmax], " (", mu, "mol "* CO[2], " ", m^-2, " ", s^-1, ")")))+
      xlab(expression(paste("Transition " * C[i] * " (" * mu * "mol" * " " * mol^-1 * ")")))+
      scale_fill_manual(name="Position",
                        limits=c("12345", "345", "45", "up", "low"),
                        values=c("blue2", "red3", "purple", "orange", "green"),
                        labels=c("Full", "T+M", "Top", "Up", "Low"))+
      scale_shape_manual(name="Scale",
                         values=c(21, 24),
                         labels=c("Canopy", "Leaf"),
                         guide=F)+
      guides(fill = guide_legend(override.aes = list(shape = c(21, 21, 21, 24, 24),
                                                     fill = c("blue2","red3", "purple",
                                                              "orange", "darkgreen"),
                                                     alpha=1.0),
                                 nrow=2, byrow = T))+
      ylim(0, 200)+
      xlim(0, 600)+
      ggtitle(expression(paste(aCO[2])))
    
    
    p2 <- ggplot(plotDF1, aes(Ci_transition_Ac_Aj, Jmax)) +
      geom_point(data=plotDF1, aes(Ci_transition_Ac_Aj, Jmax,
                                   fill=as.factor(Position), 
                                   pch = as.factor(Type)), alpha=1.0, size=4)+
      geom_smooth(method='lm', se=T, col="black")+
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
      ylab(expression(paste(J[max], " (", mu, "mol "* CO[2], " ", m^-2, " ", s^-1, ")")))+
      xlab(expression(paste("Transition " * C[i] * " (" * mu * "mol" * " " * mol^-1 * ")")))+
      scale_fill_manual(name="Position",
                        limits=c("12345", "345", "45", "up", "low"),
                        values=c("blue2", "red3", "purple", "orange", "green"),
                        labels=c("Full", "T+M", "Top", "Up", "Low"))+
      scale_shape_manual(name="Scale",
                         values=c(21, 24),
                         labels=c("Canopy", "Leaf"),
                         guide=F)+
      guides(fill = guide_legend(override.aes = list(shape = c(21, 21, 21, 24, 24),
                                                     fill = c("blue2","red3", "purple",
                                                              "orange", "darkgreen"),
                                                     alpha=1.0),
                                 nrow=2, byrow = T))+
      ylim(0, 300)+
      xlim(0, 600)
    
    
    p3 <- ggplot(plotDF1, aes(Ci_transition_Ac_Aj, JVratio)) +
      geom_point(data=plotDF1, aes(Ci_transition_Ac_Aj, JVratio,
                                   fill=as.factor(Position), 
                                   pch = as.factor(Type)), alpha=1.0, size=4)+
      geom_smooth(method='lm', se=T, col="black")+
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
      ylab("JV ratio")+
      xlab(expression(paste("Transition " * C[i] * " (" * mu * "mol" * " " * mol^-1 * ")")))+
      scale_fill_manual(name="Position",
                        limits=c("12345", "345", "45", "up", "low"),
                        values=c("blue2", "red3", "purple", "orange", "green"),
                        labels=c("Full", "T+M", "Top", "Up", "Low"))+
      scale_shape_manual(name="Scale",
                         values=c(21, 24),
                         labels=c("Canopy", "Leaf"),
                         guide=F)+
      guides(fill = guide_legend(override.aes = list(shape = c(21, 21, 21, 24, 24),
                                                     fill = c("blue2","red3", "purple",
                                                              "orange", "darkgreen"),
                                                     alpha=1.0),
                                 nrow=2, byrow = T))+
      ylim(0, 3)+
      xlim(0, 600)
    

    
    p4 <- ggplot(plotDF2, aes(Ci_transition_Ac_Aj, Vcmax)) +
      geom_point(data=plotDF2, aes(Ci_transition_Ac_Aj, Vcmax,
                                   fill=as.factor(Position), 
                                   pch = as.factor(Type)), alpha=1.0, size=4)+
      geom_smooth(method='lm', se=T, col="black")+
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
            legend.box.just = 'left',
            plot.title = element_text(size=16, face="bold.italic", 
                                      hjust = 0.5))+
      ylab(expression(paste(V[cmax], " (", mu, "mol "* CO[2], " ", m^-2, " ", s^-1, ")")))+
      xlab(expression(paste("Transition " * C[i] * " (" * mu * "mol" * " " * mol^-1 * ")")))+
      scale_fill_manual(name="Position",
                        limits=c("12345", "345", "45", "up", "low"),
                        values=c("blue2", "red3", "purple", "orange", "green"),
                        labels=c("Full", "T+M", "Top", "Up", "Low"))+
      scale_shape_manual(name="Scale",
                         values=c(21, 24),
                         labels=c("Canopy", "Leaf"),
                         guide=F)+
      guides(fill = guide_legend(override.aes = list(shape = c(21, 21, 21, 24, 24),
                                                     fill = c("blue2","red3", "purple",
                                                              "orange", "darkgreen"),
                                                     alpha=1.0),
                                 nrow=2, byrow = T))+
      ylim(0, 200)+
      xlim(0, 600)+
      ggtitle(expression(paste(eCO[2])))
    

    
    p5 <- ggplot(plotDF2, aes(Ci_transition_Ac_Aj, Jmax)) +
      geom_point(data=plotDF2, aes(Ci_transition_Ac_Aj, Jmax,
                                   fill=as.factor(Position), 
                                   pch = as.factor(Type)), alpha=1.0, size=4)+
      geom_smooth(method='lm', se=T, col="black")+
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
      ylab(expression(paste(J[max], " (", mu, "mol "* CO[2], " ", m^-2, " ", s^-1, ")")))+
      xlab(expression(paste("Transition " * C[i] * " (" * mu * "mol" * " " * mol^-1 * ")")))+
      scale_fill_manual(name="Position",
                        limits=c("12345", "345", "45", "up", "low"),
                        values=c("blue2", "red3", "purple", "orange", "green"),
                        labels=c("Full", "T+M", "Top", "Up", "Low"))+
      scale_shape_manual(name="Scale",
                         values=c(21, 24),
                         labels=c("Canopy", "Leaf"),
                         guide=F)+
      guides(fill = guide_legend(override.aes = list(shape = c(21, 21, 21, 24, 24),
                                                     fill = c("blue2","red3", "purple",
                                                              "orange", "darkgreen"),
                                                     alpha=1.0),
                                 nrow=2, byrow = T))+
      ylim(0, 300)+
      xlim(0, 600)
    
    
    p6 <- ggplot(plotDF2, aes(Ci_transition_Ac_Aj, JVratio)) +
      geom_point(data=plotDF2, aes(Ci_transition_Ac_Aj, JVratio,
                                   fill=as.factor(Position), 
                                   pch = as.factor(Type)), alpha=1.0, size=4)+
      geom_smooth(method='lm', se=T, col="black")+
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
      ylab("JV ratio")+
      xlab(expression(paste("Transition " * C[i] * " (" * mu * "mol" * " " * mol^-1 * ")")))+
      scale_fill_manual(name="Position",
                        limits=c("12345", "345", "45", "up", "low"),
                        values=c("blue2", "red3", "purple", "orange", "green"),
                        labels=c("Full", "T+M", "Top", "Up", "Low"))+
      scale_shape_manual(name="Scale",
                         values=c(21, 24),
                         labels=c("Canopy", "Leaf"),
                         guide=F)+
      guides(fill = guide_legend(override.aes = list(shape = c(21, 21, 21, 24, 24),
                                                     fill = c("blue2","red3", "purple",
                                                              "orange", "darkgreen"),
                                                     alpha=1.0),
                                 nrow=2, byrow = T))+
      ylim(0, 3)+
      xlim(0, 600)
    
    
    legend_shared <- get_legend(p1 + theme(legend.position="bottom",
                                           legend.box = 'vertical',
                                           legend.box.just = 'left'))
    
    combined_plots <- plot_grid(p1, p4, p2, p5, p3, p6,
                                labels=c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)"),
                                ncol=2, align="vh", axis = "l",
                                label_x=0.16, label_y=0.88,
                                label_size = 18)
    
    pdf("output/A-Ca/biochemical_parameter_correlations.pdf", width=10, height=12)
    plot_grid(combined_plots, legend_shared, ncol=1, rel_heights=c(1,0.1))
    dev.off()  
    
    
    ####################### split plots into leaf and canopy responses separately ##########
    ### separate into aCO2 and eCO2 DF
    stDF1 <- subset(stDF, Type == "leaf")
    stDF2 <- subset(stDF, Type == "canopy")
    
    ### perform statistics (i.e. position x CO2)
    ## leaf, Vcmax
    mod1 <- lmer(Vcmax~ Position * CO2_treatment + (1|Chamber), data=stDF1)
    out1 <- anova(mod1)
    summary(glht(mod1, linfct = mcp(Position = "Tukey")))
    write.csv(out1, "output/A-Ca/mixed_effect_leaf_Vcmax_position_by_CO2.csv")
    
    ## leaf, Jmax
    mod2 <- lmer(Jmax~ Position * CO2_treatment + (1|Chamber), data=stDF1)
    out2 <- anova(mod2)
    summary(glht(mod2, linfct = mcp(Position = "Tukey")))
    write.csv(out2, "output/A-Ca/mixed_effect_leaf_Jmax_position_by_CO2.csv")
    
    ## leaf, JV ratio
    mod3 <- lmer(JVratio~ Position * CO2_treatment + (1|Chamber), data=stDF1)
    out3 <- anova(mod3)
    summary(glht(mod3, linfct = mcp(Position = "Tukey")))
    write.csv(out3, "output/A-Ca/mixed_effect_leaf_JVratio_position_by_CO2.csv")
    
    ## leaf, Ci
    mod4 <- lmer(Ci_transition_Ac_Aj~ Position * CO2_treatment + (1|Chamber), data=stDF1)
    out4 <- anova(mod4)
    summary(glht(mod4, linfct = mcp(Position = "Tukey")))
    write.csv(out4, "output/A-Ca/mixed_effect_leaf_Ci_position_by_CO2.csv")
    
    
    
    ## canopy, Vcmax
    mod1 <- lmer(Vcmax~ Position * CO2_treatment + (1|Chamber), data=stDF2)
    out1 <- anova(mod1)
    summary(glht(mod1, linfct = mcp(Position = "Tukey")))
    write.csv(out1, "output/A-Ca/mixed_effect_canopy_Vcmax_position_by_CO2.csv")
    
    ## canopy, Jmax
    mod2 <- lmer(Jmax~ Position * CO2_treatment + (1|Chamber), data=stDF2)
    out2 <- anova(mod2)
    summary(glht(mod2, linfct = mcp(Position = "Tukey")))
    write.csv(out2, "output/A-Ca/mixed_effect_canopy_Jmax_position_by_CO2.csv")
    
    ## canopy, JV ratio
    mod3 <- lmer(JVratio~ Position * CO2_treatment + (1|Chamber), data=stDF2)
    out3 <- anova(mod3)
    summary(glht(mod3, linfct = mcp(Position = "Tukey")))
    write.csv(out3, "output/A-Ca/mixed_effect_canopy_JVratio_position_by_CO2.csv")
    
    ## canopy, Ci
    mod4 <- lmer(Ci_transition_Ac_Aj~ Position * CO2_treatment + (1|Chamber), data=stDF2)
    out4 <- anova(mod4)
    summary(glht(mod4, linfct = mcp(Position = "Tukey")))
    write.csv(out4, "output/A-Ca/mixed_effect_canopy_Ci_position_by_CO2.csv")
    
    
    ### set seed for reproducibility
    set.seed(124)
    
    stDF1$CO2_treatment <- as.character(stDF1$CO2_treatment)
    stDF2$CO2_treatment <- as.character(stDF2$CO2_treatment)
    
    stDF1$Position <- as.character(stDF1$Position)
    stDF2$Position <- as.character(stDF2$Position)
    
    
    ### plotting
    ## leaf, Vcmax
    p1 <- ggplot(stDF1, aes(Position, Vcmax, fill=CO2_treatment)) +
      geom_boxplot(aes(Position, Vcmax, 
                       fill=CO2_treatment), alpha=1.0)+
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
            legend.box.just = 'left',
            plot.title = element_text(size=16, face="bold", 
                                      hjust = 0.5))+
      xlab("")+
      ylab(expression(paste(V[cmax], " (", mu, "mol "* CO[2], " ", m^-2, " ", s^-1, ")")))+
      scale_fill_manual(name=expression(paste(C[a] * " treatment")),
                        limits=c("aCO2", "eCO2"),
                        values=c("blue2", "red3"),
                        labels=c(expression(paste(aC[a])), 
                                 expression(paste(eC[a]))))+
      scale_x_discrete(name="", 
                       breaks=c("up", "low"), 
                       labels=c("Up", "Low"))+
      guides(fill = guide_legend(override.aes = list(fill = c("blue2","red3"),
                                                     alpha=1.0),
                                 nrow=2, byrow = T))+
      ylim(0, 200)+
      ggtitle("Leaf-scale")
    
    ## canopy, Vcmax
    p2 <- ggplot(stDF2, aes(Position, Vcmax, fill=CO2_treatment)) +
      geom_boxplot(aes(Position, Vcmax, 
                       fill=CO2_treatment), alpha=1.0)+
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
            legend.box.just = 'left',
            plot.title = element_text(size=16, face="bold", 
                                      hjust = 0.5))+
      xlab("")+
      ylab(expression(paste(V[cmax], " (", mu, "mol "* CO[2], " ", m^-2, " ", s^-1, ")")))+
      scale_fill_manual(name=expression(paste(C[a] * " treatment")),
                        limits=c("aCO2", "eCO2"),
                        values=c("blue2", "red3"),
                        labels=c(expression(paste(aC[a])), 
                                 expression(paste(eC[a]))))+
      scale_x_discrete(name="", 
                       breaks=c("12345", "345", "45"), 
                       labels=c("Full", "T+M", "Top"))+
      guides(fill = guide_legend(override.aes = list(fill = c("blue2","red3"),
                                                     alpha=1.0),
                                 nrow=2, byrow = T))+
      ylim(0, 50)+
      ggtitle("Canopy-scale")
    
    
    ## leaf, Jmax
    p3 <- ggplot(stDF1, aes(Position, Jmax, fill=CO2_treatment)) +
      geom_boxplot(aes(Position, Jmax, 
                       fill=CO2_treatment), alpha=1.0)+
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
      scale_fill_manual(name=expression(paste(C[a] * " treatment")),
                        limits=c("aCO2", "eCO2"),
                        values=c("blue2", "red3"),
                        labels=c(expression(paste(aC[a])), 
                                 expression(paste(eC[a]))))+
      scale_x_discrete(name="", 
                       breaks=c("up", "low"), 
                       labels=c("Up", "Low"))+
      guides(fill = guide_legend(override.aes = list(fill = c("blue2","red3"),
                                                     alpha=1.0),
                                 nrow=2, byrow = T))+
      ylim(0, 300)
    
    ## canopy, Jmax
    p4 <- ggplot(stDF2, aes(Position, Jmax, fill=CO2_treatment)) +
      geom_boxplot(aes(Position, Jmax, 
                       fill=CO2_treatment), alpha=1.0)+
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
      xlab("")+
      ylab(expression(paste(J[max], " (", mu, "mol "* CO[2], " ", m^-2, " ", s^-1, ")")))+
      scale_fill_manual(name=expression(paste(C[a] * " treatment")),
                        limits=c("aCO2", "eCO2"),
                        values=c("blue2", "red3"),
                        labels=c(expression(paste(aC[a])), 
                                 expression(paste(eC[a]))))+
      scale_x_discrete(name="", 
                       breaks=c("12345", "345", "45"), 
                       labels=c("Full", "T+M", "Top"))+
      guides(fill = guide_legend(override.aes = list(fill = c("blue2","red3"),
                                                     alpha=1.0),
                                 nrow=2, byrow = T))+
      ylim(0, 60)
    
    ## leaf, JV ratio
    p5 <- ggplot(stDF1, aes(Position, JVratio, fill=CO2_treatment)) +
      geom_boxplot(aes(Position, JVratio, 
                       fill=CO2_treatment), alpha=1.0)+
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
      ylab(expression(paste(J[max] * " / " * V[cmax])))+
      scale_fill_manual(name=expression(paste(C[a] * " treatment")),
                        limits=c("aCO2", "eCO2"),
                        values=c("blue2", "red3"),
                        labels=c(expression(paste(aC[a])), 
                                 expression(paste(eC[a]))))+
      scale_x_discrete(name="", 
                       breaks=c("up", "low"), 
                       labels=c("Up", "Low"))+
      guides(fill = guide_legend(override.aes = list(fill = c("blue2","red3"),
                                                     alpha=1.0),
                                 nrow=2, byrow = T))+
      ylim(1, 2)
    
    ## canopy, JV ratio
    p6 <- ggplot(stDF2, aes(Position, JVratio, fill=CO2_treatment)) +
      geom_boxplot(aes(Position, JVratio, 
                       fill=CO2_treatment), alpha=1.0)+
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
      xlab("")+
      ylab(expression(paste(J[max] * " / " * V[cmax])))+
      scale_fill_manual(name=expression(paste(C[a] * " treatment")),
                        limits=c("aCO2", "eCO2"),
                        values=c("blue2", "red3"),
                        labels=c(expression(paste(aC[a])), 
                                 expression(paste(eC[a]))))+
      scale_x_discrete(name="", 
                       breaks=c("12345", "345", "45"), 
                       labels=c("Full", "T+M", "Top"))+
      guides(fill = guide_legend(override.aes = list(fill = c("blue2","red3"),
                                                     alpha=1.0),
                                 nrow=2, byrow = T))+
      ylim(1, 2)
    
    ## leaf, Ci
    p7 <- ggplot(stDF1, aes(Position, Ci_transition_Ac_Aj, fill=CO2_treatment)) +
      geom_boxplot(aes(Position, Ci_transition_Ac_Aj, 
                       fill=CO2_treatment), alpha=1.0)+
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
      scale_fill_manual(name=expression(paste(C[a] * " treatment")),
                        limits=c("aCO2", "eCO2"),
                        values=c("blue2", "red3"),
                        labels=c(expression(paste(aC[a])), 
                                 expression(paste(eC[a]))))+
      scale_x_discrete(name="", 
                       breaks=c("up", "low"), 
                       labels=c("Up", "Low"))+
      guides(fill = guide_legend(override.aes = list(fill = c("blue2","red3"),
                                                     alpha=1.0),
                                 nrow=2, byrow = T))+
      ylim(0, 600)
    
    ## canopy, Ci
    p8 <- ggplot(stDF2, aes(Position, Ci_transition_Ac_Aj, fill=CO2_treatment)) +
      geom_boxplot(aes(Position, Ci_transition_Ac_Aj, 
                       fill=CO2_treatment), alpha=1.0)+
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
      xlab("")+
      ylab(expression(paste("Transition " * C[i] * " (" * mu * "mol" * " " * mol^-1 * ")")))+
      scale_fill_manual(name=expression(paste(C[a] * " treatment")),
                        limits=c("aCO2", "eCO2"),
                        values=c("blue2", "red3"),
                        labels=c(expression(paste(aC[a])), 
                                 expression(paste(eC[a]))))+
      scale_x_discrete(name="", 
                       breaks=c("12345", "345", "45"), 
                       labels=c("Full", "T+M", "Top"))+
      guides(fill = guide_legend(override.aes = list(fill = c("blue2","red3"),
                                                     alpha=1.0),
                                 nrow=2, byrow = T))+
      ylim(0, 600)
      
      
    legend_shared <- get_legend(p1 + theme(legend.position="bottom",
                                           legend.box = 'vertical',
                                           legend.box.just = 'left'))
    
    combined_plots <- plot_grid(p1, p2, p3, p4, p5, p6, p7, p8,
                                labels=c("(a)", "(b)", "(c)", "(d)",
                                         "(e)", "(f)", "(g)", "(h)"),
                                ncol=2, align="vh", axis = "l",
                                label_x=0.16, label_y=0.88,
                                label_size = 18)
    
    pdf("output/A-Ca/biochemical_parameters_leaf_canopy_split.pdf", width=12, height=16)
    plot_grid(combined_plots, legend_shared, ncol=1, rel_heights=c(1,0.1))
    dev.off()  
      
  
}
