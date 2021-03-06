plot_A_Ca_and_perform_linear_fit_over_Ca_of_280_to_400 <- function(mgDF) {
    #### This script plots A-CA curve for leaf and canopy data on to the same plot
    #### and compare the shapes
    #### which can be related to Rogers et al. 2017 conceptual figure
    #### Only based on a subset of data that is well-watered treatment
    
  
    ################################# plot A-Ca #################################
    ### generate identity list
    idDF <- unique(mgDF[,c("Identity", "Chamber", "Position", 
                           "Type", "CO2_treatment")])
    
    ### individual chambers
    ch01DF <- subset(mgDF, Chamber == 1)
    ch03DF <- subset(mgDF, Chamber == 3)
    ch11DF <- subset(mgDF, Chamber == 11)
    ch04DF <- subset(mgDF, Chamber == 4)
    ch08DF <- subset(mgDF, Chamber == 8)

    ### combine only aCO2 and wet treatment
    plotDF1 <- rbind(ch01DF, ch03DF, ch11DF)
    plotDF2 <- rbind(ch04DF, ch08DF)
    
    ### plot a subset range
    ### i.e. Ca = 400 to 600
    subDF1 <- subset(plotDF1, Ca>=200 & Ca <= 450)
    subDF2 <- subset(plotDF2, Ca>=200 & Ca <= 450)
    
    
    ### create slope DF to store the slops
    slpDF1 <- data.frame(unique(subDF1$Identity), NA, NA, NA, NA, NA)
    slpDF2 <- data.frame(unique(subDF2$Identity), NA, NA, NA, NA, NA)
    colnames(slpDF1) <- colnames(slpDF2) <- c("Identity", "slope", "intercept", 
                                              "A400", "A280", "sens")

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
      slpDF1$A280[slpDF1$Identity==i] <- coef(lm.mod)[2] * 280 + coef(lm.mod)[1]
    }
    
    for (i in unique(slpDF2$Identity)) {
      subDF <- subset(subDF2, Identity == i)
      
      ## linear fit
      lm.mod <- lm(Photo~Ca, data=subDF)
      
      ## coefficients
      slpDF2$slope[slpDF2$Identity==i] <- coef(lm.mod)[2]
      slpDF2$intercept[slpDF2$Identity==i] <- coef(lm.mod)[1]
      slpDF2$A400[slpDF2$Identity==i] <- coef(lm.mod)[2] * 400 + coef(lm.mod)[1]
      slpDF2$A280[slpDF2$Identity==i] <- coef(lm.mod)[2] * 280 + coef(lm.mod)[1]
      
    }
    
    slpDF1$sens <- (slpDF1$A400 - slpDF1$A280) / slpDF1$A280
    slpDF2$sens <- (slpDF2$A400 - slpDF2$A280) / slpDF2$A280
    
    slpDF <- rbind(slpDF1, slpDF2)
    
    ### test statistics of the slope
    mod1 <- lmer(slope~ Position * CO2_treatment + (1|Chamber), data=slpDF)
    out1 <- anova(mod1)
    lab1 <- summary(glht(mod1, linfct = mcp(Position = "Tukey")))
    
    
    ### summarize slope
    outDF1 <- summaryBy(slope+intercept+A400+A280+sens~Position+Type+CO2_treatment,
                        FUN=c(mean, se), data=slpDF, keep.names=T, na.rm=T)
    
    write.csv(outDF1, "output/A-Ca/linear_fit_summary_table_280_400.csv", row.names=F)
    
    ### predict A-Ca based on linear fit form Ca = 280 to 400
    ftDF1 <- data.frame(rep(unique(slpDF1$Identity), each=121),
                        rep(c(280:400), length(unique(slpDF1$Identity))),
                        NA)
    ftDF2 <- data.frame(rep(unique(slpDF2$Identity), each=121),
                        rep(c(280:400), length(unique(slpDF2$Identity))),
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
    
    
    ### prepare Ci-Ca ratio
    plotDF1$CiCa <- with(plotDF1, Ci/Ca)
    plotDF2$CiCa <- with(plotDF2, Ci/Ca)
    
    subDF1$CiCa <- with(subDF1, Ci/Ca)
    subDF2$CiCa <- with(subDF2, Ci/Ca)
    
    ### prepare Ci Ca summary table and output
    rawCi <- rbind(subDF1, subDF2)
    outCi <- summaryBy(CiCa~Position+Type+CO2_treatment, FUN=c(mean, se),
                       data=rawCi, keep.names=T)
    write.csv(outCi, "output/A-Ca/CiCa_summary_table_280_400.csv", row.names=F)
    

    ## plot A-Ca sub range
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
      xlim(200,450)+
      ylim(-5,40)+
      guides(fill = guide_legend(override.aes = list(shape = c(21, 21, 21, 24, 24))))
    

    ## plot A-Ca sub range for eCO2 treatment
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
      xlim(200,450)+
      ylim(-5,40)+
      guides(fill = guide_legend(override.aes = list(shape = c(21, 21, 21, 24, 24))))
    
    
    
    ################################# plot delta A sensitivity #################################
    ###### Plot delta A at the Ca = 400 to 600 range and see the slope
    #### export biochemical parameter summary table
    myDF <- rbind(ftDF1, ftDF2)
    subDF1 <- subset(myDF, Ca == "280")
    subDF2 <- subset(myDF, Ca == "400")
    myDF2 <- merge(subDF1, subDF2, by="Identity", keep.all=T)
    myDF3 <- myDF2[,c("Identity", "Ca.x", "Ca.y", "Photo.x", "Photo.y", "Chamber.x", 
                      "Position.x", "Type.x", "CO2_treatment.x")]
    colnames(myDF3) <- c("Identity", "Ca280", "Ca400", "A280", "A400",
                         "Chamber", "Position", "Type", "CO2_treatment")
    
    ### normalized sensitivity to per CO2 concentration
    myDF3$A_sens <- with(myDF3, (A400-A280)/(400-280))
    
    ### normalized sensitivity to A400
    myDF3$A_sens_norm <- with(myDF3, (A400-A280)/A280)
    
    write.csv(myDF3, "output/A-Ca/linear_predicted_A_at_280_400_ppm.csv", row.names=F)
    
    plotDF2 <- summaryBy(A_sens+A_sens_norm~Position+Type+CO2_treatment,
                         FUN=c(mean,se), keep.names=T, data=myDF3)
    
    plotDF2$Position <- gsub("12345", "5_Full", plotDF2$Position)
    plotDF2$Position <- gsub("345", "4_TM", plotDF2$Position)
    plotDF2$Position <- gsub("45", "3_Top", plotDF2$Position)
    plotDF2$Position <- gsub("low", "2_low", plotDF2$Position)
    plotDF2$Position <- gsub("up", "1_up", plotDF2$Position)
    
    
    ### test statistics
    mod1 <- lmer(A_sens_norm ~ Position * CO2_treatment + (1|Chamber), data=myDF3)
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
      ylab(expression(paste(delta,  "A / ", A[280])))+
      scale_fill_manual(name="",
                        limits=c("aCO2", "eCO2"),
                        values=c("blue2", "red3"),
                        labels=c(expression(paste(aC[a])),
                                 expression(paste(eC[a]))))+
      scale_color_manual(name="",
                         limits=c("aCO2", "eCO2"),
                         values=c("blue2", "red3"),
                         labels=c(expression(paste(aC[a])),
                                  expression(paste(eC[a]))))+
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
    

    pdf("output/A-Ca/linear_predicted_A_sensitivity_plot_280_400.pdf", width=4, height=4)
    #plot_grid(combined_plots, legend_shared, ncol=1, rel_heights=c(1,0.1))
    plot(p2)
    dev.off()  
  
}
