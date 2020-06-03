plot_roger_2017_model_result_comparison <- function() {
    
    ### read in individual model output from Roger et al., 2017 NP.
    inDF1 <- read.csv("data/concept/Rogers_Bethy.csv")
    inDF2 <- read.csv("data/concept/Rogers_CLM.csv")
    inDF3 <- read.csv("data/concept/Rogers_ED2.csv")
    inDF4 <- read.csv("data/concept/Rogers_GDAY.csv")
    inDF5 <- read.csv("data/concept/Rogers_JSBACH.csv")
    inDF6 <- read.csv("data/concept/Rogers_JULES.csv")
    inDF7 <- read.csv("data/concept/Rogers_OCN.csv")
    
    inDF3 <- inDF3[,c("ca_ED2", "Leaf_45_ED2", "Leaf_60_ED2",
                      "Canopy_45_ED2", "Canopy_60_ED2")]
    
    inDF6 <- inDF6[,c("Ca_JULES", "Leaf_Vcmax45_JULES", "Leaf_Vcmax60_JULES",
                      "Canopy_Vcmax45_JULES", "Canopy_Vcmax60_JULES")]
    
    ### renames
    colnames(inDF1) <- colnames(inDF2) <- colnames(inDF3) <- colnames(inDF4) <- colnames(inDF5) <- colnames(inDF6) <- colnames(inDF7) <- c("Ca", "Vc45_leaf", "Vc60_leaf", "Vc45_can", "Vc60_can")
    inDF1$Model <- "BETHY"
    inDF2$Model <- "CLM"
    inDF3$Model <- "ED2"
    inDF4$Model <- "GDAY"
    inDF5$Model <- "JSBACH"
    inDF6$Model <- "JULES"
    inDF7$Model <- "OCN"

    ### merge all DF
    plotDF <- rbind(inDF1, inDF2, inDF3, inDF4, inDF5, inDF6, inDF7)
    
    subDF <- subset(plotDF, Ca>=350&Ca<=650)
    
    ### plot model outputs
    p1 <- ggplot(data=plotDF, aes(Ca, Vc60_leaf, group=Model)) +
        geom_line(data=plotDF, aes(col=Model))+
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
        xlab(expression(paste(C[a], " (", mu, "mol ", m^-2, " ", s^-1, ")")))+
        ylab(expression(paste(A, " (", mu, "mol "* CO[2], " ", m^-2, " ", s^-1, ")")))+
        scale_colour_colorblind()
    
    ### subDF
    p2 <- ggplot(data=subDF, aes(Ca, Vc60_leaf, group=Model)) +
        geom_line(data=subDF, aes(col=Model))+
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
        xlab(expression(paste(C[a], " (", mu, "mol ", m^-2, " ", s^-1, ")")))+
        ylab(expression(paste(A, " (", mu, "mol "* CO[2], " ", m^-2, " ", s^-1, ")")))+
        scale_colour_colorblind()
    
    ### plot model outputs
    p3 <- ggplot(data=plotDF, aes(Ca, Vc60_can, group=Model)) +
        geom_line(data=plotDF, aes(col=Model))+
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
        xlab(expression(paste(C[a], " (", mu, "mol ", m^-2, " ", s^-1, ")")))+
        ylab(expression(paste(A, " (", mu, "mol "* CO[2], " ", m^-2, " ", s^-1, ")")))+
        scale_colour_colorblind()
    
    
    p4 <- ggplot(data=subDF, aes(Ca, Vc60_can, group=Model)) +
        geom_line(data=subDF, aes(col=Model))+
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
        xlab(expression(paste(C[a], " (", mu, "mol ", m^-2, " ", s^-1, ")")))+
        ylab(expression(paste(A, " (", mu, "mol "* CO[2], " ", m^-2, " ", s^-1, ")")))+
        scale_colour_colorblind()

    
    legend_shared <- get_legend(p1 + theme(legend.position="bottom",
                                           legend.box = 'vertical',
                                           legend.box.just = 'left'))
    
    combined_plots <- plot_grid(p1, p2, p3, p4, 
                                labels=c("(a)", "(b)", "(c)", "(d)"),
                                ncol=2, align="vh", axis = "l",
                                label_x=0.16, label_y=0.95,
                                label_size = 18)
    
    #plot(p1)
    
    pdf("output/simulated/Roger_model_output.pdf", width=10, height=10)
    plot_grid(combined_plots, legend_shared, ncol=1, rel_heights=c(1,0.1))
    dev.off()  
    
    
    ### Fit linear function to the 350 - 650 range, 
    ### then predict A400, A600 responses
    outDF <- data.frame(c("BETHY", "CLM", "ED2", "GDAY",
                          "JSBACH", "JULES", "OCN"), NA, NA, NA, NA, NA,
                        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                        NA, NA, NA, NA, NA)
    colnames(outDF) <- c("Model", "slope_leaf_vc45", "intercept_leaf_vc45", 
                         "r_sq_leaf_vc45", "A400_leaf_vc45", "A600_leaf_vc45",
                         "slope_leaf_vc60", "intercept_leaf_vc60", 
                         "r_sq_leaf_vc60", "A400_leaf_vc60", "A600_leaf_vc60",
                         "slope_can_vc45", "intercept_can_vc45", 
                         "r_sq_can_vc45", "A400_can_vc45", "A600_can_vc45",
                         "slope_can_vc60", "intercept_can_vc60",
                         "r_sq_can_vc60", "A400_can_vc60", "A600_can_vc60")
    
    
    ### loop
    for (i in unique(outDF$Model)) {
        sDF <- subset(subDF, Model==i)
        
        ### fit linear function
        lm1 <- lm(Vc45_leaf~Ca, data=sDF)
        lm2 <- lm(Vc60_leaf~Ca, data=sDF)
        lm3 <- lm(Vc45_can~Ca, data=sDF)
        lm4 <- lm(Vc60_can~Ca, data=sDF)
        
        ### save output
        outDF[outDF$Model==i,"slope_leaf_vc45"] <- coef(lm1)[2]
        outDF[outDF$Model==i,"slope_leaf_vc60"] <- coef(lm2)[2]
        outDF[outDF$Model==i,"slope_can_vc45"] <- coef(lm3)[2]
        outDF[outDF$Model==i,"slope_can_vc60"] <- coef(lm4)[2]
        
        outDF[outDF$Model==i,"intercept_leaf_vc45"] <- coef(lm1)[1]
        outDF[outDF$Model==i,"intercept_leaf_vc60"] <- coef(lm2)[1]
        outDF[outDF$Model==i,"intercept_can_vc45"] <- coef(lm3)[1]
        outDF[outDF$Model==i,"intercept_can_vc60"] <- coef(lm4)[1]
        
        outDF[outDF$Model==i,"r_sq_leaf_vc45"] <- summary(lm1)$r.squared
        outDF[outDF$Model==i,"r_sq_leaf_vc60"] <- summary(lm2)$r.squared
        outDF[outDF$Model==i,"r_sq_can_vc45"] <- summary(lm3)$r.squared
        outDF[outDF$Model==i,"r_sq_can_vc60"] <- summary(lm4)$r.squared
        
    }
    
    
    ### predict 400 and 600
    outDF$A400_leaf_vc45 <- outDF$slope_leaf_vc45 * 400 + outDF$intercept_leaf_vc45
    outDF$A400_leaf_vc60 <- outDF$slope_leaf_vc60 * 400 + outDF$intercept_leaf_vc60
    outDF$A400_can_vc45 <- outDF$slope_can_vc45 * 400 + outDF$intercept_can_vc45
    outDF$A400_can_vc60 <- outDF$slope_can_vc60 * 400 + outDF$intercept_can_vc60
    
    outDF$A600_leaf_vc45 <- outDF$slope_leaf_vc45 * 600 + outDF$intercept_leaf_vc45
    outDF$A600_leaf_vc60 <- outDF$slope_leaf_vc60 * 600 + outDF$intercept_leaf_vc60
    outDF$A600_can_vc45 <- outDF$slope_can_vc45 * 600 + outDF$intercept_can_vc45
    outDF$A600_can_vc60 <- outDF$slope_can_vc60 * 600 + outDF$intercept_can_vc60
    
    ### (A600-A400)/A400 to look at sensitivity
    outDF$sens_leaf_vc45 <- (outDF$A600_leaf_vc45 - outDF$A400_leaf_vc45)/outDF$A400_leaf_vc45
    outDF$sens_leaf_vc60 <- (outDF$A600_leaf_vc60 - outDF$A400_leaf_vc60)/outDF$A400_leaf_vc60
    outDF$sens_can_vc45 <- (outDF$A600_can_vc45 - outDF$A400_can_vc45)/outDF$A400_can_vc45
    outDF$sens_can_vc60 <- (outDF$A600_can_vc60 - outDF$A400_can_vc60)/outDF$A400_can_vc60
    
    ### convert into long format
    plotDF2 <- data.frame(rep(c("BETHY", "CLM", "ED2", "GDAY",
                            "JSBACH", "JULES", "OCN"), 4), 
                          rep(c("Vcmax45", "Vcmax60"), each=7), 
                          rep(c("Leaf", "Canopy"), each=14), NA)
    colnames(plotDF2)<- c("Model", "Vcmax", "Position", "Sensitivity")
    
    for (i in unique(plotDF2$Model)) {
        plotDF2$Sensitivity[plotDF2$Model==i&plotDF2$Vcmax=="Vcmax45"&plotDF2$Position=="Leaf"] <- 
            outDF$sens_leaf_vc45[outDF$Model==i]
        
        plotDF2$Sensitivity[plotDF2$Model==i&plotDF2$Vcmax=="Vcmax60"&plotDF2$Position=="Leaf"] <- 
            outDF$sens_leaf_vc60[outDF$Model==i]
        
        plotDF2$Sensitivity[plotDF2$Model==i&plotDF2$Vcmax=="Vcmax45"&plotDF2$Position=="Canopy"] <- 
            outDF$sens_can_vc45[outDF$Model==i]
        
        plotDF2$Sensitivity[plotDF2$Model==i&plotDF2$Vcmax=="Vcmax60"&plotDF2$Position=="Canopy"] <- 
            outDF$sens_can_vc60[outDF$Model==i]
    }
    
    
    ### summary stats
    sumDF <- summaryBy(Sensitivity~Vcmax+Position, FUN=c(mean, se), 
                       data=plotDF2, keep.names=T, na.rm=T)
    
    
    ### plotting
    #p5 <- ggplot(data=sumDF, 
    #             aes(Vcmax, Sensitivity.mean)) +
    #    geom_bar(stat = "identity", aes(fill=Position), 
    #             position="dodge", alpha=0.5) +
    #    geom_errorbar(aes(x=Vcmax, ymin=Sensitivity.mean-Sensitivity.se, 
    #                      ymax=Sensitivity.mean+Sensitivity.se, 
    #                      group=as.factor(Position)), 
    #                  position=position_dodge(0.9), width=0.2) +
    #    geom_point(data=plotDF2, 
    #               mapping=aes(x=Vcmax, y=Sensitivity, group=Position, col=Model), 
    #               size=4, 
    #               position = position_jitterdodge(0.9))+
    #    theme_linedraw() +
    #    theme(panel.grid.minor=element_blank(),
    #          axis.text.x=element_text(size=12),
    #          axis.title.x=element_text(size=14),
    #          axis.text.y=element_text(size=12),
    #          axis.title.y=element_text(size=14),
    #          legend.text=element_text(size=12),
    #          legend.title=element_text(size=14),
    #          panel.grid.major=element_blank(),
    #          legend.position="bottom",
    #          legend.box = 'vertical',
    #          legend.box.just = 'left')+
    #    ylab(expression(paste(delta * A * " / " * A[400])))+
    #    scale_fill_colorblind(name="Level")+
    #    scale_color_colorblind(guide=guide_legend(nrow=3))+
    #    xlab("")+
    #    scale_x_discrete(breaks=c("Vcmax45", "Vcmax60"),
    #                     labels=c(expression(paste(V[cmax45])),
    #                              expression(paste(V[cmax60]))))+
    #    ylim(0, 0.4)
    
    
    ### add WTC result and compare
    wtcDF <- read.csv("output/A-Ca/linear_predicted_A_at_400_600_ppm.csv")
    
    sumDF2 <- summaryBy(A_sens_norm~Type+Position, FUN=c(mean,se),
                        data=wtcDF, keep.names=T, na.rm=T)
    
    p6 <- ggplot(data=sumDF2, 
                 aes(Position, A_sens_norm.mean)) +
        geom_bar(stat = "identity", aes(fill=Type, col=Position), 
                 position="dodge", alpha=0.5) +
        geom_errorbar(aes(x=Position, ymin=A_sens_norm.mean-A_sens_norm.se, 
                          ymax=A_sens_norm.mean+A_sens_norm.se, 
                          group=as.factor(Position)), 
                      position=position_dodge(0.9), width=0.2) +
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
        ylab(expression(paste(delta * A * " / " * A[400])))+
        scale_fill_colorblind(name="Level")+
        scale_color_colorblind(name="Position",
                               breaks=c("12345", "345", "45", "low", "up"),
                               labels=c("Full", "T+M", "T", "Low", "Up"),
                               guide=guide_legend(nrow=3))+
        xlab("")+
        scale_x_discrete(breaks=c("12345", "345", "45", "low", "up"),
                         labels=c("Full", "T+M", "T", "Low", "Up"))+
        ylim(0, 0.4)
    
    #plot(p6)
    
    #pdf("output/A-Ca/Roger_model_sensitivity.pdf", width=8, height=6)
    #plot_grid(p5, p6, labels="", ncol=2, align="v", axis = "l")
    #dev.off()
    
    
    
    #### alternative plot of Roger sensitivity
    ## only plot Vcmax = 60
    plotDF2$xlab1 <- paste0(plotDF2$Vcmax, "-", plotDF2$Position)    
    plotDF2$xlab2 <- paste0(plotDF2$Vcmax, "-", plotDF2$Model)    
    plotDF2$xlab3 <- paste0(plotDF2$Position, "-", plotDF2$Model)
    
    plotDF2$xlab1 <- gsub("Vcmax45-Leaf", "3_Vcmax45-Leaf", plotDF2$xlab1)
    plotDF2$xlab1 <- gsub("Vcmax45-Canopy", "4_Vcmax45-Canopy", plotDF2$xlab1)
    plotDF2$xlab1 <- gsub("Vcmax60-Leaf", "1_Vcmax60-Leaf", plotDF2$xlab1)
    plotDF2$xlab1 <- gsub("Vcmax60-Canopy", "2_Vcmax60-Canopy", plotDF2$xlab1)

    ### plotting
    #p7 <- ggplot(data=plotDF2, 
    #             aes(x=xlab1, y=Sensitivity, group=xlab3)) +
    #    geom_point(aes(shape=Position, fill=Model), 
    #               size=4, col="black")+
    #    geom_line(aes(col=Model))+
    #    theme_linedraw() +
    #    theme(panel.grid.minor=element_blank(),
    #          axis.text.x=element_text(size=12),
    #          axis.title.x=element_text(size=14),
    #          axis.text.y=element_text(size=12),
    #          axis.title.y=element_text(size=14),
    #          legend.text=element_text(size=12),
    #          legend.title=element_text(size=14),
    #          panel.grid.major=element_blank(),
    #          legend.position="bottom",
    #          legend.box = 'vertical',
    #          legend.box.just = 'left')+
    #    ylab(expression(paste(delta * A * " / " * A[400])))+
    #    scale_color_colorblind(name="Model",
    #                           guide=guide_legend(nrow=3))+
    #    scale_fill_colorblind(name="Model",
    #                          guide=guide_legend(nrow=3,
    #                                             override.aes = list(shape = 21)))+
    #    xlab("")+
    #    scale_x_discrete(breaks=c("3_Vcmax45-Leaf", "4_Vcmax45-Canopy", 
    #                              "1_Vcmax60-Leaf", "2_Vcmax60-Canopy"),
    #                     labels=c(expression(paste("Leaf ", V[cmax45])),
    #                              expression(paste("Canopy ", V[cmax45])),
    #                              expression(paste("Leaf ", V[cmax60])),
    #                              expression(paste("Canopy ", V[cmax60]))))+
    #    scale_shape_manual(name="Type",
    #                       values=c(21, 24),
    #                       labels=c("Canopy", "Leaf"))+
    #    ylim(0.0, 0.4)
    
    
    p7 <- ggplot(data=plotDF2, 
                 aes(x=xlab1, y=Sensitivity, group=xlab2)) +
        geom_point(aes(shape=Position, fill=Model), 
                   size=4, col="black")+
        geom_line(aes(col=Model))+
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
        ylab(expression(paste(delta * A * " / " * A[400])))+
        scale_color_colorblind(name="Model",
                               guide=guide_legend(nrow=3))+
        scale_fill_colorblind(name="Model",
                              guide=guide_legend(nrow=3,
                                                 override.aes = list(shape = 21)))+
        xlab("")+
        scale_x_discrete(breaks=c("3_Vcmax45-Leaf", "4_Vcmax45-Canopy", 
                                  "1_Vcmax60-Leaf", "2_Vcmax60-Canopy"),
                         labels=c(expression(paste("Leaf ", V[cmax45])),
                                  expression(paste("Canopy ", V[cmax45])),
                                  expression(paste("Leaf ", V[cmax60])),
                                  expression(paste("Canopy ", V[cmax60]))))+
        scale_shape_manual(name="Type",
                           values=c(21, 24),
                           labels=c("Canopy", "Leaf"))+
        ylim(0.0, 0.4)
    
    ### prepare WTC results
    sumDF3 <- summaryBy(A_sens_norm~Type+Position+CO2_treatment, FUN=c(mean,se),
                        data=wtcDF, keep.names=T, na.rm=T)
    
    sumDF3$Position <- gsub("12345", "5_Full", sumDF3$Position)
    sumDF3$Position <- gsub("345", "4_TM", sumDF3$Position)
    sumDF3$Position <- gsub("45", "3_Top", sumDF3$Position)
    sumDF3$Position <- gsub("low", "2_low", sumDF3$Position)
    sumDF3$Position <- gsub("up", "1_up", sumDF3$Position)
    
    subDF1 <- subset(sumDF3, CO2_treatment == "aCO2")
    subDF2 <- subset(sumDF3, CO2_treatment == "eCO2")
    
    p8 <- ggplot(data=subDF1, 
                 aes(Position, A_sens_norm.mean)) +
        geom_bar(stat = "identity", aes(fill=Position), 
                 position="dodge") +
        geom_errorbar(aes(x=Position, ymin=A_sens_norm.mean-A_sens_norm.se, 
                          ymax=A_sens_norm.mean+A_sens_norm.se, 
                          group=as.factor(Position)), 
                      position=position_dodge(0.9), width=0.2) +
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
        ylab(expression(paste(delta * A * " / " * A[400])))+
        scale_fill_manual(name="Position",
                          limits=c("5_Full", "4_TM", "3_Top", "2_low", "1_up"),
                          values=c("blue2", "red3", "purple", "green", "orange"),
                          labels=c("Full", "T+M", "Top", "Low", "Up"),
                          guide=guide_legend(nrow=5))+
        xlab("")+
        scale_x_discrete(name="", 
                         breaks=c("5_Full", "4_TM", "3_Top", "2_low", "1_up"), 
                         labels=c("Full", "T+M", "Top", "Low", "Up"))+
        ylim(0.0, 0.4)+
        annotate("text", x = 1, y = 0.33, size = 8, label = "atop(bold(a))", parse = TRUE)+
        annotate("text", x = 2, y = 0.33, size = 8, label = "atop(bold(b))", parse = TRUE)+
        annotate("text", x = 3, y = 0.33, size = 8, label = "atop(bold(a))", parse = TRUE)+
        annotate("text", x = 4, y = 0.33, size = 8, label = "atop(bold(b))", parse = TRUE)+
        annotate("text", x = 5, y = 0.33, size = 8, label = "atop(bold(b))", parse = TRUE)
    
    
    
    pdf("output/simulated/Roger_model_sensitivity_400_600.pdf", width=12, height=6)
    plot_grid(p8, p7, ncol=2, align="v", axis = "l",
              labels=c("(a)", "(b)"),
              label_x=0.86, label_y=0.98,
              label_size = 18)
    dev.off()
    
    
}

