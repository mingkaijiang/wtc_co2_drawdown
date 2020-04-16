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
                                labels="AUTO", ncol=2, align="vh", axis = "l")
    
    #plot(p1)
    
    pdf("output/A-Ca/Roger_model_output.pdf", width=10, height=10)
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
    p5 <- ggplot() +
        geom_bar(data=sumDF, stat = "identity", 
                 aes(Vcmax, Sensitivity.mean, color=Position, fill=Position), 
                 position="dodge", alpha=0.2) +
        geom_point(data=plotDF2, 
                   mapping=aes(x=Vcmax, y=Sensitivity, fill=Model), 
                   size=4, position = position_dodge(0.9), col="black")+
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
        ylab(expression(paste(A, " (", mu, "mol "* CO[2], " ", m^-2, " ", s^-1, ")")))+
        scale_colour_colorblind()
    
    
    plot(p5)
    
    
    
}

