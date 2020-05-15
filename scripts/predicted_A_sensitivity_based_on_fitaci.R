predicted_A_sensitivity_based_on_fitaci <- function(cDF) {
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
    
    ################################# plotting
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
                          labels=c("Full", "T+M", "Top", "Up", "Low"))+
        scale_color_manual(name="Position",
                           limits=c("12345", "345", "45", "up", "low"),
                           values=c("blue2", "red3", "purple", "orange", "darkgreen"),
                           labels=c("Full", "T+M", "Top", "Up", "Low"))+
        scale_shape_manual(name="Measurements",
                           values=c(21, 24),
                           labels=c("Canopy", "Leaf"))+
        scale_x_discrete(name="", 
                         breaks=c("12345", "345", "45", "up", "low"), 
                         labels=c("Full", "T+M", "Top", "Up", "Low"))+
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
                          labels=c("Full", "T+M", "Top", "Up", "Low"))+
        scale_color_manual(name="Position",
                           limits=c("12345", "345", "45", "up", "low"),
                           values=c("blue2", "red3", "purple", "orange", "darkgreen"),
                           labels=c("Full", "T+M", "Top", "Up", "Low"))+
        scale_shape_manual(name="Measurements",
                           values=c(21, 24),
                           labels=c("Canopy", "Leaf"))+
        scale_x_discrete(name="", 
                         breaks=c("12345", "345", "45", "up", "low"), 
                         labels=c("Full", "T+M", "Top", "Up", "Low"))+
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
                          labels=c("Full", "T+M", "Top", "Up", "Low"))+
        scale_color_manual(name="Position",
                           limits=c("12345", "345", "45", "up", "low"),
                           values=c("blue2", "red3", "purple", "orange", "darkgreen"),
                           labels=c("Full", "T+M", "Top", "Up", "Low"))+
        scale_shape_manual(name="Measurements",
                           values=c(21, 24),
                           labels=c("Canopy", "Leaf"))+
        scale_x_discrete(name="", 
                         breaks=c("12345", "345", "45", "up", "low"), 
                         labels=c("Full", "T+M", "Top", "Up", "Low"))+
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
                          labels=c("Full", "T+M", "Top", "Up", "Low"))+
        scale_color_manual(name="Position",
                           limits=c("12345", "345", "45", "up", "low"),
                           values=c("blue2", "red3", "purple", "orange", "darkgreen"),
                           labels=c("Full", "T+M", "Top", "Up", "Low"))+
        scale_shape_manual(name="Measurements",
                           values=c(21, 24),
                           labels=c("Canopy", "Leaf"))+
        scale_x_discrete(name="", 
                         breaks=c("12345", "345", "45", "up", "low"), 
                         labels=c("Full", "T+M", "Top", "Up", "Low"))+
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
                          labels=c("Full", "T+M", "Top", "Up", "Low"))+
        scale_color_manual(name="Position",
                           limits=c("12345", "345", "45", "up", "low"),
                           values=c("blue2", "red3", "purple", "orange", "darkgreen"),
                           labels=c("Full", "T+M", "Top", "Up", "Low"))+
        scale_shape_manual(name="Measurements",
                           values=c(21, 24),
                           labels=c("Canopy", "Leaf"))+
        scale_x_discrete(name="", 
                         breaks=c("12345", "345", "45", "up", "low"), 
                         labels=c("Full", "T+M", "Top", "Up", "Low"))+
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
                          labels=c("Full", "T+M", "Top", "Up", "Low"))+
        scale_color_manual(name="Position",
                           limits=c("12345", "345", "45", "up", "low"),
                           values=c("blue2", "red3", "purple", "orange", "darkgreen"),
                           labels=c("Full", "T+M", "Top", "Up", "Low"))+
        scale_shape_manual(name="Measurements",
                           values=c(21, 24),
                           labels=c("Canopy", "Leaf"))+
        scale_x_discrete(name="", 
                         breaks=c("12345", "345", "45", "up", "low"), 
                         labels=c("Full", "T+M", "Top", "Up", "Low"))+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21, 21, 24, 24))))
    
    
    
    legend_shared <- get_legend(p1 + theme(legend.position="bottom",
                                           legend.box = 'vertical',
                                           legend.box.just = 'left'))
    
    combined_plots <- plot_grid(p1, p2, p3, p4, p5, p6, 
                                labels=c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)"),
                                ncol=2, align="vh", axis = "l",
                                label_x=0.16, label_y=0.95,
                                label_size = 18)
    
    #plot(p1)
    
    pdf("output/A-Ca/predicted_ambient_A_sensitivity_plot.pdf", width=10, height=14)
    plot_grid(combined_plots, legend_shared, ncol=1, rel_heights=c(1,0.1))
    dev.off()  
    
}