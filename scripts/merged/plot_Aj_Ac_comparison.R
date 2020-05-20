plot_Aj_Ac_comparison <- function() {
    #### read in fitaci data at canopy and leaf scales and plot the Aj and Ac comparison
    
    ### we have multiple dates in leaf-scale measurements
    stDF.c <- read.csv("output/canopy/canopy_scale_parameters.csv", header=T)
    stDF.l <- read.csv("output/leaf/leaf_scale_parameters.csv", header=T)
    
    ### subset chambers
    subDF.l <- subset(stDF.l, Chamber%in%c("1", "3", "11", "4", "8"))
    subDF.c <- subset(stDF.c, Chamber%in%c("1", "3", "11", "4", "8"))
    
    ### subset columns
    subDF.l <- subDF.l[,c("Identity", "RMSE", "Vcmax", "Vcmax.se", "Jmax",
                          "Jmax.se", "Rd", "Rd.se", "Ci_400", "ALEAF_400", "GS_400", "ELEAF_400", 
                          "Ac_400","Aj_400", "Ap_400", 
                          "Ci_600", "ALEAF_600", "GS_600", "ELEAF_600", 
                          "Ac_600","Aj_600", "Ap_600", 
                          "VPD", "Tleaf", "Ca", "Cc", "PPFD", 
                          "Ci_transition_Ac_Aj", "GammaStar", "Km", "G1", "JVratio")]
    
    subDF.c <- subDF.c[,c("Identity", "RMSE", "Vcmax", "Vcmax.se", "Jmax",
                          "Jmax.se", "Rd", "Rd.se", "Ci_400", "ALEAF_400", "GS_400", "ELEAF_400", 
                          "Ac_400", "Aj_400", "Ap_400", 
                          "Ci_600", "ALEAF_600", "GS_600", "ELEAF_600", 
                          "Ac_600","Aj_600", "Ap_600", 
                          "VPD", "Tleaf", "Ca", "Cc", "PPFD", 
                          "Ci_transition_Ac_Aj", "GammaStar", "Km", "G1", "JVratio")]
    
    ### change col names
    stDF <- rbind(subDF.l, subDF.c)
    stDF <- merge(stDF, idDF, by="Identity", all=T)
    
    
    ### convert into factors
    stDF$Type <- as.factor(stDF$Type)
    stDF$CO2_treatment <- as.factor(stDF$CO2_treatment)
    
    plotDF1 <- subset(stDF, CO2_treatment=="aCO2")
    plotDF2 <- subset(stDF, CO2_treatment=="eCO2")
    
    ### fit linear regression and add R2 and slope to the plot
    lm1 <- lm(Ac_400~Aj_400, data=plotDF1)
    a1 <- round(coefficients(lm1)[2],2)
    r1 <- round(summary(lm1)$adj.r.squared,2)
    
    lm2 <- lm(Ac_400~Aj_400, data=plotDF2)
    a2 <- round(coefficients(lm2)[2],2)
    r2 <- round(summary(lm2)$adj.r.squared,2)
    
    lm3 <- lm(Ac_600~Aj_600, data=plotDF1)
    a3 <- round(coefficients(lm3)[2],2)
    r3 <- round(summary(lm3)$adj.r.squared,2)
    
    lm4 <- lm(Ac_600~Aj_600, data=plotDF2)
    a4 <- round(coefficients(lm4)[2],2)
    r4 <- round(summary(lm4)$adj.r.squared,2)
    
    
    ### plotting script
    p1 <- ggplot(stDF, aes(Aj_400, Ac_400)) +
        geom_point(data=stDF, aes(Aj_400, Ac_400,
                                     fill=as.factor(Position), 
                                     pch = as.factor(Type)), alpha=1.0, size=4)+
        geom_smooth(method='lm', se=T, col="black")+
        geom_abline(slope=1, intercept=0, col="black", lty=2)+
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
        xlab(expression(paste(A[j], " (", mu, "mol "* CO[2], " ", m^-2, " ", s^-1, ")")))+
        ylab(expression(paste(A[c], " (", mu, "mol "* CO[2], " ", m^-2, " ", s^-1, ")")))+
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
        ylim(0, 40)+
        xlim(0, 40)

    
    p2 <- ggplot(plotDF1, aes(Aj_400, Ac_400)) +
        geom_point(data=plotDF1, aes(Aj_400, Ac_400,
                                     fill=as.factor(Position), 
                                     pch = as.factor(Type)), alpha=1.0, size=4)+
        geom_smooth(method='lm', se=T, col="black")+
        geom_abline(slope=1, intercept=0, col="black", lty=2)+
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
        xlab(expression(paste(A[j], " (", mu, "mol "* CO[2], " ", m^-2, " ", s^-1, ")")))+
        ylab(expression(paste(A[c], " (", mu, "mol "* CO[2], " ", m^-2, " ", s^-1, ")")))+
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
        ylim(0, 50)+
        xlim(0, 50)+
        ggtitle(expression(paste(aC[a])))+
        annotate(geom="text", x=30, y=10, 
                 label = "atop(a == 1.04, r^2 == 0.9)", parse = TRUE, size=10)
    
    
    p3 <- ggplot(plotDF2, aes(Aj_400, Ac_400)) +
        geom_point(data=plotDF2, aes(Aj_400, Ac_400,
                                  fill=as.factor(Position), 
                                  pch = as.factor(Type)), alpha=1.0, size=4)+
        geom_smooth(method='lm', se=T, col="black")+
        geom_abline(slope=1, intercept=0, col="black", lty=2)+
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
        xlab(expression(paste(A[j], " (", mu, "mol "* CO[2], " ", m^-2, " ", s^-1, ")")))+
        ylab(expression(paste(A[c], " (", mu, "mol "* CO[2], " ", m^-2, " ", s^-1, ")")))+
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
        ylim(0, 50)+
        xlim(0, 50)+
        ggtitle(expression(paste(eC[a])))+
        annotate(geom="text", x=30, y=10, 
                 label = "atop(a == 1.05, r^2 == 0.9)", parse = TRUE, size=10)
    
    
    
    
    p4 <- ggplot(plotDF1, aes(Aj_600, Ac_600)) +
        geom_point(data=plotDF1, aes(Aj_600, Ac_600,
                                     fill=as.factor(Position), 
                                     pch = as.factor(Type)), alpha=1.0, size=4)+
        geom_smooth(method='lm', se=T, col="black")+
        geom_abline(slope=1, intercept=0, col="black", lty=2)+
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
        xlab(expression(paste(A[j], " (", mu, "mol "* CO[2], " ", m^-2, " ", s^-1, ")")))+
        ylab(expression(paste(A[c], " (", mu, "mol "* CO[2], " ", m^-2, " ", s^-1, ")")))+
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
        ylim(0, 70)+
        xlim(0, 70)+
        annotate(geom="text", x=40, y=10, 
                 label = "atop(a == 1.27, r^2 == 0.9)", parse = TRUE, size=10)
    
    
    p5 <- ggplot(plotDF2, aes(Aj_600, Ac_600)) +
        geom_point(data=plotDF2, aes(Aj_600, Ac_600,
                                     fill=as.factor(Position), 
                                     pch = as.factor(Type)), alpha=1.0, size=4)+
        geom_smooth(method='lm', se=T, col="black")+
        geom_abline(slope=1, intercept=0, col="black", lty=2)+
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
        xlab(expression(paste(A[j], " (", mu, "mol "* CO[2], " ", m^-2, " ", s^-1, ")")))+
        ylab(expression(paste(A[c], " (", mu, "mol "* CO[2], " ", m^-2, " ", s^-1, ")")))+
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
        ylim(0, 70)+
        xlim(0, 70)+
        annotate(geom="text", x=40, y=10, 
                 label = "atop(a == 1.28, r^2 == 0.9)", parse = TRUE, size=10)
    
    

    #### plot legends
    legend_shared <- get_legend(p2 + theme(legend.position="bottom",
                                           legend.box = 'vertical',
                                           legend.box.just = 'left'))
    
    combined_plots <- plot_grid(p2, p3, p4, p5,
                                labels=c("(a)", "(b)", "(c)", "(d)"),
                                ncol=2, align="vh", axis = "l",
                                label_x=0.16, label_y=0.88,
                                label_size = 18)
    
    pdf("output/biochemical_parameters/Ac_vs_Aj_comparison.pdf", width=10, height=12)
    plot_grid(combined_plots, legend_shared, ncol=1, rel_heights=c(1,0.1))
    dev.off()  
    
   
    #######################################################################################
    #### plot A600/A400 ratio
    stDF$A600_over_A400 <- with(stDF, ALEAF_600/ALEAF_400)
    stDF$Ac600_over_Ac400 <- with(stDF, Ac_600/Ac_400)
    stDF$Aj600_over_Aj400 <- with(stDF, Aj_600/Aj_400)
    
    ### subset
    subDF1 <- stDF[,c("A600_over_A400", "CO2_treatment", "Chamber", "Position", "Type")]
    subDF2 <- stDF[,c("Ac600_over_Ac400", "CO2_treatment", "Chamber", "Position", "Type")]
    subDF3 <- stDF[,c("Aj600_over_Aj400", "CO2_treatment", "Chamber", "Position", "Type")]
    
    subDF1$lab <- "A"
    subDF2$lab <- "Ac"
    subDF3$lab <- "Aj"
    
    colnames(subDF1) <- colnames(subDF2) <- colnames(subDF3) <- c("ratio", "CO2_treatment", "Chamber", "Position", "Type", "lab")
    
    plotDF1 <- rbind(subDF1, subDF2, subDF3)
    
    ### summary By
    plotDF2 <- summaryBy(ratio~Position+lab, 
                         FUN=c(mean, se), data=plotDF1, keep.names=T)
    
    ### plotting
    p1 <- ggplot(data=plotDF2[plotDF2$Position=="45",], 
                 aes(lab, ratio.mean)) +
        geom_bar(stat = "identity", aes(fill=lab), 
                 position="dodge") +
        geom_errorbar(aes(x=lab, ymin=ratio.mean-ratio.se, 
                          ymax=ratio.mean+ratio.se), 
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
              legend.position="none",
              legend.box = 'vertical',
              legend.box.just = 'left',
              plot.title = element_text(size=16, face="bold", 
                                        hjust = 0.5))+
        ylab(expression(paste(A[600] * " / " * A[400])))+
        scale_fill_manual(name="",
                          breaks=c("A", "Ac", "Aj"),
                          labels=c("A", expression(paste(A[c])),
                                   expression(paste(A[j]))),
                          values = colorblind_pal()(3 + 1)[-1])+
        xlab("")+
        scale_x_discrete(breaks=c("A", "Ac", "Aj"),
                         labels=c("A", expression(paste(A[c])),
                                  expression(paste(A[j]))))+
        ylim(0, 2)+
        ggtitle("Canopy-scale")
    
    p2 <- ggplot(data=plotDF2[plotDF2$Position=="345",], 
                 aes(lab, ratio.mean)) +
        geom_bar(stat = "identity", aes(fill=lab), 
                 position="dodge") +
        geom_errorbar(aes(x=lab, ymin=ratio.mean-ratio.se, 
                          ymax=ratio.mean+ratio.se), 
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
              legend.position="none",
              legend.box = 'vertical',
              legend.box.just = 'left',
              plot.title = element_text(size=16, face="bold", 
                                        hjust = 0.5))+
        ylab(expression(paste(A[600] * " / " * A[400])))+
        scale_fill_manual(name="",
                          breaks=c("A", "Ac", "Aj"),
                          labels=c("A", expression(paste(A[c])),
                                   expression(paste(A[j]))),
                          values = colorblind_pal()(3 + 1)[-1])+
        xlab("")+
        scale_x_discrete(breaks=c("A", "Ac", "Aj"),
                         labels=c("A", expression(paste(A[c])),
                                  expression(paste(A[j]))))+
        ylim(0, 2)
    
    p3 <- ggplot(data=plotDF2[plotDF2$Position=="12345",], 
                 aes(lab, ratio.mean)) +
        geom_bar(stat = "identity", aes(fill=lab), 
                 position="dodge") +
        geom_errorbar(aes(x=lab, ymin=ratio.mean-ratio.se, 
                          ymax=ratio.mean+ratio.se), 
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
              legend.position="none",
              legend.box = 'vertical',
              legend.box.just = 'left',
              plot.title = element_text(size=16, face="bold", 
                                        hjust = 0.5))+
        ylab(expression(paste(A[600] * " / " * A[400])))+
        scale_fill_manual(name="",
                          breaks=c("A", "Ac", "Aj"),
                          labels=c("A", expression(paste(A[c])),
                                   expression(paste(A[j]))),
                          values = colorblind_pal()(3 + 1)[-1])+
        xlab("")+
        scale_x_discrete(breaks=c("A", "Ac", "Aj"),
                         labels=c("A", expression(paste(A[c])),
                                  expression(paste(A[j]))))+
        ylim(0, 2)
    
    
    p4 <- ggplot(data=plotDF2[plotDF2$Position=="up",], 
                 aes(lab, ratio.mean)) +
        geom_bar(stat = "identity", aes(fill=lab), 
                 position="dodge") +
        geom_errorbar(aes(x=lab, ymin=ratio.mean-ratio.se, 
                          ymax=ratio.mean+ratio.se), 
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
              legend.position="none",
              legend.box = 'vertical',
              legend.box.just = 'left',
              plot.title = element_text(size=16, face="bold", 
                                        hjust = 0.5))+
        ylab(expression(paste(A[600] * " / " * A[400])))+
        scale_fill_manual(name="",
                          breaks=c("A", "Ac", "Aj"),
                          labels=c("A", expression(paste(A[c])),
                                   expression(paste(A[j]))),
                          values = colorblind_pal()(3 + 1)[-1])+
        xlab("")+
        scale_x_discrete(breaks=c("A", "Ac", "Aj"),
                         labels=c("A", expression(paste(A[c])),
                                  expression(paste(A[j]))))+
        ylim(0, 2)+
        ggtitle("Leaf-scale")
    
    p5 <- ggplot(data=plotDF2[plotDF2$Position=="low",], 
                 aes(lab, ratio.mean)) +
        geom_bar(stat = "identity", aes(fill=lab), 
                 position="dodge") +
        geom_errorbar(aes(x=lab, ymin=ratio.mean-ratio.se, 
                          ymax=ratio.mean+ratio.se), 
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
              legend.position="none",
              legend.box = 'vertical',
              legend.box.just = 'left',
              plot.title = element_text(size=16, face="bold", 
                                        hjust = 0.5))+
        ylab(expression(paste(A[600] * " / " * A[400])))+
        scale_fill_manual(name="",
                          breaks=c("A", "Ac", "Aj"),
                          labels=c("A", expression(paste(A[c])),
                                   expression(paste(A[j]))),
                          values = colorblind_pal()(3 + 1)[-1])+
        xlab("")+
        scale_x_discrete(breaks=c("A", "Ac", "Aj"),
                         labels=c("A", expression(paste(A[c])),
                                  expression(paste(A[j]))))+
        ylim(0, 2)
    
    
    legend_shared <- get_legend(p1 + theme(legend.position="bottom",
                                           legend.box = 'vertical',
                                           legend.box.just = 'left'))
    
    combined_plots <- plot_grid(p1, p4, p2, p5, p3,
                                labels=c("(a)", "(d)", "(b)", "(e)", "(c)"),
                                label_x=0.86, label_y=0.86,
                                ncol=2, align="vh", axis = "l")
    
    #plot(p1)
    
    pdf("output/biochemical_parameters/relative_contribution_Ac_Aj.pdf", width=8, height=10)
    plot_grid(combined_plots, legend_shared, ncol=1, rel_heights=c(1,0.1))
    dev.off()  
    
    
    
    
    ######################## combine all results together
    ###
    plotDF3 <- summaryBy(ratio~lab, 
                         FUN=c(mean, se), data=plotDF1, keep.names=T)
    
    p1 <- ggplot(data=plotDF3, 
                 aes(lab, ratio.mean)) +
        geom_bar(stat = "identity", aes(fill=lab), 
                 position="dodge") +
        geom_errorbar(aes(x=lab, ymin=ratio.mean-ratio.se, 
                          ymax=ratio.mean+ratio.se), 
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
              legend.position="none",
              legend.box = 'vertical',
              legend.box.just = 'left',
              plot.title = element_text(size=16, face="bold", 
                                        hjust = 0.5))+
        ylab(expression(paste(A[600] * " / " * A[400])))+
        scale_fill_manual(name="",
                          breaks=c("A", "Ac", "Aj"),
                          labels=c("A", expression(paste(A[c])),
                                   expression(paste(A[j]))),
                          values = colorblind_pal()(3 + 1)[-1])+
        xlab("")+
        scale_x_discrete(breaks=c("A", "Ac", "Aj"),
                         labels=c("A", expression(paste(A[c])),
                                  expression(paste(A[j]))))+
        ylim(0, 2)
    

    pdf("output/biochemical_parameters/relative_contribution_Ac_Aj_all_merged.pdf", width=4, height=4)
    plot(p1)
    dev.off()  
    
    
    ###############################################################################
    #### look at A, Vcmax and Jmax, following Rogers & Humphries 2000, GCB Figure 1

    
}