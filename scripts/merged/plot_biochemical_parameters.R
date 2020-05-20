plot_biochemical_parameters <- function() {
    
    
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
    
    
    ### convert into factors
    stDF$Type <- as.factor(stDF$Type)
    stDF$CO2_treatment <- as.factor(stDF$CO2_treatment)
    
    ### prepare plotDF
    plotDF3 <- summaryBy(Vcmax+Jmax+Ci_transition_Ac_Aj+JVratio~Position+Type+CO2_treatment,
                         FUN=c(mean,se), keep.names=T, data=stDF)
    plotDF3$Position <- gsub("12345", "5_Full", plotDF3$Position)
    plotDF3$Position <- gsub("345", "4_TM", plotDF3$Position)
    plotDF3$Position <- gsub("45", "3_Top", plotDF3$Position)
    plotDF3$Position <- gsub("low", "2_low", plotDF3$Position)
    plotDF3$Position <- gsub("up", "1_up", plotDF3$Position)
    
    ### perform linear mixed effect model statistics
    ### check type effect, ignoring CO2 and position effect
    mod1 <- lmer(Vcmax~ Position * CO2_treatment + (1|Chamber), data=stDF)
    out1 <- anova(mod1)
    lab1 <- summary(glht(mod1, linfct = mcp(Position = "Tukey")))
    #eff.size1 <- round(lab1$test$coefficients[1], 1)
    #eff.sig1 <- round(lab1$test$pvalues[1], 3)
    #eff.error1 <- round(lab1$test$sigma[1], 1)
    
    mod2 <- lmer(Jmax~ Position * CO2_treatment + (1|Chamber), data=stDF)
    out2 <- anova(mod2)
    lab2 <- summary(glht(mod2, linfct = mcp(Position = "Tukey")))
    
    
    mod3 <- lmer(JVratio~ Position * CO2_treatment + (1|Chamber), data=stDF)
    out3 <- anova(mod3)
    lab3 <- summary(glht(mod3, linfct = mcp(Position = "Tukey")))
    
    mod4 <- lmer(Ci_transition_Ac_Aj~ Position * CO2_treatment + (1|Chamber), data=stDF)
    out4 <- anova(mod4)
    lab4 <- summary(glht(mod4, linfct = mcp(Position = "Tukey")))
    
    
    ### plotting
    p1 <- ggplot(plotDF3, aes(Position, Vcmax.mean, fill=CO2_treatment)) +
        geom_errorbar(aes(x=Position, ymin=Vcmax.mean-Vcmax.se,
                          ymax=Vcmax.mean+Vcmax.se,
                          col=CO2_treatment), 
                      position=position_dodge(width=0.2),
                      width=0.2)+
        geom_point(aes(Position, Vcmax.mean, 
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
        ylab(expression(paste(V[cmax], " (", mu, "mol "* CO[2], " ", m^-2, " ", s^-1, ")")))+
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
                         labels=c("Full", "T+M", "Top", "Low", "Up"))+
        ylim(0, 120)
    
    p2 <- ggplot(plotDF3, aes(Position, Jmax.mean, fill=CO2_treatment)) +
        geom_errorbar(aes(x=Position, ymin=Jmax.mean-Jmax.se,
                          ymax=Jmax.mean+Jmax.se,
                          col=CO2_treatment), 
                      position=position_dodge(width=0.2),
                      width=0.2)+
        geom_point(aes(Position, Jmax.mean, 
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
        ylab(expression(paste(J[max], " (", mu, "mol "* CO[2], " ", m^-2, " ", s^-1, ")")))+
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
                         labels=c("Full", "T+M", "Top", "Low", "Up"))+
        ylim(0, 200)
    
    p3 <- ggplot(plotDF3, aes(Position, JVratio.mean, fill=CO2_treatment)) +
        geom_errorbar(aes(x=Position, ymin=JVratio.mean-JVratio.se,
                          ymax=JVratio.mean+JVratio.se,
                          col=CO2_treatment), 
                      position=position_dodge(width=0.2),
                      width=0.2)+
        geom_point(aes(Position, JVratio.mean, 
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
        ylab(expression(paste(J[max] * " / " * V[cmax])))+
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
                         labels=c("Full", "T+M", "Top", "Low", "Up"))+
        ylim(0.5, 2)
    
    p4 <- ggplot(plotDF3, aes(Position, Ci_transition_Ac_Aj.mean, fill=CO2_treatment)) +
        geom_errorbar(aes(x=Position, ymin=Ci_transition_Ac_Aj.mean-Ci_transition_Ac_Aj.se,
                          ymax=Ci_transition_Ac_Aj.mean+Ci_transition_Ac_Aj.se,
                          col=CO2_treatment), 
                      position=position_dodge(width=0.2),
                      width=0.2)+
        geom_point(aes(Position, Ci_transition_Ac_Aj.mean, 
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
        ylab(expression(paste("Transition " * C[i] * " (" * mu * "mol" * " " * mol^-1 * ")")))+
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
                         labels=c("Full", "T+M", "Top", "Low", "Up"))+
        ylim(100, 500)
    
    
    
    legend_shared <- get_legend(p1 + theme(legend.position="bottom",
                                           legend.box = 'vertical',
                                           legend.box.just = 'left'))
    
    combined_plots <- plot_grid(p1, p2, p3, p4, 
                                labels=c("(a)", "(b)", "(c)", "(d)"),
                                ncol=2, align="vh", axis = "l",
                                label_x=0.88, label_y=0.95,
                                label_size = 18)
    
    pdf("output/A-Ca/biochemical_parameter_plot_by_position.pdf", width=10, height=10)
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