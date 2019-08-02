summary_statistics_A_Ci_curves <- function() {
    
    ### 
    inDF <- read.csv("output/ambient_chambers_biochemical_parameter_summary_table.csv")
    
    ### to do next: 
    ### 1. summarize biochemical parameters and make statistical comparison
    ### 2. look at Ci = 400 to 600 range, plot (A600-A400)/A400
    ### 3. Plot LAI curve
    
    ### assign CO2 treatment factors
    inDF$CO2_trt[inDF$Chamber%in%c(1,3,7,11)] <- "aCO2"
    inDF$CO2_trt[inDF$Chamber%in%c(2,4,8,12)] <- "eCO2"
    
    inDF$campaign[inDF$Position%in%c("up", "low")] <- "Leaf"
    inDF$campaign[inDF$Position%in%c("12345", "345", "45")] <- "Canopy"
    
    
    ### testing co2 by position
    ## vcmax
    mod1 <- lme(Vcmax ~ CO2_trt * Position, random=~1|Chamber, 
                data=inDF, 
                method="REML")
    anova.lme(mod1, 
              type="sequential", 
              adjustSigma = FALSE)
    
    ## Jmax
    mod2 <- lme(Jmax ~ CO2_trt * Position, random=~1|Chamber, 
                data=inDF, 
                method="REML")
    anova.lme(mod2, 
              type="sequential", 
              adjustSigma = FALSE)
    
    ## JV ratio
    mod3 <- lme(JV_ratio ~ CO2_trt * Position, random=~1|Chamber, 
                data=inDF, 
                method="REML")
    anova.lme(mod3, 
              type="sequential", 
              adjustSigma = FALSE)
    
    ## Ci point
    mod4 <- lme(Ci_transition_Ac_Aj ~ CO2_trt * Position, random=~1|Chamber, 
                data=inDF, 
                method="REML")
    anova.lme(mod4, 
              type="sequential", 
              adjustSigma = FALSE)
    
    
    mod5 <- lme(Ac ~ CO2_trt * Position, random=~1|Chamber, 
                data=inDF, 
                method="REML")
    anova.lme(mod5, 
              type="sequential", 
              adjustSigma = FALSE)
    
    
    mod6 <- lme(Aj ~ CO2_trt * Position, random=~1|Chamber, 
                data=inDF, 
                method="REML")
    anova.lme(mod6, 
              type="sequential", 
              adjustSigma = FALSE)
    
    
    
    ##### make box plot
    sumDF <- summaryBy(Vcmax + Jmax + Rd + ALEAF + GS + ELEAF + Ac + Aj +Ci_transition_Ac_Aj + GammaStar + Km ~ CO2_trt + Position,
                           data=inDF, FUN = c(mean, se), keep.names=T)

    
    
    ### plotting
    p1 <- ggplot(inDF, aes(x=Position, y=Vcmax, fill=CO2_trt)) +
        geom_boxplot(outlier.size=0)+
        geom_point(pch = 21, size = 3, position = position_jitterdodge())+
        xlab("")+
        ylab(expression(V[cmax] * " (" * mu * "mol " * m^-2 * " " * s^-1 * ")"))+
        scale_fill_manual(name=expression(paste(CO[2] * " treatment")),
                          limits=c("aCO2", "eCO2"),
                          labels=c("ambient", "elevated"),
                          values=c("white", "grey"))+
        scale_color_manual(name=expression(paste(CO[2] * " treatment")),
                           limits=c("aCO2", "eCO2"),
                           labels=c("ambient", "elevated"),
                           values=c("white", "grey"))+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=18),
              axis.title.y=element_text(size=18),
              legend.text=element_text(size=18),
              legend.title=element_text(size=18),
              panel.grid.major=element_blank(),
              legend.position="none",
              plot.title = element_text(size = 18, face = "bold", hjust=0.1))+
        ylim(0,100)+
        ggtitle("a")
    
    
   p2 <- ggplot(inDF, aes(x=Position, y=Jmax, fill=CO2_trt)) +
       geom_boxplot(outlier.size=0)+
       geom_point(pch = 21, size = 3, position = position_jitterdodge())+
       xlab("")+
       ylab(expression(J[max] * " (" * mu * "mol " * m^-2 * " " * s^-1 * ")"))+
       scale_fill_manual(name=expression(paste(CO[2] * " treatment")),
                         limits=c("aCO2", "eCO2"),
                         labels=c("ambient", "elevated"),
                         values=c("white", "grey"))+
       scale_color_manual(name=expression(paste(CO[2] * " treatment")),
                          limits=c("aCO2", "eCO2"),
                          labels=c("ambient", "elevated"),
                          values=c("white", "grey"))+
       theme(panel.grid.minor=element_blank(),
             axis.title.x = element_blank(), 
             axis.text.x = element_blank(),
             axis.text.y=element_text(size=18),
             axis.title.y=element_text(size=18),
             legend.text=element_text(size=18),
             legend.title=element_text(size=18),
             panel.grid.major=element_blank(),
             legend.position="none",
             plot.title = element_text(size = 18, face = "bold", hjust=0.1))+
       ylim(0,150)+
       ggtitle("b")
    
   p3 <- ggplot(inDF, aes(x=Position, y=JV_ratio, fill=CO2_trt)) +
       geom_boxplot(outlier.size=0)+
       geom_point(pch = 21, size = 3, position = position_jitterdodge())+
       xlab("")+
       ylab("JV ratio")+
       scale_fill_manual(name=expression(paste(CO[2] * " treatment")),
                         limits=c("aCO2", "eCO2"),
                         labels=c("ambient", "elevated"),
                         values=c("white", "grey"))+
       scale_color_manual(name=expression(paste(CO[2] * " treatment")),
                          limits=c("aCO2", "eCO2"),
                          labels=c("ambient", "elevated"),
                          values=c("white", "grey"))+
       theme(panel.grid.minor=element_blank(),
             axis.title.x = element_blank(), 
             axis.text.x = element_blank(),
             axis.text.y=element_text(size=18),
             axis.title.y=element_text(size=18),
             legend.text=element_text(size=18),
             legend.title=element_text(size=18),
             panel.grid.major=element_blank(),
             legend.position="none",
             plot.title = element_text(size = 18, face = "bold", hjust=0.1))+
       ylim(1,3)+
       ggtitle("c")
   
   p4 <- ggplot(inDF, aes(x=Position, y=Ci_transition_Ac_Aj, fill=CO2_trt)) +
       geom_boxplot(outlier.size=0)+
       geom_point(pch = 21, size = 3, position = position_jitterdodge())+
       xlab("")+
       ylab(expression("Transition " * C[i] * " (ppm)"))+
       scale_fill_manual(name=expression(paste(CO[2] * " treatment")),
                         limits=c("aCO2", "eCO2"),
                         labels=c("ambient", "elevated"),
                         values=c("white", "grey"))+
       scale_color_manual(name=expression(paste(CO[2] * " treatment")),
                          limits=c("aCO2", "eCO2"),
                          labels=c("ambient", "elevated"),
                          values=c("white", "grey"))+
       theme(panel.grid.minor=element_blank(),
             axis.title.x = element_blank(), 
             axis.text.x = element_blank(),
             axis.text.y=element_text(size=18),
             axis.title.y=element_text(size=18),
             legend.text=element_text(size=18),
             legend.title=element_text(size=18),
             panel.grid.major=element_blank(),
             legend.position="none",
             plot.title = element_text(size = 18, face = "bold", hjust=0.1))+
       ylim(0,1000)+
       ggtitle("d")
   
    
   p5 <- ggplot(inDF, aes(x=Position, y=Ac, fill=CO2_trt)) +
       geom_boxplot(outlier.size=0)+
       geom_point(pch = 21, size = 3, position = position_jitterdodge())+
       xlab(" Canopy                Leaf")+
       ylab(expression(A[c] * " (" * mu * "mol " * m^-2 * " " * s^-1 * ")"))+
       scale_fill_manual(name=expression(paste(CO[2] * " treatment")),
                         limits=c("aCO2", "eCO2"),
                         labels=c("ambient", "elevated"),
                         values=c("white", "grey"))+
       scale_color_manual(name=expression(paste(CO[2] * " treatment")),
                          limits=c("aCO2", "eCO2"),
                          labels=c("ambient", "elevated"),
                          values=c("white", "grey"))+
       theme(panel.grid.minor=element_blank(),
             axis.title.x = element_text(size=20), 
             axis.text.x = element_text(size=18),
             axis.text.y=element_text(size=18),
             axis.title.y=element_text(size=18),
             legend.text=element_text(size=18),
             legend.title=element_text(size=18),
             panel.grid.major=element_blank(),
             legend.position="none",
             plot.title = element_text(size = 18, face = "bold", hjust=0.1))+
       ylim(0,40)+
       scale_x_discrete(breaks=c("up", "low", "12345", "345", "45"),
                        labels=c("Up", "Low", "Full", "T+M","Top"))+
       ggtitle("e")
   
   p6 <- ggplot(inDF, aes(x=Position, y=Aj, fill=CO2_trt)) +
       geom_boxplot(outlier.size=0)+
       geom_point(pch = 21, size = 3, position = position_jitterdodge())+
       xlab(" Canopy                Leaf")+
       ylab(expression(A[j] * " (" * mu * "mol " * m^-2 * " " * s^-1 * ")"))+
       scale_fill_manual(name=expression(paste(CO[2] * " treatment")),
                         limits=c("aCO2", "eCO2"),
                         labels=c("ambient", "elevated"),
                         values=c("white", "grey"))+
       scale_color_manual(name=expression(paste(CO[2] * " treatment")),
                          limits=c("aCO2", "eCO2"),
                          labels=c("ambient", "elevated"),
                          values=c("white", "grey"))+
       theme(panel.grid.minor=element_blank(),
             axis.title.x = element_text(size=20), 
             axis.text.x = element_text(size=18),
             axis.text.y=element_text(size=18),
             axis.title.y=element_text(size=18),
             legend.text=element_text(size=18),
             legend.title=element_text(size=18),
             panel.grid.major=element_blank(),
             legend.position="none",
             plot.title = element_text(size = 18, face = "bold", hjust=0.1))+
       ylim(0,30)+
       scale_x_discrete(breaks=c("up", "low", "12345", "345", "45"),
                        labels=c("Up", "Low", "Full", "T+M","Top"))+
       ggtitle("f")
       
   

    ### combined plots + shared legend
    legend_shared <- get_legend(p1 + theme(legend.position="bottom",
                                           legend.box = 'vertical',
                                           legend.box.just = 'right'))
    
    combined_plots <- plot_grid(p1, p2, p3, p4, p5, p6, rel_widths = c(1,1),
                                labels="", ncol=2, align="v", axis = "l")
    
    
    ### output
    pdf("output/chamber_result_comparison_biochemical_parameters.pdf", width=12, height=14)
    #par(mar=c(2,2,2,1),oma = c(4, 6, 0, 0))
    plot_grid(combined_plots, legend_shared, ncol=1, rel_heights=c(1,0.1))
    dev.off()    
    
    
    
    
    
    ### 
    inDF2 <- read.csv("output/predicted_A_values_at_Ci_in_400_600_ppm.csv")
    

    ### assign CO2 treatment factors
    inDF2$CO2_trt[inDF2$Chamber%in%c(1,3,7,11)] <- "aCO2"
    inDF2$CO2_trt[inDF2$Chamber%in%c(2,4,8,12)] <- "eCO2"
    
    inDF2$campaign[inDF2$Position%in%c("up", "low")] <- "Leaf"
    inDF2$campaign[inDF2$Position%in%c("12345", "345", "45")] <- "Canopy"
    
    
    ### testing co2 by position
    ## 
    mod1 <- lme(A_sens ~ CO2_trt * Position, random=~1|Chamber, 
                data=inDF2, 
                method="REML")
    anova.lme(mod1, 
              type="sequential", 
              adjustSigma = FALSE)
    
    ## 
    mod2 <- lme(Aj_sens ~ CO2_trt * Position, random=~1|Chamber, 
                data=inDF2, 
                method="REML")
    anova.lme(mod2, 
              type="sequential", 
              adjustSigma = FALSE)
    
    ## 
    mod3 <- lme(Ac_sens ~ CO2_trt * Position, random=~1|Chamber, 
                data=inDF2, 
                method="REML")
    anova.lme(mod3, 
              type="sequential", 
              adjustSigma = FALSE)
    
    
    ### plotting
    
    p1 <- ggplot(inDF2, aes(x=Position, y=A_sens, fill=CO2_trt)) +
        geom_boxplot(outlier.size=0)+
        geom_point(pch = 21, size = 3, position = position_jitterdodge())+
        xlab("")+
        ylab("A response sensitivity")+
        scale_fill_manual(name=expression(paste(CO[2] * " treatment")),
                          limits=c("aCO2", "eCO2"),
                          labels=c("ambient", "elevated"),
                          values=c("white", "grey"))+
        scale_color_manual(name=expression(paste(CO[2] * " treatment")),
                           limits=c("aCO2", "eCO2"),
                           labels=c("ambient", "elevated"),
                           values=c("white", "grey"))+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=18),
              axis.title.y=element_text(size=18),
              legend.text=element_text(size=18),
              legend.title=element_text(size=18),
              panel.grid.major=element_blank(),
              legend.position="none",
              plot.title = element_text(size = 18, face = "bold", hjust=0.1))+
        ylim(0,0.5)+
        ggtitle("a")
    
    plot(p1)
    
    p2 <- ggplot(inDF2, aes(x=Position, y=Aj_sens, fill=CO2_trt)) +
        geom_boxplot(outlier.size=0)+
        geom_point(pch = 21, size = 3, position = position_jitterdodge())+
        xlab("")+
        ylab("Aj sensitivity")+
        scale_fill_manual(name=expression(paste(CO[2] * " treatment")),
                          limits=c("aCO2", "eCO2"),
                          labels=c("ambient", "elevated"),
                          values=c("white", "grey"))+
        scale_color_manual(name=expression(paste(CO[2] * " treatment")),
                           limits=c("aCO2", "eCO2"),
                           labels=c("ambient", "elevated"),
                           values=c("white", "grey"))+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=18),
              axis.title.y=element_text(size=18),
              legend.text=element_text(size=18),
              legend.title=element_text(size=18),
              panel.grid.major=element_blank(),
              legend.position="none",
              plot.title = element_text(size = 18, face = "bold", hjust=0.1))+
        ylim(0.075,0.15)+
        ggtitle("b")
    
    plot(p2)
    
    p3 <- ggplot(inDF2, aes(x=Position, y=Ac_sens, fill=CO2_trt)) +
        geom_boxplot(outlier.size=0)+
        geom_point(pch = 21, size = 3, position = position_jitterdodge())+
        xlab(" Canopy                Leaf")+
        ylab("Ac sensitivity")+
        scale_fill_manual(name=expression(paste(CO[2] * " treatment")),
                          limits=c("aCO2", "eCO2"),
                          labels=c("ambient", "elevated"),
                          values=c("white", "grey"))+
        scale_color_manual(name=expression(paste(CO[2] * " treatment")),
                           limits=c("aCO2", "eCO2"),
                           labels=c("ambient", "elevated"),
                           values=c("white", "grey"))+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=20), 
              axis.text.x = element_text(size=18),
              axis.text.y=element_text(size=18),
              axis.title.y=element_text(size=18),
              legend.text=element_text(size=18),
              legend.title=element_text(size=18),
              panel.grid.major=element_blank(),
              legend.position="none",
              plot.title = element_text(size = 18, face = "bold", hjust=0.1))+
        ylim(0.25,0.45)+
        scale_x_discrete(breaks=c("up", "low", "12345", "345", "45"),
                         labels=c("Up", "Low", "Full", "T+M","Top"))+
        ggtitle("c")
    
    plot(p3)
    
    
    pdf("output/chamber_A_responsiveness.pdf", width=6, height=12)
    
    plot_grid(p1, p2, p3,
              rel_heights=c(1,1,1.4),
              labels=c(""), ncol=1, align="v", axis = "l")    
    
    dev.off()
    
 }