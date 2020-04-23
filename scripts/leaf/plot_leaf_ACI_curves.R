plot_leaf_ACI_curves <- function(plotDF) {
    
    ##### make box plot for long-term CO2 and position factors
    sumDF.co2 <- summaryBy(Vcmax + Jmax + Rd + ALEAF + GS + ELEAF + Ac + Aj +Ci_transition_Ac_Aj + GammaStar + Km ~ CO2_treatment,
                           data=plotDF, FUN = c(mean, se), keep.names=T)
    
    sumDF.ht <- summaryBy(Vcmax + Jmax + Rd + ALEAF + GS + ELEAF + Ac + Aj +Ci_transition_Ac_Aj + GammaStar + Km ~ Height,
                          data=plotDF, FUN = c(mean, se), keep.names=T)
    
    
    
    p1 <- ggplot(sumDF.co2) +
        geom_errorbar(aes(x=CO2_treatment, ymin=(Vcmax.mean - Vcmax.se), 
                          ymax = (Vcmax.mean+Vcmax.se)), position = "dodge", width=0.2)+
        geom_point(aes(CO2_treatment, Vcmax.mean, fill=CO2_treatment), size = 10, shape=21)+
        xlab("")+
        ylab(expression(V[cmax]*" (" * mu * "mol " * m^-2 * " " * s^-1 * ")"))+
        scale_fill_manual(name=expression(paste(CO[2] * " treatment")),
                          limits=c("ambient", "elevated"),
                          values=c("grey", "black"))+
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
        ylim(40,120)+
        ggtitle("a")
    
    
    p2 <- ggplot(sumDF.ht) +
        geom_errorbar(aes(x=Height, ymin=(Vcmax.mean - Vcmax.se), 
                          ymax = (Vcmax.mean+Vcmax.se)), position = "dodge", width=0.2)+
        geom_point(aes(Height, Vcmax.mean, fill=Height), size = 10, shape=21)+
        xlab("")+
        ylab(expression(V[cmax]*" (" * mu * "mol " * m^-2 * " " * s^-1 * ")"))+
        scale_fill_manual(name="Canopy position",
                          limits=c("up", "low"),
                          values=c("grey", "black"))+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_blank(), 
              axis.title.y=element_blank(), 
              legend.text=element_text(size=18),
              legend.title=element_text(size=18),
              panel.grid.major=element_blank(),
              legend.position="none",
              plot.title = element_text(size = 18, face = "bold", hjust=0.1))+
        ylim(40,120)+
        ggtitle("b")
    
    
    p3 <- ggplot(sumDF.co2) +
        geom_errorbar(aes(x=CO2_treatment, ymin=(Jmax.mean - Jmax.se), 
                          ymax = (Jmax.mean+Jmax.se)), position = "dodge", width=0.2)+
        geom_point(aes(CO2_treatment, Jmax.mean, fill=CO2_treatment), size = 10, shape=21)+
        xlab("")+
        ylab(expression(J[max]*" (" * mu * "mol " * m^-2 * " " * s^-1 * ")"))+
        scale_fill_manual(name=expression(paste(CO[2] * " treatment")),
                          limits=c("ambient", "elevated"),
                          values=c("grey", "black"))+
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
        ylim(50,200)+
        ggtitle("c")

    
    p4 <- ggplot(sumDF.ht) +
        geom_errorbar(aes(x=Height, ymin=(Jmax.mean - Jmax.se), 
                          ymax = (Jmax.mean+Jmax.se)), position = "dodge", width=0.2)+
        geom_point(aes(Height, Jmax.mean, fill=Height), size = 10, shape=21)+
        xlab("")+
        ylab(expression(J[max]*" (" * mu * "mol " * m^-2 * " " * s^-1 * ")"))+
        scale_fill_manual(name="Canopy position",
                          limits=c("up", "low"),
                          values=c("grey", "black"))+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_blank(), 
              axis.title.y=element_blank(), 
              legend.text=element_text(size=18),
              legend.title=element_text(size=18),
              panel.grid.major=element_blank(),
              legend.position="none",
              plot.title = element_text(size = 18, face = "bold", hjust=0.1))+
        ylim(50,200)+
        ggtitle("d")
    
    
    p5 <- ggplot(sumDF.co2) +
        geom_errorbar(aes(x=CO2_treatment, ymin=(Ci_transition_Ac_Aj.mean - Ci_transition_Ac_Aj.se), 
                          ymax = (Ci_transition_Ac_Aj.mean+Ci_transition_Ac_Aj.se)), position = "dodge", width=0.2)+
        geom_point(aes(CO2_treatment, Ci_transition_Ac_Aj.mean, fill=CO2_treatment), size = 10, shape=21)+
        xlab(expression(paste(CO[2] * " treatment")))+
        ylab(expression(C[i]*" transition point (" * mu *"mol " * mol^-1 * ")"))+
        scale_fill_manual(name=expression(paste(CO[2] * " treatment")),
                          limits=c("ambient", "elevated"),
                          values=c("grey", "black"))+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=20), 
              axis.text.x = element_text(size=18),
              axis.text.y=element_text(size=18),
              axis.title.y=element_text(size=18),
              legend.text=element_text(size=18),
              legend.title=element_text(size=18),
              panel.grid.major=element_blank(),
              legend.position="bottom",
              plot.title = element_text(size = 18, face = "bold", hjust=0.1))+
        ylim(150,400)+
        scale_x_discrete(breaks=c("ambient", "elevated"),
                         labels=c("ambient", "elevated"))+
        guides(fill = guide_legend(title.position = "top"))+
        ggtitle("e")
    
    p6 <- ggplot(sumDF.ht) +
        geom_errorbar(aes(x=Height, ymin=(Ci_transition_Ac_Aj.mean - Ci_transition_Ac_Aj.se), 
                          ymax = (Ci_transition_Ac_Aj.mean+Ci_transition_Ac_Aj.se)), position = "dodge", width=0.2)+
        geom_point(aes(Height, Ci_transition_Ac_Aj.mean, fill=Height), size = 10, shape=21)+
        xlab("Canopy position")+
        ylab(expression(C[i]*" transition point (" * mu * "mol " * mol^-1 * ")"))+
        scale_fill_manual(name="Canopy position",
                          limits=c("up", "low"),
                          values=c("grey", "black"))+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=20), 
              axis.text.x = element_text(size=18),
              axis.text.y=element_blank(), 
              axis.title.y=element_blank(), 
              legend.text=element_text(size=18),
              legend.title=element_text(size=18),
              panel.grid.major=element_blank(),
              legend.position="bottom",
              plot.title = element_text(size = 18, face = "bold", hjust=0.1))+
        ylim(150,400)+
        scale_x_discrete(breaks=c("up", "low"),
                         labels=c("Up", "Low"))+
        guides(fill = guide_legend(title.position = "top"))+
        ggtitle("g")
    
    
    pdf("output/leaf/leaf_parameter_comparison.pdf", width=12, height=14)
    plot_grid(p1, p2, p3, 
              p4, p5, p6, 
              rel_heights=c(1,1,1.5),
              rel_widths=c(1.2, 1, 1),
              labels="", ncol=2, align="h", axis = "l")
    dev.off()
    
    ### testing co2 by height, ignore date
    ## vcmax
    mod1 <- lme(Vcmax ~ CO2_treatment * Height, random=~1|Chamber, 
                data=plotDF, 
                method="REML")
    anova.lme(mod1, 
              type="sequential", 
              adjustSigma = FALSE)
    # no CO2 by height interaction on vcmax
    
    
    ##### make box plot
    plotDF$JVratio <- plotDF$Jmax/plotDF$Vcmax
    sumDF <- summaryBy(Vcmax + Jmax + Rd + ALEAF + GS + ELEAF + Ac + Aj +Ci_transition_Ac_Aj + GammaStar + Km + JVratio~ CO2_treatment+Height,
                       data=plotDF, FUN = c(mean, se), keep.names=T)
    
    
    
    p1 <- ggplot(sumDF) +
        geom_errorbar(aes(x=Height, ymin=(Vcmax.mean - Vcmax.se), 
                          ymax = (Vcmax.mean+Vcmax.se), color=CO2_treatment),
                      position=position_dodge(width=0.5),width=0.2)+
        geom_point(aes(Height, Vcmax.mean, fill=CO2_treatment), size = 10, shape=21, 
                   position=position_dodge(width=0.5))+
        xlab("")+
        ylab(expression(V[cmax]*" (" * mu *"mol " * m^-2 * " " * s^-1 * ")"))+
        scale_fill_manual(name=expression(paste(CO[2] * " treatment")),
                          limits=c("ambient", "elevated"),
                          values=c("grey", "black"))+
        scale_color_manual(name=expression(paste(CO[2] * " treatment")),
                           limits=c("ambient", "elevated"),
                           values=c("black", "black"))+
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
        ylim(20,120)
    
    p2 <- ggplot(sumDF) +
        geom_errorbar(aes(x=Height, ymin=(Jmax.mean - Jmax.se), 
                          ymax = (Jmax.mean+Jmax.se), color=CO2_treatment),
                      position=position_dodge(width=0.5),width=0.3)+
        geom_point(aes(Height, Jmax.mean, fill=CO2_treatment), size = 10, shape=21, 
                   position=position_dodge(width=0.5))+
        xlab("")+
        ylab(expression(J[max]*" (" * mu * "mol " * m^-2 * " " * s^-1 * ")"))+
        scale_fill_manual(name=expression(paste(CO[2] * " treatment")),
                          limits=c("ambient", "elevated"),
                          values=c("grey", "black"))+
        scale_color_manual(name=expression(paste(CO[2] * " treatment")),
                           limits=c("ambient", "elevated"),
                           values=c("black", "black"))+
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
        ylim(50,200)
    
    p3 <- ggplot(sumDF) +
        geom_errorbar(aes(x=Height, ymin=(JVratio.mean - JVratio.se), 
                          ymax = (JVratio.mean+JVratio.se), color=CO2_treatment),
                      position=position_dodge(width=0.5),width=0.2)+
        geom_point(aes(Height, JVratio.mean, fill=CO2_treatment), size = 10, shape=21, 
                   position=position_dodge(width=0.5))+
        xlab("")+
        ylab("JV ratio")+
        scale_fill_manual(name=expression(paste(CO[2] * " treatment")),
                          limits=c("ambient", "elevated"),
                          values=c("grey", "black"))+
        scale_color_manual(name=expression(paste(CO[2] * " treatment")),
                           limits=c("ambient", "elevated"),
                           values=c("black", "black"))+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_text(size=18),
              axis.text.y=element_text(size=18),
              axis.title.y=element_text(size=18),
              legend.text=element_text(size=18),
              legend.title=element_text(size=18),
              panel.grid.major=element_blank(),
              legend.position="none",
              plot.title = element_text(size = 18, face = "bold", hjust=0.1))+
        ylim(1.,2)
    
    p4 <- ggplot(sumDF) +
        geom_errorbar(aes(x=Height, ymin=(Ci_transition_Ac_Aj.mean - Ci_transition_Ac_Aj.se), 
                          ymax = (Ci_transition_Ac_Aj.mean+Ci_transition_Ac_Aj.se), color=CO2_treatment),
                      position=position_dodge(width=0.5),width=0.2)+
        geom_point(aes(Height, Ci_transition_Ac_Aj.mean, fill=CO2_treatment), size = 10, shape=21, 
                   position=position_dodge(width=0.5))+
        xlab("")+
        ylab(expression(C[i]*" transition (" * mu * "mol " * mol^-1 * ")"))+
        scale_fill_manual(name=expression(paste(CO[2] * " treatment")),
                          limits=c("ambient", "elevated"),
                          values=c("grey", "black"))+
        scale_color_manual(name=expression(paste(CO[2] * " treatment")),
                           limits=c("ambient", "elevated"),
                           values=c("black", "black"))+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_text(size=18),
              axis.text.y=element_text(size=18),
              axis.title.y=element_text(size=18),
              legend.text=element_text(size=18),
              legend.title=element_text(size=18),
              panel.grid.major=element_blank(),
              legend.position="none",
              plot.title = element_text(size = 18, face = "bold", hjust=0.1))+
        ylim(150,400)
    
    
    legend_shared <- get_legend(p1 + theme(legend.position="bottom",
                                           legend.box = 'vertical',
                                           legend.box.just = 'left'))
    
    combined_plots <- plot_grid(p1, p2, p3, p4, 
                                labels="AUTO", ncol=2, align="vh", axis = "l")
    
    #plot(p1)
    
    pdf("output/leaf/leaf_parameter_summary_detailed_breakdowns.pdf", 
        width=10, height=12)
    plot_grid(combined_plots, legend_shared, ncol=1, rel_heights=c(1,0.1))
    dev.off()    
    
    
    
    
    ### plot aCO2 and date to reconcile apparent date issue
    ### in low and up comparison
    subDF <- subset(plotDF, CO2_treatment == "ambient")
    subDF$year <- year(subDF$Date)
    
    pre2009DF <- subset(subDF, year < 2009)
    in2009DF <- subset(subDF, year == 2009)
    
    ### average Ci
    up.pre09 <- mean(pre2009DF$Ci_transition_Ac_Aj[pre2009DF$Height=="up"])
    up.in09 <- mean(in2009DF$Ci_transition_Ac_Aj[in2009DF$Height=="up"])
    low.in09 <- mean(in2009DF$Ci_transition_Ac_Aj[in2009DF$Height=="low"])
    
    ### create DF to store Ci
    sDF1 <- data.frame(unique(pre2009DF$Date), up.pre09)
    sDF2 <- data.frame(unique(in2009DF$Date), up.in09)
    sDF3 <- data.frame(unique(in2009DF$Date), low.in09)
    colnames(sDF1)<-colnames(sDF2) <- colnames(sDF3) <- c("Date", "Ci")
    sDF1$Group <- "A"
    sDF2$Group <- "B"
    sDF3$Group <- "C"
    
    mDF <- rbind(sDF1, sDF2, sDF3)
    
    subDF$Date <- as.Date(as.character(subDF$Date))
    mDF$Date <- as.Date(as.character(mDF$Date))
    
    p4 <- ggplot(subDF) +
        geom_point(aes(Date, Ci_transition_Ac_Aj, fill=Height), 
                   size = 10, shape=21, 
                   position=position_dodge(width=0.5))+
        geom_line(data=mDF, aes(Date, Ci, linetype=Group))+
        xlab("")+
        ylab(expression(C[i]*" transition (" * mu * "mol " * mol^-1 * ")"))+
        scale_fill_manual(name="Height",
                          limits=c("up", "low"),
                          values=c("grey", "black"))+
        scale_color_manual(name="Height",
                           limits=c("up", "low"),
                           values=c("grey", "black"))+
        scale_linetype_manual(name="Group",
                         limits=c("A", "B", "C"),
                         values=c("dotdash", "dotted", "solid"),
                         labels=c("up_pre2009",
                                  "up_2009",
                                  "low_2009"))+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_text(size=18),
              axis.text.y=element_text(size=18),
              axis.title.y=element_text(size=18),
              legend.text=element_text(size=18),
              legend.title=element_text(size=18),
              panel.grid.major=element_blank(),
              legend.position="bottom",
              plot.title = element_text(size = 18, face = "bold", hjust=0.1))+
        ylim(10,500)
    
    #plot(p4)
    
    pdf("output/leaf/leaf_Ci_over_date.pdf", 
        width=26, height=6)
    plot(p4)
    dev.off()  
 
    
    
}