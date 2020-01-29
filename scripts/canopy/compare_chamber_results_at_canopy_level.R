compare_chamber_results_at_canopy_level <- function() {
    ### this script compares canopy fluxes obtained from CO2 drawdown experiment
    ### and the scaled-up flux based on a two-leaf model
    
    ### rea in files
    ch01 <- read.csv("/Users/mingkaijiang/Documents/Research/Projects/WCT1_CO2_drawdown/Two_leaf_model/outputs/wtc_two_leaf_1.csv")
    ch02 <- read.csv("/Users/mingkaijiang/Documents/Research/Projects/WCT1_CO2_drawdown/Two_leaf_model/outputs/wtc_two_leaf_2.csv")
    ch03 <- read.csv("/Users/mingkaijiang/Documents/Research/Projects/WCT1_CO2_drawdown/Two_leaf_model/outputs/wtc_two_leaf_3.csv")
    ch04 <- read.csv("/Users/mingkaijiang/Documents/Research/Projects/WCT1_CO2_drawdown/Two_leaf_model/outputs/wtc_two_leaf_4.csv")
    ch07 <- read.csv("/Users/mingkaijiang/Documents/Research/Projects/WCT1_CO2_drawdown/Two_leaf_model/outputs/wtc_two_leaf_7.csv")
    ch08 <- read.csv("/Users/mingkaijiang/Documents/Research/Projects/WCT1_CO2_drawdown/Two_leaf_model/outputs/wtc_two_leaf_8.csv")
    ch11 <- read.csv("/Users/mingkaijiang/Documents/Research/Projects/WCT1_CO2_drawdown/Two_leaf_model/outputs/wtc_two_leaf_11.csv")
    ch12 <- read.csv("/Users/mingkaijiang/Documents/Research/Projects/WCT1_CO2_drawdown/Two_leaf_model/outputs/wtc_two_leaf_12.csv")
    
    
    ### plot
    p1 <- ggplot(ch01) +
        geom_point(aes(An_obs, An_can, col=as.factor(ch01$canopy)),
                   size=4)+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=12),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)+
        geom_abline(lty=2, color="grey")+
        xlab(expression(paste(A[observed]* " (umol "* CO[2], " ", m^-1, s^-1, ")")))+
        ylab(expression(paste(A[scaled]* " (umol "* CO[2], " ", m^-1, s^-1, ")")))+
        scale_color_manual(name="Canopy",
                           limits=c("12345", "345", "45"),
                           values=c("blue2", "red3", "orange"),
                           labels=c("whole", "middle+bottom", "bottom"))+
        xlim(-5,60)+
        ylim(-5,60)+
        ggtitle("Chamber 01")
    
    p2 <- ggplot(ch02) +
        geom_point(aes(An_obs, An_can, col=as.factor(ch02$canopy)),
                   size=4)+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=12),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)+
        geom_abline(lty=2, color="grey")+
        xlab(expression(paste(A[observed]* " (umol "* CO[2], " ", m^-1, s^-1, ")")))+
        ylab(expression(paste(A[scaled]* " (umol "* CO[2], " ", m^-1, s^-1, ")")))+
        scale_color_manual(name="Canopy",
                           limits=c("12345", "345", "45"),
                           values=c("blue2", "red3", "orange"),
                           labels=c("whole", "middle+bottom", "bottom"))+
        xlim(-5,60)+
        ylim(-5,60)+
        ggtitle("Chamber 02")
    
    p3 <- ggplot(ch03) +
        geom_point(aes(An_obs, An_can, col=as.factor(ch03$canopy)),
                   size=4)+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=12),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)+
        geom_abline(lty=2, color="grey")+
        xlab(expression(paste(A[observed]* " (umol "* CO[2], " ", m^-1, s^-1, ")")))+
        ylab(expression(paste(A[scaled]* " (umol "* CO[2], " ", m^-1, s^-1, ")")))+
        scale_color_manual(name="Canopy",
                           limits=c("12345", "345", "45"),
                           values=c("blue2", "red3", "orange"),
                           labels=c("whole", "middle+bottom", "bottom"))+
        xlim(-5,60)+
        ylim(-5,60)+
        ggtitle("Chamber 03")
    
    p4 <- ggplot(ch04) +
        geom_point(aes(An_obs, An_can, col=as.factor(ch04$canopy)),
                   size=4)+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=12),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)+
        geom_abline(lty=2, color="grey")+
        xlab(expression(paste(A[observed]* " (umol "* CO[2], " ", m^-1, s^-1, ")")))+
        ylab(expression(paste(A[scaled]* " (umol "* CO[2], " ", m^-1, s^-1, ")")))+
        scale_color_manual(name="Canopy",
                           limits=c("12345", "345", "45"),
                           values=c("blue2", "red3", "orange"),
                           labels=c("whole", "middle+bottom", "bottom"))+
        xlim(-5,60)+
        ylim(-5,60)+
        ggtitle("Chamber 04")
    
    p7 <- ggplot(ch07) +
        geom_point(aes(An_obs, An_can, col=as.factor(ch07$canopy)),
                   size=4)+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=12),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)+
        geom_abline(lty=2, color="grey")+
        xlab(expression(paste(A[observed]* " (umol "* CO[2], " ", m^-1, s^-1, ")")))+
        ylab(expression(paste(A[scaled]* " (umol "* CO[2], " ", m^-1, s^-1, ")")))+
        scale_color_manual(name="Canopy",
                           limits=c("12345", "345", "45"),
                           values=c("blue2", "red3", "orange"),
                           labels=c("whole", "middle+bottom", "bottom"))+
        xlim(-5,60)+
        ylim(-5,60)+
        ggtitle("Chamber 07")
    
    p8 <- ggplot(ch08) +
        geom_point(aes(An_obs, An_can, col=as.factor(ch08$canopy)),
                   size=4)+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=12),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)+
        geom_abline(lty=2, color="grey")+
        xlab(expression(paste(A[observed]* " (umol "* CO[2], " ", m^-1, s^-1, ")")))+
        ylab(expression(paste(A[scaled]* " (umol "* CO[2], " ", m^-1, s^-1, ")")))+
        scale_color_manual(name="Canopy",
                           limits=c("12345", "345", "45"),
                           values=c("blue2", "red3", "orange"),
                           labels=c("whole", "middle+bottom", "bottom"))+
        xlim(-5,60)+
        ylim(-5,60)+
        ggtitle("Chamber 08")
    
    p11 <- ggplot(ch11) +
        geom_point(aes(An_obs, An_can, col=as.factor(ch11$canopy)),
                   size=4)+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=12),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)+
        geom_abline(lty=2, color="grey")+
        xlab(expression(paste(A[observed]* " (umol "* CO[2], " ", m^-1, s^-1, ")")))+
        ylab(expression(paste(A[scaled]* " (umol "* CO[2], " ", m^-1, s^-1, ")")))+
        scale_color_manual(name="Canopy",
                           limits=c("12345", "345", "45"),
                           values=c("blue2", "red3", "orange"),
                           labels=c("whole", "middle+bottom", "bottom"))+
        xlim(-5,60)+
        ylim(-5,60)+
        ggtitle("Chamber 11")
    
    p12 <- ggplot(ch12) +
        geom_point(aes(An_obs, An_can, col=as.factor(ch12$canopy)),
                   size=4)+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=12),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)+
        geom_abline(lty=2, color="grey")+
        xlab(expression(paste(A[observed]* " (umol "* CO[2], " ", m^-1, s^-1, ")")))+
        ylab(expression(paste(A[scaled]* " (umol "* CO[2], " ", m^-1, s^-1, ")")))+
        scale_color_manual(name="Canopy",
                           limits=c("12345", "345", "45"),
                           values=c("blue2", "red3", "orange"),
                           labels=c("whole", "middle+bottom", "bottom"))+
        xlim(-5,60)+
        ylim(-5,60)+
        ggtitle("Chamber 12")
    
    combined_plots <- plot_grid(p1, p2, p3, p4, p7, p8, p11, p12, 
                                labels="AUTO", ncol=2, align="vh", axis = "l")

    legend_shared <- get_legend(p1 + theme(legend.position="bottom",
                                           legend.box = 'vertical',
                                           legend.box.just = 'left'))
    
    
    pdf("output/chamber_result_comparison_A_flux.pdf", width=6, height=12)
    plot_grid(combined_plots, legend_shared, ncol=1, rel_heights=c(1,0.1))
    dev.off()
    
    
    ### plot A vs. Ca
    
    p1 <- ggplot(ch01) +
        geom_point(aes(Ca, An_can, col="red3"),
                   size=4)+
        geom_smooth(aes(Ca, An_can, col="red3"),
                    method = "lm", formula = y ~ splines::bs(x, 3))+
        geom_point(aes(Ca, An_obs, col="blue2"),
                   size=4)+
        geom_smooth(aes(Ca, An_obs, col="blue2"),
                    method = "lm", formula = y ~ splines::bs(x, 3))+
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
              legend.text.align=0)+
        xlab(expression(paste(C[a]* " (umol ", " ", mol^-1, ")")))+
        ylab(expression(paste(A[scaled]* " (umol "* CO[2], " ", m^-1, s^-1, ")")))+
        scale_color_manual(name="Method",
                           values=c("blue2", "red3"),
                           labels=c("Observed", "Modeled"))+
        xlim(0,1600)+
        ylim(-5,60)+
        ggtitle("Chamber 01")
    

    p2 <- ggplot(ch02) +
        geom_point(aes(Ca, An_can, col="red3"),
                   size=4)+
        geom_smooth(aes(Ca, An_can, col="red3"),
                    method = "lm", formula = y ~ splines::bs(x, 3))+
        geom_point(aes(Ca, An_obs, col="blue2"),
                   size=4)+
        geom_smooth(aes(Ca, An_obs, col="blue2"),
                    method = "lm", formula = y ~ splines::bs(x, 3))+
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
              legend.text.align=0)+
        xlab(expression(paste(C[a]* " (umol ", " ", mol^-1, ")")))+
        ylab(expression(paste(A[scaled]* " (umol "* CO[2], " ", m^-1, s^-1, ")")))+
        scale_color_manual(name="Method",
                           values=c("blue2", "red3"),
                           labels=c("Observed", "Modeled"))+
        xlim(0,1600)+
        ylim(-5,60)+
        ggtitle("Chamber 02")
    
    p3 <- ggplot(ch03) +
        geom_point(aes(Ca, An_can, col="red3"),
                   size=4)+
        geom_smooth(aes(Ca, An_can, col="red3"),
                    method = "lm", formula = y ~ splines::bs(x, 3))+
        geom_point(aes(Ca, An_obs, col="blue2"),
                   size=4)+
        geom_smooth(aes(Ca, An_obs, col="blue2"),
                    method = "lm", formula = y ~ splines::bs(x, 3))+
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
              legend.text.align=0)+
        xlab(expression(paste(C[a]* " (umol ", " ", mol^-1, ")")))+
        ylab(expression(paste(A[scaled]* " (umol "* CO[2], " ", m^-1, s^-1, ")")))+
        scale_color_manual(name="Method",
                           values=c("blue2", "red3"),
                           labels=c("Observed", "Modeled"))+
        xlim(0,1600)+
        ylim(-5,60)+
        ggtitle("Chamber 03")
    
    p4 <- ggplot(ch04) +
        geom_point(aes(Ca, An_can, col="red3"),
                   size=4)+
        geom_smooth(aes(Ca, An_can, col="red3"),
                    method = "lm", formula = y ~ splines::bs(x, 3))+
        geom_point(aes(Ca, An_obs, col="blue2"),
                   size=4)+
        geom_smooth(aes(Ca, An_obs, col="blue2"),
                    method = "lm", formula = y ~ splines::bs(x, 3))+
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
              legend.text.align=0)+
        xlab(expression(paste(C[a]* " (umol ", " ", mol^-1, ")")))+
        ylab(expression(paste(A[scaled]* " (umol "* CO[2], " ", m^-1, s^-1, ")")))+
        scale_color_manual(name="Method",
                           values=c("blue2", "red3"),
                           labels=c("Observed", "Modeled"))+
        xlim(0,1600)+
        ylim(-5,60)+
        ggtitle("Chamber 04")
    
    p7 <- ggplot(ch07) +
        geom_point(aes(Ca, An_can, col="red3"),
                   size=4)+
        geom_smooth(aes(Ca, An_can, col="red3"),
                    method = "lm", formula = y ~ splines::bs(x, 3))+
        geom_point(aes(Ca, An_obs, col="blue2"),
                   size=4)+
        geom_smooth(aes(Ca, An_obs, col="blue2"),
                    method = "lm", formula = y ~ splines::bs(x, 3))+
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
              legend.text.align=0)+
        xlab(expression(paste(C[a]* " (umol ", " ", mol^-1, ")")))+
        ylab(expression(paste(A[scaled]* " (umol "* CO[2], " ", m^-1, s^-1, ")")))+
        scale_color_manual(name="Method",
                           values=c("blue2", "red3"),
                           labels=c("Observed", "Modeled"))+
        xlim(0,1600)+
        ylim(-5,60)+
        ggtitle("Chamber 07")
    
    p8 <- ggplot(ch08) +
        geom_point(aes(Ca, An_can, col="red3"),
                   size=4)+
        geom_smooth(aes(Ca, An_can, col="red3"),
                    method = "lm", formula = y ~ splines::bs(x, 3))+
        geom_point(aes(Ca, An_obs, col="blue2"),
                   size=4)+
        geom_smooth(aes(Ca, An_obs, col="blue2"),
                    method = "lm", formula = y ~ splines::bs(x, 3))+
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
              legend.text.align=0)+
        xlab(expression(paste(C[a]* " (umol ", " ", mol^-1, ")")))+
        ylab(expression(paste(A[scaled]* " (umol "* CO[2], " ", m^-1, s^-1, ")")))+
        scale_color_manual(name="Method",
                           values=c("blue2", "red3"),
                           labels=c("Observed", "Modeled"))+
        xlim(0,1600)+
        ylim(-5,60)+
        ggtitle("Chamber 08")
    
    p11 <- ggplot(ch11) +
        geom_point(aes(Ca, An_can, col="red3"),
                   size=4)+
        geom_smooth(aes(Ca, An_can, col="red3"),
                    method = "lm", formula = y ~ splines::bs(x, 3))+
        geom_point(aes(Ca, An_obs, col="blue2"),
                   size=4)+
        geom_smooth(aes(Ca, An_obs, col="blue2"),
                    method = "lm", formula = y ~ splines::bs(x, 3))+
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
              legend.text.align=0)+
        xlab(expression(paste(C[a]* " (umol ", " ", mol^-1, ")")))+
        ylab(expression(paste(A[scaled]* " (umol "* CO[2], " ", m^-1, s^-1, ")")))+
        scale_color_manual(name="Method",
                           values=c("blue2", "red3"),
                           labels=c("Observed", "Modeled"))+
        xlim(0,1600)+
        ylim(-5,60)+
        ggtitle("Chamber 11")
    
    p12 <- ggplot(ch12) +
        geom_point(aes(Ca, An_can, col="red3"),
                   size=4)+
        geom_smooth(aes(Ca, An_can, col="red3"),
                    method = "lm", formula = y ~ splines::bs(x, 3))+
        geom_point(aes(Ca, An_obs, col="blue2"),
                   size=4)+
        geom_smooth(aes(Ca, An_obs, col="blue2"),
                    method = "lm", formula = y ~ splines::bs(x, 3))+
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
              legend.text.align=0)+
        xlab(expression(paste(C[a]* " (umol ", " ", mol^-1, ")")))+
        ylab(expression(paste(A[scaled]* " (umol "* CO[2], " ", m^-1, s^-1, ")")))+
        scale_color_manual(name="Method",
                           values=c("blue2", "red3"),
                           labels=c("Observed", "Modeled"))+
        xlim(0,1600)+
        ylim(-5,60)+
        ggtitle("Chamber 12")
    
    legend_shared <- get_legend(p1 + theme(legend.position="bottom",
                                           legend.box = 'vertical',
                                           legend.box.just = 'left'))
    
    combined_lots <- plot_grid(p1, p2, p3, p4, p7, p8, p11, p12, 
                               labels="AUTO", ncol=2, align="vh", axis = "l")
    
    pdf("output/chamber_result_comparison_A_vs_Ca_flux.pdf", width=6, height=12)
    plot_grid(combined_plots, legend_shared, ncol=1, rel_heights=c(1,0.1))
    
    dev.off()    
    
    
    ### convert data format
    ch01.tf <- convert_modeled_data_format(inDF=ch01)
    ch02.tf <- convert_modeled_data_format(inDF=ch02)
    ch03.tf <- convert_modeled_data_format(inDF=ch03)
    ch04.tf <- convert_modeled_data_format(inDF=ch04)
    ch07.tf <- convert_modeled_data_format(inDF=ch07)
    ch08.tf <- convert_modeled_data_format(inDF=ch08)
    ch11.tf <- convert_modeled_data_format(inDF=ch11)
    ch12.tf <- convert_modeled_data_format(inDF=ch12)
    
    ### fit data
    ch01.fit <- fitacis(ch01.tf, group = "canopy", 
                        varnames = list(ALEAF = "An", Tleaf="T_can",
                                        Ci = "Ca", PPFD = "APAR_can"),
                        id = "canopy", fitmethod="bilinear")
    
    ch02.fit <- fitacis(ch02.tf, group = "canopy", 
                        varnames = list(ALEAF = "An", Tleaf="T_can",
                                        Ci = "Ca", PPFD = "APAR_can"),
                        id = "canopy", fitmethod="bilinear")
    
    ch03.fit <- fitacis(ch03.tf, group = "canopy", 
                        varnames = list(ALEAF = "An", Tleaf="T_can",
                                        Ci = "Ca", PPFD = "APAR_can"),
                        id = "canopy", fitmethod="bilinear")
    
    ch04.fit <- fitacis(ch04.tf, group = "canopy", 
                        varnames = list(ALEAF = "An", Tleaf="T_can",
                                        Ci = "Ca", PPFD = "APAR_can"),
                        id = "canopy", fitmethod="bilinear")
    
    ch07.fit <- fitacis(ch07.tf, group = "canopy", 
                        varnames = list(ALEAF = "An", Tleaf="T_can",
                                        Ci = "Ca", PPFD = "APAR_can"),
                        id = "canopy", fitmethod="bilinear")
    
    ch08.fit <- fitacis(ch08.tf, group = "canopy", 
                        varnames = list(ALEAF = "An", Tleaf="T_can",
                                        Ci = "Ca", PPFD = "APAR_can"),
                        id = "canopy", fitmethod="bilinear")
    
    ch11.fit <- fitacis(ch11.tf, group = "canopy", 
                        varnames = list(ALEAF = "An", Tleaf="T_can",
                                        Ci = "Ca", PPFD = "APAR_can"),
                        id = "canopy", fitmethod="bilinear")

    ch12.fit <- fitacis(ch12.tf, group = "canopy", 
                        varnames = list(ALEAF = "An", Tleaf="T_can",
                                        Ci = "Ca", PPFD = "APAR_can"),
                        id = "canopy", fitmethod="bilinear")
    
    ## plot
    palette(c("green", "blue", "black", "red"))
    
    pdf("output/chamber_result_comparison_A_vs_Ca_flux_at_individual_canopy_level.pdf.pdf", height=12, width=6)
    par(mfrow=c(4,2))
    
    plot(ch01.fit, how="oneplot", 
         id_legend=T, colour_by_id=T, main = "Chamber 01")
    
    plot(ch02.fit, how="oneplot", 
         id_legend=T, colour_by_id=T, main = "Chamber 02")
    
    plot(ch03.fit, how="oneplot", 
         id_legend=T, colour_by_id=T, main = "Chamber 03")
    
    plot(ch04.fit, how="oneplot", 
         id_legend=T, colour_by_id=T, main = "Chamber 04")
    
    plot(ch07.fit, how="oneplot", 
         id_legend=T, colour_by_id=T, main = "Chamber 07")
    
    plot(ch08.fit, how="oneplot", 
         id_legend=T, colour_by_id=T, main = "Chamber 08")
    
    plot(ch11.fit, how="oneplot", 
         id_legend=T, colour_by_id=T, main = "Chamber 11")
    
    plot(ch12.fit, how="oneplot", 
         id_legend=T, colour_by_id=T, main = "Chamber 12")
    
    dev.off()
    
    #### reprocessing data
    nrow <- length(ch01.tf$canopy)/2
    ch01.tf$method <- rep(c("obs", "mod"), each=nrow)
    ch01.tf$canopy[(nrow+1):(2*nrow)] <- as.character(ch01.tf$canopy[1:nrow])
    ch01.tf$canopy <- as.factor(ch01.tf$canopy)
    ch01.tf$chamber <- "ch01"
    
    nrow <- length(ch02.tf$canopy)/2
    ch02.tf$method <- rep(c("obs", "mod"), each=nrow)
    ch02.tf$canopy[(nrow+1):(2*nrow)] <- as.character(ch02.tf$canopy[1:nrow])
    ch02.tf$canopy <- as.factor(ch02.tf$canopy)
    ch02.tf$chamber <- "ch02"
    
    nrow <- length(ch03.tf$canopy)/2
    ch03.tf$method <- rep(c("obs", "mod"), each=nrow)
    ch03.tf$canopy[(nrow+1):(2*nrow)] <- as.character(ch03.tf$canopy[1:nrow])
    ch03.tf$canopy <- as.factor(ch03.tf$canopy)
    ch03.tf$chamber <- "ch03"
    
    nrow <- length(ch04.tf$canopy)/2
    ch04.tf$method <- rep(c("obs", "mod"), each=nrow)
    ch04.tf$canopy[(nrow+1):(2*nrow)] <- as.character(ch04.tf$canopy[1:nrow])
    ch04.tf$canopy <- as.factor(ch04.tf$canopy)
    ch04.tf$chamber <- "ch04"
    
    nrow <- length(ch07.tf$canopy)/2
    ch07.tf$method <- rep(c("obs", "mod"), each=nrow)
    ch07.tf$canopy[(nrow+1):(2*nrow)] <- as.character(ch07.tf$canopy[1:nrow])
    ch07.tf$canopy <- as.factor(ch07.tf$canopy)
    ch07.tf$chamber <- "ch07"
    
    nrow <- length(ch08.tf$canopy)/2
    ch08.tf$method <- rep(c("obs", "mod"), each=nrow)
    ch08.tf$canopy[(nrow+1):(2*nrow)] <- as.character(ch08.tf$canopy[1:nrow])
    ch08.tf$canopy <- as.factor(ch08.tf$canopy)
    ch08.tf$chamber <- "ch08"
    
    nrow <- length(ch11.tf$canopy)/2
    ch11.tf$method <- rep(c("obs", "mod"), each=nrow)
    ch11.tf$canopy[(nrow+1):(2*nrow)] <- as.character(ch11.tf$canopy[1:nrow])
    ch11.tf$canopy <- as.factor(ch11.tf$canopy)
    ch11.tf$chamber <- "ch11"
    
    nrow <- length(ch12.tf$canopy)/2
    ch12.tf$method <- rep(c("obs", "mod"), each=nrow)
    ch12.tf$canopy[(nrow+1):(2*nrow)] <- as.character(ch12.tf$canopy[1:nrow])
    ch12.tf$canopy <- as.factor(ch12.tf$canopy)
    ch12.tf$chamber <- "ch12"
    
    plotDF <- do.call("rbind", list(ch01.tf, ch02.tf, ch03.tf, ch04.tf,
                                    ch07.tf, ch08.tf, ch11.tf, ch12.tf))
    
    ### plot leaf vs. canopy response to eCO2
    p1 <- ggplot(plotDF) +
        geom_point(aes(Ca, An, col=method, pch=as.factor(plotDF$canopy)),
                   size=1)+
        geom_smooth(aes(Ca, An, col=method, lty=as.factor(plotDF$canopy), fill=method),
                    formula = 'y ~ log(x)', method = 'glm')+
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
              legend.text.align=0)+
        xlab(expression(paste(C[a]* " (umol ", " ", mol^-1, ")")))+
        ylab(expression(paste("A (umol "* CO[2], " ", m^-1, s^-1, ")")))+
        scale_color_manual(name="Method",
                           limits=c("obs", "mod"),
                           values=c("blue2", "red3"),
                           labels=c("Observed", "Modeled"))+
        scale_linetype_discrete(name="Canopy")+
        scale_shape_discrete(name="Canopy")+
        scale_fill_discrete(name="Method")+
        xlim(0,1600)+
        ylim(-5,60)+
        ggtitle("All")+
        theme(legend.direction = "vertical", legend.box = "horizontal")
    
    p2 <- ggplot(plotDF[plotDF$canopy=="whole",]) +
        geom_point(aes(Ca, An, color=method),
                   size=1)+
        geom_smooth(aes(Ca, An, color=method),
                    formula = 'y ~ log(x)', method = 'glm')+
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
              legend.text.align=0)+
        xlab(expression(paste(C[a]* " (umol ", " ", mol^-1, ")")))+
        ylab(expression(paste("A (umol "* CO[2], " ", m^-1, s^-1, ")")))+
        scale_color_manual(name="Method",
                           limits=c("obs", "mod"),
                           values=c("blue2", "red3"),
                           labels=c("Observed", "Modeled"))+
        scale_fill_discrete(name="Method")+
        xlim(0,1600)+
        ylim(-5,60)+
        ggtitle("Whole")+
        theme(legend.direction = "vertical", legend.box = "horizontal")
    
    p3 <- ggplot(plotDF[plotDF$canopy=="m+b",]) +
        geom_point(aes(Ca, An, color=method),
                   size=1)+
        geom_smooth(aes(Ca, An, color=method),
                    formula = 'y ~ log(x)', method = 'glm')+
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
              legend.text.align=0)+
        xlab(expression(paste(C[a]* " (umol ", " ", mol^-1, ")")))+
        ylab(expression(paste("A (umol "* CO[2], " ", m^-1, s^-1, ")")))+
        scale_color_manual(name="Method",
                           limits=c("obs", "mod"),
                           values=c("blue2", "red3"),
                           labels=c("Observed", "Modeled"))+
        scale_fill_discrete(name="Method")+
        xlim(0,1600)+
        ylim(-5,60)+
        ggtitle("M+B")+
        theme(legend.direction = "vertical", legend.box = "horizontal")
    
    
    p4 <- ggplot(plotDF[plotDF$canopy=="bottom",]) +
        geom_point(aes(Ca, An, color=method),
                   size=1)+
        geom_smooth(aes(Ca, An, color=method),
                    formula = 'y ~ log(x)', method = 'glm')+
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
              legend.text.align=0)+
        xlab(expression(paste(C[a]* " (umol ", " ", mol^-1, ")")))+
        ylab(expression(paste("A (umol "* CO[2], " ", m^-1, s^-1, ")")))+
        scale_color_manual(name="Method",
                           limits=c("obs", "mod"),
                           values=c("blue2", "red3"),
                           labels=c("Observed", "Modeled"))+
        scale_fill_discrete(name="Method")+
        xlim(0,1600)+
        ylim(-5,60)+
        ggtitle("Bottom")+
        theme(legend.direction = "vertical", legend.box = "horizontal")
    
    pdf("output/grouped_result_comparison_A_vs_Ca_flux.pdf", width=16, height=6)
    plot_grid(p1, p2, p3, p4,
              labels="AUTO", ncol=4, align="v", axis = "l")
    dev.off()  
    
    
}
