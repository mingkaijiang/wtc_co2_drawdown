compare_simulated_results_at_canopy_level <- function() {
    ### this script compares canopy fluxes obtained from CO2 drawdown experiment
    ### and the scaled-up flux based on a two-leaf model
    
    ### rea in simulated files
    ## aCO2
    ch01 <- read.csv("/Users/mingkaijiang/Documents/Research/Projects/WCT1_CO2_drawdown/Two_leaf_model/outputs/wtc_two_leaf_1.csv")
    ch03 <- read.csv("/Users/mingkaijiang/Documents/Research/Projects/WCT1_CO2_drawdown/Two_leaf_model/outputs/wtc_two_leaf_3.csv")
    ch11 <- read.csv("/Users/mingkaijiang/Documents/Research/Projects/WCT1_CO2_drawdown/Two_leaf_model/outputs/wtc_two_leaf_11.csv")
    
    ## eCO2
    ch04 <- read.csv("/Users/mingkaijiang/Documents/Research/Projects/WCT1_CO2_drawdown/Two_leaf_model/outputs/wtc_two_leaf_4.csv")
    ch08 <- read.csv("/Users/mingkaijiang/Documents/Research/Projects/WCT1_CO2_drawdown/Two_leaf_model/outputs/wtc_two_leaf_8.csv")

    
    ### merge by CO2 treatment
    plotDF1 <- rbind(ch01, ch03, ch11)
    plotDF2 <- rbind(ch04, ch08)
    
    ### fit linear coefficients
    lm1 <- lm(An_can~An_obs, data=plotDF1[plotDF1$canopy=="12345",])
    lm2 <- lm(An_can~An_obs, data=plotDF1[plotDF1$canopy=="345",])
    lm3 <- lm(An_can~An_obs, data=plotDF1[plotDF1$canopy=="45",])
    lm4 <- lm(An_can~An_obs, data=plotDF2[plotDF1$canopy=="12345",])
    lm5 <- lm(An_can~An_obs, data=plotDF2[plotDF1$canopy=="345",])
    lm6 <- lm(An_can~An_obs, data=plotDF2[plotDF1$canopy=="45",])
    
    
    ### plot
    p1 <- ggplot(plotDF1) +
        geom_point(aes(An_obs, An_can, col=as.factor(canopy)),
                   size=2)+
        geom_abline(intercept=coef(lm1)[1], 
                    slope=coef(lm1)[2], lty=1, color="blue2")+
        geom_abline(intercept=coef(lm2)[1], 
                    slope=coef(lm2)[2], lty=1, color="red3")+
        geom_abline(intercept=coef(lm3)[1], 
                    slope=coef(lm3)[2], lty=1, color="orange")+
        geom_abline(lty=2, color="grey")+
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
        xlab(expression(paste(A[obs]* " (" * mu * "mol " * CO[2] * " " * m^-2 * " " * s^-1 * ")")))+
        ylab(expression(paste(A[sim]* " (" * mu * "mol " * CO[2] * " " * m^-2 * " " * s^-1 * ")")))+
        scale_color_manual(name="Canopy",
                           limits=c("12345", "345", "45"),
                           values=c("blue2", "red3", "orange"),
                           labels=c("Full", "T+M", "Top"))+
        xlim(-5,60)+
        ylim(-5,60)
    
    
    p2 <- ggplot(plotDF2) +
        geom_point(aes(An_obs, An_can, col=as.factor(canopy)),
                   size=2)+
        geom_abline(intercept=coef(lm4)[1], 
                    slope=coef(lm4)[2], lty=1, color="blue2")+
        geom_abline(intercept=coef(lm5)[1], 
                    slope=coef(lm5)[2], lty=1, color="red3")+
        geom_abline(intercept=coef(lm6)[1], 
                    slope=coef(lm6)[2], lty=1, color="orange")+
        geom_abline(lty=2, color="grey")+
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
        xlab(expression(paste(A[obs]* " (" * mu * "mol " * CO[2] * " " * m^-2 * " " * s^-1 * ")")))+
        ylab(expression(paste(A[sim]* " (" * mu * "mol " * CO[2] * " " * m^-2 * " " * s^-1 * ")")))+
        scale_color_manual(name="Canopy",
                           limits=c("12345", "345", "45"),
                           values=c("blue2", "red3", "orange"),
                           labels=c("Full", "T+M", "Top"))+
        xlim(-5,60)+
        ylim(-5,60)
    
    combined_plots <- plot_grid(p1, p2, 
                                labels=c("(a)", "(b)"),
                                ncol=1, align="vh", axis = "l",
                                label_x=0.16, label_y=0.95,
                                label_size = 18)

    legend_shared <- get_legend(p1 + theme(legend.position="bottom",
                                           legend.box = 'vertical',
                                           legend.box.just = 'left'))
    
    
    pdf("output/simulated/simulated_vs_observed_A_flux_individual_linear_fit.pdf", width=4, height=6)
    plot_grid(combined_plots, legend_shared, ncol=1, rel_heights=c(1,0.1))
    dev.off()

    
    
    p1 <- ggplot(plotDF1,aes(An_obs, An_can)) +
        geom_point(aes(An_obs, An_can, col=as.factor(canopy)),
                   size=2)+
        geom_smooth(method='lm', se=T, col="black")+
        geom_abline(lty=2, color="grey")+
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
        xlab(expression(paste(A[obs]* " (" * mu * "mol " * CO[2] * " " * m^-2 * " " * s^-1 * ")")))+
        ylab(expression(paste(A[sim]* " (" * mu * "mol " * CO[2] * " " * m^-2 * " " * s^-1 * ")")))+
        scale_color_manual(name="Canopy",
                           limits=c("12345", "345", "45"),
                           values=c("blue2", "red3", "orange"),
                           labels=c("Full", "T+M", "Top"))+
        xlim(-5,60)+
        ylim(-5,60)
    
    
    p2 <- ggplot(plotDF2,aes(An_obs, An_can)) +
        geom_point(aes(An_obs, An_can, col=as.factor(canopy)),
                   size=2)+
        geom_smooth(method='lm', se=T, col="black")+
        geom_abline(lty=2, color="grey")+
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
        xlab(expression(paste(A[obs]* " (" * mu * "mol " * CO[2] * " " * m^-2 * " " * s^-1 * ")")))+
        ylab(expression(paste(A[sim]* " (" * mu * "mol " * CO[2] * " " * m^-2 * " " * s^-1 * ")")))+
        scale_color_manual(name="Canopy",
                           limits=c("12345", "345", "45"),
                           values=c("blue2", "red3", "orange"),
                           labels=c("Full", "T+M", "Top"))+
        xlim(-5,60)+
        ylim(-5,60)
    
    combined_plots <- plot_grid(p1, p2, 
                                labels=c("(a)", "(b)"),
                                ncol=2, align="vh", axis = "l",
                                label_x=0.16, label_y=0.95,
                                label_size = 18)
    
    legend_shared <- get_legend(p1 + theme(legend.position="bottom",
                                           legend.box = 'vertical',
                                           legend.box.just = 'left'))
    
    
    pdf("output/simulated/simulated_vs_observed_A_flux.pdf", width=8, height=4)
    plot_grid(combined_plots, legend_shared, ncol=1, rel_heights=c(1,0.2))
    dev.off()
    
    

}
