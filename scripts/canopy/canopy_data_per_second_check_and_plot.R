canopy_data_per_second_check_and_plot <- function(inDF){
    
    ### Chamber 1 example
    p3 <- ggplot(inDF[inDF$Chamber==1,], aes(x=WTC_CO2, y=Norm_corr_CO2_flux, color=factor(Canopy)))+
        geom_point(size=2.0)+
        geom_smooth(aes(col=as.factor(Canopy)), 
                    method="nls", 
                    formula=y~a*exp(b/x),
                    fullrange=T,
                    method.args = list(start=c(a=1,b=0.1)), 
                    se=F)+
        labs(x=expression(CO[2], " (ppm)"), y=expression(paste(CO[2], " flux (", mu, "mol ", m^-2, " ", s^-1, ")")))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="right")+
        scale_colour_manual(name="Canopy", values = c("12345" = "red", "345" = "green",
                                                      "45" = "blue"),
                            labels=c("full", "top + middle", "top"))+
        ggtitle("Chamber 1 example")
    
    ### Chamber 2 example
    #p4 <- ggplot(inDF[inDF$Chamber==2,], aes(x=WTC_CO2, y=Norm_corr_CO2_flux, color=factor(Canopy)))+
    #    geom_point(size=2.0)+
    #    geom_smooth(se=TRUE, method="gam", formula = y~s(x))+
    #    labs(x=expression(CO[2], " (ppm)"), y=expression(paste(CO[2], " flux (", mu, "mol ", m^-2, " ", s^-1, ")")))+
    #    theme_linedraw() +
    #    theme(panel.grid.minor=element_blank(),
    #          axis.title.x = element_text(size=14), 
    #          axis.text.x = element_text(size=12),
    #          axis.text.y=element_text(size=12),
    #          axis.title.y=element_text(size=14),
    #          legend.text=element_text(size=12),
    #          legend.title=element_text(size=14),
    #          panel.grid.major=element_blank(),
    #          legend.position="right")+
    #    scale_colour_manual(name="Canopy", values = c("12345" = "red", "345" = "green",
    #                                                  "45" = "blue"),
    #                        labels=c("full", "top + middle", "top"))+
    #    ggtitle("Chamber 2 example")
    
    ### Chamber 3 example
    p5 <- ggplot(inDF[inDF$Chamber==3,], aes(x=WTC_CO2, y=Norm_corr_CO2_flux, color=factor(Canopy)))+
        geom_point(size=2.0)+
        geom_smooth(aes(col=as.factor(Canopy)), 
                    method="nls", 
                    formula=y~a*exp(b/x),
                    fullrange=T,
                    method.args = list(start=c(a=1,b=0.1)), 
                    se=F)+
        labs(x=expression(CO[2], " (ppm)"), y=expression(paste(CO[2], " flux (", mu, "mol ", m^-2, " ", s^-1, ")")))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="right")+
        scale_colour_manual(name="Canopy", values = c("12345" = "red", "345" = "green",
                                                      "45" = "blue"),
                            labels=c("full", "top + middle", "top"))+
        ggtitle("Chamber 3 example")
    
    
    ### Chamber 4 example
    p6 <- ggplot(inDF[inDF$Chamber==4,], aes(x=WTC_CO2, y=Norm_corr_CO2_flux, color=factor(Canopy)))+
        geom_point(size=2.0)+
        geom_smooth(aes(col=as.factor(Canopy)), 
                    method="nls", 
                    formula=y~a*exp(b/x),
                    fullrange=T,
                    method.args = list(start=c(a=1,b=0.1)), 
                    se=F)+
        labs(x=expression(CO[2], " (ppm)"), y=expression(paste(CO[2], " flux (", mu, "mol ", m^-2, " ", s^-1, ")")))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="right")+
        scale_colour_manual(name="Canopy", values = c("12345" = "red", "345" = "green",
                                                      "45" = "blue"),
                            labels=c("full", "top + middle", "top"))+
        ggtitle("Chamber 4 example")
    
    ### Chamber 7 example
    #p7 <- ggplot(inDF[inDF$Chamber==7,], aes(x=WTC_CO2, y=Norm_corr_CO2_flux, color=factor(Canopy)))+
    #    geom_point(size=2.0)+
    #    geom_smooth(se=TRUE, method="gam", formula = y~s(x))+
    #    labs(x=expression(CO[2], " (ppm)"), y=expression(paste(CO[2], " flux (", mu, "mol ", m^-2, " ", s^-1, ")")))+
    #    theme_linedraw() +
    #    theme(panel.grid.minor=element_blank(),
    #          axis.title.x = element_text(size=14), 
    #          axis.text.x = element_text(size=12),
    #          axis.text.y=element_text(size=12),
    #          axis.title.y=element_text(size=14),
    #          legend.text=element_text(size=12),
    #          legend.title=element_text(size=14),
    #          panel.grid.major=element_blank(),
    #          legend.position="right")+
    #    scale_colour_manual(name="Canopy", values = c("12345" = "red", "345" = "green",
    #                                                  "45" = "blue"),
    #                        labels=c("full", "top + middle", "top"))+
    #    ggtitle("Chamber 7 example")
    
    ### Chamber 8 example
    p8 <- ggplot(inDF[inDF$Chamber==8,], aes(x=WTC_CO2, y=Norm_corr_CO2_flux, color=factor(Canopy)))+
        geom_point(size=2.0)+
        geom_smooth(aes(col=as.factor(Canopy)), 
                    method="nls", 
                    formula=y~a*exp(b/x),
                    fullrange=T,
                    method.args = list(start=c(a=1,b=0.1)), 
                    se=F)+
        labs(x=expression(CO[2], " (ppm)"), y=expression(paste(CO[2], " flux (", mu, "mol ", m^-2, " ", s^-1, ")")))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="right")+
        scale_colour_manual(name="Canopy", values = c("12345" = "red", "345" = "green",
                                                      "45" = "blue"),
                            labels=c("full", "top + middle", "top"))+
        ggtitle("Chamber 8 example")
    
    ### Chamber 11 example
    p9 <- ggplot(inDF[inDF$Chamber==11,], aes(x=WTC_CO2, y=Norm_corr_CO2_flux, color=factor(Canopy)))+
        geom_point(size=2.0)+
        geom_smooth(aes(col=as.factor(Canopy)), 
                    method="nls", 
                    formula=y~a*exp(b/x),
                    fullrange=T,
                    method.args = list(start=c(a=1,b=0.1)), 
                    se=F)+
        labs(x=expression(CO[2], " (ppm)"), y=expression(paste(CO[2], " flux (", mu, "mol ", m^-2, " ", s^-1, ")")))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="right")+
        scale_colour_manual(name="Canopy", values = c("12345" = "red", "345" = "green",
                                                      "45" = "blue"),
                            labels=c("full", "top + middle", "top"))+
        ggtitle("Chamber 11 example")
    
    ### Chamber 12 example
    #p10 <- ggplot(inDF[inDF$Chamber==12,], aes(x=WTC_CO2, y=Norm_corr_CO2_flux, color=factor(Canopy)))+
    #    geom_point(size=2.0)+
    #    geom_smooth(se=TRUE, method="gam", formula = y~s(x))+
    #    labs(x=expression(CO[2], " (ppm)"), y=expression(paste(CO[2], " flux (", mu, "mol ", m^-2, " ", s^-1, ")")))+
    #    theme_linedraw() +
    #    theme(panel.grid.minor=element_blank(),
    #          axis.title.x = element_text(size=14), 
    #          axis.text.x = element_text(size=12),
    #          axis.text.y=element_text(size=12),
    #          axis.title.y=element_text(size=14),
    #          legend.text=element_text(size=12),
    #          legend.title=element_text(size=14),
    #          panel.grid.major=element_blank(),
    #          legend.position="right")+
    #    scale_colour_manual(name="Canopy", values = c("12345" = "red", "345" = "green",
    #                                                  "45" = "blue"),
    #                        labels=c("full", "top + middle", "top"))+
    #    ggtitle("Chamber 12 example")
    
    
    ### output
    pdf("output/canopy/canopy_data_chamber_CO2_drawdown_fluxes_over_Ca.pdf", width=12, height=6)
    
    plot(p3)
    #plot(p4)
    plot(p5)
    plot(p6)
    #plot(p7)
    plot(p8)
    plot(p9)
    #plot(p10)
    dev.off()

    
    ################################ H2O flux #######################################
    
    ### Chamber 1 example
    p3 <- ggplot(inDF[inDF$Chamber==1,], aes(x=WTC_CO2, y=Norm_H2O_flux, color=factor(Canopy)))+
        geom_point(size=2.0)+
        geom_smooth(aes(col=as.factor(Canopy)), 
                    method="nls", 
                    formula=y~a*exp(b/x),
                    fullrange=T,
                    method.args = list(start=c(a=1,b=0.1)), 
                    se=F)+
        labs(x=expression(CO[2], " (ppm)"), 
             y=expression(paste(H[2], "O flux (", mu, "mol ", H[2], "O ", min^-1, ")")))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="right")+
        scale_colour_manual(name="Canopy", values = c("12345" = "red", "345" = "green",
                                                      "45" = "blue"),
                            labels=c("full", "top + middle", "top"))+
        ggtitle("Chamber 1 example")
    
    ### Chamber 2 example
    #p4 <- ggplot(inDF[inDF$Chamber==2,], aes(x=WTC_CO2, y=Norm_H2O_flux, color=factor(Canopy)))+
    #    geom_point(size=2.0)+
    #    geom_smooth(se=TRUE, method="gam", formula = y~s(x))+
    #    labs(x=expression(CO[2], " (ppm)"), 
    #         y=expression(paste(H[2], "O flux (", mu, "mol ", H[2], "O ", min^-1, ")")))+
    #    theme_linedraw() +
    #    theme(panel.grid.minor=element_blank(),
    #          axis.title.x = element_text(size=14), 
    #          axis.text.x = element_text(size=12),
    #          axis.text.y=element_text(size=12),
    #          axis.title.y=element_text(size=14),
    #          legend.text=element_text(size=12),
    #          legend.title=element_text(size=14),
    #          panel.grid.major=element_blank(),
    #          legend.position="right")+
    #    scale_colour_manual(name="Canopy", values = c("12345" = "red", "345" = "green",
    #                                                  "45" = "blue"),
    #                        labels=c("full", "top + middle", "top"))+
    #    ggtitle("Chamber 2 example")
    
    ### Chamber 3 example
    p5 <- ggplot(inDF[inDF$Chamber==3,], aes(x=WTC_CO2, y=Norm_H2O_flux, color=factor(Canopy)))+
        geom_point(size=2.0)+
        geom_smooth(aes(col=as.factor(Canopy)), 
                    method="nls", 
                    formula=y~a*exp(b/x),
                    fullrange=T,
                    method.args = list(start=c(a=1,b=0.1)), 
                    se=F)+
        labs(x=expression(CO[2], " (ppm)"), 
             y=expression(paste(H[2], "O flux (", mu, "mol ", H[2], "O ", min^-1, ")")))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="right")+
        scale_colour_manual(name="Canopy", values = c("12345" = "red", "345" = "green",
                                                      "45" = "blue"),
                            labels=c("full", "top + middle", "top"))+
        ggtitle("Chamber 3 example")
    
    
    ### Chamber 4 example
    p6 <- ggplot(inDF[inDF$Chamber==4,], aes(x=WTC_CO2, y=Norm_H2O_flux, color=factor(Canopy)))+
        geom_point(size=2.0)+
        geom_smooth(aes(col=as.factor(Canopy)), 
                    method="nls", 
                    formula=y~a*exp(b/x),
                    fullrange=T,
                    method.args = list(start=c(a=1,b=0.1)), 
                    se=F)+
        labs(x=expression(CO[2], " (ppm)"), 
             y=expression(paste(H[2], "O flux (", mu, "mol ", H[2], "O ", min^-1, ")")))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="right")+
        scale_colour_manual(name="Canopy", values = c("12345" = "red", "345" = "green",
                                                      "45" = "blue"),
                            labels=c("full", "top + middle", "top"))+
        ggtitle("Chamber 4 example")
    
    ### Chamber 7 example
    #p7 <- ggplot(inDF[inDF$Chamber==7,], aes(x=WTC_CO2, y=Norm_H2O_flux, color=factor(Canopy)))+
    #    geom_point(size=2.0)+
    #    geom_smooth(se=TRUE, method="gam", formula = y~s(x))+
    #    labs(x=expression(CO[2], " (ppm)"), 
    #         y=expression(paste(H[2], "O flux (", mu, "mol ", H[2], "O ", min^-1, ")")))+
    #    theme_linedraw() +
    #    theme(panel.grid.minor=element_blank(),
    #          axis.title.x = element_text(size=14), 
    #          axis.text.x = element_text(size=12),
    #          axis.text.y=element_text(size=12),
    #          axis.title.y=element_text(size=14),
    #          legend.text=element_text(size=12),
    #          legend.title=element_text(size=14),
    #          panel.grid.major=element_blank(),
    #          legend.position="right")+
    #    scale_colour_manual(name="Canopy", values = c("12345" = "red", "345" = "green",
    #                                                  "45" = "blue"),
    #                        labels=c("full", "top + middle", "top"))+
    #    ggtitle("Chamber 7 example")
    
    ### Chamber 8 example
    p8 <- ggplot(inDF[inDF$Chamber==8,], aes(x=WTC_CO2, y=Norm_H2O_flux, color=factor(Canopy)))+
        geom_point(size=2.0)+
        geom_smooth(aes(col=as.factor(Canopy)), 
                    method="nls", 
                    formula=y~a*exp(b/x),
                    fullrange=T,
                    method.args = list(start=c(a=1,b=0.1)), 
                    se=F)+
        labs(x=expression(CO[2], " (ppm)"), 
             y=expression(paste(H[2], "O flux (", mu, "mol ", H[2], "O ", min^-1, ")")))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="right")+
        scale_colour_manual(name="Canopy", values = c("12345" = "red", "345" = "green",
                                                      "45" = "blue"),
                            labels=c("full", "top + middle", "top"))+
        ggtitle("Chamber 8 example")
    
    ### Chamber 11 example
    p9 <- ggplot(inDF[inDF$Chamber==11,], aes(x=WTC_CO2, y=Norm_H2O_flux, color=factor(Canopy)))+
        geom_point(size=2.0)+
        geom_smooth(aes(col=as.factor(Canopy)), 
                    method="nls", 
                    formula=y~a*exp(b/x),
                    fullrange=T,
                    method.args = list(start=c(a=1,b=0.1)), 
                    se=F)+
        labs(x=expression(CO[2], " (ppm)"), 
             y=expression(paste(H[2], "O flux (", mu, "mol ", H[2], "O ", min^-1, ")")))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="right")+
        scale_colour_manual(name="Canopy", values = c("12345" = "red", "345" = "green",
                                                      "45" = "blue"),
                            labels=c("full", "top + middle", "top"))+
        ggtitle("Chamber 11 example")
    
    ### Chamber 12 example
    #p10 <- ggplot(inDF[inDF$Chamber==12,], aes(x=WTC_CO2, y=Norm_H2O_flux, color=factor(Canopy)))+
    #    geom_point(size=2.0)+
    #    geom_smooth(se=TRUE, method="gam", formula = y~s(x))+
    #    labs(x=expression(CO[2], " (ppm)"), 
    #         y=expression(paste(H[2], "O flux (", mu, "mol ", H[2], "O ", min^-1, ")")))+
    #    theme_linedraw() +
    #    theme(panel.grid.minor=element_blank(),
    #          axis.title.x = element_text(size=14), 
    #          axis.text.x = element_text(size=12),
    #          axis.text.y=element_text(size=12),
    #          axis.title.y=element_text(size=14),
    #          legend.text=element_text(size=12),
    #          legend.title=element_text(size=14),
    #          panel.grid.major=element_blank(),
    #          legend.position="right")+
    #    scale_colour_manual(name="Canopy", values = c("12345" = "red", "345" = "green",
    #                                                  "45" = "blue"),
    #                        labels=c("full", "top + middle", "top"))+
    #    ggtitle("Chamber 12 example")
    
    
    ### output
    pdf("output/canopy/canopy_data_chamber_H2O_drawdown_fluxes_over_Ca.pdf", width=12, height=6)
    
    plot(p3)
    #plot(p4)
    plot(p5)
    plot(p6)
    #plot(p7)
    plot(p8)
    plot(p9)
    #plot(p10)
    dev.off()
}