process_two_leaf_model_results <- function() {
    
    ### set source dir
    source.dir <- "/Users/mingkaijiang/Documents/Research/Projects/WCT1_CO2_drawdown/Two_leaf_model/outputs/"
    met.dir <- "/Users/mingkaijiang/Documents/Research/Projects/WCT1_CO2_drawdown/Two_leaf_model/met_data/"
    
    ### read input
    aCO2 <- read.csv(paste0(source.dir, "wtc_two_leaf_aCO2.csv"))
    eCO2 <- read.csv(paste0(source.dir, "wtc_two_leaf_aCO2.csv"))
    metDF <- read.csv(paste0(met.dir, "met_constant_forcing_aCO2.csv"))
    
    ### merge met and simulation output
    aCO2 <- cbind(aCO2, metDF)
    eCO2 <- cbind(eCO2, metDF)
    
    
    ### split the ID column and then select the right data to look at
    ### currently we have light response curves (at two CO2 levels) and 
    ### CO2 response curves (at high light) for different canopy LAI and 
    ### for trees of different CO2 exposure histories.
    ### to directly compare WTC and model result, we need 
    ### the CO2 response curves to see (A600 - A400) / A400
    aCO2$Model_Trt <- sub("_.*", "", aCO2$ID)
    eCO2$Model_Trt <- sub("_.*", "", eCO2$ID)
    
    asubDF <- aCO2[aCO2$Model_Trt == "Ca",]
    esubDF <- eCO2[eCO2$Model_Trt == "Ca",]
    
    ### prepare sensitivity DF as output
    outDF <- data.frame(rep(c("aCO2", "eCO2"), each = 3),
                        rep(c("12345", "345", "45"), 2),
                        NA, NA, NA, NA, NA, NA, 
                        NA, NA, NA, NA, NA, NA,
                        NA, NA, NA)
    
    colnames(outDF) <- c("CO2_treatment", "canopy",
                         "Acan_400", "Asun_400", "Asha_400",
                         "Acan_600", "Asun_600", "Asha_600",
                         "Acan", "Asun", "Asha",
                         "APAR_can", "APAR_sun", "APAR_sha",
                         "LAI_can", "LAI_sun", "LAI_sha")
    

    ## aCO2
    outDF$Acan_400[outDF$CO2_treatment=="aCO2"&outDF$canopy=="12345"] <- asubDF$An_can[asubDF$Ca==400&asubDF$canopy=="12345"]
    outDF$Asun_400[outDF$CO2_treatment=="aCO2"&outDF$canopy=="12345"] <- asubDF$An_sun[asubDF$Ca==400&asubDF$canopy=="12345"]
    outDF$Asha_400[outDF$CO2_treatment=="aCO2"&outDF$canopy=="12345"] <- asubDF$An_sha[asubDF$Ca==400&asubDF$canopy=="12345"]
    
    outDF$Acan_600[outDF$CO2_treatment=="aCO2"&outDF$canopy=="12345"] <- asubDF$An_can[asubDF$Ca==600&asubDF$canopy=="12345"]
    outDF$Asun_600[outDF$CO2_treatment=="aCO2"&outDF$canopy=="12345"] <- asubDF$An_sun[asubDF$Ca==600&asubDF$canopy=="12345"]
    outDF$Asha_600[outDF$CO2_treatment=="aCO2"&outDF$canopy=="12345"] <- asubDF$An_sha[asubDF$Ca==600&asubDF$canopy=="12345"]
    
    
    outDF$Acan_400[outDF$CO2_treatment=="aCO2"&outDF$canopy=="345"] <- asubDF$An_can[asubDF$Ca==400&asubDF$canopy=="345"]
    outDF$Asun_400[outDF$CO2_treatment=="aCO2"&outDF$canopy=="345"] <- asubDF$An_sun[asubDF$Ca==400&asubDF$canopy=="345"]
    outDF$Asha_400[outDF$CO2_treatment=="aCO2"&outDF$canopy=="345"] <- asubDF$An_sha[asubDF$Ca==400&asubDF$canopy=="345"]
    
    outDF$Acan_600[outDF$CO2_treatment=="aCO2"&outDF$canopy=="345"] <- asubDF$An_can[asubDF$Ca==600&asubDF$canopy=="345"]
    outDF$Asun_600[outDF$CO2_treatment=="aCO2"&outDF$canopy=="345"] <- asubDF$An_sun[asubDF$Ca==600&asubDF$canopy=="345"]
    outDF$Asha_600[outDF$CO2_treatment=="aCO2"&outDF$canopy=="345"] <- asubDF$An_sha[asubDF$Ca==600&asubDF$canopy=="345"]
    
    
    outDF$Acan_400[outDF$CO2_treatment=="aCO2"&outDF$canopy=="45"] <- asubDF$An_can[asubDF$Ca==400&asubDF$canopy=="45"]
    outDF$Asun_400[outDF$CO2_treatment=="aCO2"&outDF$canopy=="45"] <- asubDF$An_sun[asubDF$Ca==400&asubDF$canopy=="45"]
    outDF$Asha_400[outDF$CO2_treatment=="aCO2"&outDF$canopy=="45"] <- asubDF$An_sha[asubDF$Ca==400&asubDF$canopy=="45"]
    
    outDF$Acan_600[outDF$CO2_treatment=="aCO2"&outDF$canopy=="45"] <- asubDF$An_can[asubDF$Ca==600&asubDF$canopy=="45"]
    outDF$Asun_600[outDF$CO2_treatment=="aCO2"&outDF$canopy=="45"] <- asubDF$An_sun[asubDF$Ca==600&asubDF$canopy=="45"]
    outDF$Asha_600[outDF$CO2_treatment=="aCO2"&outDF$canopy=="45"] <- asubDF$An_sha[asubDF$Ca==600&asubDF$canopy=="45"]
    
    
    outDF$APAR_can[outDF$CO2_treatment=="aCO2"&outDF$canopy=="12345"] <- asubDF$APAR_can[asubDF$Ca==400&asubDF$canopy=="12345"]
    outDF$APAR_sun[outDF$CO2_treatment=="aCO2"&outDF$canopy=="12345"] <- asubDF$APAR_sun[asubDF$Ca==400&asubDF$canopy=="12345"]
    outDF$APAR_sha[outDF$CO2_treatment=="aCO2"&outDF$canopy=="12345"] <- asubDF$APAR_sha[asubDF$Ca==400&asubDF$canopy=="12345"]
    
    
    outDF$APAR_can[outDF$CO2_treatment=="aCO2"&outDF$canopy=="345"] <- asubDF$APAR_can[asubDF$Ca==400&asubDF$canopy=="345"]
    outDF$APAR_sun[outDF$CO2_treatment=="aCO2"&outDF$canopy=="345"] <- asubDF$APAR_sun[asubDF$Ca==400&asubDF$canopy=="345"]
    outDF$APAR_sha[outDF$CO2_treatment=="aCO2"&outDF$canopy=="345"] <- asubDF$APAR_sha[asubDF$Ca==400&asubDF$canopy=="345"]
    
    
    outDF$APAR_can[outDF$CO2_treatment=="aCO2"&outDF$canopy=="45"] <- asubDF$APAR_can[asubDF$Ca==400&asubDF$canopy=="45"]
    outDF$APAR_sun[outDF$CO2_treatment=="aCO2"&outDF$canopy=="45"] <- asubDF$APAR_sun[asubDF$Ca==400&asubDF$canopy=="45"]
    outDF$APAR_sha[outDF$CO2_treatment=="aCO2"&outDF$canopy=="45"] <- asubDF$APAR_sha[asubDF$Ca==400&asubDF$canopy=="45"]
    
    
    outDF$LAI_can[outDF$CO2_treatment=="aCO2"&outDF$canopy=="12345"] <- asubDF$LAI_can[asubDF$Ca==400&asubDF$canopy=="12345"]
    outDF$LAI_sun[outDF$CO2_treatment=="aCO2"&outDF$canopy=="12345"] <- asubDF$LAI_sun[asubDF$Ca==400&asubDF$canopy=="12345"]
    outDF$LAI_sha[outDF$CO2_treatment=="aCO2"&outDF$canopy=="12345"] <- asubDF$LAI_sha[asubDF$Ca==400&asubDF$canopy=="12345"]
    
    
    outDF$LAI_can[outDF$CO2_treatment=="aCO2"&outDF$canopy=="345"] <- asubDF$LAI_can[asubDF$Ca==400&asubDF$canopy=="345"]
    outDF$LAI_sun[outDF$CO2_treatment=="aCO2"&outDF$canopy=="345"] <- asubDF$LAI_sun[asubDF$Ca==400&asubDF$canopy=="345"]
    outDF$LAI_sha[outDF$CO2_treatment=="aCO2"&outDF$canopy=="345"] <- asubDF$LAI_sha[asubDF$Ca==400&asubDF$canopy=="345"]
    
    
    outDF$LAI_can[outDF$CO2_treatment=="aCO2"&outDF$canopy=="45"] <- asubDF$LAI_can[asubDF$Ca==400&asubDF$canopy=="45"]
    outDF$LAI_sun[outDF$CO2_treatment=="aCO2"&outDF$canopy=="45"] <- asubDF$LAI_sun[asubDF$Ca==400&asubDF$canopy=="45"]
    outDF$LAI_sha[outDF$CO2_treatment=="aCO2"&outDF$canopy=="45"] <- asubDF$LAI_sha[asubDF$Ca==400&asubDF$canopy=="45"]
    
    
    ## eCO2
    outDF$Acan_400[outDF$CO2_treatment=="eCO2"&outDF$canopy=="12345"] <- esubDF$An_can[esubDF$Ca==400&esubDF$canopy=="12345"]
    outDF$Asun_400[outDF$CO2_treatment=="eCO2"&outDF$canopy=="12345"] <- esubDF$An_sun[esubDF$Ca==400&esubDF$canopy=="12345"]
    outDF$Asha_400[outDF$CO2_treatment=="eCO2"&outDF$canopy=="12345"] <- esubDF$An_sha[esubDF$Ca==400&esubDF$canopy=="12345"]
    
    outDF$Acan_600[outDF$CO2_treatment=="eCO2"&outDF$canopy=="12345"] <- esubDF$An_can[esubDF$Ca==600&esubDF$canopy=="12345"]
    outDF$Asun_600[outDF$CO2_treatment=="eCO2"&outDF$canopy=="12345"] <- esubDF$An_sun[esubDF$Ca==600&esubDF$canopy=="12345"]
    outDF$Asha_600[outDF$CO2_treatment=="eCO2"&outDF$canopy=="12345"] <- esubDF$An_sha[esubDF$Ca==600&esubDF$canopy=="12345"]
    
    
    outDF$Acan_400[outDF$CO2_treatment=="eCO2"&outDF$canopy=="345"] <- esubDF$An_can[esubDF$Ca==400&esubDF$canopy=="345"]
    outDF$Asun_400[outDF$CO2_treatment=="eCO2"&outDF$canopy=="345"] <- esubDF$An_sun[esubDF$Ca==400&esubDF$canopy=="345"]
    outDF$Asha_400[outDF$CO2_treatment=="eCO2"&outDF$canopy=="345"] <- esubDF$An_sha[esubDF$Ca==400&esubDF$canopy=="345"]
    
    outDF$Acan_600[outDF$CO2_treatment=="eCO2"&outDF$canopy=="345"] <- esubDF$An_can[esubDF$Ca==600&esubDF$canopy=="345"]
    outDF$Asun_600[outDF$CO2_treatment=="eCO2"&outDF$canopy=="345"] <- esubDF$An_sun[esubDF$Ca==600&esubDF$canopy=="345"]
    outDF$Asha_600[outDF$CO2_treatment=="eCO2"&outDF$canopy=="345"] <- esubDF$An_sha[esubDF$Ca==600&esubDF$canopy=="345"]
    
    
    outDF$Acan_400[outDF$CO2_treatment=="eCO2"&outDF$canopy=="45"] <- esubDF$An_can[esubDF$Ca==400&esubDF$canopy=="45"]
    outDF$Asun_400[outDF$CO2_treatment=="eCO2"&outDF$canopy=="45"] <- esubDF$An_sun[esubDF$Ca==400&esubDF$canopy=="45"]
    outDF$Asha_400[outDF$CO2_treatment=="eCO2"&outDF$canopy=="45"] <- esubDF$An_sha[esubDF$Ca==400&esubDF$canopy=="45"]
    
    outDF$Acan_600[outDF$CO2_treatment=="eCO2"&outDF$canopy=="45"] <- esubDF$An_can[esubDF$Ca==600&esubDF$canopy=="45"]
    outDF$Asun_600[outDF$CO2_treatment=="eCO2"&outDF$canopy=="45"] <- esubDF$An_sun[esubDF$Ca==600&esubDF$canopy=="45"]
    outDF$Asha_600[outDF$CO2_treatment=="eCO2"&outDF$canopy=="45"] <- esubDF$An_sha[esubDF$Ca==600&esubDF$canopy=="45"]
    
    
    outDF$APAR_can[outDF$CO2_treatment=="eCO2"&outDF$canopy=="12345"] <- esubDF$APAR_can[esubDF$Ca==400&esubDF$canopy=="12345"]
    outDF$APAR_sun[outDF$CO2_treatment=="eCO2"&outDF$canopy=="12345"] <- esubDF$APAR_sun[esubDF$Ca==400&esubDF$canopy=="12345"]
    outDF$APAR_sha[outDF$CO2_treatment=="eCO2"&outDF$canopy=="12345"] <- esubDF$APAR_sha[esubDF$Ca==400&esubDF$canopy=="12345"]
    
    
    outDF$APAR_can[outDF$CO2_treatment=="eCO2"&outDF$canopy=="345"] <- esubDF$APAR_can[esubDF$Ca==400&esubDF$canopy=="345"]
    outDF$APAR_sun[outDF$CO2_treatment=="eCO2"&outDF$canopy=="345"] <- esubDF$APAR_sun[esubDF$Ca==400&esubDF$canopy=="345"]
    outDF$APAR_sha[outDF$CO2_treatment=="eCO2"&outDF$canopy=="345"] <- esubDF$APAR_sha[esubDF$Ca==400&esubDF$canopy=="345"]
    
    
    outDF$APAR_can[outDF$CO2_treatment=="eCO2"&outDF$canopy=="45"] <- esubDF$APAR_can[esubDF$Ca==400&esubDF$canopy=="45"]
    outDF$APAR_sun[outDF$CO2_treatment=="eCO2"&outDF$canopy=="45"] <- esubDF$APAR_sun[esubDF$Ca==400&esubDF$canopy=="45"]
    outDF$APAR_sha[outDF$CO2_treatment=="eCO2"&outDF$canopy=="45"] <- esubDF$APAR_sha[esubDF$Ca==400&esubDF$canopy=="45"]
    
    
    outDF$LAI_can[outDF$CO2_treatment=="eCO2"&outDF$canopy=="12345"] <- esubDF$LAI_can[esubDF$Ca==400&esubDF$canopy=="12345"]
    outDF$LAI_sun[outDF$CO2_treatment=="eCO2"&outDF$canopy=="12345"] <- esubDF$LAI_sun[esubDF$Ca==400&esubDF$canopy=="12345"]
    outDF$LAI_sha[outDF$CO2_treatment=="eCO2"&outDF$canopy=="12345"] <- esubDF$LAI_sha[esubDF$Ca==400&esubDF$canopy=="12345"]
    
    
    outDF$LAI_can[outDF$CO2_treatment=="eCO2"&outDF$canopy=="345"] <- esubDF$LAI_can[esubDF$Ca==400&esubDF$canopy=="345"]
    outDF$LAI_sun[outDF$CO2_treatment=="eCO2"&outDF$canopy=="345"] <- esubDF$LAI_sun[esubDF$Ca==400&esubDF$canopy=="345"]
    outDF$LAI_sha[outDF$CO2_treatment=="eCO2"&outDF$canopy=="345"] <- esubDF$LAI_sha[esubDF$Ca==400&esubDF$canopy=="345"]
    
    
    outDF$LAI_can[outDF$CO2_treatment=="eCO2"&outDF$canopy=="45"] <- esubDF$LAI_can[esubDF$Ca==400&esubDF$canopy=="45"]
    outDF$LAI_sun[outDF$CO2_treatment=="eCO2"&outDF$canopy=="45"] <- esubDF$LAI_sun[esubDF$Ca==400&esubDF$canopy=="45"]
    outDF$LAI_sha[outDF$CO2_treatment=="eCO2"&outDF$canopy=="45"] <- esubDF$LAI_sha[esubDF$Ca==400&esubDF$canopy=="45"]
    
    
    ## calculate sensitivity
    outDF$Acan <- with(outDF, (Acan_600-Acan_400)/Acan_400)
    outDF$Asun <- with(outDF, (Asun_600-Asun_400)/Asun_400)
    outDF$Asha <- with(outDF, (Asha_600-Asha_400)/Asha_400)
    
    
    ### save output
    write.csv(outDF, "output/simulated/two_leaf_result_summary.csv", row.names=F)
    

  
    ### plot APAR and LAI by sun and shaded leaf layers
    plotDF2 <- data.frame(rep(c("12345", "345", "45"), each=2),
                          rep(c("sun", "shade"), 3),
                          NA, NA)
    colnames(plotDF2) <- c("canopy", "layer", "APAR", "LAI")
    
    plotDF2$APAR[plotDF2$canopy=="12345"&plotDF2$layer=="sun"]<-outDF$APAR_sun[outDF$canopy=="12345"][1]
    plotDF2$APAR[plotDF2$canopy=="345"&plotDF2$layer=="sun"]<-outDF$APAR_sun[outDF$canopy=="345"][1]
    plotDF2$APAR[plotDF2$canopy=="45"&plotDF2$layer=="sun"]<-outDF$APAR_sun[outDF$canopy=="45"][1]
    
    plotDF2$APAR[plotDF2$canopy=="12345"&plotDF2$layer=="shade"]<-outDF$APAR_sha[outDF$canopy=="12345"][1]
    plotDF2$APAR[plotDF2$canopy=="345"&plotDF2$layer=="shade"]<-outDF$APAR_sha[outDF$canopy=="345"][1]
    plotDF2$APAR[plotDF2$canopy=="45"&plotDF2$layer=="shade"]<-outDF$APAR_sha[outDF$canopy=="45"][1]
    
    plotDF2$LAI[plotDF2$canopy=="12345"&plotDF2$layer=="sun"]<-outDF$LAI_sun[outDF$canopy=="12345"][1]
    plotDF2$LAI[plotDF2$canopy=="345"&plotDF2$layer=="sun"]<-outDF$LAI_sun[outDF$canopy=="345"][1]
    plotDF2$LAI[plotDF2$canopy=="45"&plotDF2$layer=="sun"]<-outDF$LAI_sun[outDF$canopy=="45"][1]
    
    plotDF2$LAI[plotDF2$canopy=="12345"&plotDF2$layer=="shade"]<-outDF$LAI_sha[outDF$canopy=="12345"][1]
    plotDF2$LAI[plotDF2$canopy=="345"&plotDF2$layer=="shade"]<-outDF$LAI_sha[outDF$canopy=="345"][1]
    plotDF2$LAI[plotDF2$canopy=="45"&plotDF2$layer=="shade"]<-outDF$LAI_sha[outDF$canopy=="45"][1]
    
    ## label
    plotDF2$lab <- plotDF2$canopy
    plotDF2$lab <- gsub("12345", "1_Full", plotDF2$lab)
    plotDF2$lab <- gsub("345", "2_TM", plotDF2$lab)
    plotDF2$lab <- gsub("45", "3_T", plotDF2$lab)
    
    ## plot
    p1 <- ggplot(data=plotDF2, 
                 aes(lab, APAR, group=layer)) +
        geom_bar(stat = "identity", aes(fill=layer), 
                 position="stack") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=10),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'vertical',
              legend.box.just = 'right',
              plot.title = element_text(size=16, face="bold", 
                                        hjust = 0.5),
              legend.text.align = 0)+
        ylab(expression(paste("APAR (" * mu * "mol " * m^-2 * " " * s^-1 * ")")))+
        scale_x_discrete(breaks=c("1_Full", "2_TM", "3_T"),
                         labels=c("Full",
                                  "T+M",
                                  "T"))+
        scale_fill_manual(name="Leaf layer",
                          breaks=c("sun", "shade"),
                          labels=c("sun", "shade"),
                          values = colorblind_pal()(2 + 1)[-1])+
    xlab("")
        
    
    p2 <- ggplot(data=plotDF2, 
                 aes(lab, LAI, group=layer)) +
        geom_bar(stat = "identity", aes(fill=layer), 
                 position="stack") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=10),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'vertical',
              legend.box.just = 'right',
              plot.title = element_text(size=16, face="bold", 
                                        hjust = 0.5),
              legend.text.align = 0)+
        ylab("LAI")+
        scale_x_discrete(breaks=c("1_Full", "2_TM", "3_T"),
                         labels=c("Full",
                                  "T+M",
                                  "T"))+
        scale_fill_manual(name="leaf layer",
                          breaks=c("sun", "shade"),
                          labels=c("sun", "shade"),
                          values = colorblind_pal()(2 + 1)[-1])+
        xlab("")
    
    
    ## pdf
    legend_shared <- get_legend(p1 + theme(legend.position="bottom",
                                           legend.box = 'vertical',
                                           legend.box.just = 'left'))
    
    combined_plots <- plot_grid(p1, p2, 
                                labels=c("(a)", "(b)"),
                                ncol=1, align="vh", axis = "l",
                                label_x=0.8, label_y=0.95,
                                label_size = 18)
    
    
    pdf("output/simulated/two_leaf_result_APAR_LAI.pdf", width=4, height=6)
    plot_grid(combined_plots, legend_shared, ncol=1, rel_heights=c(1,0.1))
    dev.off()  
    
    
    
    ### plot sensitivity
    plotDF1 <- data.frame(rep(c("12345", "345", "45"), each=3),
                          rep(c("Anet", "Asun", "Ashade"), 3),
                          NA)
    colnames(plotDF1) <- c("canopy", "layer", "A")
    plotDF1$lab <- plotDF1$canopy
    plotDF1$lab <- gsub("12345", "1_Full", plotDF1$lab)
    plotDF1$lab <- gsub("345", "2_TM", plotDF1$lab)
    plotDF1$lab <- gsub("45", "3_T", plotDF1$lab)
    
    plotDF2 <- plotDF1
    
    
    ## assign values
    plotDF1$A[plotDF1$canopy=="12345"&plotDF1$layer=="Anet"] <- outDF$Acan[outDF$CO2_treatment=="aCO2"&outDF$canopy=="12345"]
    plotDF1$A[plotDF1$canopy=="12345"&plotDF1$layer=="Asun"] <- outDF$Asun[outDF$CO2_treatment=="aCO2"&outDF$canopy=="12345"]
    plotDF1$A[plotDF1$canopy=="12345"&plotDF1$layer=="Ashade"] <- outDF$Asha[outDF$CO2_treatment=="aCO2"&outDF$canopy=="12345"]
    
    plotDF1$A[plotDF1$canopy=="345"&plotDF1$layer=="Anet"] <- outDF$Acan[outDF$CO2_treatment=="aCO2"&outDF$canopy=="345"]
    plotDF1$A[plotDF1$canopy=="345"&plotDF1$layer=="Asun"] <- outDF$Asun[outDF$CO2_treatment=="aCO2"&outDF$canopy=="345"]
    plotDF1$A[plotDF1$canopy=="345"&plotDF1$layer=="Ashade"] <- outDF$Asha[outDF$CO2_treatment=="aCO2"&outDF$canopy=="345"]
    
    plotDF1$A[plotDF1$canopy=="45"&plotDF1$layer=="Anet"] <- outDF$Acan[outDF$CO2_treatment=="aCO2"&outDF$canopy=="45"]
    plotDF1$A[plotDF1$canopy=="45"&plotDF1$layer=="Asun"] <- outDF$Asun[outDF$CO2_treatment=="aCO2"&outDF$canopy=="45"]
    plotDF1$A[plotDF1$canopy=="45"&plotDF1$layer=="Ashade"] <- outDF$Asha[outDF$CO2_treatment=="aCO2"&outDF$canopy=="45"]
    
    
    plotDF2$A[plotDF2$canopy=="12345"&plotDF2$layer=="Anet"] <- outDF$Acan[outDF$CO2_treatment=="eCO2"&outDF$canopy=="12345"]
    plotDF2$A[plotDF2$canopy=="12345"&plotDF2$layer=="Asun"] <- outDF$Asun[outDF$CO2_treatment=="eCO2"&outDF$canopy=="12345"]
    plotDF2$A[plotDF2$canopy=="12345"&plotDF2$layer=="Ashade"] <- outDF$Asha[outDF$CO2_treatment=="eCO2"&outDF$canopy=="12345"]
    
    plotDF2$A[plotDF2$canopy=="345"&plotDF2$layer=="Anet"] <- outDF$Acan[outDF$CO2_treatment=="eCO2"&outDF$canopy=="345"]
    plotDF2$A[plotDF2$canopy=="345"&plotDF2$layer=="Asun"] <- outDF$Asun[outDF$CO2_treatment=="eCO2"&outDF$canopy=="345"]
    plotDF2$A[plotDF2$canopy=="345"&plotDF2$layer=="Ashade"] <- outDF$Asha[outDF$CO2_treatment=="eCO2"&outDF$canopy=="345"]
    
    plotDF2$A[plotDF2$canopy=="45"&plotDF2$layer=="Anet"] <- outDF$Acan[outDF$CO2_treatment=="eCO2"&outDF$canopy=="45"]
    plotDF2$A[plotDF2$canopy=="45"&plotDF2$layer=="Asun"] <- outDF$Asun[outDF$CO2_treatment=="eCO2"&outDF$canopy=="45"]
    plotDF2$A[plotDF2$canopy=="45"&plotDF2$layer=="Ashade"] <- outDF$Asha[outDF$CO2_treatment=="eCO2"&outDF$canopy=="45"]
    
    
    ### plot two leaf results
    p1 <- ggplot(data=plotDF1, 
                 aes(lab, A, group=layer)) +
        geom_bar(stat = "identity", aes(fill=layer), 
                 position="dodge") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=10),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'vertical',
              legend.box.just = 'right',
              plot.title = element_text(size=16, face="bold", 
                                        hjust = 0.5),
              legend.text.align = 0)+
        ylab(expression(paste(Delta * A * " / " * A[400])))+
        scale_x_discrete(breaks=c("1_Full", "2_TM", "3_T"),
                         labels=c("Full",
                                  "T+M",
                                  "T"))+
        scale_fill_manual(name="leaf layer",
                          breaks=c("Anet", "Asun", "Ashade"),
                          labels=c("canopy", "sun", "shade"),
                          values = c("grey", colorblind_pal()(2 + 1)[-1]))+
    xlab(expression(aCO[2]))+
    coord_cartesian(ylim = c(0.19, 0.21)) 
    
    
    p2 <- ggplot(data=plotDF2, 
                 aes(lab, A, group=layer)) +
        geom_bar(stat = "identity", aes(fill=layer), 
                 position="dodge") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=10),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'vertical',
              legend.box.just = 'right',
              plot.title = element_text(size=16, face="bold", 
                                        hjust = 0.5),
              legend.text.align = 0)+
        ylab(expression(paste(Delta * A * " / " * A[400])))+
        scale_x_discrete(breaks=c("1_Full", "2_TM", "3_T"),
                         labels=c("Full",
                                  "T+M",
                                  "T"))+
        scale_fill_manual(name="leaf layer",
                          breaks=c("Anet", "Asun", "Ashade"),
                          labels=c("canopy", "sun", "shade"),
                          values = c("grey", colorblind_pal()(2 + 1)[-1]))+
        xlab(expression(eCO[2]))+
        coord_cartesian(ylim = c(0.19, 0.21)) 
    
    ## pdf
    legend_shared <- get_legend(p1 + theme(legend.position="bottom",
                                           legend.box = 'vertical',
                                           legend.box.just = 'left'))
    
    combined_plots <- plot_grid(p1, p2, 
                                labels=c("(a)", "(b)"),
                                ncol=2, align="vh", axis = "l",
                                label_x=0.8, label_y=0.95,
                                label_size = 18)
    
    
    pdf("output/simulated/two_leaf_result_A_sensitivity.pdf", width=6, height=3)
    plot_grid(combined_plots, legend_shared, ncol=1, rel_heights=c(1,0.1))
    dev.off()  
    
}


