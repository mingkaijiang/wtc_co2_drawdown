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
    
    ### plot two leaf results
    p1 <- ggplot(data=outDF, 
                 aes(CO2_treatment, Acan, group=canopy)) +
        #geom_rect(aes(xmin = 0, xmax = 2.5, 
        #              ymin = 0.8, ymax = 1.6),
        #          alpha = 0.2, fill = "lightgray")+
        geom_bar(stat = "identity", aes(fill=canopy), 
                 position="dodge") +
        #geom_vline(xintercept=2.5, lty=2)+
        #geom_vline(xintercept=3.5, lty=2)+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=10),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="right",
              legend.box = 'vertical',
              legend.box.just = 'right',
              plot.title = element_text(size=16, face="bold", 
                                        hjust = 0.5),
              legend.text.align = 0)+
        ylab(expression(paste(delta * A * " / " * A[400])))+
        #scale_x_discrete(breaks=c("1_up", "2_low", "3_Full", "4_MATE", "5_MATE", 
        #                          "6_two_leaf", "7_multi"),
        #                 labels=c("Up", 
        #                          "Low",
        #                          "Full",
        #                          "MATE",
        #                          "MATE2",
        #                          "Two-leaf",
        #                          "Multi-layer"))+
        #scale_fill_manual(name="",
        #                  breaks=c("A", "Ac", "Aj", "A1sun", "A2sha"),
        #                  labels=c("A", expression(paste(A[c])),
        #                           expression(paste(A[j])),
        #                           expression(paste(A[sun])),
        #                           expression(paste(A[sha]))),
        #                  values = colorblind_pal()(5 + 1)[-1])+
        xlab("")+
        coord_cartesian(ylim = c(0, 0.5)) 
    
    
    plot(p1)
  
    
}


