process_MAAT_output <- function() {
    
    ### this is code to process MAAT output and generate figures
    ### currently MAAT outputs single bigleaf and multi-layer model results
    
    ### read input
    aDF <- read.csv("data/MAAT/out_aco2co2.csv")
    eDF <- read.csv("data/MAAT/out_eco2co2.csv")
    
    aDF$CO2_treatment <- "aCO2"
    eDF$CO2_treatment <- "eCO2"
    
    myDF <- rbind(aDF, eDF)
    
    ### split according to big model and multi-layer model
    bigDF <- myDF[myDF$canopy.sys=="f_sys_bigleaf_s1992",]
    mulDF <- myDF[myDF$canopy.sys=="f_sys_multilayer",]
    
    ### data structure:
    ### Each model simulation set contains A-Ca responses under 
    ### real LAI (6.59 and 5) and 
    ### a list of LAI (1 - 10),
    ### and long-term CO2 treatment (aCO2 and eCO2)
    
    ### Calculate A sensitivity
    outDF1 <- data.frame(rep(c("aCO2", "eCO2"), each = 12),
                        rep(c(6.59, 5.01, c(1:10)), 2), 
                        NA, NA, NA, NA, NA, NA, NA)
    colnames(outDF1) <- c("CO2_treatment", "LAI", 
                          "A400", "A600", 
                          "Ac400", "Ac600",
                          "Aj400", "Aj600",
                          "Model")
    outDF2 <- outDF1     
        
    outDF1$Model <- "Bigleaf"
    outDF2$Model <- "Multilayer"
    
    ### assign values
    for(i in c(6.59, 5.01, c(1:10))){
        for (j in c("aCO2", "eCO2")) {
            outDF1$A400[outDF1$CO2_treatment==j&outDF1$LAI==i] <- bigDF$A[bigDF$canopy.lai==i&bigDF$CO2_treatment==j&bigDF$canopy.ca_conc==400]
            outDF1$A600[outDF1$CO2_treatment==j&outDF1$LAI==i] <- bigDF$A[bigDF$canopy.lai==i&bigDF$CO2_treatment==j&bigDF$canopy.ca_conc==600]
            
            outDF1$Ac400[outDF1$CO2_treatment==j&outDF1$LAI==i] <- bigDF$Acg_lim[bigDF$canopy.lai==i&bigDF$CO2_treatment==j&bigDF$canopy.ca_conc==400]
            outDF1$Ac600[outDF1$CO2_treatment==j&outDF1$LAI==i] <- bigDF$Acg_lim[bigDF$canopy.lai==i&bigDF$CO2_treatment==j&bigDF$canopy.ca_conc==600]
            
            outDF1$Aj400[outDF1$CO2_treatment==j&outDF1$LAI==i] <- bigDF$Ajg_lim[bigDF$canopy.lai==i&bigDF$CO2_treatment==j&bigDF$canopy.ca_conc==400]
            outDF1$Aj600[outDF1$CO2_treatment==j&outDF1$LAI==i] <- bigDF$Ajg_lim[bigDF$canopy.lai==i&bigDF$CO2_treatment==j&bigDF$canopy.ca_conc==600]
            
        }
    }
    
    for(i in c(6.59, 5.01, c(1:10))){
        for (j in c("aCO2", "eCO2")) {
            outDF2$A400[outDF2$CO2_treatment==j&outDF2$LAI==i] <- mulDF$A[mulDF$canopy.lai==i&mulDF$CO2_treatment==j&mulDF$canopy.ca_conc==400]
            outDF2$A600[outDF2$CO2_treatment==j&outDF2$LAI==i] <- mulDF$A[mulDF$canopy.lai==i&mulDF$CO2_treatment==j&mulDF$canopy.ca_conc==600]
            
            outDF2$Ac400[outDF2$CO2_treatment==j&outDF2$LAI==i] <- mulDF$Acg_lim[mulDF$canopy.lai==i&mulDF$CO2_treatment==j&mulDF$canopy.ca_conc==400]
            outDF2$Ac600[outDF2$CO2_treatment==j&outDF2$LAI==i] <- mulDF$Acg_lim[mulDF$canopy.lai==i&mulDF$CO2_treatment==j&mulDF$canopy.ca_conc==600]
            
            outDF2$Aj400[outDF2$CO2_treatment==j&outDF2$LAI==i] <- mulDF$Ajg_lim[mulDF$canopy.lai==i&mulDF$CO2_treatment==j&mulDF$canopy.ca_conc==400]
            outDF2$Aj600[outDF2$CO2_treatment==j&outDF2$LAI==i] <- mulDF$Ajg_lim[mulDF$canopy.lai==i&mulDF$CO2_treatment==j&mulDF$canopy.ca_conc==600]
            
        }
    }
    
    # combine
    outDF <- rbind(outDF1, outDF2)
    
    ## calculate Asens
    outDF$Asens <- with(outDF, (A600-A400)/A400)
    outDF$Ac_sens <- with(outDF, (Ac600-Ac400)/Ac400)
    outDF$Aj_sens <- with(outDF, (Aj600-Aj400)/Aj400)
    
    
    write.csv(outDF, "output/MAAT/MAAT_Asens_output_table.csv", row.names=F)
    
    ### plotDF1 - compare bigleaf and multi-layer model A sensitivity
    ### under a fixed LAI = 6.59
    subDF <- outDF[outDF$LAI==6.59,]
    
    plotDF1 <- melt(setDT(subDF), id.vars = c("CO2_treatment", "Model"), 
                 measure.vars = 10:12, variable.name = "Limiting")
    
    
    ### plot
    p1 <- ggplot(plotDF1[plotDF1$CO2_treatment == "aCO2",]) +
        geom_bar(stat = "identity", 
                 aes(x=Model, y=value, fill = Limiting),
                 position="dodge")+
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
        scale_x_discrete(breaks=c("Bigleaf", "Multilayer"),
                         labels=c("Big leaf", "Multi-layer"))+
        scale_fill_manual(name="Limiting rate",
                          breaks=c("Asens", "Ac_sens", "Aj_sens"),
                          labels=c(expression(A[net]), 
                                   expression(A[c]), 
                                   expression(A[j])),
                          values = c("grey", colorblind_pal()(2 + 1)[-1]))+
        coord_cartesian(ylim = c(0.0, 0.15))+
        xlab(expression(aCO[2]))
    
    
    p2 <- ggplot(plotDF1[plotDF1$CO2_treatment == "eCO2",]) +
        geom_bar(stat = "identity", 
                 aes(x=Model, y=value, fill = Limiting),
                 position="dodge")+
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
        scale_x_discrete(breaks=c("Bigleaf", "Multilayer"),
                         labels=c("Big leaf", "Multi-layer"))+
        scale_fill_manual(name="Limiting rate",
                          breaks=c("Asens", "Ac_sens", "Aj_sens"),
                          labels=c(expression(A[net]), 
                                   expression(A[c]), 
                                   expression(A[j])),
                          values = c("grey", colorblind_pal()(2 + 1)[-1]))+
        coord_cartesian(ylim = c(0.0, 0.15)) +
        xlab(expression(eCO[2]))
    
    ## pdf
    legend_shared <- get_legend(p1 + theme(legend.position="bottom",
                                           legend.box = 'vertical',
                                           legend.box.just = 'left'))
    
    combined_plots <- plot_grid(p1, p2, 
                                labels=c("(a)", "(b)"),
                                ncol=2, align="vh", axis = "l",
                                label_x=0.8, label_y=0.95,
                                label_size = 18)
    
    
    pdf("output/MAAT/Asens_at_fixed_LAI.pdf", width=6, height=3)
    plot_grid(combined_plots, legend_shared, ncol=1, rel_heights=c(1,0.1))
    dev.off()  
    
    
    
    
    
    ### check effect of LAI
    plotDF2 <- melt(setDT(outDF), id.vars = c("CO2_treatment", "Model", "LAI"), 
                    measure.vars = 10:12, variable.name = "Limiting")
    
    
    ### plot
    p1 <- ggplot(plotDF2[plotDF2$Limiting == "Asens",]) +
        geom_point(aes(x=LAI, y=value, pch=Model, col=CO2_treatment))+
        geom_line(aes(x=LAI, y=value, lty=Model, col=CO2_treatment))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=10),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="bottom",
              legend.box = 'vertical',
              legend.box.just = 'right',
              plot.title = element_text(size=16, face="bold", 
                                        hjust = 0.5),
              legend.text.align = 0)+
        ylab(expression(paste(Delta * A * " / " * A[400])))+
        #scale_pch_manual(breaks=c("Bigleaf", "Multilayer"),
        #                 labels=c("Big leaf", "Multi-layer"))+
        scale_color_manual(name=expression(paste(CO[2] * " treatment")),
                          breaks=c("aCO2", "eCO2"),
                          labels=c(expression(aCO[2]), 
                                   expression(eCO[2])),
                          values = c(colorblind_pal()(5)[-3]))
    
    
    pdf("output/MAAT/Asens_effect_of_LAI.pdf", width=4, height=4)
    plot(p1)
    dev.off()  

}


