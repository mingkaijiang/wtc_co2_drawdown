plot_chamber_leaf_area <- function(canopyDF) {
    
    
    ### summarize chamber leaf area by chamber and canopy position
    plotDF1 <- summaryBy(Leaf_area~Chamber+Canopy, FUN=mean,
                        data=canopyDF, keep.names=T)
    
    plotDF1$CO2_treatment <- "aCO2"
    plotDF1$CO2_treatment[plotDF1$Chamber%in%c(4,8)] <- "eCO2"
    
    plotDF2 <- summaryBy(Leaf_area~Canopy+CO2_treatment, FUN=c(mean, se),
                         data=plotDF1, keep.names=T)
    
    ### label
    plotDF2$Canopy <- gsub("12345", "5_Full", plotDF2$Canopy)
    plotDF2$Canopy <- gsub("345", "4_TM", plotDF2$Canopy)
    plotDF2$Canopy <- gsub("45", "3_Top", plotDF2$Canopy)
    
    
    
    #### read in biomass DF and process it
    bDF <- biomass_data_processing()
    
    
    
    ### come back to 
    ### 1. investigate why leaf biomass is so large
    ### 2. split the DF to prepare the plotting DFs
    ### 3. plot biomass and SLA. 
    
    
    p1 <- ggplot(data=plotDF2, 
                 aes(Canopy, Leaf_area.mean, group=CO2_treatment)) +
        geom_bar(stat = "identity", aes(fill=Canopy, alpha=CO2_treatment), 
                 position="dodge") +
        geom_errorbar(aes(x=Canopy, ymin=Leaf_area.mean-Leaf_area.se, 
                          ymax=Leaf_area.mean+Leaf_area.se),
                      position=position_dodge(0.9), 
                      width=0.2) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.box = 'vertical',
              legend.box.just = 'left',
              legend.position = c(0.3, 0.8))+
        ylab(expression(paste("Leaf area (" * m^2 * ")")))+
        scale_fill_manual(name="Position",
                          limits=c("5_Full", "4_TM", "3_Top"),
                          values=c("blue2", "red3", "purple"),
                          labels=c("Full", "T+M", "Top"),
                          guide=guide_legend(nrow=1))+
        xlab("")+
        scale_x_discrete(name="", 
                         breaks=c("5_Full", "4_TM", "3_Top"), 
                         labels=c("Full", "T+M", "Top"))+
        ylim(0.0, 60)+
        scale_alpha_manual(name=expression(paste(C[a] * " treatment")),
                           breaks=c("aCO2", "eCO2"),
                           labels=c(expression(aC[a]),
                                    expression(eC[a])),
                           values=c(0.5, 1))+
        guides(fill=FALSE)
    
    
    pdf("output/canopy/canopy_leaf_area.pdf", width=5, height=4)
    plot(p1)
    dev.off()  
    
    
    
}