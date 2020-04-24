plot_eucface_A_Ci <- function() {
    ### read in eucface aci data
    aciDF <- read.csv("data/eucface/Aci.EucFACE.csv")
    
    ### clean data
    aciDF <- aciDF[aciDF$Number!=290,]
    aciDF <- aciDF[aciDF$Number!=293,]
    aciDF <- aciDF[aciDF$Number!=304,]
    aciDF <- aciDF[aciDF$Number!=305,]
    aciDF <- aciDF[aciDF$Number!=322,]
    aciDF <- aciDF[aciDF$Number!=324,]
    aciDF <- aciDF[aciDF$Number!=341,]
    aciDF <- aciDF[aciDF$Number!=385,]
    aciDF <- aciDF[aciDF$Number!=387,]
    aciDF <- aciDF[aciDF$Number!=478,]
    aciDF <- aciDF[aciDF$Number!=647,]
    aciDF <- aciDF[aciDF$Number!=664,]
    aciDF <- aciDF[aciDF$Number!=670,]
    
    ## id list
    id.list <- unique(aciDF$Number)
    
    ### prepare storage DF
    ### prepare an output df
    outDF <- data.frame(id.list, NA, NA, NA, NA, NA, 
                        NA, NA, NA, NA, NA, NA,
                        NA)
    colnames(outDF) <- c("Number", "RMSE", "Vcmax", "Vcmax.se", 
                         "Jmax", "Jmax.se", "Rd", "Rd.se",
                         "Ci_transition_Ac_Aj","curve.fitting", 
                         "GammaStar", "Km", "G1")
    
    
    
    ### the for loop
    for (i in id.list) {
        ## subset each data
        test <- subset(aciDF, Number == i)
        
        ### fit aci curve
        fit1 <- fitaci(test, 
                        fitmethod="bilinear",
                        varnames = list(ALEAF="Photo",
                                        Tleaf="Tleaf", 
                                        Ci = "Ci",
                                        PPFD="PARi"),
                        Tcorrect=T, fitTPU=F)
        
        ### fit g1 value
        fit2 <- fitBB(test, 
                      varnames = list(ALEAF = "Photo", GS = "Cond", VPD = "VpdL",
                                      Ca = "CO2S", RH = "RH_S"),
                      gsmodel="BBOpti")
        
        ## get information on Number
        outDF[outDF$Number == i, "curve.fitting"] <- fit1$fitmethod
        
        ## assign fitted values
        outDF[outDF$Number == i, "RMSE"] <- fit1$RMSE
        outDF[outDF$Number == i, "Vcmax"] <- fit1$pars[1,1]
        outDF[outDF$Number == i, "Vcmax.se"] <- fit1$pars[1,2]
        outDF[outDF$Number == i, "Jmax"] <- fit1$pars[2,1]
        outDF[outDF$Number == i, "Jmax.se"] <- fit1$pars[2,2]
        outDF[outDF$Number == i, "Rd"] <- fit1$pars[3,1]
        outDF[outDF$Number == i, "Rd.se"] <- fit1$pars[3,2]
        outDF[outDF$Number == i, "Ci_transition_Ac_Aj"] <- fit1$Ci_transition
        outDF[outDF$Number == i, "GammaStar"] <- fit1$GammaStar
        outDF[outDF$Number == i, "Km"] <- fit1$Km
        # G1
        outDF[outDF$Number == i, "G1"] <- coef(fit2)[2]
        
    }
    
    outDF$JVratio <- outDF$Jmax / outDF$Vcmax
    
    
    ### add campagin, ring, tree, height information
    idDF <- unique(aciDF[,c("Number", "Tree", "Leaf", "Species", "Ring",
                            "C.treat", "Age", "Height..m.", "Date",
                            "Campaign")])
    
    outDF <- merge(outDF, idDF, by="Number")
    
    ### save output
    write.csv(outDF, "output/eucface/aci_results.csv", row.names=F)

    
    ### fits all
    fits.all <- fitacis(aciDF, group="Number", 
                        fitmethod="bilinear", varnames = list(ALEAF="Photo",
                                                              Tleaf="Tleaf", 
                                                              Ci = "Ci",
                                                              PPFD="PARi"),
                        Tcorrect=T, fitTPU=F)
    
    ### fit g1 value
    fits.bb <- fitBBs(myDF, varnames = list(ALEAF = "Photo", GS = "Cond", VPD = "VpdL",
                                            Ca = "CO2_S", RH = "RH_S"),
                      group="Number")
    
    
    ### create pdf
    pdf("output/eucface/eucface_individual_result.pdf", height=20, width=20)
    par(mfrow=c(5,5))

    ### make plot
    for (i in 1:length(id.list)) {
        plot(fits.all[[i]], main=paste0("Ring ", outDF$Ring[i], ", ", 
                                        outDF$C.treat[i], ", ",
                                        outDF$Age[i], ", ",
                                        outDF$Campaign[i]),
             xlim=c(0, 1600))
        abline(v=c(280, 385), lwd=2, lty=c(1,3))
    }
    
    dev.off()
    
    ### plot summary tables
    outDF$CO2_treatment <- "ambient"
    outDF$CO2_treatment[outDF$Ring%in%c(1,4,5)] <- "elevated"
    sumDF1 <- summaryBy(Vcmax + Jmax +Ci_transition_Ac_Aj + JVratio~ CO2_treatment+Age,
                       data=outDF, FUN = c(mean, se), keep.names=T)
    
    
    
    p1 <- ggplot(sumDF1) +
        geom_errorbar(aes(x=Age, ymin=(Vcmax.mean - Vcmax.se), 
                          ymax = (Vcmax.mean+Vcmax.se), color=as.factor(CO2_treatment)),
                      position=position_dodge(width=0.5),width=0.2)+
        geom_point(aes(Age, Vcmax.mean, fill=as.factor(CO2_treatment)), size = 5, shape=21, 
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
        ylim(50,100)
    

    p2 <- ggplot(sumDF1) +
        geom_errorbar(aes(x=Age, ymin=(Jmax.mean - Jmax.se), 
                          ymax = (Jmax.mean+Jmax.se), color=CO2_treatment),
                      position=position_dodge(width=0.5),width=0.3)+
        geom_point(aes(Age, Jmax.mean, fill=CO2_treatment), size = 5, shape=21, 
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
        ylim(100,200)
    
    p3 <- ggplot(sumDF1) +
        geom_errorbar(aes(x=Age, ymin=(JVratio.mean - JVratio.se), 
                          ymax = (JVratio.mean+JVratio.se), color=CO2_treatment),
                      position=position_dodge(width=0.5),width=0.2)+
        geom_point(aes(Age, JVratio.mean, fill=CO2_treatment), size = 5, shape=21, 
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
        ylim(1.5,2.5)
    
    p4 <- ggplot(sumDF1) +
        geom_errorbar(aes(x=Age, ymin=(Ci_transition_Ac_Aj.mean - Ci_transition_Ac_Aj.se), 
                          ymax = (Ci_transition_Ac_Aj.mean+Ci_transition_Ac_Aj.se), color=CO2_treatment),
                      position=position_dodge(width=0.5),width=0.2)+
        geom_point(aes(Age, Ci_transition_Ac_Aj.mean, fill=CO2_treatment), size = 5, shape=21, 
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
        ylim(250,400)
    
    
    legend_shared <- get_legend(p1 + theme(legend.position="bottom",
                                           legend.box = 'vertical',
                                           legend.box.just = 'left'))
    
    combined_plots <- plot_grid(p1, p2, p3, p4, 
                                labels="AUTO", ncol=2, align="vh", axis = "l")
    
    #plot(p1)
    
    pdf("output/eucface/eucface_leaf_parameters.pdf", 
        width=10, height=12)
    plot_grid(combined_plots, legend_shared, ncol=1, rel_heights=c(1,0.1))
    dev.off()    
    
    
}
