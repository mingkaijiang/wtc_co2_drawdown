make_conceptual_overview <- function() {
    
    #---------------------------------------------------------------------------------------------------------------------------
    #---------------------------------------------------------------------------------------------------------------------------
    ### code from Dushan
    
    
    # read data 
    aci_data<-read.csv("data/concept/PPCGlob_V1.1.csv", stringsAsFactors=FALSE, fileEncoding="latin1")
    
    
    # get a ACi curve
    curve_1<-subset(aci_data,aci_data$Dataset=="Eucalyptus parramattensis, AU-NSW" & aci_data$Curve==1260) #38
    
    
    #fit ACi curve
    fit_15<-fitaci(curve_1,fitTPU = T)
    
    plotDF <- fit_15$df
    
    ## color blind friendly
    COL<-c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

    
    ### pdf
    pdf("output/concept/conceptual_overview.pdf",width = 5, height = 5)
    
    plot(fit_15,linecols = c("black",COL[2],COL[7]),lwd=2,legendbty="n",
         addzeroline = F,axes=F,
         ylim=c(0,40),xlim=c(0,1800),
         ann=FALSE,pch=21,cex=1.2,
         bg=alpha("lightgray",0.8),addlegend=F,whichA = c("Ac", "Aj", "Amin"))
    
    require(magicaxis)
    magaxis(side=c(1,2,4),labels=c(1,1,0),frame.plot=T,las=1,cex.axis=1.1,ratio=0.4,tcl=0.2)
    
    title(ylab=expression(A[n]~(mu*mol~m^-2~s^-1)),cex.lab=1.2,line=2)
    title(xlab=expression(C[i]~(mu*mol~mol^-1)),cex.lab=1.2,line=2)
    
    #----------------------------------------------------------
    #add supply line assuming Ci:Ca of 0.7
    # ambient CO2
    
    Ca <- 400 # ambient CO2
    gctogw <- 1.57 # conversion
    
    # Provide  Ci as input, gives intersection pointat Ci=Ca*0.7
    
    g <- Photosyn(Ca=Ca,Ci=Ca*.7)
    gc <- g[[3]] / gctogw # stomatal conductance to CO2
    
    # supply curve
    abline(gc * Ca, -gc, lty=2)
    
    # elevated CO2
    
    Ca <- 600
    g <- Photosyn(Ca=Ca,Ci=Ca*.7)
    gc <- g[[3]] / gctogw # stomatal conductance to CO2
    
    abline(gc * Ca, -gc, lty=3)
    
    
    points(fit_15$Ci_transition[[1]],fit_15$Photosyn(Ci=fit_15$Ci_transition[[1]])$ALEAF,pch=21,
           bg=alpha(COL[4],1),cex=1.5,col="black")
    
    #points(fit_15$Ci_transition2[[1]],fit_15$Photosyn(Ci=fit_15$Ci_transition2[[1]])$ALEAF,pch=21,
    #       bg=alpha(COL[5],1),cex=1.5,col="red")
    

    legend("bottomright",
           legend=c(expression(Measured~A[n]),
                    expression(italic(A[c])~limitation),
                    expression(italic(A[j])~limitation),
                    expression(Overall~italic(AC[i])~curve),
                    expression(italic(CO[2])~supply~(italic(C[a])==400~mu*mol~mol^-1)),
                    expression(italic(CO[2])~supply~(italic(C[a])==600~mu*mol~mol^-1)),
                    expression(Transitional~C[i])),
           lty=c(NA,1,1,1,2,3,NA),lwd=2,pt.cex=1.5,cex=0.7,
           pch=c(21,NA,NA,NA,NA,NA,21),
           bty="n",col=c("black",COL[7],COL[2],
                         "black","black","black","black"),
           pt.bg=c("lightgray", NA, NA, NA, NA, NA, COL[4]))
    
    dev.off()
    
    
    #---------------------------------------------------------------------------------------------------------------------------
    #---------------------------------------------------------------------------------------------------------------------------
    
}
