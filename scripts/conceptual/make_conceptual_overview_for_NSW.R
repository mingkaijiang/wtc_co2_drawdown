make_conceptual_overview_for_NSW <- function() {
    
    #---------------------------------------------------------------------------------------------------------------------------
    #---------------------------------------------------------------------------------------------------------------------------
    ### code from Dushan
    
    
    # read data 
    aci_data<-read.csv("data/concept/PPCGlob_V1.1.csv", stringsAsFactors=FALSE, fileEncoding="latin1")
    
    
    # get a ACi curve
    curve_1<-subset(aci_data,aci_data$Dataset=="Eucalyptus saligna, AU-NSW" & aci_data$Curve==1380) #38
    
    
    #fit ACi curve
    fit_15<-fitaci(curve_1,fitTPU = T)
    
    plotDF <- fit_15$df
    
    
    
    ### pdf
    pdf("output/concept/conceptual_overview_for_NSW_proposal.pdf",
        width = 5, height = 5)
    
    plot(fit_15,linecols = c("black"),lwd=2,
         legendbty="n",
         addzeroline = F,axes=F, 
         ylim=c(0,30),xlim=c(0,1800),
         ann=FALSE,pch=19,cex=0.0001,
         bg=alpha("lightgray",0.5),
         col=alpha("white",alpha=1),
         addlegend=F,whichA = c("Amin"))
    
    require(magicaxis)
    magaxis(side=c(1,2,4),labels=c(1,1,0),frame.plot=T,las=1,cex.axis=1.1,ratio=0.4,tcl=0.2)
    
    title(ylab=expression("Leaf C assimi. " ~(mu*mol~m^-2~s^-1)),cex.lab=1.2,line=2)
    title(xlab=expression("Atmospheric " * CO[2]~(mu*mol~mol^-1)),cex.lab=1.2,line=2)
    
    #----------------------------------------------------------
    #add supply line assuming Ci:Ca of 0.7
    
    
    # CO2 concentration
    
    
    abline(v=260,lty=2, col="black")
    abline(v=410,lty=1, col="orange")
    abline(v=560,lty=3, col="red")
    
    
    
    legend("bottomright",
           legend=c(expression("Photo. response curve"),
                    expression(italic(C[a])==260~mu*mol~mol^-1),
                    expression(italic(C[a])==410~mu*mol~mol^-1),
                    expression(italic(C[a])==560~mu*mol~mol^-1)),
           lty=c(1,2,1,3),lwd=2,pt.cex=1.5,cex=1.,
           pch=c(NA,NA),
           bty="n",col=c("black","black","orange","red"),
           pt.bg=c(NA, NA))
    
    dev.off()
    
    
    #---------------------------------------------------------------------------------------------------------------------------
    #---------------------------------------------------------------------------------------------------------------------------
    
}
