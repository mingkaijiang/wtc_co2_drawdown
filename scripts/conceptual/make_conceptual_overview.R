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
    # findCiTransition(fit_15)
    # ret_15<-coef(fit_15)
    
    
    COL<-c("#E41A1C" ,"#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#8DD3C7")
    
    
    # tiff("Figures/Figure_1.tiff", width = 6, height = 3, units = 'in', res = 300)
    
    pdf("output/concept/conceptual_overview.pdf",width = 6, height = 3)
    par(mfrow=c(1,2),oma=c(0,0,0,0),cex.axis=1.5,las=1)
    
    par(mar=c(3,3.5,0.5,0.5))
    # first plot a blank
    plot(10,10,col="white",axes=F,ylim=c(0,40),xlim=c(0,1800),ann=FALSE)
    
    
    # add rate limiting area
    
    rect(xleft=-500,ybottom=-10,xright=findCiTransition(fit_15)[[1]],ytop=50,col=alpha(COL[4],0.3),border=NA)    
    rect(xleft=findCiTransition(fit_15)[[1]],ybottom=-10,xright=findCiTransition(fit_15)[[2]],ytop=50,col=alpha(COL[5],0.3),border=NA)    
    rect(xleft=findCiTransition(fit_15)[[2]],ybottom=-10,xright=2200,ytop=50,col=alpha("lightgray",0.3),border=NA)    
    
    
    # abline(h=min(fit_15$df$Ap)-fit_15$df$Rd,lty=1,col="gray",lwd=2)
    
    # plot TPU limitation
    ablineclip(x1=-5,x2=max(fit_15$df$Ci),h=min(fit_15$df$Ap)-fit_15$df$Rd,lty=1,col="gray",lwd=2)
    
    
    par(new=T)
    plot(fit_15,linecols = c("black",COL[5],COL[4]),lwd=2,legendbty="n",addzeroline = F,
         axes=F,ylim=c(0,40),xlim=c(0,1800),ann=FALSE,pch=21,cex=1.2,bg=alpha("lightgray",0.8),addlegend=F,whichA = c("Ac", "Aj", "Amin"))
    
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
    
    
    # abline(v=280,lty=2)
    # abline(v=385,lty=3)
    
    
    points(fit_15$Ci_transition[[1]],fit_15$Photosyn(Ci=fit_15$Ci_transition[[1]])$ALEAF,pch=21,bg=alpha(COL[4],1),cex=1.5,col="purple")
    
    points(fit_15$Ci_transition2[[1]],fit_15$Photosyn(Ci=fit_15$Ci_transition2[[1]])$ALEAF,pch=21,bg=alpha(COL[5],1),cex=1.5,col="red")
    
    # add legend
    
    par(mar=c(3,0,0,0))
    plot(10,10,col="white",axes=F,ylim=c(0,40),xlim=c(0,1800),ann=FALSE)
    
    legend("bottomleft",legend=c(expression(Measured~A[n]),expression(italic(W[c])~limitation),expression(italic(W[j])~limitation),expression(italic(W[p])~limitation),
                                 expression(Overall~italic(AC[i])~curve),expression(italic(CO[2])~supply~(italic(C[a])==400~mu*mol~mol^-1)),
                                 expression(italic(CO[2])~supply~(italic(C[a])==600~mu*mol~mol^-1)),
                                 expression(italic(C[i-1])),expression(italic(C[i-2]))),lty=c(NA,1,1,1,1,2,3,NA,NA),lwd=2,pt.cex=1.5,cex=0.7,
           pch=c(21,NA,NA,NA,NA,NA,NA,16,16),bty="n",col=c("black",COL[4],COL[5],"lightgray","black","black","black",COL[4],COL[5]),pt.bg="lightgray")
    
    dev.off()
    
    
    #---------------------------------------------------------------------------------------------------------------------------
    #---------------------------------------------------------------------------------------------------------------------------
    
}
