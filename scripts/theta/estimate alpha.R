compute_theta_and_alpha_based_on_WTC3 <- function() {
  
  # 
  # # get diurnal photosynthesis data
  
  avt_long<-read.csv("data/WTC_TEMP_CM_GX-DIURNAL_20130710-20140220_L1_v2.csv")
  
  
  # low canopy data (only ambient chambers)
  ambient_low<-subset(avt_long,avt_long$T_treatment=="ambient" & avt_long$position=="low")
  
  # top canopy data (only ambient chambers)
  ambient_top<-subset(avt_long,avt_long$T_treatment=="ambient" & avt_long$position=="top")
  
  
  # function calling Photosyn
  pslow <- function(alpha,theta,PAR,Tleaf,Vcmax25,Jmax25,EaV,EaJ,delsV,delsJ,EdVC=NULL,Rd0=NULL,q10=NULL,TrefR=NULL,Rdayfrac=NULL,g1,gsmodel=NULL,
                    VPD,CO2S) {
    
    ps <- Photosyn(alpha=alpha,theta=theta,PPFD=PAR,Tleaf=Tleaf,Vcmax=Vcmax25,Jmax=Jmax25,EaV=EaV*10^3,EaJ=EaJ*10^3,delsC=delsV*10^3,delsJ=delsJ*10^3,EdVC=2e+05,
                   Rd0=0.68,Q10=2.1,TrefR=22,Rdayfrac=0.7,g1=g1,gsmodel="BBOpti",VPD=VPD,Ca=CO2S)
    ret <- ps$Aj - ps$Rd
    return(ret)
  }
  
  # set vcmax at 25C
  Vcmax25<-105.2 # summer (mumolm-2s-1)
  
  # set jmax at 25C
  Jmax25<-193.8
  
  #------------------------------------
  # Day respiration parameters
  #------------------------------------
  
  # Parameter values based on Kristine Crouse (pers.comm)
  # Data measured at the latter part of the experiment (~April-May 2014)
  # No data to constrain seasonal acclimation. So assumed a fixed rate throughout
  
  # this set to zero as Kashif seperately estimates daily Rdark 
  
  # Basal rate of Rday at 25C
  Rday<-0.8 #(mumolm-2s-1)
  Q10<- .1 # (Q10=2.8; log(2.8)/10)
  RdayFrac<-1 # Fractional reduction in Rdark in light
  
  
  #----------------------------------------
  # Temperature response of Vcmax and Jmax
  #----------------------------------------
  
  # These parameters were not significantly different between seasons.So used fixed rates throughout
  
  EaV<-51.9 #(Jmol-1)
  delsV<-.633 #(Jmol-1K-1)
  
  EaJ<-23.200 #(kJmol-1)
  delsJ<-.635 #(Jmol-1K-1)
  
  
  #--------------------------------------------------------------
  # parameters of the Medlyn Stomatal Conductance model
  #--------------------------------------------------------------
  
  g1_a=2.43 # (Aspinwall et al; diurnal data across seasons)
  
  #--------------------------------------------------------------------------------------------------------------------------
  #--------------------------------------------------------------------------------------------------------------------------
  
  # call to NLS
  
  # find best fitted alpha and theta for low canopy
  fitalpha_low <- nls(Photo ~ pslow(alpha,theta,PAR=PARi, 
                                    Tleaf=Tleaf,Vcmax=Vcmax25,Jmax=Jmax25,
                                    EaV=EaV,EaJ=EaJ,delsV=delsV,delsJ=delsJ,g1=g1_a,
                                    VPD=VpdL,CO2S=CO2S),
                      start=list(alpha=0.3,theta=0.1), data=ambient_low, trace=TRUE)
  
  
  # find best fitted alpha and theta for top canopy (<1400 par data to remove few unusual data points)
  fitalpha_top<- nls(Photo ~ pslow(alpha,theta,PAR=PARi, 
                                   Tleaf=Tleaf,Vcmax=Vcmax25,Jmax=Jmax25,
                                   EaV=EaV,EaJ=EaJ,delsV=delsV,delsJ=delsJ,g1=g1_a,
                                   VPD=VpdL,CO2S=CO2S),
                     start=list(alpha=0.3,theta=0.5), 
                     data=subset(ambient_top,ambient_top$PARi<1400), trace=TRUE)
  
  
  
  coef(fitalpha_low)
  
  coef(fitalpha_top) # do not considet the negetive sign. Just take the absolute number as it has no units
  
  
  
}



