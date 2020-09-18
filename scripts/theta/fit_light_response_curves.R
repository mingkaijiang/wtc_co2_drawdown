fit_light_response_curves <- function() {
  
  ### script from Jim
  
  # 
  # # get diurnal photosynthesis data
  myDF<-read.csv("data/Light_curves/Light_response_data_all.csv")
  
  ### only include wet treatment
  myDF <- subset(myDF, Water_Trt=="w")
  
  
  # low canopy data (only ambient chambers)
  alDF<-subset(myDF,Ca_Trt=="a" & Position=="low")
  
  # top canopy data (only ambient chambers)
  auDF<-subset(myDF,Ca_Trt=="a" & Position=="up")
  
  # low canopy data (only elevated chambers)
  elDF<-subset(myDF,Ca_Trt=="e" & Position=="low")
  
  # top canopy data (only elevated chambers)
  euDF<-subset(myDF,Ca_Trt=="e" & Position=="up")
  
  # aDF
  aDF<-subset(myDF,Ca_Trt=="a")
  
  # eDF
  eDF<-subset(myDF,Ca_Trt=="e")
  
  
  # the arrhenius function
  arrhenius <- function(k25 = 100, Ea = 60, Rgas = 0.008314, TTK = 293.15) {
    fn <- k25 * exp((Ea*(TTK - 298.15))/(298.15*Rgas*TTK)) 
    return(fn)
  }
  
  # get theta and alpha from LRC######
  lrc.df <- alDF
  
  lrc.df$gammas <- arrhenius(42.75,37830.0/1000, TTK =c(lrc.df$Tleaf + 273.15))
  lrc.df$apar <- lrc.df$PARi * (1 - 0.093 - 0.082)
  lrc.df.low.par <- lrc.df[lrc.df$PARi < 100,]
  fit.a.lrc <- lm(Photo~apar,
                  data = lrc.df.low.par)
  alpha.a <- coef(summary(fit.a.lrc))[2]
  
  alpha.j <- mean(4*alpha.a*(lrc.df.low.par$Ci + 2*lrc.df.low.par$gammas)/
                    (lrc.df.low.par$Ci - lrc.df.low.par$gammas))
  
  result.lrc <- coef(nls(Photo~
                            alpha.a * apar+ Pm - ((alpha.a * apar+ Pm)^2-4*alpha.a * apar*Pm*theta)^0.5/2/theta,
                          data=lrc.df,start = list(Pm = 30, theta = 0.5)))
  
  result.lrc$alpha.j <- alpha.j
  
  result.lrc1 <- as.data.frame(result.lrc)
  
  
  # get theta and alpha from LRC######
  lrc.df <- auDF
  
  lrc.df$gammas <- arrhenius(42.75,37830.0/1000, TTK =c(lrc.df$Tleaf + 273.15))
  lrc.df$apar <- lrc.df$PARi * (1 - 0.093 - 0.082)
  lrc.df.low.par <- lrc.df[lrc.df$PARi < 100,]
  fit.a.lrc <- lm(Photo~apar,
                  data = lrc.df.low.par)
  alpha.a <- coef(summary(fit.a.lrc))[2]
  
  alpha.j <- mean(4*alpha.a*(lrc.df.low.par$Ci + 2*lrc.df.low.par$gammas)/
                    (lrc.df.low.par$Ci - lrc.df.low.par$gammas))
  
  result.lrc <- coef(nls(Photo~
                           alpha.a * apar+ Pm - ((alpha.a * apar+ Pm)^2-4*alpha.a * apar*Pm*theta)^0.5/2/theta,
                         data=lrc.df,start = list(Pm = 30, theta = 0.5)))
  
  result.lrc$alpha.j <- alpha.j
  
  result.lrc2 <- as.data.frame(result.lrc)
  
  # get theta and alpha from LRC######
  lrc.df <- elDF
  
  lrc.df$gammas <- arrhenius(42.75,37830.0/1000, TTK =c(lrc.df$Tleaf + 273.15))
  lrc.df$apar <- lrc.df$PARi * (1 - 0.093 - 0.082)
  lrc.df.low.par <- lrc.df[lrc.df$PARi < 100,]
  fit.a.lrc <- lm(Photo~apar,
                  data = lrc.df.low.par)
  alpha.a <- coef(summary(fit.a.lrc))[2]
  
  alpha.j <- mean(4*alpha.a*(lrc.df.low.par$Ci + 2*lrc.df.low.par$gammas)/
                    (lrc.df.low.par$Ci - lrc.df.low.par$gammas))
  
  result.lrc <- coef(nls(Photo~
                           alpha.a * apar+ Pm - ((alpha.a * apar+ Pm)^2-4*alpha.a * apar*Pm*theta)^0.5/2/theta,
                         data=lrc.df,start = list(Pm = 30, theta = 0.5)))
  
  result.lrc$alpha.j <- alpha.j
  
  result.lrc3 <- as.data.frame(result.lrc)
  
  # get theta and alpha from LRC######
  lrc.df <- euDF
  
  lrc.df$gammas <- arrhenius(42.75,37830.0/1000, TTK =c(lrc.df$Tleaf + 273.15))
  lrc.df$apar <- lrc.df$PARi * (1 - 0.093 - 0.082)
  lrc.df.low.par <- lrc.df[lrc.df$PARi < 100,]
  fit.a.lrc <- lm(Photo~apar,
                  data = lrc.df.low.par)
  alpha.a <- coef(summary(fit.a.lrc))[2]
  
  alpha.j <- mean(4*alpha.a*(lrc.df.low.par$Ci + 2*lrc.df.low.par$gammas)/
                    (lrc.df.low.par$Ci - lrc.df.low.par$gammas))
  
  result.lrc <- coef(nls(Photo~
                           alpha.a * apar+ Pm - ((alpha.a * apar+ Pm)^2-4*alpha.a * apar*Pm*theta)^0.5/2/theta,
                         data=lrc.df,start = list(Pm = 30, theta = 0.5)))
  
  result.lrc$alpha.j <- alpha.j
  
  result.lrc4 <- as.data.frame(result.lrc)
  
  # get theta and alpha from LRC######
  lrc.df <- aDF
  
  lrc.df$gammas <- arrhenius(42.75,37830.0/1000, TTK =c(lrc.df$Tleaf + 273.15))
  lrc.df$apar <- lrc.df$PARi * (1 - 0.093 - 0.082)
  lrc.df.low.par <- lrc.df[lrc.df$PARi < 100,]
  fit.a.lrc <- lm(Photo~apar,
                  data = lrc.df.low.par)
  alpha.a <- coef(summary(fit.a.lrc))[2]
  
  alpha.j <- mean(4*alpha.a*(lrc.df.low.par$Ci + 2*lrc.df.low.par$gammas)/
                    (lrc.df.low.par$Ci - lrc.df.low.par$gammas))
  
  result.lrc <- coef(nls(Photo~
                           alpha.a * apar+ Pm - ((alpha.a * apar+ Pm)^2-4*alpha.a * apar*Pm*theta)^0.5/2/theta,
                         data=lrc.df,start = list(Pm = 30, theta = 0.5)))
  
  result.lrc$alpha.j <- alpha.j
  
  result.lrc5 <- as.data.frame(result.lrc)
  
  # get theta and alpha from LRC######
  lrc.df <- eDF
  
  lrc.df$gammas <- arrhenius(42.75,37830.0/1000, TTK =c(lrc.df$Tleaf + 273.15))
  lrc.df$apar <- lrc.df$PARi * (1 - 0.093 - 0.082)
  lrc.df.low.par <- lrc.df[lrc.df$PARi < 100,]
  fit.a.lrc <- lm(Photo~apar,
                  data = lrc.df.low.par)
  alpha.a <- coef(summary(fit.a.lrc))[2]
  
  alpha.j <- mean(4*alpha.a*(lrc.df.low.par$Ci + 2*lrc.df.low.par$gammas)/
                    (lrc.df.low.par$Ci - lrc.df.low.par$gammas))
  
  result.lrc <- coef(nls(Photo~
                           alpha.a * apar+ Pm - ((alpha.a * apar+ Pm)^2-4*alpha.a * apar*Pm*theta)^0.5/2/theta,
                         data=lrc.df,start = list(Pm = 30, theta = 0.5)))
  
  result.lrc$alpha.j <- alpha.j
  
  result.lrc6 <- as.data.frame(result.lrc)
  
  ### merge
  outDF <- rbind(result.lrc1, result.lrc2, result.lrc3, result.lrc4, result.lrc5, result.lrc6)
  outDF$Ca_Trt <- c("a", "a", "e", "e", "a", "e")
  outDF$Position <- c("low", "up", "low", "up", "all", "all")
  
  
  write.csv(outDF, "output/leaf/leaf_scale_alpha_and_theta.csv", row.names=F)

  
}



