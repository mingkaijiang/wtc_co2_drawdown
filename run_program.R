#### This script is used to process WCT1 CO2 drawdown experiment data
###
### author: Mingkai Jiang
###         m.jiang@westernsydney.edu.au
###

#### read in necessary stuffs
source("prepare.R")

#### read in raw data
myDF <- read.table("data/mergeall.txt", sep=",", header=T)

### set up dataset
myDF$canopy <- as.character(myDF$canopy)
myDF$datetime <- as.POSIXct(as.character(myDF$datetime))

### extract time information
myDF$time <- strftime(myDF$datetime, format="%H:%M:%S")
myDF$time <- as.POSIXct(myDF$time, format="%H:%M:%S")
myDF$date <- strftime(myDF$datetime, format="%Y-%m-%d")


### data explained
### column canopy: 12345 - full canopy
###                345 - middle canopy
###                45 - lower canopy
###                0 - no canopy

### need to correct for different sizes of trees

### plot overall data - CO2 concentration over time
p1 <- ggplot(myDF, aes(datetime))+
    geom_point(aes(y=CO2Local, shape=factor(canopy), color=factor(chamber)), size=2.0)+
    labs(x="Date Time", y=expression(paste(CO[2], " concentration (ppm)")))+
    theme_linedraw() +
    theme(panel.grid.minor=element_blank(),
          axis.title.x = element_text(size=14), 
          axis.text.x = element_text(size=12),
          axis.text.y=element_text(size=12),
          axis.title.y=element_text(size=14),
          legend.text=element_text(size=12),
          legend.title=element_text(size=14),
          panel.grid.major=element_blank(),
          legend.position="right")+
    scale_colour_manual(name="Chamber", values = c("1" = "brown", "2" = "red", "3" = "pink",
                                                   "4" = "orange", "7" = "yellow", "8" = "green",
                                                   "11" = "cyan", "12" = "blue"))+
    scale_shape_manual(name="Canopy", values = c("12345" = 16, "345" = 17, "45" = 15),
                       labels=c("full", "top + middle", "top"))+
    scale_x_datetime(labels = date_format("%b %d %H:%M:%S"))

### CO2 against leaf area
p2 <- ggplot(myDF, aes(SumOfarea_fully_exp))+
    geom_point(aes(y=CO2Local, shape=factor(canopy), color=factor(chamber)), size=2.0)+
    labs(x="Sum of leaf area", y=expression(paste(CO[2], " concentration (ppm)")))+
    theme_linedraw() +
    theme(panel.grid.minor=element_blank(),
          axis.title.x = element_text(size=14), 
          axis.text.x = element_text(size=12),
          axis.text.y=element_text(size=12),
          axis.title.y=element_text(size=14),
          legend.text=element_text(size=12),
          legend.title=element_text(size=14),
          panel.grid.major=element_blank(),
          legend.position="right")+
    scale_colour_manual(name="Chamber", values = c("1" = "brown", "2" = "red", "3" = "pink",
                                                   "4" = "orange", "7" = "yellow", "8" = "green",
                                                   "11" = "cyan", "12" = "blue"))+
    scale_shape_manual(name="Canopy", values = c("12345" = 16, "345" = 17, "45" = 15),
                       labels=c("full", "top + middle", "top"))

### plot all responses onto the same day
p3 <- ggplot(myDF, aes(time))+
    geom_point(aes(y=CO2Local, shape=factor(canopy), color=factor(chamber)), size=2.0)+
    labs(x="Time", y=expression(paste(CO[2], " concentration (ppm)")))+
    theme_linedraw() +
    theme(panel.grid.minor=element_blank(),
          axis.title.x = element_text(size=14), 
          axis.text.x = element_text(size=12),
          axis.text.y=element_text(size=12),
          axis.title.y=element_text(size=14),
          legend.text=element_text(size=12),
          legend.title=element_text(size=14),
          panel.grid.major=element_blank(),
          legend.position="right")+
    scale_colour_manual(name="Chamber", values = c("1" = "brown", "2" = "red", "3" = "pink",
                                                   "4" = "orange", "7" = "yellow", "8" = "green",
                                                   "11" = "cyan", "12" = "blue"))+
    scale_shape_manual(name="Canopy", values = c("12345" = 16, "345" = 17, "45" = 15),
                       labels=c("full", "top + middle", "top"))+
    scale_x_datetime(limits = as.POSIXct(c('2019-01-23 07:00:00','2019-01-23 16:59:59')))


### CO2 concentration agains slope
p4 <- ggplot(myDF, aes(CO2Local))+
    geom_point(aes(y=slope, shape=factor(canopy), color=factor(chamber)), size=2.0)+
    labs(x=expression(paste(CO[2], " concentration (ppm)")),
         y=expression(paste(Delta, CO[2], " slope")))+
    theme_linedraw() +
    theme(panel.grid.minor=element_blank(),
          axis.title.x = element_text(size=14), 
          axis.text.x = element_text(size=12),
          axis.text.y=element_text(size=12),
          axis.title.y=element_text(size=14),
          legend.text=element_text(size=12),
          legend.title=element_text(size=14),
          panel.grid.major=element_blank(),
          legend.position="right")+
    scale_colour_manual(name="Chamber", values = c("1" = "brown", "2" = "red", "3" = "pink",
                                                   "4" = "orange", "7" = "yellow", "8" = "green",
                                                   "11" = "cyan", "12" = "blue"))+
    scale_shape_manual(name="Canopy", values = c("12345" = 16, "345" = 17, "45" = 15),
                       labels=c("full", "top + middle", "top"))

### output
pdf("output/overall_data.pdf", width=12, height=6)
plot(p1)
plot(p2)
plot(p3)
plot(p4)
dev.off()


### Calculate CO2 flux for each minute and output in the unit of ppm CO2 s-1
myDF2 <- calculate_co2_flux_per_second(myDF)


### plot CO2 flux over CO2 concentration
p1 <- ggplot(myDF2, aes(CO2Local))+
    geom_point(aes(y=co2_flux, shape=factor(canopy), color=factor(chamber)), size=2.0)+
    labs(x=expression(paste(CO[2], " concentration (ppm)")),
         y=expression(paste(CO[2], " flux (ppm ", CO[2], " ", s^-1, ")")))+
    theme_linedraw() +
    theme(panel.grid.minor=element_blank(),
          axis.title.x = element_text(size=14), 
          axis.text.x = element_text(size=12),
          axis.text.y=element_text(size=12),
          axis.title.y=element_text(size=14),
          legend.text=element_text(size=12),
          legend.title=element_text(size=14),
          panel.grid.major=element_blank(),
          legend.position="right")+
    scale_colour_manual(name="Chamber", values = c("1" = "brown", "2" = "red", "3" = "pink",
                                                   "4" = "orange", "7" = "yellow", "8" = "green",
                                                   "11" = "cyan", "12" = "blue"))+
    scale_shape_manual(name="Canopy", values = c("12345" = 16, "345" = 17, "45" = 15),
                       labels=c("full", "top + middle", "top"))


### plot CO2 flux over time elapsed
p2 <- ggplot(myDF2, aes(time_elapsed))+
    geom_point(aes(y=co2_flux, shape=factor(canopy), color=factor(chamber)), size=2.0)+
    labs(x="Time Elapsed (s)", y=expression(paste(CO[2], " flux (ppm ", CO[2], " ", s^-1, ")")))+
    theme_linedraw() +
    theme(panel.grid.minor=element_blank(),
          axis.title.x = element_text(size=14), 
          axis.text.x = element_text(size=12),
          axis.text.y=element_text(size=12),
          axis.title.y=element_text(size=14),
          legend.text=element_text(size=12),
          legend.title=element_text(size=14),
          panel.grid.major=element_blank(),
          legend.position="right")+
    scale_colour_manual(name="Chamber", values = c("1" = "brown", "2" = "red", "3" = "pink",
                                                   "4" = "orange", "7" = "yellow", "8" = "green",
                                                   "11" = "cyan", "12" = "blue"))+
    scale_shape_manual(name="Canopy", values = c("12345" = 16, "345" = 17, "45" = 15),
                       labels=c("full", "top + middle", "top"))


### chamber 1 example
p3 <- ggplot(myDF2[myDF2$chamber==1,], aes(x=time_elapsed, y=CO2Local, color=factor(canopy)))+
    geom_point(size=2.0)+
    geom_smooth(se=TRUE, method="gam", formula = y~s(x))+
    labs(x="Time", y=expression(paste(CO[2], " concentration (ppm)")))+
    theme_linedraw() +
    theme(panel.grid.minor=element_blank(),
          axis.title.x = element_text(size=14), 
          axis.text.x = element_text(size=12),
          axis.text.y=element_text(size=12),
          axis.title.y=element_text(size=14),
          legend.text=element_text(size=12),
          legend.title=element_text(size=14),
          panel.grid.major=element_blank(),
          legend.position="right")+
    scale_colour_manual(name="Canopy", values = c("12345" = "red", "345" = "green",
                                                    "45" = "blue"),
                        labels=c("full", "top + middle", "top"))+
    ggtitle("Chamber 1 example")

### chamber 2 example
p4 <- ggplot(myDF2[myDF2$chamber==2,], aes(x=time_elapsed, y=CO2Local, color=factor(canopy)))+
    geom_point(size=2.0)+
    geom_smooth(se=TRUE, method="gam", formula = y~s(x))+
    labs(x="Time", y=expression(paste(CO[2], " concentration (ppm)")))+
    theme_linedraw() +
    theme(panel.grid.minor=element_blank(),
          axis.title.x = element_text(size=14), 
          axis.text.x = element_text(size=12),
          axis.text.y=element_text(size=12),
          axis.title.y=element_text(size=14),
          legend.text=element_text(size=12),
          legend.title=element_text(size=14),
          panel.grid.major=element_blank(),
          legend.position="right")+
    scale_colour_manual(name="Canopy", values = c("12345" = "red", "345" = "green",
                                                  "45" = "blue"),
                        labels=c("full", "top + middle", "top"))+
    ggtitle("Chamber 2 example")

### chamber 3 example
p5 <- ggplot(myDF2[myDF2$chamber==3,], aes(x=time_elapsed, y=CO2Local, color=factor(canopy)))+
    geom_point(size=2.0)+
    geom_smooth(se=TRUE, method="gam", formula = y~s(x))+
    labs(x="Time", y=expression(paste(CO[2], " concentration (ppm)")))+
    theme_linedraw() +
    theme(panel.grid.minor=element_blank(),
          axis.title.x = element_text(size=14), 
          axis.text.x = element_text(size=12),
          axis.text.y=element_text(size=12),
          axis.title.y=element_text(size=14),
          legend.text=element_text(size=12),
          legend.title=element_text(size=14),
          panel.grid.major=element_blank(),
          legend.position="right")+
    scale_colour_manual(name="Canopy", values = c("12345" = "red", "345" = "green",
                                                  "45" = "blue"),
                        labels=c("full", "top + middle", "top"))+
    ggtitle("Chamber 3 example")


### chamber 4 example
p6 <- ggplot(myDF2[myDF2$chamber==4,], aes(x=time_elapsed, y=CO2Local, color=factor(canopy)))+
    geom_point(size=2.0)+
    geom_smooth(se=TRUE, method="gam", formula = y~s(x))+
    labs(x="Time", y=expression(paste(CO[2], " concentration (ppm)")))+
    theme_linedraw() +
    theme(panel.grid.minor=element_blank(),
          axis.title.x = element_text(size=14), 
          axis.text.x = element_text(size=12),
          axis.text.y=element_text(size=12),
          axis.title.y=element_text(size=14),
          legend.text=element_text(size=12),
          legend.title=element_text(size=14),
          panel.grid.major=element_blank(),
          legend.position="right")+
    scale_colour_manual(name="Canopy", values = c("12345" = "red", "345" = "green",
                                                  "45" = "blue"),
                        labels=c("full", "top + middle", "top"))+
    ggtitle("Chamber 4 example")

### chamber 7 example
p7 <- ggplot(myDF2[myDF2$chamber==7,], aes(x=time_elapsed, y=CO2Local, color=factor(canopy)))+
    geom_point(size=2.0)+
    geom_smooth(se=TRUE, method="gam", formula = y~s(x))+
    labs(x="Time", y=expression(paste(CO[2], " concentration (ppm)")))+
    theme_linedraw() +
    theme(panel.grid.minor=element_blank(),
          axis.title.x = element_text(size=14), 
          axis.text.x = element_text(size=12),
          axis.text.y=element_text(size=12),
          axis.title.y=element_text(size=14),
          legend.text=element_text(size=12),
          legend.title=element_text(size=14),
          panel.grid.major=element_blank(),
          legend.position="right")+
    scale_colour_manual(name="Canopy", values = c("12345" = "red", "345" = "green",
                                                  "45" = "blue"),
                        labels=c("full", "top + middle", "top"))+
    ggtitle("Chamber 7 example")

### chamber 8 example
p8 <- ggplot(myDF2[myDF2$chamber==8,], aes(x=time_elapsed, y=CO2Local, color=factor(canopy)))+
    geom_point(size=2.0)+
    geom_smooth(se=TRUE, method="gam", formula = y~s(x))+
    labs(x="Time", y=expression(paste(CO[2], " concentration (ppm)")))+
    theme_linedraw() +
    theme(panel.grid.minor=element_blank(),
          axis.title.x = element_text(size=14), 
          axis.text.x = element_text(size=12),
          axis.text.y=element_text(size=12),
          axis.title.y=element_text(size=14),
          legend.text=element_text(size=12),
          legend.title=element_text(size=14),
          panel.grid.major=element_blank(),
          legend.position="right")+
    scale_colour_manual(name="Canopy", values = c("12345" = "red", "345" = "green",
                                                  "45" = "blue"),
                        labels=c("full", "top + middle", "top"))+
    ggtitle("Chamber 8 example")

### chamber 11 example
p9 <- ggplot(myDF2[myDF2$chamber==11,], aes(x=time_elapsed, y=CO2Local, color=factor(canopy)))+
    geom_point(size=2.0)+
    geom_smooth(se=TRUE, method="gam", formula = y~s(x))+
    labs(x="Time", y=expression(paste(CO[2], " concentration (ppm)")))+
    theme_linedraw() +
    theme(panel.grid.minor=element_blank(),
          axis.title.x = element_text(size=14), 
          axis.text.x = element_text(size=12),
          axis.text.y=element_text(size=12),
          axis.title.y=element_text(size=14),
          legend.text=element_text(size=12),
          legend.title=element_text(size=14),
          panel.grid.major=element_blank(),
          legend.position="right")+
    scale_colour_manual(name="Canopy", values = c("12345" = "red", "345" = "green",
                                                  "45" = "blue"),
                        labels=c("full", "top + middle", "top"))+
    ggtitle("Chamber 11 example")

### chamber 12 example
p10 <- ggplot(myDF2[myDF2$chamber==12,], aes(x=time_elapsed, y=CO2Local, color=factor(canopy)))+
    geom_point(size=2.0)+
    geom_smooth(se=TRUE, method="gam", formula = y~s(x))+
    labs(x="Time", y=expression(paste(CO[2], " concentration (ppm)")))+
    theme_linedraw() +
    theme(panel.grid.minor=element_blank(),
          axis.title.x = element_text(size=14), 
          axis.text.x = element_text(size=12),
          axis.text.y=element_text(size=12),
          axis.title.y=element_text(size=14),
          legend.text=element_text(size=12),
          legend.title=element_text(size=14),
          panel.grid.major=element_blank(),
          legend.position="right")+
    scale_colour_manual(name="Canopy", values = c("12345" = "red", "345" = "green",
                                                  "45" = "blue"),
                        labels=c("full", "top + middle", "top"))+
    ggtitle("Chamber 12 example")


### output
pdf("output/overall_data_fluxes.pdf", width=12, height=6)
plot(p1)
plot(p2)
plot(p3)
plot(p4)
plot(p5)
plot(p6)
plot(p7)
plot(p8)
plot(p9)
plot(p10)
dev.off()


### look at chambers where CO2 concentration is unstable
test <- subset(myDF2, chamber==7)

### plot time series data to confirm status of CO2 concentration over time
with(test[test$canopy=="45",], plot(CO2Local~datetime))
with(test[test$canopy=="45",], plot(CO2Local~time))
with(test[test$canopy=="45",], plot(CO2Local~time_elapsed))

### to do next
### work on individual regression models to see if the slopes differ statistically
### if not, then one equation can be used to represent CO2 drawdown and therefore can use it to contrast with leaf-scale measurements