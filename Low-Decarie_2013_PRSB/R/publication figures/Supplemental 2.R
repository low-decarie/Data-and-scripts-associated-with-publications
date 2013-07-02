rm(list=ls())
library(ggplot2)
library(devtools)
library(plyr)
library(car)
library(gridExtra)
setwd("~/Dropbox/co2_selection")
source("./R/publication figures/theme.R")
load(file="./Outputs/fitted readings.RData")

readings<-readings[readings$assayCO2!="low", ,drop=T]

readings<-droplevels(readings)

#final<-readings

final<-ddply(.data=readings,
             .variables=c("Species", "regime"),
             function(x){ final<-x[x$transplant==max(x$transplant),]
                          return(final)})


#Add regime label
final$regime<-with(final, paste("Regime", regime))

#Split Chlamydomonas and Pseudokirchneriella to next line
final$Species<-as.character(final$Species)
final$Species[final$Species=="Chlamydomonas"]<-"Chlamy-\ndomonas"
final$Species[final$Species=="Pseudokirchneriella"]<-"Pseudo-\nkirchneriella"
final$Species<-as.factor(final$Species)

final$time.transfer[final$regime=="Regime 1"]<-5
final$time.transfer[final$regime!="Regime 1"]<-3.5
  
  #Plot of absorbance through time
 
pdf("./Plots/publication figures/Supplemental 2.pdf",height=3.5, width=7)

  #Plot the measured values as points
  plot.sp<-qplot(data=final,
                 x=Time,
                 y=logistic.mle.predicted,
                 color=assayCO2,
                 shape=CO2history,
                 linetype=CO2history,
                 alpha=I(0.1),
                 geom=c("point"),
                 group=culture,
                 ylab="Absorbance",
                 xlab="Time (days)")
  
  #Add a vertical line at transfer time (3.5 days)
  plot.sp<-plot.sp+geom_vline(aes(xintercept=time.transfer), size=I(1), alpha=I(0.5))
  
  #Add the fitted values as lines
  plot.sp<-plot.sp+geom_line(aes(y=logistic.mle.predicted), alpha=I(0.3))
  #plot.sp<-plot.sp+geom_point(aes(y=final$ABS))
  
  #Customize colour legend
  plot.sp<-plot.sp+scale_colour_manual(values=c("ambient"="blue","high"= "red", "low"= "green"))
  
  #Seperate by regime and transplant
  plot.sp<-plot.sp+facet_grid(regime~Species, scale="free")
  
  print(plot.sp)
  





dev.off()


