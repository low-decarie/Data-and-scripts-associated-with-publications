#Housekeeping
rm(list=ls())
library(ggplot2)
library(plyr)
library(reshape2)
library(devtools)
library(gridExtra)
setwd("~/Dropbox/co2_selection")
source("./R/publication figures/theme.R")


#Load data
load(file="./Outputs/CO2media.RData")

#Get only final measurements
final<-ddply(.data=CO2,
             .variables=c("period"),
             function(x){ final<-x[x$Time==max(x$Time),]})

#Remove blank and plate measurements
final<-final[final$vessel=="flask",]
final<-final[final$Species!="Blank",]
final<-final[final$period %in% c(1:3, 6),]

#Add regime label
final$regime<-with(final, paste("Regime", regime))



#Split Chlamydomonas and Pseudokirchneriella to next line
final$Species<-as.character(final$Species)
final$Species[final$Species=="Chlamydomonas"]<-"Chlamy-\ndomonas"
final$Species[final$Species=="Pseudokirchneriella"]<-"Pseudo-\nkirchneriella"
final$Species<-as.factor(final$Species)



#Get only used data
final<-final[,names(final) %in% c("assayCO2",
                                            "Species",
                                            "pCO2_calc",
                                            "regime", "Line")]

  plot.sp<-qplot(data=final,
                 x=assayCO2,
                 y=pCO2_calc,
                 colour=assayCO2,
                 ylab=expression(paste("Aqueous ", CO[2], " (ppm)")),
                 xlab=expression(paste(CO[2], " environment")),
                 main="",
                 geom="boxplot")
  
  
  #plot.sp<-plot.sp+geom_boxplot(aes(group=assayCO2))
  
  #Customize colour legend
  plot.sp<-plot.sp+scale_colour_manual(values=c("ambient"="blue",
                                                "high"= "red"))
  
  #Seperate by regime and legend
  plot.sp<-plot.sp+facet_grid(regime~Species, scale="free") 

  plot.sp<-plot.sp+opts(axis.text.x=theme_text(size=12*0.7))

  pdf("./Plots/publication figures/Supplemental 1.pdf", width=7, height=3.5)
  print(plot.sp)
  dev.off()
