rm(list=ls())
library(ggplot2)
library(devtools)
library(plyr)
library(car)
library(gridExtra)
#source("./R/publication figures/theme.R")
source_url("https://raw.github.com/low-decarie/Useful-R-functions/master/ggplot2_themes/theme.R")
theme_set(theme_minimal())
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

#Get a single value for parameters (all identical for a given culture)
final<-final[!duplicated(cbind(final$culture, final$logistic.mle.r)),]



pdf("./Plots/publication figures/Figure 2 for presentations.pdf",height=3.5, width=8)

#Plot the measured values as points
plot.sp<-qplot(data=final,
               x=assayCO2,
               y=logistic.mle.r,
               color=CO2history,
               alpha=I(0.3),
               geom=c("boxplot"),
               ylab="Growth rate (r)",
               xlab="CO2 level in assay environment")


#Customize colour legend
plot.sp<-plot.sp+scale_colour_manual(values=c("ambient"="blue","high"= "red"))

plot.sp<-plot.sp+geom_smooth(method=lm, se=F, aes(linetype=CO2history, group=CO2history))

#Seperate by regime and transplant
plot.sp<-plot.sp+facet_grid(regime~Species, scale="free")

plot.sp<-plot.sp+theme(legend.position="right")

print(plot.sp)






dev.off()

