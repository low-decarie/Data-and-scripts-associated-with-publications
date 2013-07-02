#Housekeeping
  rm(list=ls())
  library(plyr)
  library(ggplot2)
  library(doMC)
  library(devtools)
  source("./R/growth/Growth curves/00_source_all.R")
  source("./R/publication figures/theme.R")
  registerDoMC(2)
  load(file="./Outputs/compile data.Rdata")


#Selection of type of fit to perform
  do.exp<-F
  do.logistic.nls<-F
  do.logistic.mle<-T
  do.plot<-T
  
  readings<-readings[readings$assayCO2!="low",]


  #Add upper variable for bounding fit
  readings$upper[readings$regime==1]<-2.8
  readings$upper[readings$regime!=1]<-1

#######
# Exponential growth curve fitting
#######

if(do.exp){


#Apply exponential growth to data of the first three days of assay
#(exponential phase)

readings<-join(readings, ddply(.data=readings,
                  .variable="culture",
                  function(x) {exponential.growth.nls(x[x$Time<3,])},
                .progress="text",
                .parallel=T))



}
#######
# Logistic curve fitting using nls
#######

if(do.logistic.nls){

#Apply logistic equation using nls
  
readings<-ddply(.data=readings,
                .variable="culture",
                function(x) {logistic.growth.nls(x, upper=unique(x$upper))}) #,
                #.progress="text",
                #.parallel=T)

}



#######
# Logistic curve fitting using mle
#######



if(do.logistic.mle){



#Apply logistic function using MLE
readings<-ddply(.data=readings,
                .variable="culture",
                function(x) {logistic.growth.mle.norm(x, printer=T, upper=unique(x$upper))})


}


save(readings, file="./Outputs/fitted readings.RData")
write.csv(readings, file="./Outputs/fitted readings.csv")






####
# Plot fit function
###


plot.species<-function(Species, readings, fit="exp"){
  
  
  selected<-readings[readings$Species==Species,]
  
  #Plot of absorbance through time
  
  #Plot the measured values as points
  plot.sp<-qplot(data=selected,
                 x=Time,
                 y=ABS,
                 color=assayCO2,
                 shape=CO2history,
                 linetype=CO2history,
                 alpha=I(0.3),
                 geom=c("point"),
                 group=culture,
                 main=paste("Values for",Species, "fit with", fit))
  
  #Add a vertical line at transfer time (3.5 days)
  #plot.sp<-plot.sp+geom_vline(x=I(3.5), size=I(1))
  
  #Add the fitted values as lines
  plot.sp<-plot.sp+geom_line(aes(y=get(paste(fit,".predicted", sep=""))))
  
  #Customize colour legend
  plot.sp<-plot.sp+scale_colour_manual(values=c("ambient"="blue","high"= "red", "low"= "green"))
  
  #Seperate by regime and transplant
  plot.sp<-plot.sp+facet_grid(regime~transplant, scale="free")
  
  print(plot.sp)
  
  ########################################## 
  #Plot of fitted compared to measured values
#   
#   plot.sp<-qplot(data=selected,
#                  x=ABS,
#                  y=get(paste(fit,".predicted", sep="")),
#                  color=assayCO2,
#                  shape=CO2history,
#                  linetype=CO2history,
#                  main=Species,
#                  alpha=I(0.3),
#                  geom=c("point"),
#                  group=culture,
#                  ylab=paste(fit,".predicted", sep=""))
#   
#   #Add a linear regression
#   plot.sp<-plot.sp+geom_smooth(se=F, method="lm", aes(group=culture))
#   
#   #Customize colour legend
#   plot.sp<-plot.sp+scale_colour_manual(values=c("ambient"="blue","high"= "red", "low"= "green"))
#   
#   #Add a 1:1 line
#   plot.sp<-plot.sp+geom_abline(intercept = 0, slope = 1, size=I(2), alpha=I(0.3))
#   
#   #Seperate by regime and transplant
#   plot.sp<-plot.sp+facet_grid(regime~transplant, scale="free")
#   
#   print(plot.sp)
  
}

  
if(do.plot){
  
#Plot the exp nls fits
  pdf("./Plots/growth/species plots exp fit.pdf", width=20)
  Species.list<-unique(readings$Species)
  for(Species in Species.list){
    print(Species)
    plot.species(Species, readings, fit="exp")}
  dev.off()
  
#Plot logistic nls fits
  pdf("./Plots/growth/species plots nls logistic fit.pdf", width=20)
  Species.list<-unique(readings$Species)
  for(Species in Species.list){
    print(Species)
    plot.species(Species, readings, fit="logistic.nls")}
  dev.off()

#Plot logistic mle fits
pdf("./Plots/growth/species plots mle logistic fit.pdf", width=20)
Species.list<-unique(readings$Species)
for(Species in Species.list){
  print(Species)
  plot.species(Species, readings, fit="logistic.mle")}
dev.off()
}