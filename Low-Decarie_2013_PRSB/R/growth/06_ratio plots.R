#Housekeeping
  rm(list=ls())
  library(ggplot2)
  library(devtools)
  library(plyr)
#  source_url("https://raw.github.com/edielivon/Useful-R-functions/master/ggplot2_themes/theme_minimal.R")
  load(file="./Outputs/fitted readings.RData")
  source("./R/publication figures/theme.R")
  
#List of parameters of interest
para.list<-c(paste("logistic.mle", c("r",
                                     "K",
                                     "N0"), sep="."),
             paste("logistic.nls", c("N0",
                                     "r",
                                     "K"), sep="."),
             paste("exp", c("N0",
                            "r"), sep="."))


#Drop low CO2 assay environment
readings<-readings[readings$assayCO2!="low",, drop=TRUE]

#Round generations so that transplants fall within the same generations bin
readings$round.gen<-round_any(readings$generations, 10, round)


readings.para<-readings[!is.na(readings$logistic.mle.r),]
readings.para<-readings[!duplicated(cbind(readings$culture, readings$logistic.mle.r)),]

Species.list<-unique(readings.para$Species)

readings.para$Species.regime<-with(readings.para, paste(Species, regime))


printer<-T

pdf("./Plots/growth/ratio plots.pdf")

for(para in para.list){
  
  selected<-ddply(.data=readings.para,
                        .variables=c("Species", "Line", "transplant", "regime", "round.gen", "CO2history"),
                        function(x){ ratio<-mean(x[x$assayCO2=="high", para])/mean(x[x$assayCO2=="ambient", para])
                                     return(ratio)})
  
  sub.cast.1<-cast(melt(selected, measure.vars="V1"),
                 transplant+regime+Species~CO2history,
                 fun.aggregate=mean,
                 na.rm=T)
  sub.cast.2<-cast(melt(selected, measure.vars="V1"),
                 transplant+regime+Species~.,
                 fun.aggregate=mean,
                 na.rm=T)
  sub.cast.3<-cast(melt(selected, measure.vars="V1"),
                 transplant+regime~.,
                 fun.aggregate=mean,
                 na.rm=T)
  
  grid.arrange(tableGrob(format(sub.cast.1, digits=3), main=para),
               tableGrob(format(sub.cast.2, digits=3), main=para),
               tableGrob(format(sub.cast.3, digits=3), main=para), main=para)
  
  for(Species in Species.list){
    try({
      if(printer){
        print(para)
        print(Species)}
      
      subselected<-selected[selected$Species==Species,]
      
      par.plot<-qplot(data=subselected,
                      x=round.gen,
                      y=V1,
                      colour=CO2history,
                      ylab=para,
                      main=paste(Species,para, "ratio"),
                      xlab="generations")+
                        facet_grid(regime~.,
                                   scales = "free")+
                                     geom_line(aes(group=Line, colour=CO2history))+
                                     scale_colour_manual(values = c("high"="red","ambient"="blue"))+
                                     geom_hline(aes(yintercept=0))
      
      print(par.plot)
      
      

      

      
})
  }
}


dev.off()

