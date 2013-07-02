rm(list=ls())

load(file="./Outputs/fitted readings.RData")
load(file="./Outputs/growth parameters.RData")

library(ggplot2)
library(devtools)
library(plyr)
library(car)
library(gridExtra)
#source("./R/publication figures/theme.R")

readings<-readings[readings$assayCO2!="low", ,drop=T]

readings<-droplevels(readings)

readings.para<-readings.para[readings.para$assayCO2!="low", ,drop=T]

readings.para<-droplevels(readings.para)

final<-ddply(.data=readings,
             .variables=c("Species", "regime"),
             function(x){ final<-x[x$transplant==max(x$transplant),]})

min(final$generation[final$regime==1])
min(final$generation[final$regime==2])
min(final$generation[final$regime==3])

plot.sp<-function(para){
  
  #Plot of absorbance through time
  
  #Plot the measured values as points
  plot.sp<-qplot(data=final,
                 x=Time,
                 y=ABS,
                 color=assayCO2,
                 shape=CO2history,
                 linetype=CO2history,
                 alpha=I(0.3),
                 geom=c("point"),
                 group=culture,
                 main=para)
  
  #Add a vertical line at transfer time (3.5 days)
  #plot.sp<-plot.sp+geom_vline(x=I(3.5), size=I(1))
  
  #Add the fitted values as lines
  plot.sp<-plot.sp+geom_line(aes(y=get(para)))
  
  #Customize colour legend
  plot.sp<-plot.sp+scale_colour_manual(values=c("ambient"="blue","high"= "red", "low"= "green"))
  
  #Seperate by regime and transplant
  plot.sp<-plot.sp+facet_grid(regime~Species, scale="free")
  
  print(plot.sp)
  
}

para.list<-c("exp.predicted",
             "logistic.nls.predicted",
             "logistic.mle.predicted")


pdf("./Plots/growth/final plots.pdf",height=10, width=20)
l_ply(.data=para.list, 
      function(x) {plot.sp(x)})




final<-ddply(.data=readings.para,
             .variables=c("Species", "regime"),
             function(x){ final<-x[x$transplant==max(x$transplant),]})



para.list<-c(paste("logistic.mle", c("r",
                                     "K",
                                     "N0"), sep="."),
             paste("logistic.nls", c("N0",
                                     "r",
                                     "K"), sep="."),
             paste("exp", c("N0",
                            "r"), sep="."))



for(para in para.list){
        print(para)
      
      par.plot<-qplot(data=final,
                      x=reorder(factor(assayCO2), assay.code),
                      y=get(para),
                      colour=CO2history,
                      geom="boxplot",
                      ylab=para,
                      main=paste(para))+
                        facet_grid(regime~Species,
                                   scales = "free")+
                                     geom_line(aes(group=Line ,linetype=Line))+
                                     scale_colour_manual(values = c("high"="red","ambient"="blue"))
      
        print(par.plot)
        
#       for(regime in unique(final$regime)){    
#       fit<-aov(get(para)~assayCO2*CO2history*Species, data=final[final$regime==regime,])
#       fit.table<-Anova(fit)
#       fit.table$regime<-regime
#       assign(paste("regime.", regime, sep=""), fit.table)
#       }
#       
# 
#        grid.arrange(tableGrob(format(regime.1, digits=3), main=para),
#                     tableGrob(format(regime.2, digits=3), main=para),
#                     tableGrob(format(regime.3, digits=3), main=para), main=para)
      
        
#         for(regime in unique(final$regime)){
#         fit<-aov(get(para)~Species*assayCO2*CO2history+Error(Line), data=final[final$regime==regime,])
#         fit.table<-summary(fit)[2][[1]][[1]]
#         fit.table$regime<-regime
#         assign(paste("regime.", regime, sep=""),fit.table)
#         }
#         
#         grid.arrange(tableGrob(format(regime.1, digits=3), main=para),
#                      tableGrob(format(regime.2, digits=3), main=para),
#                      tableGrob(format(regime.3, digits=3), main=para), main=para)
#       
#         
        
        
        for(regime in unique(final$regime)){
#         for(Species in unique(final$Species[final$regime==regime])){
#         fit<-aov(get(para)~assayCO2*CO2history, data=final[final$Species==Species & final$regime==regime,])
#         table.fit<-Anova(fit)
#         table.fit$Species<-Species
#         table.fit$regime<-regime
#         assign(Species, table.fit)
#         }
#         
#         grid.arrange(tableGrob(format(Anabaena, digits=3, scientific=F)),
#                      tableGrob(format(Chlamydomonas, digits=3, scientific=F)),
#                      tableGrob(format(Navicula, digits=3, scientific=F)),
#                      tableGrob(format(Nitzschia, digits=3, scientific=F)),
#                      tableGrob(format(Pseudokirchneriella, digits=3, scientific=F)),
#                      tableGrob(format(Scenedesmus, digits=3, scientific=F)),
#                      tableGrob(format(Synechococcus, digits=3, scientific=F)),main=paste(para,"regime", regime))
#         
        for(Species in unique(final$Species)){
          try.test<-try({fit<-aov(get(para)~assayCO2*CO2history+Error(Line), data=final[final$Species==Species & final$regime==regime,])
          table.fit<-summary(fit)[2][[1]][[1]] 
          table.fit$Species<-Species
          table.fit$regime<-regime
          assign(Species, table.fit)})
          if(class(try.test)[1]=="try-error"){assign(Species, NA)}
          
        }
        
        grid.arrange(tableGrob(format(Anabaena, digits=3, scientific=F)),
                     tableGrob(format(Chlamydomonas, digits=3, scientific=F)),
                     tableGrob(format(Navicula, digits=3, scientific=F)),
                     tableGrob(format(Nitzschia, digits=3, scientific=F)),
                     tableGrob(format(Pseudokirchneriella, digits=3, scientific=F)),
                     tableGrob(format(Scenedesmus, digits=3, scientific=F)),
                     tableGrob(format(Synechococcus, digits=3, scientific=F)),main=paste(para,"regime", regime))
      
        for(Species in unique(final$Species)){
          try(rm(list=paste(Species)))
        }
        
        
        for(Species in unique(final$Species)){
          try.test<-try({fit<-aov(get(para)~assayCO2*CO2history+Error(Line), data=final[final$Species==Species & final$regime==regime,])
                         table.fit<-summary(fit)[1][[1]][[1]] 
                         table.fit$Species<-Species
                         table.fit$regime<-regime
                         assign(Species, table.fit)})
          if(class(try.test)[1]=="try-error"){assign(Species, NA)}
          
        }
        
        grid.arrange(tableGrob(format(Anabaena, digits=3, scientific=F)),
                     tableGrob(format(Chlamydomonas, digits=3, scientific=F)),
                     tableGrob(format(Navicula, digits=3, scientific=F)),
                     tableGrob(format(Nitzschia, digits=3, scientific=F)),
                     tableGrob(format(Pseudokirchneriella, digits=3, scientific=F)),
                     tableGrob(format(Scenedesmus, digits=3, scientific=F)),
                     tableGrob(format(Synechococcus, digits=3, scientific=F)),main=paste(para,"regime", regime))
        
        for(Species in unique(final$Species)){
          try(rm(list=paste(Species)))
        }
        
  }
          }

dev.off()
