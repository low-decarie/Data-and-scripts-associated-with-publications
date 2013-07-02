rm(list=ls())

load(file="./Outputs/fitted readings.RData")

library(ggplot2)
library(devtools)
library(plyr)

source("./R/publication figures/theme.R")

para.list<-c(paste("logistic.mle", c("r",
                                 "K",
                                 "N0"), sep="."),
             paste("logistic.nls", c("N0",
                                     "r",
                                     "K"), sep="."),
             paste("exp", c("N0",
                            "r"), sep="."))


readings$round.gen<-round_any(readings$generations, 10, round)


readings.para<-readings[!is.na(readings$logistic.mle.r),]
readings.para<-readings[!duplicated(cbind(readings$culture, readings$logistic.mle.r)),]

Species.list<-unique(readings.para$Species)

readings.para$Species.regime<-with(readings.para, paste(Species, regime))


readings.para$assay.code[readings.para$assayCO2=="low"]<-1
readings.para$assay.code[readings.para$assayCO2=="ambient"]<-2
readings.para$assay.code[readings.para$assayCO2=="high"]<-3




printer<-TRUE
pdf("./Plots/growth/parameter plots.pdf", width=16)


par.plot<-qplot(data=readings.para,
                x=exp.r,
                y=logistic.nls.r)

print(par.plot)


par.plot<-qplot(data=readings.para,
                x=logistic.mle.r,
                y=logistic.nls.r,
                colour=Species)+
                  geom_abline(intercept = 0, slope=1)


print(par.plot)

par.plot<-qplot(data=readings.para,
                x=logistic.mle.K,
                y=logistic.nls.K,
                colour=Species,
                alpha=I(0.5))+
                  geom_abline(intercept = 0, slope=1)


print(par.plot)

  for(para in para.list){
    for(Species in Species.list){
    try({
    if(printer){
    print(para)
    print(Species)}
    
    selected<-readings.para[readings.para$Species==Species,]

    par.plot<-qplot(data=selected,
          x=reorder(factor(assayCO2), assay.code),
          y=get(para),
          colour=CO2history,
          geom="boxplot",
          ylab=para,
          main=paste(Species,para))+
          facet_grid(regime~transplant,
                    scales = "free")+
            geom_line(aes(group=Line ,linetype=Line))+
            scale_colour_manual(values = c("high"="red","ambient"="blue"))
  
  print(par.plot)  
    
    selected<-selected[selected$assayCO2!="low",, drop=TRUE]
    
    selected<-droplevels(selected)
    
    selected$line.assay<-with(selected, paste(Line, assayCO2))
  
    par.plot<-qplot(data=selected,
                    x=round.gen,
                    y=get(para),
                    colour=assayCO2,
                    ylab=para,
                    main=paste(Species,para),
                    xlab="generations")+
                      facet_grid(regime~.,
                                 scales = "free")+
                                   geom_line(aes(group=line.assay, colour=assayCO2, linetype=CO2history))+
                                   scale_colour_manual(values = c("high"="red","ambient"="blue"))
    
    print(par.plot)  
    
  })
    }
    }

dev.off()


save(readings.para, file="./Outputs/growth parameters.RData")
  