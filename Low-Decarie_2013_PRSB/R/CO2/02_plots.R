#Housekeeping
rm(list=ls())
library(ggplot2)
library(plyr)
library(devtools)
library(gridExtra)
source("./R/publication figures/theme.R")

CO2<-CO2[CO2$period==1|CO2$period==6]

#Load data
load(file="./Outputs/CO2media.RData")

#Plot time line for regime 1
reg.1<-CO2[CO2$regime==1,]

para.list<-list("CO2_airspace",
                "pH",
                "pCO2_calc")

plot.sp<-function(para){
plot.sp<-qplot(data=reg.1,
                x=Time,
                y=reg.1[,para],
                colour=assayCO2,
               ylab=para,
               main=paste(para, "in Regime 1"),
               shape=Line)

plot.sp<-plot.sp+geom_line(aes(group=Line, linetype=Line))

plot.sp<-plot.sp+geom_smooth(aes(group=assayCO2))

#Add a vertical line at transfer time (3.5 days)
plot.sp<-plot.sp+geom_vline(x=I(5), size=I(1))

#Customize colour legend
plot.sp<-plot.sp+scale_colour_manual(values=c("ambient"="blue",
                                              "high"= "red"))

#Seperate by regime and legend
plot.sp<-plot.sp+facet_grid(~Species)

print(plot.sp)
}

pdf("./Plots/CO2/regime 1 time.pdf", width=12)
l_ply(para.list,function(x){plot.sp(x)})
dev.off()


final<-ddply(.data=CO2,
             .variables=c("period"),
              function(x){ final<-x[x$Time==max(x$Time),]})




final<-final[final$vessel=="flask",]
final<-final[final$Species!="Blank",]
final<-final[final$period %in% c(1:3, 6),]


plot.sp<-function(para){
  plot.sp<-qplot(data=final,
                 x=assayCO2,
                 y=final[,para],
                 colour=assayCO2,
                 ylab=para,
                 main=para,
                 shape=Line)
  
  
  plot.sp<-plot.sp+geom_boxplot(aes(group=assayCO2))
  
  #Customize colour legend
  plot.sp<-plot.sp+scale_colour_manual(values=c("ambient"="blue",
                                                "high"= "red"))
  
  #Seperate by regime and legend
  plot.sp<-plot.sp+facet_grid(regime~Species)
  
  print(plot.sp)
}




final.melt<-melt(final[,!(names(final) %in% c("X",
                                              "period",
                                              "date",
                                              "Time",
                                              "CO2_chamber",
                                              "initial.temperature",
                                              "final_temperature",
                                              "vessel"))],
                 id.vars=c("Species", "regime", "assayCO2", "Line"))


analyse.sp<-function(para){
  

  final.melt.temp<-final.melt[final.melt$variable==para,]
  
  final.cast<-cast(final.melt.temp,
                   Species+assayCO2~variable+regime,
                   fun.aggregate=mean,
                   na.rm=T)
  
  grid.newpage()
  grid.draw(tableGrob(format(data.frame(final.cast), digits=4), 
                      gp=gpar(fontsize=12, lwd=.5),
                      gpar.corefill = gpar(fill=c("grey80"),alpha=0.5, col=NA),
                      h.even.alpha = 0.5,
                      main=para))
  
  
  
  final.cast<-cast(final.melt.temp,
                   assayCO2~variable+regime,
                   fun.aggregate=mean,
                   na.rm=T)
  
  grid.newpage()
  grid.draw(tableGrob(format(data.frame(final.cast), digits=4), 
                      gp=gpar(fontsize=12, lwd=.5),
                      gpar.corefill = gpar(fill=c("grey80"),alpha=0.5, col=NA),
                      h.even.alpha = 0.5,
                      main=para))
  
  
  
  temp<-final[final$regime==1,]
  model<-aov(temp[,para]~temp[,"assayCO2"]*temp[,"Species"])
  sum.model.1<-anova(model)
  sum.model.1$regime<-1
  
  temp<-final[final$regime==2,]
  model<-aov(temp[,para]~temp[,"assayCO2"]*temp[,"Species"])
  sum.model.2<-anova(model)
  sum.model.2$regime<-2
  
  grid.arrange(tableGrob(format(sum.model.1, digits=4)),
                         tableGrob(format(sum.model.2, digits=4)), main=para)
  
}

pdf("./Plots/CO2/final values.pdf", width=12)
l_ply(para.list,function(x){plot.sp(x)
                            analyse.sp(x)})
dev.off()


