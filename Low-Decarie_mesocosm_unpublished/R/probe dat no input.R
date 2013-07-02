##############################################################################
# Housekeeping
##############################################################################

#Load libraries
require(ggplot2)
require(plyr)
require(reshape)
require(gridExtra)
require(xtable)
library(devtools)
library(car)
library(vegan)

#Load ggplot2 fix
source_gist("https://gist.github.com/4578531")

#Set contrasts
op <- options(contrasts=c("contr.helmert", "contr.poly"))


pdf("./Plots/Chlorophyll plot.pdf", width=10, height=4)


#Set up theme
#theme_set(theme_bw())
source("./R/theme.R")
theme_set(theme_minimal())
theme_update(legend.position="right")


load("./Outputs/probe data.RData")


##############################################################################
# Plot total chlorophyll
##############################################################################
  
  stat_sum_single <- function(fun, geom="point", ...) {
    stat_summary(fun.y=fun, colour="red",
                 geom=geom, size = 3, ...)
  }
  
probe.dat$fertilizer<-factor(probe.dat$fertilizer,
                             levels=levels(probe.dat$fertilizer)[c(2,1)])
  
  p<-qplot(data=probe.dat,
           x=date,
           y=log10(Total_chlorophyll),
           ylab=expression(paste("Log"[10],"(Total Chlorophyll-a)",
                                  " (",mu,"g/L)",sep="")),
           xlab="Date",
           colour=CO2,
           size=CO2,
           #shape=input,
           linetype=fertilizer)+
    facet_grid(facets=~experiment,
               scales="free")+
   geom_line(aes(group=sampleID.exp), 
             #stat="summary", fun.y = "mean",
             alpha=I(0.3),
             size=CO2)+
     scale_linetype_manual(values=c("Fertilized"=1,
                                    "Not fertilized"=3))+
#    scale_shape_manual(values=c("Did not receive input"=1,
#                                "Received input"=19,
#                                "Lake"=3))+
    scale_size_manual(values=c("Ambient CO2"=0.5, 
                               "High CO2"=1,
                               "Lake"=0.25))+
   scale_colour_manual(values=c("Ambient CO2"="blue", 
                                "High CO2"="red",
                                "Lake"="black"))+
    geom_line(aes(group=treatment), 
              stat="summary", fun.y = "mean",
              alpha=I(0.6),
              size=I(2),
              show_guide=F)+
                theme(axis.text.x=element_text(angle=-90, hjust=0))
  
  fertilization.addition<-data.frame(date=as.numeric(as.Date("13-09-2012", "%d-%m-%Y")),
                                     experiment="Experiment 2")
  
  fertilization.addition$experiment<-factor(fertilization.addition$experiment, levels=unique(probe.dat$experiment))
 
  p<-p+geom_vline(aes(xintercept =date),linetype=I(3), data=fertilization.addition, show_guide=F)
  
  print(p)



p<-qplot(data=probe.dat,
         x=date,
         group=treatment, 
         stat="summary", fun.y = "mean",
         alpha=I(0.6),
         y=log10(Total_chlorophyll),
         ylab=expression(paste("Log"[10],"(Total Chlorophyll-a)",
                               " (",mu,"g/L)",sep="")),
         xlab="Date",
         colour=CO2,
         #size=CO2,
         #shape=input,
         geom="line",
         size=fertilizer)+
  facet_grid(facets=~experiment,
             scales="free")+
#   geom_line(aes(group=sampleID.exp), 
#             #stat="summary", fun.y = "mean",
#             alpha=I(0.3),
#             size=CO2)+
#   scale_linetype_manual(values=c("Fertilized"=1,
#                                  "Not fertilized"=3))+
  #    scale_shape_manual(values=c("Did not receive input"=1,
  #                                "Received input"=19,
  #                                "Lake"=3))+
   scale_size_manual(values=c("Fertilized"=3, 
                              "Not fertilized"=1))+
  scale_colour_manual(values=c("Ambient CO2"="blue", 
                               "High CO2"="red",
                               "Lake"="black"))+
  theme(axis.text.x=element_text(angle=-90, hjust=0))

fertilization.addition<-data.frame(date=as.numeric(as.Date("13-09-2012", "%d-%m-%Y")),
                                   experiment="Experiment 2")

fertilization.addition$experiment<-factor(fertilization.addition$experiment, levels=unique(probe.dat$experiment))

p<-p+geom_vline(aes(xintercept =date),linetype=I(3), data=fertilization.addition, show_guide=F)

print(p)





  
  p<-qplot(data=probe.dat,
           x=date,
           y=Total_chlorophyll,
           ylab=expression(paste("Total Chlorophyll-a (",mu,"g/L)",sep="")),
           xlab="Date",
           colour=CO2,
           size=CO2,
           #shape=input,
           linetype=fertilizer)+
             geom_line(aes(group=sampleID.exp), 
                       #stat="summary", fun.y = "mean",
                       alpha=I(0.3))+
    scale_linetype_manual(values=c("Fertilized"=1,
                                   "Not fertilized"=3))+
    scale_size_manual(values=c("Ambient CO2"=0.5, 
                               "High CO2"=1,
                               "Lake"=1))+
#        scale_shape_manual(values=c("Did not receive input"=1,
#                                    "Received input"=19,
#                                    "Lake"=3))+
         facet_grid(facets=~experiment,
                    scales="free")+
                      scale_colour_manual(values=c("Ambient CO2"="blue", 
                                                   "High CO2"="red",
                                                   "Lake"="black"))+
             geom_line(aes(group=treatment), 
                       stat="summary", fun.y = "mean",
                       alpha=I(0.6),
                       size=I(2),
                       show_guide=F)+
                         theme(axis.text.x=element_text(angle=-90, hjust=0))
  
  fertilization.addition<-data.frame(date=as.numeric(as.Date("13-09-2012", "%d-%m-%Y")),
                                     experiment="Experiment 2")
  
  fertilization.addition$experiment<-factor(fertilization.addition$experiment, levels=unique(probe.dat$experiment))
  
  p<-p+geom_vline(aes(xintercept =date), data=fertilization.addition,
                  linetype=I(3))
  
  print(p)




###########################



##############################################################################
# Plot and analyze mean total chlorophyll
##############################################################################


probe.dat.temp<-probe.dat
# # probe.dat.temp$fertilizer[probe.dat.temp$date<=as.Date("13-09-2012", "%d-%m-%Y") & probe.dat.temp$experiment=="Experiment 2"]<-"Not fertilized"
# probe.dat.temp$treatment<-with(probe.dat.temp, paste(CO2, fertilizer))


probe.dat.mean<-ddply(.data=probe.dat.temp,
                      .variables=c("sampleID",
                                   "CO2",
                                   "input",
                                   "treatment",
                                   "fertilizer",
                                   "experiment.fert"),
                      function(x){ colMeans(x[,!names(x) %in% c("date", "sampleID", "CO2", "input", "fertilizer",  "treatment", "experiment", "experiment.fert", "sampleID.exp")],
                                            na.rm=T)})







plot.mean.chl<-function(selected.par="Total_chlorophyll", ylab=expression(paste("Log"[10],"(Mean total chlorophyll-a", " (",mu,"g/L)",sep=""))){
  
p<-qplot(data=probe.dat.mean,
         x=CO2,
         colour=CO2,
         y=log10(get(selected.par)),
         ylab=ylab,
         xlab="",
         fill=fertilizer,
         #shape=input,
         size=fertilizer,
         geom="boxplot")+
         #linetype=fertilizer,
         
#   scale_linetype_manual(values=c("Fertilized"=1,
#                                  "Not fertilized"=3))+
  scale_size_manual(values=c("Fertilized"=1, 
                             "Not fertilized"=0.5))+
           facet_grid(.~experiment.fert)+
           geom_boxplot(aes(group=treatment), outlier.size=I(0), show_guide=F)+
           #geom_point()+
#            scale_shape_manual(values=c("Did not receive input"=1,
#                                        "Received input"=19,
#                                        "Lake"=3))+
            theme(axis.text.x=element_text(angle=-90, hjust=0), legend.position = "none")+
            scale_fill_manual(values=c("Fertilized"="grey",
                                       "Not fertilized"="white"))+
                                             scale_colour_manual(values=c("Ambient CO2"="blue", 
                                                                          "High CO2"="red",
                                                                          "Lake"="black"))
print(p+theme(legend.position = "right")+theme(axis.text.x = element_text(colour=c("blue","red","black"))))


p<-qplot(data=probe.dat.mean,
         x=CO2,
         colour=CO2,
         y=get(selected.par),
         ylab=expression(paste("Mean total chlorophyll-a", " (",mu,"g/L)",sep="")),
         xlab="",
         fill=fertilizer,
         #shape=input,
         size=fertilizer,
         geom="boxplot")+
  scale_size_manual(values=c("Fertilized"=1, 
                             "Not fertilized"=0.5))+
           facet_grid(.~experiment.fert)+
           geom_boxplot(aes(group=treatment), outlier.size=I(0))+
           #geom_point()+
#            scale_shape_manual(values=c("Did not receive input"=1,
#                                        "Received input"=19,
#                                        "Lake"=3))+
                                         theme(axis.text.x=element_text(angle=-90, hjust=0), legend.position = "none")+
                                         scale_fill_manual(values=c("Fertilized"="grey",
                                                                    "Not fertilized"="white"))+
                                                                      scale_colour_manual(values=c("Ambient CO2"="blue", 
                                                                                                   "High CO2"="red",
                                                                                                   "Lake"="black"))
print(p+theme(legend.position = "right")+theme(axis.text.x = element_text(colour=c("blue","red","black"))))





p<-qplot(data=probe.dat.mean[probe.dat.mean$CO2!="Lake" &
                               probe.dat.mean$experiment.fert %in% c("Experiment 2 after fertilization", "Experiment 3"),] ,
         x=CO2,
         colour=CO2,
         y=get(selected.par),
         ylab=expression(paste("Mean total chlorophyll-a", " (",mu,"g/L)",sep="")),
         xlab="",
         fill=fertilizer,
         #shape=input,
         size=fertilizer,
         geom="boxplot")+
  scale_size_manual(values=c("Fertilized"=1, 
                             "Not fertilized"=0.5))+
  facet_grid(.~experiment.fert)+
  geom_boxplot(aes(group=treatment), outlier.size=I(0))+
  #geom_point()+
  #            scale_shape_manual(values=c("Did not receive input"=1,
  #                                        "Received input"=19,
  #                                        "Lake"=3))+
  theme(axis.text.x=element_text(angle=-90, hjust=0), legend.position = "none")+
  scale_fill_manual(values=c("Fertilized"="grey",
                             "Not fertilized"="white"))+
  scale_colour_manual(values=c("Ambient CO2"="blue", 
                               "High CO2"="red"))
print(p+theme(legend.position = "right")+theme(axis.text.x = element_text(colour=c("blue","red","black"))))

}





plot.mean.perc<-function(selected.par="Total_chlorophyll", ylab="Log(Mean Total Chlorophyll-a (ug/L))"){
  
  
  
  p<-qplot(data=probe.dat.mean,
           x=CO2,
           y=get(selected.par),
           ylab=ylab,
           xlab="",
           #shape=input,
           fill=fertilizer,
           geom="boxplot")+
    facet_grid(~experiment.fert)+
    geom_boxplot(aes(group=treatment), outlier.size=I(0))+
    #              scale_shape_manual(values=c("Did not receive input"=1,
    #                                          "Received input"=19,
    #                                          "Lake"=3))+
    scale_fill_manual(values=c("Not fertilized"="white",
                               "Fertilized"="grey"))
  
  print(p+
          theme(axis.text.x=element_text(angle=-90, hjust=0), legend.position = "none")+theme(axis.text.x = element_text(colour=c("blue","red","black"))))
  
  p<-qplot(data=probe.dat.mean[probe.dat.mean$CO2!="Lake" &
                                 probe.dat.mean$experiment.fert %in% c("Experiment 2 after fertilization", "Experiment 3"),],
           x=CO2,
           y=get(selected.par),
           ylab=ylab,
           xlab="",
           #shape=input,
           fill=fertilizer,
           geom="boxplot")+
             facet_grid(~experiment.fert)+
             geom_boxplot(aes(group=treatment), outlier.size=I(0))+
#              scale_shape_manual(values=c("Did not receive input"=1,
#                                          "Received input"=19,
#                                          "Lake"=3))+
                                          scale_fill_manual(values=c("Not fertilized"="white",
                                                                         "Fertilized"="grey"))
  
  print(p+
    theme(axis.text.x=element_text(angle=-90, hjust=0), legend.position = "none")+theme(axis.text.x = element_text(colour=c("blue","red","black"))))
}



probe.dat.mean$CO2<-as.factor(probe.dat.mean$CO2)


probe.dat.mean.no.lake<-probe.dat.mean[probe.dat.mean$sampleID!="lake",,drop=T]


anova.fit<-function(selected.parameter="Total_chlorophyll", label="Total chlorophyll"){
  
fit<-aov(data=probe.dat.mean.no.lake,
         get(selected.parameter)~CO2*input*fertilizer+experiment.fert)
fit.table<-drop1(fit,~.,test="F")
grid.newpage()
grid.text(label=paste("All experiments", label), x=0.5,y=0.9)
grid.table(round(fit.table, 7))
# grid.newpage()
# grid.text(label=paste("All experiments", label), x=0.5,y=0.9)
# grid.table(xtable(summary(fit)))
grid.text(label=paste("All experiments", label), x=0.5,y=0.9)
par(mfrow=c(2,2))
plot(fit)


fit<-aov(data=probe.dat.mean.no.lake,
         get(selected.parameter)~CO2*fertilizer+experiment.fert)
fit.table<-drop1(fit,~.,test="F")
# grid.newpage()
# grid.table(xtable(summary(fit)))
# grid.text(label=paste("All experiments without input", label), x=0.5,y=0.9)
grid.newpage()
grid.table(round(fit.table, 7))
grid.text(label=paste("All experiments without input", label), x=0.5,y=0.9)
par(mfrow=c(2,2))
plot(fit)


# fit<-aov(data=probe.dat.mean.no.lake[probe.dat.mean.no.lake$fertilizer=="Not fertilized",],
#          get(selected.parameter)~CO2+experiment.fert)
# fit.table<-drop1(fit,~.,test="F")
# grid.text(label=paste("All experiments not fertilized", label), x=0.5,y=0.9)
# grid.newpage()
# grid.table(round(fit.table, 5))
# grid.text(label=paste("All experiments not fertilized", label), x=0.5,y=0.9)
# par(mfrow=c(2,2))
# plot(fit)
# 
# 
# fit<-aov(data=probe.dat.mean.no.lake[probe.dat.mean.no.lake$CO2=="Ambient CO2",],
#          get(selected.parameter)~fertilizer+experiment.fert)
# fit.table<-drop1(fit,~.,test="F")
# grid.text(label=paste("All experiments ambient CO2", label), x=0.5,y=0.9)
# grid.newpage()
# grid.table(round(fit.table, 6))
# grid.text(label=paste("All experiments ambient CO2", label), x=0.5,y=0.9)
# par(mfrow=c(2,2))
# plot(fit)
#   
#   
#   
# fit<-aov(data=probe.dat.mean.no.lake[probe.dat.mean.no.lake$experiment.fert=="Experiment 1",, drop=T],
#          get(selected.parameter)~CO2*input)
# fit.table<-drop1(fit,~.,test="F")
# grid.newpage()
# grid.table(round(fit.table, ))
# grid.text(label=paste("Experiment 1", label), x=0.5,y=0.9)
# par(mfrow=c(2,2))
# plot(fit)
# #
# fit<-aov(data=probe.dat.mean.no.lake[probe.dat.mean.no.lake$experiment.fert=="Experiment 1",, drop=T],
#          get(selected.parameter)~CO2)
# fit.table<-drop1(fit,~.,test="F")
# grid.newpage()
# grid.table(round(fit.table, 6))
# grid.text(label=paste("Experiment 1", label), x=0.5,y=0.9)
# par(mfrow=c(2,2))
# plot(fit)
# # 
# #  
# # 
# fit<-aov(data=probe.dat.mean.no.lake[probe.dat.mean.no.lake$experiment.fert=="Experiment 2 before fertilization",, drop=T],
#          get(selected.parameter)~CO2*input)
# fit.table<-drop1(fit,~.,test="F")
# grid.newpage()
# grid.table(round(fit.table, 6))
# grid.text(label=paste("Experiment 2 before fertilization",label), x=0.5,y=0.9)
# par(mfrow=c(2,2))
# # plot(fit)
# fit<-aov(data=probe.dat.mean.no.lake[probe.dat.mean.no.lake$experiment.fert=="Experiment 2 before fertilization",, drop=T],
#          get(selected.parameter)~CO2)
# fit.table<-drop1(fit,~.,test="F")
# grid.newpage()
# grid.table(round(fit.table, 6))
# grid.text(label=paste("Experiment 2 before fertilization",label), x=0.5,y=0.9)
# par(mfrow=c(2,2))
# plot(fit)
# # 
# fit<-aov(data=probe.dat.mean.no.lake[probe.dat.mean.no.lake$experiment.fert=="Experiment 2 after fertilization",, drop=T],
#          get(selected.parameter)~CO2*input*fertilizer)
# fit.table<-drop1(fit,~.,test="F")
# grid.newpage()
# grid.table(round(fit.table, 6))
# grid.text(label=paste("Experiment 2 after fertilization:", label), x=0.5,y=0.9)
# par(mfrow=c(2,2))
# plot(fit)
# fit<-aov(data=probe.dat.mean.no.lake[probe.dat.mean.no.lake$experiment.fert=="Experiment 2 after fertilization",, drop=T],
#          get(selected.parameter)~CO2*fertilizer)
# fit.table<-drop1(fit,~.,test="F")
# grid.newpage()
# grid.table(round(fit.table, 6))
# grid.text(label=paste("Experiment 2 after fertilization:", label), x=0.5,y=0.9)
# par(mfrow=c(2,2))
# plot(fit)
# # 
# # 
# # 
# fit<-aov(data=probe.dat.mean.no.lake[probe.dat.mean.no.lake$experiment.fert=="Experiment 3",],
#          get(selected.parameter)~CO2*fertilizer)
# fit.table<-drop1(fit,~.,test="F")
# grid.newpage()
# grid.table(round(fit.table, 6))
# grid.text(label=paste("Experiment 3:",label), x=0.5,y=0.9)
# par(mfrow=c(2,2))
# plot(fit)


}

probe.dat.mean.no.lake$log.chl<-log10(probe.dat.mean.no.lake$Total_chlorophyll)


plot.mean.chl()
anova.fit("Total_chlorophyll", label="Total chlorophyll")






##############################################################################
# Relative frequency plots
##############################################################################


probe.dat.mean.temp<-probe.dat.mean[,names(probe.dat.mean) %in% c("sampleID",
                                                                  "CO2",
                                                                  "input",
                                                                  "fertilizer",
                                                                  "treatment",
                                                                  "experiment.fert",
                                                                  "Chlorophytes.perc",
                                                                  "Cyanobacteria.perc",
                                                                  "Diatoms.perc",
                                                                  "Cryptophytes.perc")]


probe.dat.mean.temp.melt<-melt(data=probe.dat.mean.temp,
                               measure.vars=c("Chlorophytes.perc",
                                              "Cyanobacteria.perc",
                                              "Diatoms.perc",
                                              "Cryptophytes.perc"),
                               variable_name="Taxon")


probe.dat.mean.temp.melt$Taxon<-sub(".perc","", probe.dat.mean.temp.melt$Taxon)

p<-qplot(data=probe.dat.mean.temp.melt[probe.dat.mean.temp.melt$sampleID!="lake" &
                                         probe.dat.mean.temp.melt$experiment.fert %in% c("Experiment 2 after fertilization", "Experiment 3"),],
         x=CO2,
         y=value,
         ylab="Relative frequency",
         colour=Taxon,
         xlab="",
         #shape=input,
         fill=fertilizer,
         geom="boxplot")+
           facet_grid(~experiment.fert)+
           geom_boxplot(outlier.size=I(0))+
#            scale_shape_manual(values=c("Did not receive input"=1,
#                                        "Received input"=19,
#                                        "Lake"=3))+
                                         scale_fill_manual(values=c("Not fertilized"="white",
                                                                    "Fertilized"="grey"))
#                                         scale_colour_manual(values=c("Chlorophytes.perc"="green",
#                                                                      "Cyanobacteria.perc"="cyan",
#                                                                      "Diatom.perc"="black",
#                                                                      "Cryptophytes.perc"="orange"))
                                                                      

print(p+theme(axis.text.x=element_text(angle=-90, hjust=0), legend.position = "right")+ theme(legend.title=element_blank())+theme(axis.text.x = element_text(colour=c("blue","red","black"))))



p<-qplot(data=probe.dat.mean.temp.melt[probe.dat.mean.temp.melt$sampleID!="lake",],
         x=reorder(Taxon, -value),
         y=value,
         ylab="Relative frequency",
         colour=CO2,
         xlab="",
         #linetype=fertilizer,
         fill=fertilizer,
         size=fertilizer,
         geom="boxplot")+
  facet_grid(~experiment.fert)+
  geom_boxplot(outlier.size=I(0))+
  scale_size_manual(values=c("Not fertilized"=0.5, 
                               "Fertilized"=1))+
#   scale_shape_manual(values=c("Did not receive input"=1,
#                               "Received input"=19,
#                               "Lake"=3))+
  scale_fill_manual(values=c("Not fertilized"="white",
                             "Fertilized"="grey"))+
  scale_colour_manual(values=c("Ambient CO2"="blue", 
                               "High CO2"="red",
                               "Lake"="black"))


print(p+theme(axis.text.x=element_text(angle=-90, hjust=0), legend.position = "right")+ theme(legend.title=element_blank()))


##############################################################################
# Relative frequency analysis
##############################################################################



fit<-with(probe.dat.mean.no.lake,
         fit<-manova(cbind(Chlorophytes.perc,Cyanobacteria.perc,Diatoms.perc,Cryptophytes.perc)~CO2*fertilizer*input+experiment.fert))
Anova(fit, test="Wilks")
# summary.aov(fit)

fit<-with(probe.dat.mean.no.lake,
          fit<-manova(cbind(Chlorophytes.perc,Cyanobacteria.perc,Diatoms.perc,Cryptophytes.perc)~CO2*fertilizer+experiment.fert))
Anova(fit, test="Wilks")
# summary.aov(fit)


##############################################################################
# Plot each species seperatly
##############################################################################

plot.perc<-function(selected.par="Total_chlorophyll", ylab=expression(paste("Log"[10],"(Total Chlorophyll-a)", " (",mu,"g/L)",sep=""))){
  p<-qplot(data=probe.dat,
           x=date,
           y=get(selected.par),
           ylab="Relative frequency (%)",
           xlab="Date",
           colour=CO2,
           #shape=input,
           linetype=fertilizer)+
    geom_line(aes(group=sampleID.exp), 
              stat="summary", fun.y = "mean",alpha=I(0.3))+
#     scale_shape_manual(values=c("Did not receive input"=1,
#                                 "Received input"=19,
#                                 "Lake"=3))+
    facet_grid(facets=~experiment,
               scales="free")+
    scale_colour_manual(values=c("Ambient CO2"="blue", 
                                 "High CO2"="red",
                                 "Lake"="black"))+
    geom_line(aes(group=treatment), 
              stat="summary", fun.y = "mean",
              alpha=I(0.6),
              size=I(2))+
    theme(axis.text.x=element_text(angle=-90, hjust=0), legend.position = "right")+theme(axis.text.x = element_text(colour=c("blue","red","black")))
  
  fertilization.addition<-data.frame(date=as.numeric(as.Date("13-09-2012", "%d-%m-%Y")),
                                     experiment="Experiment 2")
  
  fertilization.addition$experiment<-factor(fertilization.addition$experiment, levels=unique(probe.dat$experiment))
  
  p<-p+geom_vline(aes(xintercept =date,
                      linetype="Fertilizer addition"), data=fertilization.addition)
  
  print(p)
}



# plot.perc("Chlorophytes.perc", "Percentage chlorophytes")
# plot.mean.perc("Chlorophytes.perc", "Percentage chlorophytes")
anova.fit("Chlorophytes.perc", "Percentage chlorophytes")

# plot.perc("Diatoms.perc", "Percentage diatoms")
# plot.mean.perc("Diatoms.perc", "Percentage diatoms")
anova.fit("Diatoms.perc", "Percentage diatoms")

# plot.perc("Cyanobacteria.perc", "Percentage cyanobacteria")
# plot.mean.perc("Cyanobacteria.perc", "Percentage cyanobacteria")
anova.fit("Cyanobacteria.perc", "Percentage cyanobacteria")

# plot.perc("Cryptophytes.perc", "Percentage cryptophytes")
# plot.mean.perc("Cryptophytes.perc", "Percentage cryptophytes")
anova.fit("Cryptophytes.perc", "Percentage cryptophytes")

# plot.perc("shannon", "Shannon diversity index")
# plot.mean.perc("shannon", "Shannon diversity index")
# anova.fit("shannon", label="Shannon diversity index")

save(probe.dat.mean.no.lake, file="./Outputs/mean values by experiment.RData")

probe.dat.mean.no.lake<-probe.dat.mean.no.lake[,!names(probe.dat.mean.no.lake) %in% c("depth",
                                                                                      "temperature")]

##############################################################################
# Calculating ratios and differences
##############################################################################


#Calculating differences in means for CO2
probe.dat.mean.no.lake<-probe.dat.mean.no.lake[,!names(probe.dat.mean.no.lake) %in% c("Chlorophytes", "Cyanobacteria", "Diatoms", "Cryptophytes","Transmittance","log.chl")]
probe.dat.mean.no.lake.melt<-melt(probe.dat.mean.no.lake)
probe.dat.mean.no.lake.melt<-probe.dat.mean.no.lake.melt[,!names(probe.dat.mean.no.lake.melt) %in% c("sampleID","treatment")]
probe.dat.mean.no.lake.CO2cast<-cast(probe.dat.mean.no.lake.melt, ...~CO2, fun.aggregate=mean, na.rm=T)
probe.dat.mean.no.lake.CO2cast$dif<-with(probe.dat.mean.no.lake.CO2cast, get("High CO2")-get("Ambient CO2"))
probe.dat.mean.no.lake.CO2cast$rel.dif<-with(probe.dat.mean.no.lake.CO2cast, (get("High CO2")-get("Ambient CO2"))/get("Ambient CO2"))
qplot(data=probe.dat.mean.no.lake.CO2cast,
      y=dif,
      x=fertilizer)+
      facet_grid(variable~experiment.fert, scale="free")+
  geom_hline(yintercept=I(0), colour=I("red"))
mean.probe.dat.mean.no.lake.CO2cast<-ddply(.data=probe.dat.mean.no.lake.CO2cast,
      .variables=c("fertilizer", "variable"),
      function(x) data.frame(mean.dif=round(mean(x$dif, na.rm=T),4)))

grid.newpage()
grid.table(mean.probe.dat.mean.no.lake.CO2cast)


#Calculating differences in mean for fertilizer
probe.dat.mean.no.lake.fertcast<-cast(probe.dat.mean.no.lake.melt, ...~fertilizer, fun.aggregate=mean, na.rm=T)
probe.dat.mean.no.lake.fertcast$dif<-with(probe.dat.mean.no.lake.fertcast, get("Fertilized")-get("Not fertilized"))
probe.dat.mean.no.lake.fertcast$rel.dif<-with(probe.dat.mean.no.lake.fertcast, (get("Fertilized")-get("Not fertilized"))/get("Not fertilized"))
qplot(data=probe.dat.mean.no.lake.fertcast,
      y=dif,
      x=CO2)+
  facet_grid(variable~experiment.fert, scale="free")+
  geom_hline(yintercept=I(0), colour=I("red"))
mean.probe.dat.mean.no.lake.fertcast<-ddply(.data=probe.dat.mean.no.lake.fertcast,
      .variables=c("CO2", "variable"),
      function(x) with(x, data.frame(Fertilized=round(mean(Fertilized, na.rm=T),4),
                                     Notfertilized=round(mean(get("Not fertilized"), na.rm=T),4),
                                     mean.dif=round(mean(dif, na.rm=T),4))))

grid.newpage()
grid.table(mean.probe.dat.mean.no.lake.fertcast)

graphics.off()