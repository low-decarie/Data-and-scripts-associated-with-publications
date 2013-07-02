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


pdf("./Plots/growth rate in fertilized mesocosms.pdf", width=10, height=4)


#Set up theme
#theme_set(theme_bw())
source("./R/theme.R")
theme_set(theme_minimal())
theme_update(legend.position="right")


load("./Outputs/probe data.RData")


pdf("./Plots/fertilized growth rate.pdf")

#######
#  Calculations of growth rate for fertilized mesocosms
######


#ln

fertilized<-probe.dat[probe.dat$fertilizer=="Fertilized",]

plot.fert<-qplot(data=fertilized,
                 x=date,
                 y=log(Total_chlorophyll),
                 colour=CO2)+
  facet_grid(.~experiment, scale="free")+
  geom_smooth(method="lm",se=F, aes(group=sampleID.exp))

plot.fert<-qplot(data=fertilized,
                 x=date,
                 y=log(Chlorophytes.perc),
                 colour=CO2)+
  facet_grid(.~experiment, scale="free")+
  geom_smooth(method="lm",se=F, aes(group=sampleID.exp))

print(plot.fert)

growth.comp.fertilized<-ddply(.data=fertilized,
                              .variables=c("sampleID",
                                           "CO2",
                                           "treatment",
                                           "sampleID.exp",
                                           "experiment"),
                              function(x) {growth.comp<-with(x,{
                                try.test<-try(growth.community<-lm(log(Total_chlorophyll)~date)$coefficients[2])
                                if(class(try.test)=="try-error"){growth.comp<-growth.community<-NA}
                                
                                try.test<-try(growth.Chlorophytes<-lm(log(Chlorophytes)~date)$coefficients[2])
                                if(class(try.test)=="try-error"){growth.Chlorophytes<-NA}
                                
                                try.test<-try(growth.Diatoms<-lm(log(Diatoms)~date)$coefficients[2])
                                if(class(try.test)=="try-error"){ growth.Diatoms<-NA}
                                
                                try.test<-try(growth.Cryptophytes<-lm(log(Cryptophytes)~date)$coefficients[2])
                                if(class(try.test)=="try-error"){growth.Cryptophytes<-NA}
                                
                                try.test<-try(growth.Cyanobacteria<-lm(log(Cyanobacteria)~date)$coefficients[2])
                                if(class(try.test)=="try-error"){growth.Cyanobacteria<-NA}
                                
                                try.test<-try(comp.Chlorophytes<-lm(log(Chlorophytes.perc)~date)$coefficients[2])
                                if(class(try.test)=="try-error"){comp.Chlorophytes<-NA}
                                
                                try.test<-try(comp.Cryptophytes<-lm(log(Cryptophytes.perc)~date)$coefficients[2])
                                if(class(try.test)=="try-error"){comp.Cryptophytes<-NA}
                                
                                try.test<-try(comp.Diatoms<-lm(log(Diatoms.perc)~date)$coefficients[2])
                                if(class(try.test)=="try-error"){comp.Diatoms<-NA}
                                
                                try.test<-try(comp.Cyanobacteria<-lm(log(Cyanobacteria.perc)~date)$coefficients[2])
                                if(class(try.test)=="try-error"){comp.Cyanobacteria<-NA}
                                
                                data.frame(growth.community,
                                           growth.Chlorophytes,
                                           growth.Diatoms,
                                           growth.Cryptophytes,
                                           growth.Cyanobacteria,
                                           comp.Chlorophytes,
                                           comp.Cryptophytes,
                                           comp.Diatoms,
                                           comp.Cyanobacteria)})
                                           
                                           return(growth.comp)})






box.p<-function(var.interest){
  p<-qplot(data=growth.comp.fertilized,
           x=CO2,
           y=get(var.interest),
           ylab=var.interest,
           colour=experiment)+
    geom_boxplot()
  print(p)
  
  fit<-with(growth.comp.fertilized, aov(get(var.interest)~CO2+experiment))
  sum.fit<-summary(fit)
  grid.newpage()
  grid.table(xtable(sum.fit))
  
  sum.mean<-data.frame(mean.ambient=mean(growth.comp.fertilized[growth.comp.fertilized$CO2=="Ambient CO2", var.interest], na.rm=T),
                       mean.high=mean(growth.comp.fertilized[growth.comp.fertilized$CO2=="High CO2", var.interest], na.rm=T))
  
  grid.newpage()
  grid.table(xtable(sum.mean))
}


var.list<-c("growth.community", "growth.Chlorophytes", "growth.Diatoms", 
            "growth.Cryptophytes", "growth.Cyanobacteria", "comp.Chlorophytes", 
            "comp.Cryptophytes", "comp.Diatoms", "comp.Cyanobacteria")

l_ply(.data=var.list,
      .fun=box.p)



###########################

fertilized<-probe.dat[probe.dat$fertilizer=="Fertilized",]

plot.fert<-qplot(data=fertilized,
                 x=date,
                 y=log2(Total_chlorophyll),
                 colour=CO2)+
  facet_grid(.~experiment, scale="free")+
  geom_smooth(method="lm",se=F, aes(group=sampleID.exp))

plot.fert<-qplot(data=fertilized,
                 x=date,
                 y=log2(Chlorophytes.perc),
                 colour=CO2)+
  facet_grid(.~experiment, scale="free")+
  geom_smooth(method="lm",se=F, aes(group=sampleID.exp))

print(plot.fert)

growth.doublingscomp.fertilized<-ddply(.data=fertilized,
                                       .variables=c("sampleID",
                                                    "CO2",
                                                    "treatment",
                                                    "sampleID.exp",
                                                    "experiment"),
                                       function(x) {growth.doublingscomp<-with(x,{
                                         try.test<-try(growth.doublingscommunity<-lm(log2(Total_chlorophyll)~date)$coefficients[2])
                                         if(class(try.test)=="try-error"){growth.doublingscomp<-growth.doublingscommunity<-NA}
                                         
                                         try.test<-try(growth.doublingsChlorophytes<-lm(log2(Chlorophytes)~date)$coefficients[2])
                                         if(class(try.test)=="try-error"){growth.doublingsChlorophytes<-NA}
                                         
                                         try.test<-try(growth.doublingsDiatoms<-lm(log2(Diatoms)~date)$coefficients[2])
                                         if(class(try.test)=="try-error"){ growth.doublingsDiatoms<-NA}
                                         
                                         try.test<-try(growth.doublingsCryptophytes<-lm(log2(Cryptophytes)~date)$coefficients[2])
                                         if(class(try.test)=="try-error"){growth.doublingsCryptophytes<-NA}
                                         
                                         try.test<-try(growth.doublingsCyanobacteria<-lm(log2(Cyanobacteria)~date)$coefficients[2])
                                         if(class(try.test)=="try-error"){growth.doublingsCyanobacteria<-NA}
                                         
                                         try.test<-try(comp.Chlorophytes<-lm(log2(Chlorophytes.perc)~date)$coefficients[2])
                                         if(class(try.test)=="try-error"){comp.Chlorophytes<-NA}
                                         
                                         try.test<-try(comp.Cryptophytes<-lm(log2(Cryptophytes.perc)~date)$coefficients[2])
                                         if(class(try.test)=="try-error"){comp.Cryptophytes<-NA}
                                         
                                         try.test<-try(comp.Diatoms<-lm(log2(Diatoms.perc)~date)$coefficients[2])
                                         if(class(try.test)=="try-error"){comp.Diatoms<-NA}
                                         
                                         try.test<-try(comp.Cyanobacteria<-lm(log2(Cyanobacteria.perc)~date)$coefficients[2])
                                         if(class(try.test)=="try-error"){comp.Cyanobacteria<-NA}
                                         
                                         data.frame(growth.doublingscommunity,
                                                    growth.doublingsChlorophytes,
                                                    growth.doublingsDiatoms,
                                                    growth.doublingsCryptophytes,
                                                    growth.doublingsCyanobacteria,
                                                    comp.Chlorophytes,
                                                    comp.Cryptophytes,
                                                    comp.Diatoms,
                                                    comp.Cyanobacteria)})
                                                    
                                                    return(growth.doublingscomp)})






box.p<-function(var.interest){
  p<-qplot(data=growth.doublingscomp.fertilized,
           x=CO2,
           y=get(var.interest),
           ylab=var.interest,
           colour=experiment)+
    geom_boxplot()
  print(p)
  
  fit<-with(growth.doublingscomp.fertilized, aov(get(var.interest)~CO2+experiment))
  sum.fit<-summary(fit)
  grid.newpage()
  grid.table(xtable(sum.fit))
  
  sum.mean<-data.frame(mean.ambient=mean(growth.doublingscomp.fertilized[growth.doublingscomp.fertilized$CO2=="Ambient CO2", var.interest], na.rm=T),
                       mean.high=mean(growth.doublingscomp.fertilized[growth.doublingscomp.fertilized$CO2=="High CO2", var.interest], na.rm=T))
  
  grid.newpage()
  grid.table(xtable(sum.mean))
}


var.list<-c("growth.doublingscommunity", "growth.doublingsChlorophytes", "growth.doublingsDiatoms", 
            "growth.doublingsCryptophytes", "growth.doublingsCyanobacteria", "comp.Chlorophytes", 
            "comp.Cryptophytes", "comp.Diatoms", "comp.Cyanobacteria")

l_ply(.data=var.list,
      .fun=box.p)



graphics.off()