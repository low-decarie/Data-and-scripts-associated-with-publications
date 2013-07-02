rm(list=ls())
options(contrasts=c("contr.helmert","contr.poly"))
library(ggplot2)
library(plyr)

setwd("/Users/LowDecarie/Documents/PhD/Reports/Manuscripts/Active manuscripts/Global Change Biology (ecology)/Analysis")

pure.culture<-read.csv("./data/pure_growth.csv")
pair.culture<-read.csv("./data/pair_competition.csv")

qplot(data=pure.culture, x=assay, y=growth, colour=Assay.CO2, facets=species~., geom=c("point", "smooth"))+scale_colour_manual("Assay.CO2",c("ambient"="blue", "rising"="red"))

means<-aggregate(data=pure.culture, x=list(growth= pure.culture$growth), by=list(species=pure.culture$species, Assay.CO2=pure.culture$Assay.CO2, assay=pure.culture$assay, group=pure.culture$group), FUN=mean)

response<-data.frame(means[means$Assay.CO2=="rising",-5], response=means[means$Assay.CO2=="rising","growth"]-means[means$Assay.CO2=="ambient","growth"])

qplot(data=response, x=assay, y=response, colour=species, geom=c("point", "smooth"),se=F, facets=group~., ylim=c(0,0.25))+scale_colour_manual("species", c("Navicula"="brown","Nitzschia"="sandybrown","Scenedesmus"="green","Pseudokirchneriella"="green4","Synechococcus"="turquoise1","Anabaena"="turquoise4"))+aes(group=species)

pair.culture$NA.label<-"keep"
pair.culture$NA.label[is.na(pair.culture$comp.coef.species)]<-"omit"
pair.culture.na.rm<-pair.culture[pair.culture$NA.label!="omit",]

qplot(data= pair.culture.na.rm, x=CO2.of.rising, y= comp.coef.species, colour= Assay.CO2, facets=.~ species, geom=c("point", "smooth"), method="lm", ylim=c(-0.4,0.4), xlab="Atmospheric CO2 in high CO2 treatment chamber (ppm)", ylab="Competition coefficient")+scale_colour_manual("Assay.CO2",c("ambient"="blue", "rising"="red"))+geom_hline(yintercept = 0, colour = "#FFFFFF75", size =2)


slope.fun<-function(CO2.of.rising, comp.coef.species){
	lm.fit<-lm(comp.coef.species~ CO2.of.rising)
	slope<-lm.fit$coefficients["CO2.of.rising"]
	}


pair.lm<-ddply(.data=pair.culture, .variable=c("species", "Assay.CO2"), slope=slope.fun(CO2.of.rising, comp.coef.species), .fun= transform)