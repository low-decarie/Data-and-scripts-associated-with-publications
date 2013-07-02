rm(list=ls())

library(ggplot2)
library(plyr)
library(xtable)

#set working directory
setwd("/Users/LowDecarie/Documents/PhD/Reports/Manuscripts/Active manuscripts/Global Change Biology (ecology)/Analysis")


pair.culture<-read.csv("./data/pair_competition.csv")
pure.culture<-read.csv("./data/pure_growth.csv")

pure.means<-with(pure.culture, aggregate(x=list(growth.mean=growth), by=list(Assay.CO2=Assay.CO2, species=species, assay=assay), FUN=mean, na.rm=T))

pure.dif<-data.frame(pure.means[pure.means$Assay.CO2=="ambient",-4], growth.dif=pure.means$growth.mean[pure.means$Assay.CO2=="rising"]-pure.means$growth.mean[pure.means$Assay.CO2=="ambient"])

pair.means<-aggregate(x=list(comp.mean=pair.culture$comp.coef.species),by=list(species=pair.culture$species, Assay.CO2=pair.culture$Assay.CO2, assay=pair.culture$assay),FUN=mean, na.rm=T)

pair.dif<-data.frame(pair.means[pair.means$Assay.CO2=="ambient",-4], comp.dif=pair.means$comp.mean[pair.means$Assay.CO2=="rising"]-pair.means$comp.mean[pair.means$Assay.CO2=="ambient"])

response<-merge(pure.dif[,-1], pair.dif[,-2])

response$assay<-paste("Assay",response$assay)

R2<-function(growth.dif, comp.dif){
	lm.stats<-summary(lm(comp.dif~growth.dif))
	r.squared<-data.frame(lm.stats["r.squared"])
	return(r.squared)
	}
p.value<-function(growth.dif, comp.dif){
	lm.stats<-summary(lm(comp.dif~growth.dif))
	p<-data.frame(lm.stats$coefficients[8])
	return(p)
	}

r.squared<-ddply(.data=response, .variables="assay", summarise, r.squared=R2(growth.dif, comp.dif), .parallel=T)
p.value <-ddply(.data=response, .variables="assay", summarise, p= p.value(growth.dif, comp.dif), .parallel=T)

lm.stats<-merge(r.squared, p.value)

qplot(x=growth.dif, y=comp.dif, data=response, colour=species, asp=1, geom=c("point", "smooth"), method="lm", xlab="Growth response", ylab="Competition response in pair culture")+scale_colour_manual("species", c("Navicula"="brown","Nitzschia"="sandybrown","Scenedesmus"="green","Pseudokirchneriella"="green4","Synechococcus"="turquoise1","Anabaena"="turquoise4"))+aes(group = 1)+facet_grid(facets=.~ assay, scale="free")+scale_x_continuous(breaks=seq(-0.5, 0.3, by=0.05), labels=seq(-0.5, 0.3, by=0.05))+opts(axis.text.x=theme_text(size=7))+opts(legend.position = "bottom")
ggsave("pure_to_pair.pdf")