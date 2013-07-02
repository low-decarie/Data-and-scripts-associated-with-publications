rm(list=ls())

setwd("/Users/LowDecarie/Documents/MSc/Reports/Global Change Biology (ecology)/Analysis")
options(contrasts=c("contr.sum","contr.poly"))
pair.culture<-read.csv("./data/pair_competition.csv")

library(xtable)
library(car)

digits.table<-c(2,2,2,3,4,0,0,0)

fct.comp.list<-unique(pair.culture$Fct.competition)

for(j in 1:3){
	
	selected.fct.comp<-pair.culture[pair.culture$Fct.competition==fct.comp.list[j],]
	
	for(i in 1:6){
	
	selected<-selected.fct.comp[selected.fct.comp$assay==i,]
	selected<-na.omit(selected)
	
	if(i==1 & j==1) appending=F  else appending=T
	
	
	fit<-aov(selected$comp.coef~selected$Assay.CO2*selected$species*selected$competitor)
	
		anova.fit<-Anova(fit, type="III")
anova.fit$sig.[anova.fit[,"Pr(>F)"]<0.05]<-"***"
anova.fit$"cor. sig. 6 tests "[anova.fit[,"Pr(>F)"]<0.0083]<-"***"
anova.fit$"cor. sig. 18 tests "[anova.fit[,"Pr(>F)"]<0.0028]<-"***"
rownames(anova.fit)<-c("Intercept","CO2 treatment", "Species from first group (Focal Species)", "Species from second group (Competitor Species)", "CO2 treatment and Focal Species interaction",  "CO2 treatment and Competitor Species interaction", "Focal and Competitor Species interaction", "CO2 treatment, Focal and Competitor Species interaction", "Residuals")

	
	print(xtable(anova.fit,caption=paste("Competitions between ", fct.comp.list[j]," in assay ", i), digits=digits.table), append= appending, file="./Analysis_outputs/3_anova fct competitions.html", type="html", caption.placement="top")	
	}
	}


