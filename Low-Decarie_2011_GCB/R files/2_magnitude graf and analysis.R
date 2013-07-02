#Analysis of the amplitude (absolute value) of competition

#####
#	House Cleaning
#####
	#Clear R memory
	rm(list=ls())
	#options(contrasts=c("contr.poly","contr.poly"))

	#Set working directory
	setwd("/Users/LowDecarie/Documents/MSc/Reports/Global Change Biology (ecology)/Analysis")

	#Read data file
	pair.culture<-read.csv("./data/pair_competition.csv")

	#Load libraries
	library(xtable)
	library(car)
	
	pair.culture<-na.omit(pair.culture)
	
	pair.culture<-with(pair.culture,aggregate(x=list(comp.coef=comp.coef),by=list(assay=assay,Assay.CO2=Assay.CO2, Competition.code= Competition.code), FUN=mean))



#####
#	Analysis
#####

	#Calculate the magnitude (absolute value) of the competition coefficients
	pair.culture$comp.coef.abs<-abs(pair.culture$comp.coef)

	#Analyze each assay
	for(i in 1:6){
	selected<-pair.culture[pair.culture$assay==i,]

	if(i==1){app<-"F"}else{app<-"T"}
	
	#fit an ANOVA to the magnitude of competition coefficient with CO2 as the main fator
	fit<-aov(selected$comp.coef.abs~selected$Assay.CO2)
	

	table.fit<-Anova(fit)
	table.fit$sig.[table.fit[,"Pr(>F)"]<0.05]<-"***"
	table.fit$"sig. 6 tests Sidak"[table.fit[,"Pr(>F)"]<0.0085]<-"***"
	
	row.names(table.fit)<-c("CO2 Treatment", "Residuals")
	print(xtable(table.fit,caption=paste("Assay ", i), digits=c(0,3,0,3,4,0,0,0)), append=app, file="./Analysis_outputs/2_anova_magnitude.html", type="html", caption.placement="top")

	}


with(pair.culture[pair.culture$Assay.CO2=="rising",], boxplot(comp.coef.abs~assay, col="#FF000050", ylim=c(0,0.4), notch=T))
par(new=T)
with(pair.culture[pair.culture$Assay.CO2=="ambient",], boxplot(comp.coef.abs~assay, col="#0000FF50", ylim=c(0,0.4), notch=T))
ambient.lm<-with(pair.culture[pair.culture$Assay.CO2=="ambient",], lm(comp.coef.abs~assay))
rising.lm<-with(pair.culture[pair.culture$Assay.CO2=="rising",], lm(comp.coef.abs~assay))
abline(ambient.lm, col="blue", lwd=2)
abline(h=mean(pair.culture$comp.coef.abs[pair.culture$Assay.CO2=="ambient"]), col="blue", lwd=1, lty=2)
abline(rising.lm, col="red", lwd=2)

library(nlme)
repeated.fit<-lme(fixed=comp.coef.abs~Assay.CO2*assay, random=~1|Competition.code, data=pair.culture)
anova(repeated.fit)