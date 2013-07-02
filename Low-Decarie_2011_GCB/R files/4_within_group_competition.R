rm(list=ls())
library(xtable)
options(contrasts=c("contr.helmert","contr.poly"))

setwd("/Users/LowDecarie/Documents/MSc/Reports/Global Change Biology (ecology)/Analysis")

chlorophytes<-read.csv("./data/within group competition/Chlorophytes.csv")

scenedesmus<-chlorophytes[chlorophytes$species=="SCENEDESMUS",]

fit<-aov(scenedesmus$competition~scenedesmus$CO2)

fit.info<-anova(fit)
fit.info$sig.[fit.info[,"Pr(>F)"]<0.05]<-"***"

rownames(fit.info)<-c("CO2 Treatment", "Residuals")

print(xtable(fit.info,caption="Chlorphytes"), append=F, file="./Analysis_outputs/4_competition_within_same_group.html", type="html", caption.placement="top")

diatoms<-read.csv("./data/within group competition/Diatoms.csv")

navicula<-diatoms[diatoms$species=="Navicula",]

fit<-aov(navicula$competition~navicula$CO2)


fit.info<-anova(fit)
fit.info$sig.[fit.info[,"Pr(>F)"]<0.05]<-"***"

rownames(fit.info)<-c("CO2 Treatment", "Residuals")

print(xtable(fit.info,caption="Diatoms"), append=T, file="./Analysis_outputs/4_competition_within_same_group.html", type="html", caption.placement="top")


cyano<-read.csv("./data/within group competition/Cyanobacteria.csv")

syn<-cyano[cyano$species=="Synechococcus",]

fit<-aov(syn$competition~syn$CO2)

fit.info<-anova(fit)
fit.info$sig.[fit.info[,"Pr(>F)"]<0.05]<-"***"

rownames(fit.info)<-c("CO2 Treatment", "Residuals")

print(xtable(fit.info,caption="Cyanobacteria"), append=T, file="./Analysis_outputs/4_competition_within_same_group.html", type="html", caption.placement="top")
