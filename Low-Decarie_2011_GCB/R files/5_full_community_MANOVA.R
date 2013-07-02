rm(list=ls())

#set working directory
setwd("/Users/LowDecarie/Documents/MSc/Reports/Global Change Biology (ecology)/Analysis")


#read the full community file
full.com<-read.csv("./data/full_community.csv")


#Load libraries
library(xtable)

attach(full.com)

species<-cbind(Navicula,   Nitzschia, Scenedesmus, Pseudokirchneriella, Synechococcus, Anabaena)

model<-manova(species~CO2)
model.summary<-summary(model)$stats
row.names(model.summary)<-c("CO2 treatment", "Residuals")
	print(xtable(model.summary,caption="Full community MANOVA", digits=c(0,0,0,3,2,2,4)), append=F, file="./Analysis_outputs/8_Full_community_MANOVA.html", type="html", caption.placement="top")



model.species<-summary.aov(model)

for(i in 1:6){
	print(xtable(model.species[i],caption=colnames(species)[i], digits=c(0,0,3,2,2,4)), append=T, file="./Analysis_outputs/8_Full_community_MANOVA.html", type="html", caption.placement="top")}