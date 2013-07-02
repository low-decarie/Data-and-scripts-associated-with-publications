rm(list=ls())

#set working directory
setwd("/Users/LowDecarie/Documents/MSc/Reports/Global Change Biology (ecology)/Analysis")

#open pdf
pdf("./Figures/9_Competitive response in full community and in pair competition.pdf")

library(xtable)


#read the pair competition file
pair.culture<-read.csv("./data/pair_competition.csv")

#select only last assay
pair.culture<-pair.culture[pair.culture$assay==6,]


#read the full community file
full.com<-read.csv("./data/full_community_long.csv")


#aggregate by species and CO2 treatment
pair.means<-aggregate(x=list(comp.mean=pair.culture$comp.coef.species),by=list(species=pair.culture$species, Assay.CO2=pair.culture$Assay.CO2),FUN=mean)
pair.dif<-data.frame(species=pair.means$species[pair.means$Assay.CO2=="ambient"], comp.dif=pair.means$comp.mean[pair.means$Assay.CO2=="rising"]-pair.means$comp.mean[pair.means$Assay.CO2=="ambient"])
full.means<-aggregate(x=list(comp.mean=full.com$competition),by=list(species=full.com$species, CO2=full.com$CO2),FUN=mean)
full.dif<-data.frame(species=full.means$species[full.means$CO2=="ambient"], comp.dif=full.means$comp.mean[full.means$CO2=="high"]-full.means$comp.mean[full.means$CO2=="ambient"])



#add species codes for point character and colors
species.list<-c("Navicula","Nitzschia","Scenedesmus", "Pseudokirchneriella","Synechococcus","Anabaena")
color.list<-c("brown","sandybrown","green","green4","turquoise1","turquoise4")
for(i in 1:6){
pair.dif$species.code[pair.dif$species==species.list[i]]<-i
full.dif$species.code[full.dif$species==species.list[i]]<-i
pair.dif$color[pair.dif$species==species.list[i]]<-color.list[i]
full.dif$color[full.dif$species==species.list[i]]<-color.list[i]
}

#plot the results
plot(x=pair.dif$comp.dif, y=full.dif$comp.dif, pch=pair.dif$species.code, col=pair.dif$color, cex=2, xlab="Average competitive response in pairwise competition", ylab="Competitive response in full community", main="Competitive response in full community and in pairwise competition", lwd=4)
abline(h=0)
abline(v=0)

legend(0.05,-0.05, col=color.list, pch=1:6, legend=species.list, title="Species")


#linear model fit
model<-lm(full.dif$comp.dif~pair.dif$comp.dif)

#add linear model line to graph
abline(model, lwd=2,lty=2)

adj.r.squarred<-summary(model)$adj.r.squared
text(0.1,0.1, paste("Adjusted R squarred=",format(adj.r.squarred, digits=3, nsmall=3)))
text(0.1,0.05, paste("P=",format(summary(model)$coefficients[8], digits=3, nsmall=3)))

model.coef<-summary(model)$coefficients
rownames(model.coef)<-c("Intercept", "Competition response in pair culture")

print(xtable(model.coef, caption="Competition response in pair competition predicting competition response in full community"), append=F, file="./Analysis_outputs/9_pair_prediction_full_community.html", type="html", caption.placement="top")	
Rsquared<-as.data.frame(summary(model)$r.squared)

print(xtable(Rsquared, caption="R squared", digits=4), append=T, file="./Analysis_outputs/9_pair_prediction_full_community.html", type="html", caption.placement="top")



par(mfrow=c(2,2))
plot(model)
par(mfrow=c(1,1))



#######################################################################################
#######################################################################################
##		Removing Nitzschia in competition with Pseudokirchneriella
##
##		
#######################################################################################
#######################################################################################




pair.culture<-pair.culture[pair.culture$Competition!=unique(pair.culture$Competition)[6],]

#select only last assay
pair.culture<-pair.culture[pair.culture$assay==6,]



#aggregate by species and CO2 treatment
pair.means<-aggregate(x=list(comp.mean=pair.culture$comp.coef.species),by=list(species=pair.culture$species, Assay.CO2=pair.culture$Assay.CO2),FUN=mean)
pair.dif<-data.frame(species=pair.means$species[pair.means$Assay.CO2=="ambient"], comp.dif=pair.means$comp.mean[pair.means$Assay.CO2=="rising"]-pair.means$comp.mean[pair.means$Assay.CO2=="ambient"])
full.means<-aggregate(x=list(comp.mean=full.com$competition),by=list(species=full.com$species, CO2=full.com$CO2),FUN=mean)
full.dif<-data.frame(species=full.means$species[full.means$CO2=="ambient"], comp.dif=full.means$comp.mean[full.means$CO2=="high"]-full.means$comp.mean[full.means$CO2=="ambient"])



#add species codes for point character and colors
species.list<-c("Navicula","Nitzschia","Scenedesmus", "Pseudokirchneriella","Synechococcus","Anabaena")
color.list<-c("brown","sandybrown","green","green4","turquoise1","turquoise4")
for(i in 1:6){
pair.dif$species.code[pair.dif$species==species.list[i]]<-i
full.dif$species.code[full.dif$species==species.list[i]]<-i
pair.dif$color[pair.dif$species==species.list[i]]<-color.list[i]
full.dif$color[full.dif$species==species.list[i]]<-color.list[i]
}

#plot the results
plot(x=pair.dif$comp.dif, y=full.dif$comp.dif, pch=pair.dif$species.code, col=pair.dif$color, cex=2, xlab="Average competitive response in pairwise competition", ylab="Competitive response in full community", main="Competitive response in full community and in pairwise competition", sub="without Nitzschia in competition with Pseudokirchneriella", lwd=4)
abline(h=0)
abline(v=0)

legend(0.02,-0.05, col=color.list, pch=1:6, legend=species.list, title="Species")


#linear model fit
model<-lm(full.dif$comp.dif~pair.dif$comp.dif)

#add linear model line to graph
abline(model, lwd=2,lty=2)

adj.r.squarred<-summary(model)$adj.r.squared
text(0.1,0.1, paste("Adjusted R squarred=",format(adj.r.squarred, digits=3, nsmall=3)))
text(0.1,0.05, paste("P=",format(summary(model)$coefficients[8], digits=3, nsmall=3)))


model.coef<-summary(model)$coefficients
rownames(model.coef)<-c("Intercept", "Competition response in pair culture")

print(xtable(model.coef, caption="Competition response in pair competition predicting competition response in full community (Removing Nitzschia in competition with Pseudokirchneriella)"), append=T, file="./Analysis_outputs/9_pair_prediction_full_community.html", type="html", caption.placement="top")

Rsquared<-as.data.frame(summary(model)$r.squared)

print(xtable(Rsquared, caption="R squared", digits=4), append=T, file="./Analysis_outputs/9_pair_prediction_full_community.html", type="html", caption.placement="top")


par(mfrow=c(2,2))
plot(model)
par(mfrow=c(1,1))

#######################################################################################
#######################################################################################
##		Removing Synechococcus
##
##		
#######################################################################################
#######################################################################################




pair.culture<-pair.culture[pair.culture$species!="Synechococcus",]
full.com<-full.com[full.com$species!="Synechococcus",]

#select only last assay
pair.culture<-pair.culture[pair.culture$assay==6,]


#aggregate by species and CO2 treatment
pair.means<-aggregate(x=list(comp.mean=pair.culture$comp.coef.species),by=list(species=pair.culture$species, Assay.CO2=pair.culture$Assay.CO2),FUN=mean)
pair.dif<-data.frame(species=pair.means$species[pair.means$Assay.CO2=="ambient"], comp.dif=pair.means$comp.mean[pair.means$Assay.CO2=="rising"]-pair.means$comp.mean[pair.means$Assay.CO2=="ambient"])
full.means<-aggregate(x=list(comp.mean=full.com$competition),by=list(species=full.com$species, CO2=full.com$CO2),FUN=mean)
full.dif<-data.frame(species=full.means$species[full.means$CO2=="ambient"], comp.dif=full.means$comp.mean[full.means$CO2=="high"]-full.means$comp.mean[full.means$CO2=="ambient"])



#add species codes for point character and colors
species.list<-c("Navicula","Nitzschia","Scenedesmus", "Pseudokirchneriella","Synechococcus","Anabaena")
color.list<-c("brown","sandybrown","green","green4","turquoise1","turquoise4")
for(i in 1:6){
pair.dif$species.code[pair.dif$species==species.list[i]]<-i
full.dif$species.code[full.dif$species==species.list[i]]<-i
pair.dif$color[pair.dif$species==species.list[i]]<-color.list[i]
full.dif$color[full.dif$species==species.list[i]]<-color.list[i]
}

#plot the results
plot(x=pair.dif$comp.dif, y=full.dif$comp.dif, pch=pair.dif$species.code, col=pair.dif$color, cex=2, xlab="Average competitive response in pairwise competition", ylab="Competitive response in full community", main="Competitive response in full community and in pairwise competition", sub="without Synechococcus", lwd=4)
abline(h=0)
abline(v=0)

legend(0.02,-0.05, col=color.list, pch=1:6, legend=species.list, title="Species")


#linear model fit
model<-lm(full.dif$comp.dif~pair.dif$comp.dif)

#add linear model line to graph
abline(model, lwd=2,lty=2)

adj.r.squarred<-summary(model)$adj.r.squared
text(0.1,0.1, paste("Adjusted R squarred=",format(adj.r.squarred, digits=3, nsmall=3)))
text(0.1,0.05, paste("P=",format(summary(model)$coefficients[8], digits=3, nsmall=3)))


par(mfrow=c(2,2))
plot(model)
par(mfrow=c(1,1))


model.coef<-summary(model)$coefficients
rownames(model.coef)<-c("Intercept", "Competition response in pair culture")

print(xtable(model.coef, caption="Competition response in pair competition predicting competition response in full community (Removing Nitzschia in competition with Pseudokirchneriella & Synechococcus)"), append=T, file="./Analysis_outputs/9_pair_prediction_full_community.html", type="html", caption.placement="top")

Rsquared<-as.data.frame(summary(model)$r.squared)

print(xtable(Rsquared, caption="R squared", digits=4), append=T, file="./Analysis_outputs/9_pair_prediction_full_community.html", type="html", caption.placement="top")



dev.off()
