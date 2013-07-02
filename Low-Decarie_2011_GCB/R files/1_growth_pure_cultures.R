rm(list=ls())
options(contrasts=c("contr.helmert","contr.poly"))
library(xtable)
library(car)

pure.culture<-read.csv("./data/pure_growth.csv")

species.list<-unique(pure.culture$species)

pure.culture$assay<-as.factor(pure.culture$assay)


treat.CO2.conc<-c(560,680,800,920,1000,1000)

for(j in 1:6){
	
selected.pure.culture<-pure.culture[pure.culture$assay==j,]

if(j==1){app<-"F"} else {app<-"T"}

fit<-aov(formula=growth~Assay.CO2*species, data=selected.pure.culture)

table.fit<-Anova(fit)
table.fit$sig.[table.fit[,"Pr(>F)"]<0.05]<-"***"
table.fit$"cor. sig. 6 tests "[table.fit[,"Pr(>F)"]<0.0083]<-"***"
table.fit$"cor. sig. 18 tests "[table.fit[,"Pr(>F)"]<0.0028]<-"***"

rownames(table.fit)<-c("CO2 treatment", "Species", "CO2 treatement by Species interaction", "Residuals")

print(xtable(table.fit,caption=paste("Assay", j, "( treatment at ", treat.CO2.conc[j], "ppm CO2)"), digits=c(0,3,0,3,4,0,0,0)), append=app, file="./Analysis_outputs/1a_pure_culture_by_assay.html", type="html", caption.placement="top")

}

for(k in species.list){

selected.pure.culture<-pure.culture[pure.culture$species==k,]

if(which(species.list==k)==1){app<-"F"} else {app<-"T"}

fit<-aov(formula=growth~Assay.CO2*assay, data=selected.pure.culture)


table.fit<-Anova(fit)
table.fit$sig.[table.fit[,"Pr(>F)"]<0.05]<-"***"
table.fit$"cor. sig. 6 tests "[table.fit[,"Pr(>F)"]<0.0083]<-"***"
table.fit$"cor. sig. 18 tests "[table.fit[,"Pr(>F)"]<0.0028]<-"***"

rownames(table.fit)<-c("CO2 treatment", "Assay/[CO2] of treatmet", "CO2 treatement by Assay/[CO2] interaction", "Residuals")

print(xtable(table.fit,caption=paste(k), digits=c(0,3,0,3,4,0,0,0)), append=app, file="./Analysis_outputs/1b_pure_culture_by_species.html", type="html", caption.placement="top")

}


for(j in 1:6){
	
selected.pure.culture<-pure.culture[pure.culture$assay==j,]

if(j==1){app<-"F"} else {app<-"T"}

selected.pure.culture<-with(selected.pure.culture,aggregate(x=list(growth=growth), by=list(Assay.CO2 =Assay.CO2, species= species, group= group), FUN=mean))

fit<-aov(formula=growth~Assay.CO2*group, data=selected.pure.culture)

table.fit<-Anova(fit)
table.fit$sig.[table.fit[,"Pr(>F)"]<0.05]<-"***"
table.fit$"cor. sig. 6 tests "[table.fit[,"Pr(>F)"]<0.0083]<-"***"
table.fit$"cor. sig. 18 tests "[table.fit[,"Pr(>F)"]<0.0028]<-"***"

rownames(table.fit)<-c("CO2 treatment", "Taxonomic group", "CO2 treatement by Taxonomic group interaction", "Residuals")


print(xtable(table.fit,caption=paste("Assay", j, "( treatment at ", treat.CO2.conc[j], "ppm CO2)"), digits=c(0,3,0,3,4,0,0,0)), append=app, file="./Analysis_outputs/1c_pure_culture_by_assay_for_groups.html", type="html", caption.placement="top")

}