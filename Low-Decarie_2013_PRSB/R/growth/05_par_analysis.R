readings.para$Species.transplant<-with(readings.para, paste(Species, transplant, media))

Species.transplant.list<-unique(readings.para$Species.transplant)


for(i in Species.transplant.list){

print(i)
selected<-readings.para[readings.para$Species.transplant==i,]

print(summary(aov(data=selected,exp.r~assayCO2*CO2history+ Error(Line))))

}