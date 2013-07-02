#Housekeeping
  rm(list=ls())

#Load file
  readings<-read.csv(file="./Data/CO2media.csv")

# Curate file

  #Format variables
  readings$generations<-as.numeric(readings$generations)
  readings$regime<-factor(readings$regime)
  readings$time<-as.Date(readings$time, format="%m-%d-%Y %H:%M:%S")
  
  #Remove NAs and negative values
  readings<-readings[readings$ABS>=0,]
  #readings<-readings[!(readings$Time>3 & readings$ABS<0.15),]
  readings<-readings[!is.na(readings$ABS),]
  readings<-readings[!is.na(readings$transplant),]
  
  #Create a unique identifier for each culture and each species/regime pair
  readings$culture<-with(readings, paste(Species, Line, regime, transplant, assayCO2))
  readings$sp.regime<-with(readings, paste(Species, regime))

#Save curated data

save(readings, file="./Outputs/compile data.Rdata")




#########
# Inspection plots (histograms of each variable)
#########

inspect<- T

if(inspect){

library(plyr)
library(ggplot2)


pdf("./Plots/growth/error checking.pdf")

with(readings, plot(ABS~Time))


for(i in 1:length(readings)){

    col.plot<-qplot(x=readings[,i],
                    main=names(readings)[i],
                    xlab=names(readings)[i])
    print(col.plot)

}
dev.off()}