#Housekeeping
rm(list=ls())
library(devtools)
source_url("https://raw.github.com/edielivon/Useful-R-functions/master/CO2/headspace_pCO2.R")


#Load file
CO2<-read.csv(file="./Data/CO2media.csv", as.is="date")

# Curate file

#Format variables
CO2$regime<-factor(CO2$regime)
CO2$date<-as.Date(CO2$date, format="%m-%d-%Y %H:%M:%S")

#Calculate pCO2 using equations provided by Prof. Yves Prairie (UQAM)

CO2$pCO2_calc<-with(CO2, headCO2(startCO2 = 0,
                                 raw.pCO2=CO2_media,
                                 initial.temp=initial.temperature,
                                 final.temp=final_temperature,
                                 pressure = 101.325,
                                 head.ratio = 1,
                                 salinity=salinity))



#Save values
                    
save(list="CO2", file="./Outputs/CO2media.RData")
write.csv(CO2, file="./Outputs/CO2media.csv")
                    
                    
