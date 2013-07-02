load("./Outputs/probe data.RData")


(mean(probe.dat$Total_chlorophyll[probe.dat$CO2=="High CO2" & probe.dat$fertilizer=="Not fertilized"], na.rm=T)-mean(probe.dat$Total_chlorophyll[probe.dat$CO2=="Ambient CO2"  & probe.dat$fertilizer=="Not fertilized"], na.rm=T))/mean(probe.dat$Total_chlorophyll[probe.dat$CO2=="Ambient CO2"  & probe.dat$fertilizer=="Not fertilized"], na.rm=T)


(mean(probe.dat$Total_chlorophyll[probe.dat$CO2=="High CO2" & probe.dat$experiment=="Experiment 1"], na.rm=T)-mean(probe.dat$Total_chlorophyll[probe.dat$CO2=="Ambient CO2" & probe.dat$experiment=="Experiment 1"], na.rm=T))/mean(probe.dat$Total_chlorophyll[probe.dat$CO2=="Ambient CO2" & probe.dat$experiment=="Experiment 1"], na.rm=T)

(mean(probe.dat$Total_chlorophyll[probe.dat$fertilizer=="Fertilized"], na.rm=T)-mean(probe.dat$Total_chlorophyll[probe.dat$fertilizer=="Not fertilized"], na.rm=T))/mean(probe.dat$Total_chlorophyll[probe.dat$fertilizer=="Not fertilized"], na.rm=T)

(mean(probe.dat$Total_chlorophyll[probe.dat$CO2=="High CO2" & probe.dat$fertilizer=="Fertilized"], na.rm=T)-mean(probe.dat$Total_chlorophyll[probe.dat$CO2=="Ambient CO2" & probe.dat$fertilizer=="Fertilized"], na.rm=T))/mean(probe.dat$Total_chlorophyll[probe.dat$CO2=="Ambient CO2" & probe.dat$fertilizer=="Fertilized"], na.rm=T)

(mean(probe.dat$Total_chlorophyll[probe.dat$CO2=="High CO2" & probe.dat$fertilizer=="Fertilized"], na.rm=T)-mean(probe.dat$Total_chlorophyll[probe.dat$CO2=="High CO2" & probe.dat$fertilizer=="Not fertilized"], na.rm=T))/mean(probe.dat$Total_chlorophyll[probe.dat$CO2=="High CO2" & probe.dat$fertilizer=="Not fertilized"], na.rm=T)

(mean(probe.dat$Total_chlorophyll[probe.dat$CO2=="High CO2" & probe.dat$fertilizer=="Fertilized"], na.rm=T)-mean(probe.dat$Total_chlorophyll[probe.dat$CO2=="Ambient CO2" & probe.dat$fertilizer=="Not fertilized"], na.rm=T))/mean(probe.dat$Total_chlorophyll[probe.dat$CO2=="Ambient CO2" & probe.dat$fertilizer=="Not fertilized"], na.rm=T)



(mean(probe.dat$Chlorophytes.perc[probe.dat$CO2=="High CO2" & probe.dat$fertilizer=="Not fertilized"], na.rm=T)-mean(probe.dat$Chlorophytes.perc[probe.dat$CO2=="Ambient CO2"  & probe.dat$fertilizer=="Not fertilized"], na.rm=T))

(mean(probe.dat$Chlorophytes.perc[probe.dat$CO2=="Ambient CO2" & probe.dat$fertilizer=="Fertilized"], na.rm=T)-mean(probe.dat$Chlorophytes.perc[probe.dat$CO2=="Ambient CO2"  & probe.dat$fertilizer=="Not fertilized"], na.rm=T))

(mean(probe.dat$Chlorophytes.perc[probe.dat$CO2=="High CO2" & probe.dat$fertilizer=="Fertilized"], na.rm=T)-mean(probe.dat$Chlorophytes.perc[probe.dat$CO2=="Ambient CO2"  & probe.dat$fertilizer=="Fertilized"], na.rm=T))


(mean(probe.dat$shannon[probe.dat$CO2=="High CO2" & probe.dat$sampleID!="lake"], na.rm=T)-mean(probe.dat$shannon[probe.dat$CO2=="Ambient CO2"  & probe.dat$sampleID!="lake"], na.rm=T))/mean(probe.dat$shannon[probe.dat$CO2=="Ambient CO2"  & probe.dat$sampleID!="lake"], na.rm=T)

(mean(probe.dat$shannon[probe.dat$fertilizer=="Fertilized" & probe.dat$sampleID!="lake"], na.rm=T)-mean(probe.dat$shannon[probe.dat$fertilizer=="Not fertilized"  & probe.dat$sampleID!="lake"], na.rm=T))/mean(probe.dat$shannon[probe.dat$fertilizer=="Not fertilized"  & probe.dat$sampleID!="lake"], na.rm=T)

(mean(probe.dat$Chlorophytes.perc[probe.dat$CO2=="High CO2" & probe.dat$sampleID!="lake"], na.rm=T)-mean(probe.dat$Chlorophytes.perc[probe.dat$CO2=="Ambient CO2"  & probe.dat$sampleID!="lake"], na.rm=T))

(mean(probe.dat$Cryptophytes.perc[probe.dat$CO2=="High CO2" & probe.dat$sampleID!="lake"], na.rm=T)-mean(probe.dat$Cryptophytes.perc[probe.dat$CO2=="Ambient CO2"  & probe.dat$sampleID!="lake"], na.rm=T))

(mean(probe.dat$Diatoms.perc[probe.dat$CO2=="High CO2" & probe.dat$sampleID!="lake"], na.rm=T)-mean(probe.dat$Diatoms.perc[probe.dat$CO2=="Ambient CO2"  & probe.dat$sampleID!="lake"], na.rm=T))

(mean(probe.dat$Cyanobacteria.perc[probe.dat$CO2=="High CO2" & probe.dat$sampleID!="lake"], na.rm=T)-mean(probe.dat$Cyanobacteria.perc[probe.dat$CO2=="Ambient CO2"  & probe.dat$sampleID!="lake"], na.rm=T))

(mean(probe.dat$Cyanobacteria.perc[probe.dat$CO2=="Ambient CO2" & probe.dat$fertilizer=="Fertilized" & probe.dat$experiment!="Experiment 1"], na.rm=T)-mean(probe.dat$Cyanobacteria.perc[probe.dat$CO2=="Ambient CO2"  & probe.dat$sampleID!="lake" & probe.dat$fertilizer=="Not fertilized" & probe.dat$experiment!="Experiment 1"], na.rm=T))


(mean(probe.dat$Cyanobacteria.perc[probe.dat$fertilizer=="Fertilized" & probe.dat$sampleID!="lake"], na.rm=T)-mean(probe.dat$Cyanobacteria.perc[probe.dat$fertilizer=="Not fertilized"  & probe.dat$sampleID!="lake"], na.rm=T))

(mean(probe.dat$Diatoms.perc[probe.dat$fertilizer=="Fertilized" & probe.dat$sampleID!="lake"], na.rm=T)-mean(probe.dat$Diatoms.perc[probe.dat$fertilizer=="Not fertilized"  & probe.dat$sampleID!="lake"], na.rm=T))

(mean(probe.dat$Diatoms.perc[probe.dat$fertilizer=="Fertilized" & probe.dat$sampleID!="lake" & probe.dat$experiment.fert=="Experiment 2 after fertilization"], na.rm=T)-mean(probe.dat$Diatoms.perc[probe.dat$fertilizer=="Not fertilized"  & probe.dat$sampleID!="lake" & probe.dat$experiment.fert=="Experiment 2 after fertilization"], na.rm=T))

(mean(probe.dat$Diatoms.perc[probe.dat$fertilizer=="Fertilized" & probe.dat$sampleID!="lake" & probe.dat$experiment.fert=="Experiment 3"], na.rm=T)-mean(probe.dat$Diatoms.perc[probe.dat$fertilizer=="Not fertilized"  & probe.dat$sampleID!="lake" & probe.dat$experiment.fert=="Experiment 3"], na.rm=T))


(mean(probe.dat$Cryptophytes.perc[probe.dat$fertilizer=="Fertilized" & probe.dat$sampleID!="lake"], na.rm=T)-mean(probe.dat$Cryptophytes.perc[probe.dat$fertilizer=="Not fertilized"  & probe.dat$sampleID!="lake"], na.rm=T))


(mean(probe.dat$Diatoms.perc[probe.dat$fertilizer=="Fertilized" & probe.dat$sampleID!="lake"], na.rm=T)-mean(probe.dat$Diatoms.perc[probe.dat$fertilizer=="Not fertilized"  & probe.dat$sampleID!="lake"], na.rm=T))
