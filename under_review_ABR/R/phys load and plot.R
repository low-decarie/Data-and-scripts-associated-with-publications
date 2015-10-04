#Clear memory
rm(list=ls())

#Load libraries
require(plyr)
require(ggplot2)
require(reshape)



pdf("./Plots/physical parameters.pdf", width=12, height=12)


phys.dat<-read.csv("./Data/physical par.csv")

phys.dat$date<-as.Date(phys.dat$date, format="%Y-%m-%d")

phys.dat$input.type<-NA
phys.dat$input.type[phys.dat$sampleID %in% c(2,4,5,7)]<-"raw"

phys.dat$input.type[phys.dat$sampleID %in% c(1,3,6,8)]<-"sterile"

phys.dat$treatment<-NA
phys.dat$treatment[phys.dat$sampleID %in% c(1,2)]<-"HCl"
phys.dat$treatment[phys.dat$sampleID %in% c(3,4)]<-"NaOH"
phys.dat$treatment[phys.dat$sampleID %in% c(5,6)]<-"Control"
phys.dat$treatment[phys.dat$sampleID %in% c(7,8)]<-"NaCl"

phys.dat<-phys.dat[!is.na(phys.dat$date),]

plot.par<-function(selected.par="pH"){
p<-qplot(data=phys.dat,
         x=date,
         y=get(selected.par),
         ylab=selected.par,
         colour=treatment,
         linetype=input.type,
         xlim=c(min(phys.dat$date), max(phys.dat$date)),
         shape=sampleID)+
           scale_shape_manual(values=1:9)

p<-p+geom_line(aes(group=sampleID))

print(p)

}

plot.par()

plot.par("salt")
plot.par("cond")
plot.par("temp")


graphics.off()