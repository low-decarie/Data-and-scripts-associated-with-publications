#Clear memory
rm(list=ls())

#Load libraries
require(plyr)
require(ggplot2)
require(reshape)
require(gridExtra)
require(digest)


#Load ggplot2 fix
source_gist("https://gist.github.com/4578531")

pdf("./Plots/physical parameters.pdf", width=10, height=4)

source("./R/theme.R")
theme_set(theme_minimal())


load(file="./Outputs/Physical data.RData")

plot.par<-function(selected.par="pH", ylab="pH"){

        p<-qplot(data=phys.dat,
                 x=date,
                 y=get(selected.par),
                 linetype=fertilizer,
                 colour=CO2,
                 shape=input,
                 alpha=I(0.3),
                 ylab=ylab,
                 xlab="Date")+
                   scale_colour_manual(values=c("Ambient CO2"="blue",
                                       "High CO2"="red",
                                       "Lake"="black"))+
                  scale_shape_manual(values=c("Did not receive input"=1,
                                                                     "Received input"=19,
                                                                     "Lake"=3))
        
                                                                       
                                                                       
        p<-p+geom_line(aes(group=sampleID.exp), stat="summary", fun.y = "mean", 
                       alpha=I(0.3))
        p<-p+geom_line(aes(group=treatment) , stat="summary", fun.y = "mean",
                       alpha=I(0.6), size=I(2),
                       show_guide=F)
        
        p<-p+facet_grid(~experiment, scale="free", space="free")
        p<-p+ theme(legend.position = "right", axis.text.x=element_text(angle=-90, hjust=0)) 
        
        
        fertilization.addition<-data.frame(date=as.numeric(as.Date("13-09-2012", "%d-%m-%Y")),
                                           experiment="Experiment 2")
        
        fertilization.addition$experiment<-factor(fertilization.addition$experiment, levels=unique(phys.dat$experiment))
        
        p<-p+geom_vline(aes(xintercept =date,
                            linetype="Fertilizer addition"), data=fertilization.addition)
        
        
        print(p)
        }

plot.par()

plot.par("CO2_raw", ylab=expression(paste("Raw CO"[2], " measurement (ppm)")))
plot.par("CO2_calc", ylab=expression(paste("Aqueous CO"[2], " concentration (ppm)")))
plot.par("cond", ylab="Conductivity (uS)")
plot.par("temp", ylab="Temperature (°C)")

col.means<-numcolwise(median)
col.sd<-numcolwise(sd)

phys.dat.mean<-ddply(.data=phys.dat,
                     .variables=c("experiment.fert", "fertilizer", "input", "CO2"),
                     function(x) col.means(x, na.rm=T))

phys.dat.sub.mean<-ddply(.data=phys.dat,
                         .variables=c("experiment.fert", "fertilizer", "CO2"),
                         function(x) col.means(x, na.rm=T))
grid.newpage()
grid.table(format(phys.dat.sub.mean, digits=3))


phys.dat.sub.sd<-ddply(.data=phys.dat,
                     .variables=c("experiment.fert", "fertilizer", "CO2"),
                     function(x) col.sd(x, na.rm=T))
grid.newpage()
grid.table(format(phys.dat.sub.sd, digits=3))
                     


q.plot<-qplot(data=phys.dat,
              x=log(CO2_calc),
              y=pH,
              colour=CO2)+
                scale_colour_manual(values=c("Ambient CO2"="blue",
                                             "High CO2"="red",
                                             "Lake"="black"))+
                  geom_smooth(method="lm", se=F)

print(q.plot)

fit<-with(phys.dat,
     lm(pH~log(CO2_calc)))

summary(fit)


mean(phys.dat$CO2_calc[phys.dat$CO2=="Ambient CO2"], na.rm=T)

mean(phys.dat$CO2_calc[phys.dat$CO2=="High CO2"], na.rm=T)

mean(phys.dat$pH[phys.dat$CO2=="Ambient CO2"], na.rm=T)-mean(phys.dat$pH[phys.dat$CO2=="High CO2"], na.rm=T)

mean(phys.dat$pH[phys.dat$fertilizer=="Not fertilized" & phys.dat$experiment=="Experiment 3"], na.rm=T)-mean(phys.dat$pH[phys.dat$fertilizer=="Fertilized" & phys.dat$experiment=="Experiment 3"], na.rm=T)

mean(phys.dat$CO2_calc[phys.dat$fertilizer=="Fertilized" & phys.dat$experiment=="Experiment 3"], na.rm=T)-mean(phys.dat$CO2_calc[phys.dat$fertilizer=="Not fertilized" & phys.dat$experiment=="Experiment 3"], na.rm=T)




graphics.off()