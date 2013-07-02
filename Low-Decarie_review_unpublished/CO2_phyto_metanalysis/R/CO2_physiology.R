rm(list=ls())
require(ggplot2)
require(plyr)
require(reshape)
library(GGally)

source("./R/theme.R")
theme_set(theme_minimal())



d <- read.csv("./Data/metaanalysis.csv")

d$RRCO2 <- with(d, log(ResponseElevatedCO2/ResponseAmbientCO2))



median.d <- ddply(.data=d,
.variable="Species",
function(x){with(x,
data.frame(
###
Carbon.Concentration.Factor=median(Carbon.Concentration.Factor,na.rm=T),
Carbon.Concentration.Factor.se=sd(Carbon.Concentration.Factor, na.rm=T)/length(na.omit(Carbon.Concentration.Factor)),
RubisCO.specifity.factor=median(RubisCO.specifity.factor,na.rm=T),
RubisCO.specifity.factor.se=sd(RubisCO.specifity.factor, na.rm=T)/length(na.omit(RubisCO.specifity.factor)),
HalfSaturationCO2=median(HalfSaturationCO2,na.rm=T),
HalfSaturationCO2.se=sd(HalfSaturationCO2, na.rm=T)/length(na.omit(HalfSaturationCO2)),
RRCO2=median(RRCO2,na.rm=T),
RRCO2.se=sd(RRCO2, na.rm=T)/length(na.omit(RRCO2))))})



median.d <- ddply(.data=d,
                .variable="Group",
function(x){with(x,
data.frame(
###
Carbon.Concentration.Factor=median(Carbon.Concentration.Factor,na.rm=T),
Carbon.Concentration.Factor.se=sd(Carbon.Concentration.Factor, na.rm=T)/length(na.omit(Carbon.Concentration.Factor)),
RubisCO.specifity.factor=median(RubisCO.specifity.factor,na.rm=T),
RubisCO.specifity.factor.se=sd(RubisCO.specifity.factor, na.rm=T)/length(na.omit(RubisCO.specifity.factor)),
HalfSaturationCO2=median(HalfSaturationCO2,na.rm=T),
HalfSaturationCO2.se=sd(HalfSaturationCO2, na.rm=T)/length(na.omit(HalfSaturationCO2)),
RRCO2=median(RRCO2,na.rm=T),
RRCO2.se=sd(RRCO2, na.rm=T)/length(na.omit(RRCO2))))})

p <- qplot(data=median.d[!is.na(median.d$Carbon.Concentration.Factor),],
           x=reorder(Group,Carbon.Concentration.Factor),
           colour=Group,
           y=Carbon.Concentration.Factor,
           ymin=Carbon.Concentration.Factor-Carbon.Concentration.Factor.se,
           ymax=Carbon.Concentration.Factor+Carbon.Concentration.Factor.se,
           geom="pointrange",
           log="y")

p <- p + theme(axis.text.x=element_text(angle=90))

print(p)


p <- qplot(data=median.d[!is.na(median.d$RubisCO.specifity.factor),],
           x=reorder(Group,RubisCO.specifity.factor),
           colour=Group,
           y=RubisCO.specifity.factor,
           ymin=RubisCO.specifity.factor-RubisCO.specifity.factor.se,
           ymax=RubisCO.specifity.factor+RubisCO.specifity.factor.se,
           geom="pointrange")

p <- p + theme(axis.text.x=element_text(angle=90))

print(p)

p <- qplot(data=median.d[!is.na(median.d$HalfSaturationCO2),],
           x=reorder(Group,HalfSaturationCO2),
           colour=Group,
           y=HalfSaturationCO2,
           ymin=HalfSaturationCO2-HalfSaturationCO2.se,
           ymax=HalfSaturationCO2+HalfSaturationCO2.se,
           geom="pointrange")

p <- p + theme(axis.text.x=element_text(angle=90))

print(p)




p <- qplot(data=median.d[!is.na(median.d$RRCO2),],
           x=reorder(Group,RRCO2),
           colour=Group,
           y=RRCO2,
           ymin=RRCO2-RRCO2.se,
           ymax=RRCO2+RRCO2.se,
           geom="pointrange")

p <- p + theme(axis.text.x=element_text(angle=90))

print(p)



#Plot used ####






median.d$log.Carbon.Concentration.Factor <- log10(median.d$Carbon.Concentration.Factor)
median.d.sub <- na.omit(median.d[,names(median.d) %in% c("Group", "log.Carbon.Concentration.Factor", 
                           "RubisCO.specifity.factor", "HalfSaturationCO2", 
                           "RRCO2")])
                     
ggpairs(data=median.d.sub,
        colour="Group",
        upper = "blank",
        lower = list(continuous = "smooth"),
        columns=2:ncol(median.d.sub))


d <- d[!is.na(d$Carbon.Concentration.Factor),]

write.csv(unique(d$Reference),"./Output/References physiological characteristics.csv")


graphics.off()
