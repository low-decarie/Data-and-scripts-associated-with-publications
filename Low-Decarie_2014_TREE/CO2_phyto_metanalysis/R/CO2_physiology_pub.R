rm(list=ls())
require(ggplot2)
require(plyr)
require(reshape)
library(GGally)

source("./R/theme.R")
theme_set(theme_minimal())

pdf("./Plots/CO2 physiology.pdf")

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




p <- qplot(data=median.d[!is.na(median.d$Carbon.Concentration.Factor)&!is.na(median.d$RubisCO.specifity.factor),,drop=T],
           x=Carbon.Concentration.Factor,
           xlab="Carbon concentration factor  (log scale)",
           xmin=Carbon.Concentration.Factor-Carbon.Concentration.Factor.se,
           xmax=Carbon.Concentration.Factor+Carbon.Concentration.Factor.se,
           #colour=Group,
           y=RubisCO.specifity.factor,
           ylab="RubiscCO specificity factor",
           ymin=RubisCO.specifity.factor-RubisCO.specifity.factor.se,
           ymax=RubisCO.specifity.factor+RubisCO.specifity.factor.se,
           geom=c("errorbarh","errorbar"),
           log="x")

p <- p + theme(axis.text.x=element_text(angle=90))
p <- p + theme(legend.position="none")
p <- p + geom_text(aes(label=Group, 
                       x=Carbon.Concentration.Factor*1.2,
                       y=RubisCO.specifity.factor*1.15))

print(p)

graphics.off()