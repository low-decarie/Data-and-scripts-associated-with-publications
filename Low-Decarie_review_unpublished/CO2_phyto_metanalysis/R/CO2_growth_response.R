rm(list=ls())
require(ggplot2)
require(plyr)
require(reshape)

source("./R/theme.R")
theme_set(theme_minimal())

pdf("./Plots/response to CO2.pdf")

d <- read.csv("./Data/metaanalysis.csv")

d$RRCO2 <- with(d, log(ResponseElevatedCO2/ResponseAmbientCO2))
d <- d[!is.na(d$RRCO2),]

write.csv(unique(d$Reference),"./Output/References growth response by group.csv")

p <- qplot(data=d,
           x=Group,
           y=RRCO2,
           ylab=expression(paste("Relative response to ",
                                 CO[2]," [ ln(high/ambient) ]")),
           #colour=Group,
           geom="boxplot")

p <- p + theme(axis.text.x=element_text(angle=90))

print(p)

p <- qplot(data=d,
           x=Group,
           y=RRCO2,
           ylab=expression(paste("Relative response to ",
                                 CO[2]," [ ln(high/ambient) ]")),
           colour=Marine.Fresh,
           geom="boxplot")

p <- p + theme(axis.text.x=element_text(angle=90))

print(p)

p <- qplot(data=d,
           x=Marine.Fresh,
           y=RRCO2,
           ylab=expression(paste("Relative response to ",
                                 CO[2]," [ ln(high/ambient) ]")),
           colour=Marine.Fresh,
           geom="boxplot")

p <- p + theme(axis.text.x=element_text(angle=90))

print(p)

fit <- aov(formula=RRCO2~Group,data=d)
summary(fit)
drop1(fit, test="F")
fit <- aov(formula=RRCO2~Marine.Fresh+Group,data=d)
summary(fit)
drop1(fit, test="F")

graphics.off()