rm(list=ls())
require(ggplot2)
require(plyr)
require(reshape)

source("./R/theme.R")
theme_set(theme_minimal())



d <- read.csv("./Data/metaanalysis.csv")
d$Study <- 1:nrow(d)

d$RRCO2 <- with(d, log(ResponseElevatedCO2/ResponseAmbientCO2))
d$RRSecondaryResource <- with(d, log(ResponseElevatedSecondary/ResponseAmbientCO2))
d$RRIntereaction <- with(d, log(ResponseIntereaction/ResponseAmbientCO2))
d <- d[!is.na(d$RRIntereaction),]

molten.d <- melt(d[!is.na(d$RRIntereaction),],measure.vars=c("RRCO2",
                                                             "RRSecondaryResource",
                                                             "RRIntereaction"))

pdf("./Plots/CO2 interaction with secondary nutrient.pdf")

p <- qplot(data=molten.d,
           x=variable,
           y=value,
           ylab="Relative Response [ ln(treatment/control ]",
           colour=Secondary.nutrient)+
  geom_line(aes(group=Study))
p <- p + theme(axis.text.x=element_text(angle=90))
p <- p + scale_colour_discrete("Secondary resource")
p <- p + scale_x_discrete("Treatment",
                          labels = c("RRCO2"=expression(CO[2]),
                                     "RRSecondaryResource"="Secondary resource",
                                     "RRIntereaction"="Combination"))
print(p)
graphics.off()