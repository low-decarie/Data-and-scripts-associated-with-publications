rm(list=ls())
require(plyr)
require(ggplot2)
require(reshape)
require(scales)
require(gridExtra)
library(grid)

source("./R/theme.R")
theme_set(theme_minimal())

pdf("./A de Kluijver et al 2010.pdf")

d <- read.csv("./A de Kluijver et al 2010.csv")

p <- qplot(data=d,
           x=CO2,
           y=Biomass,
           stat="identity",
           ylab="Biomass",
           fill=Group,
           geom="bar",
           size=I(2))

p <- p + scale_fill_brewer(type="qualitative",
                             palette="Paired")

print(p)

p <- qplot(data=d,
           x=CO2,
           y=Relative.frequency,
           stat="identity",
           ylab="Relative frequency",
           colour=Group,
           geom="line",
           size=I(2),
           ylim=c(0,0.6))

p <- p + scale_colour_brewer(type="qualitative",
                           palette="Paired")

print(p)

graphics.off()