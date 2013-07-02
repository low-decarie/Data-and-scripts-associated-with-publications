rm(list=ls())
require(plyr)
require(ggplot2)
require(reshape)
require(network)
require(grid)

source("./R/theme.R")

pdf("./Plots/nitrogen_cycle_gg.pdf")

d <- read.csv("./Data/nitrogen_cycle.csv")
d <- na.omit(d)

coordinates <- data.frame(reservoir=c("Mineral reserves","Atmosphere",
                                      "Land","Freshwater", "Oceans"),
                          x=c(1,2,1,2,3),
                          y=c(2,2,1,1,1),
                          offset=c(1,1,-1,-1,-1))

sink.coord <- coordinates[,!names(coordinates) %in% "offset"]
names(sink.coord) <- c("Sink", "x.sink", "y.sink")

source.coord <- coordinates
names(source.coord) <- c("Source", "x.source", "y.source","offset")

d <- merge(d, sink.coord)
d <- merge(d, source.coord)

d$Std.Flux <- d$Flux/max(d$Flux,na.rm=T)

p <- qplot(data=d,
           #alpha=I(0.4),
           colour=Process,
           size=Std.Flux,
           xlim=c(0,4),
           ylim=c(0,3),
           x=x.source+as.numeric(Process)/10,
           y=y.source+as.numeric(Process)/10,
           xend=x.sink+as.numeric(Process)/10,
           yend=y.sink+as.numeric(Process)/10,
           geom="segment",
           arrow = arrow(type="closed",
                         length = unit(d$Std.Flux,"cm")))

p <- p + geom_text(aes(label=Source, y=y.source+0.25*offset), size=I(3), colour=I("black"))


print(p)

graphics.off()
