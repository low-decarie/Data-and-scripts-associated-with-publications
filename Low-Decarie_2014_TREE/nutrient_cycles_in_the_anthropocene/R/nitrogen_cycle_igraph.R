rm(list=ls())
require(plyr)
require(ggplot2)
require(reshape)
require(network)
require(grid)

pdf("./Plots/nitrogen_cycle_igraph.pdf")


d <- read.csv("./Data/nitrogen_cycle.csv")
d <- na.omit(d)

process.color <- data.frame(Process=c("Anthropogenic","Natural"),
                            process.color=c("red","green"))

d <- merge(d, process.color)

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

library(igraph)

graph <- graph.data.frame(d, directed=T, vertices=coordinates)

plot(graph,
     vertex.color="white",
     #vertex.size=15,
     vertex.label.dist=1.1*V(graph)$offset,
     vertex.label.cex=0.8,
     edge.width=E(graph)$Flux/30,
     edge.arrow.size=1,
     edge.arrow.width=1,
     edge.color=E(graph)$process.color,
     #edge.curved=1/as.numeric(as.factor(E(graph)$Process))-0.5,
     edge.lty=as.numeric(as.factor(E(graph)$Process)))

# edge.curved=E(graph)$offset*0.5 does not seem to work

graphics.off()