rm(list=ls())
require(plyr)
require(ggplot2)
require(reshape)
require(network)


pdf("./Plots/nitrogen_cycle_network.pdf")

d <- read.csv("./Data/nitrogen_cycle.csv")
d <- na.omit(d)

source.sink <- d[,names(d) %in% c("Source","Sink")]

net <- network(as.matrix(source.sink),
               matrix.type="edgelist",
               directed=T,
               multiple=T)

plot(net,
     main="Nitrogen cycle",
     displaylabels=T,
     edge.lwd=d$Flux/10,
     arrowhead.cex=d$Flux/20,
     #jitter=T,
     edge.lty=as.numeric(d$Process),
     edge.col=2/as.numeric(d$Process),
     usecurve=T,
     edge.curve=as.numeric(d$Process)/20)


# Seperate plotting  ####

natural <- d[d$Process=="Natural",]
human <- d[d$Process=="Anthropogenic",]

set.seed(1818)

#Atm Fresh Land Ocean Mineral

human.net <- network(as.matrix(human[,names(human) %in% c("Source","Sink")]),
                       matrix.type="edgelist",
                       directed=T)

coordman <- as.matrix(data.frame(x=c(0.5,0.5,0.2,0.8,0.5),y=c(0.8,0.2,0.2,0.8,0.2)))

plot(human.net,
     coord=coordman,
     displaylabels=T,
     edge.lwd=human$Flux/10,
     arrowhead.cex=human$Flux/20,
     edge.col="#FF000050",
     usecurve=T,
     edge.curve=1/20,
     pad=0.5,
     displayisolates=T)



natural.net <- network(as.matrix(natural[,names(natural) %in% c("Source","Sink")]),
                       matrix.type="edgelist",
                       directed=T)

plot(natural.net,
     displaylabels=T,
     edge.lwd=natural$Flux/10,
     arrowhead.cex=natural$Flux/20,
     edge.col="#00FF0050",
     usecurve=T,
     edge.curve=1/20,
     pad=0.5,
     displayisolates=T,
     new=T)

graphics.off()