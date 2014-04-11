rm(list=ls())
require(plyr)
require(ggplot2)
require(reshape)
library(igraph)
require(grid)

pdf("./Plots/all_cycle_all_sources_igraph.pdf", height=9, width=6)


autocurve.edges2 <-function (graph, start = 0.5)
{
  cm <- count.multiple(graph)
  mut <-is.mutual(graph)  #are connections mutual?
  el <- apply(get.edgelist(graph, names = FALSE), 1, paste,
              collapse = ":")
  ord <- order(el)
  res <- numeric(length(ord))
  p <- 1
  while (p <= length(res)) {
    m <- cm[ord[p]]
    mut.obs <-mut[ord[p]] #are the connections mutual for this point?
    idx <- p:(p + m - 1)
    if (m == 1 & mut.obs==FALSE) { #no mutual conn = no curve
      r <- 0
    }
    else {
      r <- seq(-start, start, length = m)
    }
    res[ord[idx]] <- r
    p <- p + m
  }
  res
}

eqarrowPlot <- function(graph, layout, edge.lty=rep(1, ecount(graph)),
                        edge.arrow.size,
                        vertex.shape="circle",
                        edge.curved=autocurve.edges(graph),
                        edge.color,edge.width,
                        edge.arrow.width,main,margin,...) {
  plot(graph, edge.lty=0, edge.arrow.size=0, layout=layout,
       vertex.shape="none", vertex.label="", main=main,margin=margin)
  for (e in seq_len(ecount(graph))) {
    graph2 <- delete.edges(graph, E(graph)[(1:ecount(graph))[-e]])
    plot(graph2, edge.lty=edge.lty[e], edge.color=edge.color[e],
         edge.width=edge.width[e],margin=margin,
         edge.arrow.width=edge.arrow.width[e],
         edge.arrow.size=edge.arrow.size, #[e],
         edge.curved=edge.curved[e], layout=layout, vertex.shape="none",
         vertex.label=NA, add=TRUE, ...)
  }
  plot(graph, edge.lty=0, edge.arrow.size=0, layout=layout,
       vertex.shape=vertex.shape, add=TRUE, ...)
  invisible(NULL)
}


#Load data
d <- read.csv("./Data/all_cycles_all_sources.csv")
d <- na.omit(d)
d$Source[d$Source="Mineral reserves"] <- "Mineral reserves"
d$Sink[d$Sink="Mineral reserves"] <- "Mineral reserves"

#Take average across references
d <- ddply(.data=d,
                .variables=c("Source", "Element", "Process", "Sink"),
                function(x){
                  data.frame(Flux=mean(x$Flux.converted))})

#Assign colors to type of flux
process.color <- data.frame(Process=c("Anthropogenic","Natural"),
                            process.color=c("#FF0000","#000000"))
d <- merge(d, process.color)
element.color <- data.frame(Element=c("N","C", "P", "Mg"),
                            element.color=c("#008000","#6F6F6F",
                                            "#FFB700","#07859E"))

# process.color <- data.frame(Process=c("Anthropogenic","Natural"),
#                             process.color=c("#FF000050","#00000050"))
# d <- merge(d, process.color)
# element.color <- data.frame(Element=c("N","C", "P", "Mg"),
#                             element.color=c("#00800050","#6F6F6F50",
#                                             "#FFB70050","#07859E50"))
d <- merge(d, element.color)


#d=d[d$Element!="Mg",]

par(mfrow=c(3,2))

#Specify the plot coordinates for the different pools
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



plot.cycle <- function(cycle.data=d[d$Element=="C",]){

  cycle.data$Std.Flux <- cycle.data$Flux/mean(cycle.data$Flux)
  
  cycle.graph <- graph.data.frame(cycle.data, directed=T, vertices=coordinates)
  
  eqarrowPlot(cycle.graph,
        asp=0,
       margin=0.1,
       #rescale=F,       
       edge.curved=autocurve.edges2(cycle.graph),
              layout=layout.auto(cycle.graph),
        #layout=as.matrix(coordinates[,names(coordinates) %in% c("x","y")]),
        ylim=c(0.75,2.25),
       xlim=c(0.75,3.25),
       #axes=TRUE,
       main=paste("Cycle for",unique(cycle.data$Element)),
       vertex.color="white",
       #vertex.size=15,
       vertex.label.dist=1.1*V(cycle.graph)$offset,
       vertex.label.cex=1,
       edge.width=E(cycle.graph)$Std.Flux*3,
       edge.color=E(cycle.graph)$process.color,
       edge.arrow.size=1,
              edge.arrow.width=E(cycle.graph)$Std.Flux)

}

d_ply(.data=d,
      .variable="Element",
      .progress="text",
      function(x){plot.cycle(x)})

d_ply(.data=d,
      .variable="Element",
      function(cycle.data){
  ratio <- sum(cycle.data[cycle.data$Process=="Anthropogenic","Flux"])/sum(cycle.data[cycle.data$Process=="Natural","Flux"])
  print(paste("Ratio for", unique(cycle.data$Element), "is", ratio))})


#############

#cycle.data=d[d$Process=="Anthropogenic" & d$Element!="Mg",]
cycle.data=d[d$Process=="Anthropogenic",]


plot.all.element <- function(cycle.data=d[d$Process=="Anthropogenic" & d$Element!="Mg",]){

  
cycle.graph <- graph.data.frame(cycle.data, directed=T, vertices=coordinates)


eqarrowPlot(cycle.graph,
     asp=0,
     margin=0.1,
     #rescale=F,
    layout=layout.auto(cycle.graph),
     ylim=c(0.75,2.25),
     xlim=c(0.75,3.25),
     edge.curved=autocurve.edges2(cycle.graph),
     main=paste(unique(cycle.data$Process),"cycles"),
     vertex.color="white",
     vertex.label.dist=1.1*V(cycle.graph)$offset,
            vertex.label.cex=1,
     edge.width=E(cycle.graph)$Flux/300,
     edge.arrow.size=1,
     edge.arrow.width=E(cycle.graph)$Flux/1000,
     edge.color=E(cycle.graph)$element.color,
     edge.lty=as.numeric(as.factor(E(cycle.graph)$Process)))
}

#par(mfrow=c(1,2))
plot.all.element(cycle.data=d[d$Process=="Anthropogenic",])
plot.all.element(cycle.data=d[d$Process=="Natural",])




graphics.off()
  
  