rm(list=ls())


require(plyr)
require(extrafont)
require(ggplot2)
require(devtools)
require(gridExtra)
library(scales)

source("./R/theme.R")
theme_set(theme_minimal())


pdf("./Plots/For presentation/individual landscapes_plot.pdf",height=8)

fmt <- function(x) format(x,nsmall = 2,scientific = FALSE)


# Landscape occupancy at start####

benign_rect <- geom_rect(aes(xmin = 0.55, xmax = 6.45, ymin = 0.55, ymax = 3.45),colour=I("#2FB3CA"), fill=NA,size=I(2))

sub_lethal_rect <- geom_rect(aes(xmin = 0.55, xmax = 6.45, ymin = 3.55, ymax = 7.45),colour=I("#F69654"), fill=NA,size=I(2))

lethal_rect <- geom_rect(aes(xmin = 0.55, xmax = 6.45, ymin = 7.55, ymax = 10.45),colour=I("#F1564F"), fill=NA,size=I(2))


load("./Outputs/phase 1 robot OD data.RData")
#vialbe only if viable on both last days
robo.counts <- ddply(.data=robo.data[robo.data$transfer %in% c(1,2),],
                     .variables=c("dalapon",
                                  "glucose",
                                  "Soil",
                                  "Dilution",
                                  "Dispersal",
                                  "block"),
                     function(x){data.frame(is.viable=all(x$is.viable))})

robo.counts <- ddply(.data=robo.counts,
                     .variables=c("dalapon",
                                  "glucose",
                                  "Dilution",
                                  "Dispersal"),
                     function(x){data.frame(counts.viable=sum(x$is.viable),
                                            n=length(x$is.viable))})




p_start<-qplot(data=robo.counts,
               main="a. At start of selection\n(Phase 1)\n",
               x=as.factor(fmt(glucose)),
               xlab="Glucose concentration (g/L)",
               y=as.factor(fmt(dalapon)),
               ylab="Dalapon concentration (g/L)",
                 fill=counts.viable/n,colour=counts.viable/n,
               geom="tile")+
  facet_grid(Dilution~Dispersal,labeller = label_both)+
  scale_fill_gradient(name="Frequency viable",
                      low="white",
                      high="#004953",
                      labels = percent,
                      limits = c(0,1))+
  scale_colour_gradient(name="Frequency viable",
                        low="white",
                        high="#004953",  #004953
                        labels = percent,
                        limits = c(0,1))+
  coord_fixed()+scale_x_discrete(expand=c(0,0))+scale_y_discrete(expand=c(0,0))

print(p_start)

# Landscape occupancy at end####

load("./Outputs/phase 1 robot OD data.RData")
#vialbe only if viable on both last days
robo.counts <- ddply(.data=robo.data[robo.data$transfer %in% c(13,14),],
                     .variables=c("dalapon",
                                  "glucose",
                                  "Soil",
                                  "Dilution",
                                  "Dispersal",
                                  "block"),
                     function(x){data.frame(is.viable=all(x$is.viable))})

robo.counts <- ddply(.data=robo.counts,
                     .variables=c("dalapon",
                                  "glucose",
                                  "Dilution",
                                  "Dispersal"),
                     function(x){data.frame(counts.viable=sum(x$is.viable),
                                            n=length(x$is.viable))})




p_end<-qplot(data=robo.counts,
             main="b. At end of selection\n(Phase 1)\n",
             x=as.factor(fmt(glucose)),
             xlab="Glucose concentration (g/L)",
             y=as.factor(fmt(dalapon)),
             ylab="Dalapon concentration (g/L)",
             fill=counts.viable/n,
             colour=counts.viable/n,
             geom="tile")+
  facet_grid(Dilution~Dispersal,labeller = label_both)+
  scale_fill_gradient(name="Frequency viable",
                      low="white",
                      high="#004953",
                      labels = percent,
                      limits = c(0,1))+
  scale_colour_gradient(name="Frequency viable",
                        low="white",
                        high="#004953",  #2FB3CA
                        labels = percent,
                        limits = c(0,1))+
  coord_fixed()+scale_x_discrete(expand=c(0,0))+scale_y_discrete(expand=c(0,0))

print(p_end)


# Landscape occupancy after degradation ####

load("./Outputs/phase 2 robot OD data without recent disp.RData")

counts.from.robo.data <- ddply(.data=robo.data,
                               .variables=c("dalapon",
                                            "glucose",
                                            "Dilution",
                                            "Dispersal"),
                               function(x){data.frame(counts.viable=sum(as.numeric(x$is.viable)),
                                                      n=length(as.numeric(x$is.viable)))}
)


p_assay<-qplot(data=counts.from.robo.data,
               x=as.factor(fmt(glucose)),
               xlab="Glucose concentration in Phase 1 (g/L)",
               y=as.factor(fmt(dalapon)),
               ylab="Dalapon concentration in Phase 1 (g/L)",
                 fill=counts.viable/n,colour=counts.viable/n,
               geom="tile",
               main="c. Degraded landscape\n(Phase 2)\n0.65 g/L Dalapon and 2.00 g/L glucose")+
  facet_grid(Dilution~Dispersal,labeller = label_both)+
  scale_fill_gradient(name="Frequency viable",
                      low="white",
                      high="#004953",
                      labels = percent,
                      limits = c(0,1),
                      guide=guide_colourbar(title.position="top",
                                            barwidth = 10, barheight = 1,
                                            ticks=F))+
  scale_colour_gradient(name="Frequency viable",
                      low="white",
                      high="#004953",
                      labels = percent,
                      limits = c(0,1))+
  coord_fixed()+scale_x_discrete(expand=c(0,0))+scale_y_discrete(expand=c(0,0))


print(p_assay)

print(p_start+benign_rect+sub_lethal_rect+lethal_rect)

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}

legend <- g_legend(p_assay + theme(legend.position="bottom"))

p_start <- p_start + theme(legend.position="none")+
  geom_rect(aes(xmin = 4.5, xmax = 5.5, ymin = 7.5 , ymax = 8.5), colour=I("orange"),fill=NA)
p_end <- p_end + theme(legend.position="none")+
  geom_rect(aes(xmin = 4.5, xmax = 5.5, ymin = 7.5 , ymax = 8.5), colour=I("orange"),fill=NA)
p_assay <- p_assay + theme(legend.position="none", 
                           panel.border=element_rect(colour=I("orange"), size=I(2)))

orange_rect <- ggplot()+geom_point(aes(1:5,1:5), colour="white") + coord_equal()+
  theme(                              
    plot.background = element_blank(), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    panel.border = element_blank(), 
    panel.background = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
  )+
  geom_rect(aes(xmin = 1, xmax = 2, ymin = 2 , ymax = 3), colour=I("orange"),fill=NA, size=I(1))

# +
#   geom_text(aes(x=2.5,y=2.5), label="Treatment levels of phase 2: 0.65 g/L Dalapon and 2.00 g/L glucose", size=2, hjust=0, vjust=0.5)

#+geom_vline(xintercept=5,colour=I("orange"))+geom_hline(yintercept=8,colour=I("orange"))


graphics.off()

pdf("./Plots/For presentation/all_landscapes_plot.pdf", width=12,height=10)


grid.arrange(p_start,p_end,p_assay,legend,orange_rect,ncol=3,heights=c(9/10,1/10))
grid.text(label="Treatment levels of phase 2: 0.65 g/L Dalapon and 2.00 g/L glucose", x=0.72, y=0.055)

graphics.off()