rm(list=ls())
require(plyr)
require(ggplot2)
require(reshape)
require(scales)
require(gridExtra)
library(grid)

source("./R/theme.R")
theme_set(theme_minimal())


#create data
R <- (0:5)/5
a <- 1
h <- 1
functional_response <- function(R,a,h){
  #Type II
  Biomass <- (a*R)/(1+a*h*R)
  return(Biomass)
}

# Liebig and serial colimitation ####

liebig <- data.frame(NULL)
for(R1 in R){
  for(R2 in R){
    #print(paste(R1,R2))
    temp <- data.frame(R1=R1,
                       R2=R2,
                       Biomass=min(c(functional_response(R=R1, a=a, h=h),
                                   functional_response(R=R2, a=a, h=h))))
    liebig <- rbind(liebig,temp)
  }
}
                       
    

p <- qplot(data=liebig, 
           x=R1+0.1, xlab=expression(bold(R[1])),
           y=R2+0.1, ylab=expression(bold(R[2])),
           fill=Biomass/max(Biomass),
           geom="tile")
p <- p + scale_y_continuous(labels = percent, limits=c(0,1), expand=c(0,0))
p <- p + scale_x_continuous(labels = percent, limits=c(0,1), expand=c(0,0))
p <- p + scale_fill_gradient(limits=c(0,1),
                             low="white",
                             high="grey",
                             labels = percent)
p <- p + theme(legend.position="none")
#print(p)

serial <- p + geom_segment(x=0.3,xend=0.7,
                           y=0.5,yend=0.5, 
                           arrow= arrow(length = unit(0.4,"cm"), type="closed"))
serial <- serial + geom_segment(x=0.7,xend=0.7,
                           y=0.5,yend=0.9, 
                           arrow= arrow(length = unit(0.4,"cm"), type="closed"),
                           colour="darkgrey")
#print(serial)

# Liebig and equal values of each resource ####

equal <- p + geom_abline(slope=1, colour="azure", lty=3)
equal <- equal + geom_segment(x=0.5,xend=0.9,
                          y=0.5,yend=0.5, 
                          arrow= arrow(length = unit(0.4,"cm"), type="closed"))
equal <- equal + geom_segment(x=0.5,xend=0.5,
                                y=0.5,yend=0.9, 
                                arrow= arrow(length = unit(0.4,"cm"), type="closed"),
                                colour="darkgrey")
equal <- equal + geom_segment(x=0.5,xend=0.9,
                              y=0.48,yend=0.88, 
                              arrow= arrow(length = unit(0.4,"cm"), type="closed"))
equal <- equal + geom_segment(x=0.5,xend=0.9,
                              y=0.52,yend=0.92, 
                              arrow= arrow(length = unit(0.4,"cm"), type="closed"),
                              colour="darkgrey")

equal <- equal + theme(legend.position="none")
#print(equal)


# Aiding or substitutable resources ####

substitutable <- data.frame(NULL)
for(R1 in R){
  for(R2 in R){
    #print(paste(R1,R2))
    temp <- data.frame(R1=R1,
                       R2=R2,
                       Biomass=prod(c(functional_response(R=R1, a=a, h=h),
                                     functional_response(R=R2, a=a, h=h))))
    substitutable <- rbind(substitutable,temp)
  }
}

substitutable.p <- qplot(data=substitutable, 
           x=R1+0.1, xlab=expression(bold(R[1])),
           y=R2+0.1, ylab=expression(bold(R[2])),
           fill=Biomass/max(Biomass),
           geom="tile")
substitutable.p <- substitutable.p + scale_y_continuous(labels = percent, limits=c(0,1), expand=c(0,0))
substitutable.p <- substitutable.p + scale_x_continuous(labels = percent, limits=c(0,1), expand=c(0,0))
substitutable.p <- substitutable.p + scale_fill_gradient(limits=c(0,1),
                             low="white",
                             high="grey",
                             labels = percent)

substitutable.p <- substitutable.p + geom_segment(x=0.3,xend=0.7,
                              y=0.5,yend=0.5, 
                              arrow= arrow(length = unit(0.4,"cm"), type="closed"))
substitutable.p <- substitutable.p + geom_segment(x=0.3,xend=0.3,
                              y=0.5,yend=0.9, 
                              arrow= arrow(length = unit(0.4,"cm"), type="closed"),
                              colour="darkgrey")
substitutable.p <- substitutable.p + theme(legend.position="none")
#print(substitutable.p)

# Multiple species with different limitations

Species_1 <- data.frame(NULL)
for(R1 in R){
  for(R2 in R){
    #print(paste(R1,R2))
    temp <- data.frame(R1=R1,
                       R2=R2,
                       Biomass=functional_response(R=R1, a=a, h=h))
    Species_1 <- rbind(Species_1,temp)
  }
}

Species_2 <- data.frame(NULL)
for(R1 in R){
  for(R2 in R){
    #print(paste(R1,R2))
    temp <- data.frame(R1=R1,
                       R2=R2,
                       Biomass=functional_response(R=R2, a=a, h=h))
    Species_2 <- rbind(Species_2,temp)
  }
}

Species_1.p <- qplot(data=Species_1,
                         x=R1+0.1, xlab=expression(bold(R[1])),
                         y=R2+0.1, ylab=expression(bold(R[2])),
                         fill=Biomass/max(Biomass),
                         geom="tile")
Species_1.p <- Species_1.p + scale_y_continuous(labels = percent, limits=c(0,1), expand=c(0,0))
Species_1.p <- Species_1.p + scale_x_continuous(labels = percent, limits=c(0,1), expand=c(0,0))
Species_1.p <- Species_1.p + scale_fill_gradient(limits=c(0,1),
                                                         low="white",
                                                         high="grey",
                                                         labels = percent)
Species_1.p <- Species_1.p + geom_segment(x=0.3,xend=0.7,
                                    y=0.5,yend=0.5, 
                                    arrow= arrow(length = unit(0.4,"cm"), type="closed"))
Species_1.p <- Species_1.p + theme(legend.position="none")

#print(Species_1.p)


Species_2.p <- qplot(data=Species_2,
                     x=R1+0.1, xlab=expression(bold(R[1])),
                     y=R2+0.1, ylab=expression(bold(R[2])),
                     fill=Biomass/max(Biomass),
                     geom="tile")
Species_2.p <- Species_2.p + scale_y_continuous(labels = percent, limits=c(0,1), expand=c(0,0))
Species_2.p <- Species_2.p + scale_x_continuous(labels = percent, limits=c(0,1), expand=c(0,0))
Species_2.p <- Species_2.p + scale_fill_gradient(limits=c(0,1),
                                                 low="white",
                                                 high="grey",
                                                 labels = percent)

Species_2.p <- Species_2.p + geom_segment(x=0.3,xend=0.3,
                                                  y=0.5,yend=0.9, 
                              arrow= arrow(length = unit(0.4,"cm"), type="closed"),
                                                  colour="darkgrey")
Species_2.p <- Species_2.p + theme(legend.position="none")
#print(Species_2.p)

pdf("./Plots/colimitation.pdf", width=12)
grid.arrange(serial+ggtitle("a. Serial co-limitation")+
               coord_fixed(),
 equal+ggtitle("b. Equally limiting resources")+
               coord_fixed(),
 substitutable.p+ggtitle("c. Aiding or subsitutable resources")+
               coord_fixed(),
 Species_1.p+ggtitle("d. Species 1")+
               coord_fixed(),
 Species_2.p+ggtitle("e. Species 2")+
               coord_fixed(),
substitutable.p+ggtitle("f. Community")+
               coord_fixed(),
 nrow=2)
graphics.off()