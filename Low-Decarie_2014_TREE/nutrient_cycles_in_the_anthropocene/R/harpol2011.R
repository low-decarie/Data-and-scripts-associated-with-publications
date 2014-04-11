#Data from 1 Harpole, W. S. et al. 2011 Nutrient co-limitation of primary producer communities. Ecology letters 14, 852–62. (doi:10.1111/j.1461-0248.2011.01651.x)



rm(list=ls())
require(plyr)
require(ggplot2)
require(reshape)

source("./R/theme.R")
theme_set(theme_minimal())

pdf("./Plots/harpole2011.pdf")

harpole2011 <- read.csv("./Data/harpole2011.txt")
names(harpole2011) <- c("id_study", "system", "control", "nitrogen", "phosphorus", "both", 
                        "co-limitation-type", "rn", "rp", "rnp", "rint")

elser2007 <- read.csv("./Data/elser2007.csv")
d <- merge(elser2007,harpole2011)


p <- qplot(data=d,
           x=log((both-control)/((nitrogen-control)+(phosphorus-control))),
           xlab="Relative response to combined addition compared to seperate addition",
           fill=cat,
           xlim=c(-5,10))+
  geom_vline(xintercept=0)+
  facet_grid(system~., scale="free")

print(p)

nsynergistic <- nrow(with(d, d[log((both-control)/((nitrogen-control)+(phosphorus-control)))>0,]))
ntotal <- nrow(with(d, d[!is.na(log((both-control)/((nitrogen-control)+(phosphorus-control)))),]))
nsynergistic
ntotal
nsynergistic/ntotal

p <- qplot(data=d,
           x=log(n_avail),
           y=log(nitrogen/control),
           colour=system)

print(p)

p <- qplot(data=d,
           x=log(n_avail/p_avail),
           y=log(nitrogen/control),
           colour=system)

print(p)



p <- qplot(data=d,
           x=log(p_avail),
           y=log(phosphorus/control),
           colour=system)

print(p)

p <- qplot(data=d,
           x=log(p_avail/n_avail),
           y=log(phosphorus/control),
           colour=system)

print(p)

graphics.off()