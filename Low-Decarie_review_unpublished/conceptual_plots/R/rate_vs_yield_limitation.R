rm(list=ls())
require(plyr)
require(ggplot2)
require(reshape)

source("./R/theme.R")
theme_set(theme_minimal())

#create data
Time <- 1:100
r <- 0.05
K <- 0.5
P0 <- 0.1
growth <- function(Time,r,K, P0){
  biomass <- (K*P0*exp(r*Time))/(K+P0*(exp(r*Time)-1))
  return(biomass)
}
  
d <- data.frame(Time=Time,
                limited=growth(Time,r,K,P0),
                rate_increase=growth(Time,r*2,K,P0),
                yield_increase=growth(Time,r,K*2,P0),
                both=growth(Time,r*2,K*2,P0))

moltend.d <- melt(d, id.vars="Time")

p <- qplot(data=moltend.d,
           x=Time,
           y=value,
           ylab="Biomass",
           colour=variable,
           lty=variable,
           geom="line",
           size=I(2))

p <- p + scale_colour_brewer(name="Scenarios",
                             type="qualitative",
                             palette="Paired",
                             labels=c("Rate and yield limited",
                                      "Addition of rate limiting resource",
                                      "Addition of yield limiting resource",
                        "Addition of both rate and yield limiting resources"),
                             guide=guide_legend(ncol=2,label.hjust=0,keywidth = 2))

p <- p + scale_linetype_manual(name="Scenarios",
                                 values=c("limited"=1,
                                          "rate_increase"=11,
                                          "yield_increase"=22,
                                          "both"=33),
                                 labels=c("Rate and yield limited",
                                        "Addition of rate limiting resource",
                                        "Addition of yield limiting resource",
                        "Addition of both rate and yield limiting resources"),
                                 guide=guide_legend(ncol=2,label.hjust=0,keywidth = 10))

p <- p + theme(legend.position="bottom")




pdf("./Plots/rate_vs_yield_limitation.pdf", width=10)
print(p)
graphics.off()

print(p)