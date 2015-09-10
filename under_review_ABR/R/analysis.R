#Housekeeping ####
rm(list=ls())
require(plyr)
require(ggplot2)
require(reshape)
theme_set(theme_bw())

load("./Output/growth_parameters_exp.RData")

sink("./Output/analysis difference between selection.txt")
d_ply(.data=growth_parameters,
      .variables="assay",
      function(x){with(x,{
        print(unique(assay))
        fit <- aov(r~ampli_bio)
        print(summary(fit))
        print(TukeyHSD(fit))
      })
      })
sink()


sink("./Output/analysis difference between assay.txt")
d_ply(.data=growth_parameters,
      .variables="ampli_bio",
      function(x){with(x,{
        print(unique(ampli_bio))
        fit <- aov(r~assay)
        print(summary(fit))
        print(TukeyHSD(fit))
      })
      })

sink()
