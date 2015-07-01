rm(list=ls())


require(plyr)
require(ggplot2)
require(devtools)
require(gridExtra)
library(scales)

source("./R/theme.R")
theme_set(theme_minimal())

pdf("./Plots/For presentation/effect of glucose.pdf", width=12,height=9)

load("./Outputs/phase 1 robot OD data.RData")

robo.data$dalapon_category[robo.data$dalapon<=0.15] <- "benign"
robo.data$dalapon_category[robo.data$dalapon>0.15 & robo.data$dalapon<0.65] <- "sub-lethal"
robo.data$dalapon_category[robo.data$dalapon>=0.65] <- "lethal"

p_start <- qplot(data=robo.data[robo.data$transfer %in% c(1,2),],
           x=glucose,
           alpha=I(0.5),
           xlab="Glucose concentration (g/L)",
           y=OD.blanked,
#            stat="summary",
#            fun.y="mean",
           ylab="Yield (blanked optical density)",
           size=dalapon,
           main="a. At start of selection\n(Phase 1)\n")+
  geom_smooth(method="lm",se=F, size=I(1),
              alpha=I(1),
              aes(colour=dalapon_category))+
  scale_size_continuous("Dalapon concentration (g/L)", range=c(0.01,3))+
  scale_colour_manual("Dalapon category\nduring selection\n(Phase 1)",
                    values=c("benign"="#2FB3CA",
                             "sub-lethal"="#F69654",
                             "lethal"="#F1564F"))+
  facet_grid(Dilution~Dispersal,labeller="label_both")

p_end <- qplot(data=robo.data[robo.data$transfer %in% c(13,14),],
                 x=glucose,
                 xlab="Glucose concentration (g/L)",
                 y=OD.blanked,
#                stat="summary",
#                fun.y="mean",
               ylab="Yield (blanked optical density)",
                 size=dalapon,
                 main="b. At end of selection\n(Phase 1)\n")+
  geom_smooth(method="lm",se=F, size=I(1),
              alpha=I(1),
              aes(colour=dalapon_category))+
  scale_size_continuous("Dalapon concentration (g/L)", range=c(0.01,3))+
  scale_colour_manual("Dalapon category\nduring selection\n(Phase 1)",
                      values=c("benign"="#2FB3CA",
                               "sub-lethal"="#F69654",
                               "lethal"="#F1564F"))+
  facet_grid(Dilution~Dispersal,labeller="label_both")


load("./Outputs/phase 2 robot OD data without recent disp.RData")
robo.data$dalapon_category[robo.data$dalapon<=0.15] <- "benign"
robo.data$dalapon_category[robo.data$dalapon>0.15 & robo.data$dalapon<0.65] <- "sub-lethal"
robo.data$dalapon_category[robo.data$dalapon>=0.65] <- "lethal"

p_assay <- qplot(data=robo.data,
                 x=glucose,
                 xlab="Glucose concentration in Phase 1 (g/L)",
                 y=mean.OD,
#                  stat="summary",
#                  fun.y="mean",
                 ylab="Yield (blanked optical density)",
                 size=dalapon,
                 main="c. Degraded landscape\n(Phase 2)\n0.65 g/L Dalapon and 2.00 g/L glucose")+
  geom_smooth(method="lm",se=F, size=I(1),
              alpha=I(1),
              aes(colour=dalapon_category))+
  scale_size_continuous("Dalapon concentration (g/L)", range=c(0.01,3))+
  scale_colour_manual("Dalapon category\nduring selection\n(Phase 1)",
                      values=c("benign"="#2FB3CA",
                               "sub-lethal"="#F69654",
                               "lethal"="#F1564F"))+
  facet_grid(Dilution~Dispersal,labeller="label_both")



g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}

legend <- g_legend(p_assay + theme(legend.position="bottom"))



p_start <- p_start + theme(legend.position="none")
p_end <- p_end + theme(legend.position="none")
p_assay <- p_assay + theme(legend.position="none")

empty <- ggplot()+geom_point(aes(1,1), colour="white") +
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
  )
grid.arrange(p_start,p_end,p_assay,empty,legend,ncol=3,heights=c(8/10,2/10))

graphics.off()