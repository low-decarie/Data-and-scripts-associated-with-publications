rm(list=ls())


require(plyr)
require(ggplot2)
require(devtools)
require(gridExtra)
require(scales)


source("./R/theme.R")
theme_set(theme_minimal(14))
# theme_update(text=element_text(size=12))


load("./Outputs/phase 2 robot OD data without recent disp.RData")


robo.data$dalapon_category[robo.data$dalapon<=0.15] <- "benign"
robo.data$dalapon_category[robo.data$dalapon>0.15 & robo.data$dalapon<0.65] <- "sub-lethal"
robo.data$dalapon_category[robo.data$dalapon>=0.65] <- "lethal"

robo.data$dalapon_category <- as.factor(robo.data$dalapon_category)
robo.data$dalapon_category <- factor(robo.data$dalapon_category,
                                     levels=levels(robo.data$dalapon_category)[c(1,3,2)])

save(robo.data, file="./Outputs/phase 2 as robo.data landscape.RData")

counts.from.robo.data <- ddply(.data=robo.data,
                            .variables=c("Soil",
                                         "Dilution",
                                         "Dispersal",
                                         "dalapon_category"),
                            function(x){data.frame(counts.viable=sum(as.numeric(x$is.viable)),
                                                   occupancy=sum(as.numeric(x$is.viable))/length(as.numeric(x$is.viable)))})



pdf("./Plots/occupancy of landscape occupancy bar plots.pdf", width=6,
    height=5)

counts.from.robo.data <- ddply(.variable=c("Dilution","Dispersal","dalapon_category",
                               "Soil"),
                   .data=counts.from.robo.data,
                   function(x)data.frame(occupancy=mean(x$occupancy,na.rm=T)))

#ANOVA (include soil if not previously averaging by soil)
fit <- glm(occupancy~dalapon_category*Dilution*Dispersal, #*Soil,
           data=counts.from.robo.data,
           family=quasibinomial)
sink("./Outputs/Phase_2_Analysis/glm of landscape occupancy.txt")

print(Anova(fit,type=2))
print(Anova(fit,type=3))

sink()

print(xtable(Anova(fit,type=2),
             digits=c(0,4,0,3)), 
      type="html",
      file="./Outputs/Phase_2_Analysis/glm of landscape occupancy.html")

dodge <- position_dodge(width = 1)

occupancy_Dilution_plot <- qplot(data=counts.from.robo.data,
                             x=as.factor(Dilution),
                             xlab="Dilution of initial inoculum",
                             y=occupancy,
                             ylab="Percentage of degraded landscape occupied\n(in Phase 2)",
                             geom="bar",
                             stat="summary",
                             fun.y=mean,
                             fill=dalapon_category,
                             position=dodge,
                             width=0.75,
                             main="")+
  scale_y_continuous(labels=percent)+
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               mult = 1,
               width=0.25,
               position=dodge)+
  facet_grid(Dispersal~., labeller=label_both)+
  scale_fill_manual("Dalapon category\nduring selection\n(Phase 1)",
                    values=c("benign"="#2FB3CA",
                             "sub-lethal"="#F69654",
                             "lethal"="#F1564F"))

print(occupancy_Dilution_plot)




graphics.off()




