rm(list=ls())


require(plyr)
require(ggplot2)
require(devtools)
require(gridExtra)
require(car)
require(xtable)

source("./R/theme.R")
theme_set(theme_minimal(14))
# theme_update(text=element_text(size=12))
load("./Outputs/phase 1 robot OD data.RData")



pdf("./Plots/slope of landscape occupancy bar plots.pdf", width=5,
    height=5)

robo.data <- na.omit(robo.data)

#To prevent carry over effect of dispersal only
robo.data <- robo.data[robo.data$transfer %in% c(1,2,13,14),]

robo.data$dalapon_category[robo.data$dalapon<=0.15] <- "benign"
robo.data$dalapon_category[robo.data$dalapon>0.15 & robo.data$dalapon<0.65] <- "sub-lethal"
robo.data$dalapon_category[robo.data$dalapon>=0.65] <- "lethal"



robo.counts <- ddply(.data=robo.data,
                     .variables=c("Soil",
                                  "Dilution",
                                  "Dispersal",
                                  "transfer",
                                  "block",
                                  "dalapon_category"),
                     function(x){data.frame(count.viable=sum(x$is.viable),
                                            proportion.viable=sum(x$is.viable)/length(x$is.viable))})

robo.counts$treatment <- with(robo.counts, paste(Soil,
                                                 Dilution,
                                                 Dispersal,
                                                 block))

robo.counts <- robo.counts[robo.counts$transfer!=10,]


slope.max <- ddply(.data=robo.counts,
                   .variables=c("Soil", "Dilution", "Dispersal",
                                "block","dalapon_category"),
                   function(x){
                     fit <- lm(proportion.viable~transfer, data=x)
                     slope <- coef(fit)["transfer"]
                     final <- mean(x$proportion.viable[x$transfer %in% c(10,11)],na.rm=T)
                     temp <- data.frame(slope=slope,
                                        final=final)
                     return(temp)})

slope.max$slope[!is.finite(slope.max$slope)] <- NA
slope.max$final[!is.finite(slope.max$final)] <- NA

slope.max$dalapon_category <- as.factor(slope.max$dalapon_category)
slope.max$dalapon_category <- factor(slope.max$dalapon_category,
                                     levels=levels(slope.max$dalapon_category)[c(1,3,2)])


#Averaging by soil first
slope.max <- ddply(.variable=c("Dilution","Dispersal","dalapon_category",
                               "Soil"),
                   .data=slope.max,
                   function(x)data.frame(slope=mean(x$slope,na.rm=T)))


#ANOVA (include soil if not previously averaging by soil)
fit <- aov(slope~Dilution*Dispersal*dalapon_category, #*Soil,
           data=slope.max)
sink("./Outputs/Phase_1_Analysis/anova of slope.txt")

print(Anova(fit,type=2))
print(Anova(fit,type=3))

sink()

print(xtable(Anova(fit,type=2),
             digits=c(0,6,0,3,3)), 
      type="html",
      file="./Outputs/Phase_1_Analysis/anova of slope.html")

dodge <- position_dodge(width = 1)

slope_Dilution_plot <- qplot(data=na.omit(slope.max),
                             x=as.factor(Dilution),
                             xlab="Dilution of initial inoculum",
                             y=slope,
                             ylab="Slope (% landscape occupancy over transfers)",
                             geom="bar",
                             stat="summary",
                             fun.y=mean,
                             fill=dalapon_category,
                             position=dodge,
                             width=0.75)+
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               mult = 1,
               width=0.25,
               position=dodge)+
  facet_grid(Dispersal~., labeller=label_both)+
  scale_fill_manual("Dalapon category",
                    values=c("benign"="#2FB3CA",
                             "sub-lethal"="#F69654",
                             "lethal"="#F1564F"))+ theme(panel.margin = unit(1, "lines"))

print(slope_Dilution_plot)




graphics.off()


print(slope_Dilution_plot)
print(Anova(fit,type=2))
print(Anova(fit,type=3))
