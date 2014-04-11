rm(list=ls())
require(ggplot2)
require(plyr)
require(reshape)

source("./R/theme.R")
theme_set(theme_minimal())


d <- read.csv("./Data/metaanalysis.csv")
d$Study <- 1:nrow(d)

d$RRCO2 <- with(d, log(ResponseElevatedCO2/ResponseAmbientCO2))
d$RRSecondaryResource <- with(d, log(ResponseElevatedSecondary/ResponseAmbientCO2))
d$RRIntereaction <- with(d, log(ResponseIntereaction/ResponseAmbientCO2))
d <- d[!is.na(d$RRIntereaction),]

write.csv(unique(d$Reference),"./Output/References interaction with secondary.csv")

molten.d <- melt(d[!is.na(d$RRIntereaction),],measure.vars=c("RRCO2",
                                  "RRSecondaryResource",
                                  "RRIntereaction"))

p <- qplot(data=molten.d,
           x=Secondary.nutrient,
           y=value,
           ylab="Relative Response [ ln(treatment/control ]",
           colour=variable,
           geom="boxplot")
print(p)


p <- qplot(data=d,
           x=log((ResponseIntereaction/ResponseAmbientCO2)/((ResponseElevatedCO2/ResponseAmbientCO2)+(ResponseElevatedSecondary/ResponseAmbientCO2))),
           fill=Secondary.nutrient)
print(p)


p <- qplot(data=d,
           y=RRCO2/RRSecondaryResource,
           x=Secondary.nutrient,
           geom="boxplot")
#p <- p + facet_grid(.~Marine.Fresh)
print(p)

d$RRadditive <- with(d, log(((ResponseElevatedCO2/ResponseAmbientCO2)+(ResponseElevatedSecondary/ResponseAmbientCO2))))

p <- qplot(data=d,
           x=RRadditive,
           y=RRIntereaction,
           + theme(aspect.ratio=1),
           colour=Secondary.nutrient)
p <- p + geom_abline(slope=1)
p <- p + geom_smooth(method="lm", se=F, aes(group=1))
print(p)

fit <- with(d, lm(RRIntereaction~RRadditive))
summary(fit)

molten.d <- melt(d[!is.na(d$RRIntereaction),],measure.vars=c("ResponseAmbientCO2",
                                                             "ResponseElevatedCO2",
                                                             "ResponseElevatedSecondary",
                                                             "ResponseIntereaction"))

p <- qplot(data=molten.d,
           x=variable,
           y=value,
           ylab="Response",
           colour=Secondary.nutrient)+
  geom_line(aes(group=Study))
p <- p + theme(axis.text.x=element_text(angle=90))
print(p)

