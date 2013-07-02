#Housekeeping
rm(list=ls())
library(plyr)
library(ggplot2)
library(devtools)
source_url("https://raw.github.com/edielivon/Useful-R-functions/master/ggplot2_themes/theme_minimal.R")

load(file="./Outputs/compile data.Rdata")

#Create a plotting function
plot.species<-function(Species){
  try({
  
              selected<-readings[readings$Species==Species,]
              selected<-droplevels(selected)
              
              #Basic plot
              plot.sp<-qplot(data=selected,
                              x=Time,
                              y=ABS,
                              color=assayCO2,
                              shape=CO2history,
                              linetype=CO2history,
                              main=Species,
                              alpha=I(0.3),
                              geom=c("point", "line"),
                              group=culture)
              
              #Add a vertical line at transfer time (3.5 days)
              plot.sp<-plot.sp+geom_vline(x=I(3.5), size=I(1))
              
              #Customize colour legend
              plot.sp<-plot.sp+scale_colour_manual(values=c("ambient"="blue",
                                                            "high"= "red",
                                                            "low"= "green"))
              
              #Seperate by regime and legend
              plot.sp<-plot.sp+facet_grid(regime~transplant, scale="free")
              
              print(plot.sp)
  })
}


#Apply the plotting function to all species

pdf("./Plots/growth/species plots.pdf", width=20)

Species.list<-unique(readings$Species)

for(Species in Species.list){
  print(Species)
  plot.species(Species)}
dev.off()


