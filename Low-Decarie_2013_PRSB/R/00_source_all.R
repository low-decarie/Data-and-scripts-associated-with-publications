rm(list=ls())
library(plyr)
setwd("/Users/LowDecarie/Documents/PhD/Experiments (PhD)/co2_selection")

files.growth<-list.files("./R/growth")
files.growth<-paste("./R/growth/", files, sep="")
lapply(files.growth, function(x) {source(x)})

files.CO2<-list.files("./R/CO2")
files.CO2<-paste("./R/CO2/", files, sep="")
lapply(files.CO2, function(x) {source(x)})

files.theoretical<-list.files("./R/theoretical")
files.theoretical<-paste("./R/theoretical/", files, sep="")
lapply(files.growth, function(x) {source(x)})