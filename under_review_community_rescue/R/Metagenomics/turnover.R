#Housekeeping ####
rm(list=ls())
require(dplyr)
require(ggplot2)
require(reshape)
theme_set(theme_bw())

require(vegetarian)
require(vegan)
require(Hmisc)

load("./Outputs/Metagenomics/reprocessed_sequences_counts.Rdata")

otu <- group_by(otu, Soil, treatment,X.OTU.ID)
otu <- summarise(otu, value=mean(value, na.rm=T))

cast_otu <- cast(otu, ...~treatment+Soil, value="value")
A <- cast_otu[!(cast_otu[,"Source_A"]==0 & cast_otu[,"High Dalapon (0.65 g/L)_A"]==0),names(cast_otu) %in% c("Source_A","High Dalapon (0.65 g/L)_A")]
cast_otu <- na.omit(cast_otu)
C <- cast_otu[!(cast_otu[,"Source_C"]==0 & cast_otu[,"High Dalapon (0.65 g/L)_C"]==0),names(cast_otu) %in% c("Source_C","High Dalapon (0.65 g/L)_C")]
cast_otu <- na.omit(cast_otu)
D <- cast_otu[!(cast_otu[,"Source_D"]==0 & cast_otu[,"High Dalapon (0.65 g/L)_D"]==0),names(cast_otu) %in% c("Source_D","High Dalapon (0.65 g/L)_D")]
cast_otu <- na.omit(cast_otu)

calc_turnover <- c(turnover(A[,1],A[,2]),
                   turnover(C[,1],C[,2]),
                   turnover(D[,1],D[,2]))

#Note: no communities adapted to the lethal conditions in soil B

smean.cl.boot(calc_turnover)
