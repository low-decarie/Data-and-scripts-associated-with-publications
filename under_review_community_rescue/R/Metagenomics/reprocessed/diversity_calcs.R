#Housekeeping ####
rm(list=ls())
require(dplyr)
require(ggplot2)
require(reshape)
theme_set(theme_bw())

require(vegan)
Sample_code <- read.csv("./Data/Metagenomics/Sample_codes.csv")

otu <- read.csv("./Data/Metagenomics/Re_processed_data/OTU_table_usearchNOchimera_97_Dalapon.csv")

otu <- otu[,!names(otu) %in% c("X","X.OTU.ID")]

#Calculate diversity
otu_diversity <- data.frame(t(diversity(t(otu), index="shannon")), index="shannon")

#Melt data frame
molten_diversity <- melt(otu_diversity, id.vars = c("index"))

#Extract sample number and assay
molten_diversity$Sample_number <- sub("X","", sapply(strsplit(as.character(molten_diversity$variable), "\\."), "[", 1))
molten_diversity$assay <- sapply(strsplit(as.character(molten_diversity$variable), "\\."), "[", 2)

#Merge otu_diversity with metadata
otu_diversity <- merge(Sample_code,molten_diversity, all=T)

otu_diversity$treatment <- as.character(otu_diversity$treatment)
otu_diversity$treatment[otu_diversity$treatment=="0.65"] <- "High Dalapon (0.65 g/L)"
otu_diversity$treatment[otu_diversity$treatment=="0.05"] <- "Low Dalapon (0.05 g/L)"
otu_diversity$treatment <- factor(otu_diversity$treatment, levels=levels(as.factor(otu_diversity$treatment))[c(3,2,1)])
otu_diversity$treatment <- as.character(otu_diversity$treatment)
otu_diversity$treatment[otu_diversity$treatment=="High Dalapon (0.65 g/L)"] <- "Lethal \n(0.65 g/L Dalapon)"
otu_diversity$treatment[otu_diversity$treatment=="Low Dalapon (0.05 g/L)"] <- "Benign \n(0.05 g/L Dalapon)"
otu_diversity$treatment[otu_diversity$treatment=="Source"] <- "Source community"
otu_diversity$treatment <- factor(otu_diversity$treatment, levels=levels(as.factor(otu_diversity$treatment))[c(3,1,2)])

summary(aov(value~treatment+assay+Soil,data=otu_diversity))




