#Housekeeping ####
rm(list=ls())
require(dplyr)
require(ggplot2)
require(reshape)
theme_set(theme_bw())

load_data <- function(frequency){

otu <- read.delim("./Metagenomics data/Re_processed_data/OTU_table_usearchNOchimera_97.txt", skip=1)

Sample_code <- read.csv("./Metagenomics data/sample_legend.csv")


#Calculate frequency
if(frequency==T){
  otu[,!names(otu) %in% c("X", "X.OTU.ID","taxonomy_Eukaryota",  "taxonomy_Procaryota")] <- apply(otu[,!names(otu) %in% c("X", "X.OTU.ID","taxonomy_Eukaryota",  "taxonomy_Procaryota")], 2, function(x)x/sum(x, na.rm=T))}

#Melt data frame
molten_otu <- melt(otu, id.vars = c("X.OTU.ID", "taxonomy_Eukaryota",	"taxonomy_Procaryota"))

#Fix periods used in names
molten_otu <- melt(otu, id.vars = c("X.OTU.ID", "taxonomy_Eukaryota",	"taxonomy_Procaryota"))
molten_otu$variable <- sub("pH2.r","pH2_r",molten_otu$variable)
molten_otu$variable <- sub("pH2.s","pH2_s",molten_otu$variable)
molten_otu$variable <- sub("LB","",molten_otu$variable) #####

#Extract sample number and assay
molten_otu$Sample_number <- sub("X","", sapply(strsplit(as.character(molten_otu$variable), "\\."), "[", 1))
molten_otu$assay <- sapply(strsplit(as.character(molten_otu$variable), "\\."), "[", 2)
molten_otu$assay[molten_otu$assay=="algae2411A"] <- "algae"
molten_otu <- molten_otu[molten_otu$assay!="SSU",]

#Merge otu with metadata
otu <- merge(Sample_code,molten_otu)

return(otu)
}

otu <- load_data(F)
save(otu, file="./Metagenomics data/Re_processed_data/otu_table.RData")
otu <- load_data(T)
save(otu, file="./Metagenomics data/Re_processed_data/otu_table_frequency.RData")
