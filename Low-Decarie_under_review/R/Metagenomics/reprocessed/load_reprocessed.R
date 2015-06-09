#Housekeeping ####
rm(list=ls())
require(reshape)
require(stringr)

#Produces values scaled to total sequence counts per sample/assay

load_reprocessed <- function(frequency=T){
# Sample_name code
Sample_code <- read.csv("./Data/Metagenomics/Sample_codes.csv")
# Sample_code$Sample_number <- paste("X",Sample_code$Sample_number, sep="")


#Load otu data
otu <- read.delim("./Data/Metagenomics/Re_processed_data/OTU_table_usearchNOchimera_97.txt", skip=1)

if(frequency==T){
otu[,2:76] <- apply(otu[,2:76], 2, function(x)x/sum(x, na.rm=T))}



molten_otu <- melt(otu, id.vars = c("X.OTU.ID", "taxonomy"))
molten_otu$variable <- sub("pH2.r","pH2_r",molten_otu$variable)
molten_otu$variable <- sub("pH2.s","pH2_s",molten_otu$variable)
molten_otu$variable <- sub("LB","",molten_otu$variable) #####

#Remove bioreactor data with same code
#Only 28F and SSU used in this study
#28FLB is part of this study
molten_otu <-  molten_otu[!molten_otu$variable %in% c("X2.28F",
                                                      "X2.algae",
                                                      "X2.proto300",
                                                      "X4.28F",
                                                      "X4.algae",
                                                      "X4.proto300"),]


#Extract sample number and assay
molten_otu$Sample_number <- sub("X","", sapply(strsplit(as.character(molten_otu$variable), "\\."), "[", 1))
molten_otu$assay <- sapply(strsplit(as.character(molten_otu$variable), "\\."), "[", 2)

#Merge otu with metadata
otu <- merge(molten_otu, Sample_code, all=T)

#Select dalapon experiment
otu <- otu[!is.na(otu$Soil),]
Sample_code <- otu[!is.na(Sample_code$Soil),]

#Assign treatments
otu$treatment <-as.character(otu$Dalapon)
otu$treatment[is.na(otu$treatment)] <- "Source"

return(otu)
}


otu <- load_reprocessed(frequency=T)
write.csv(otu, "./Outputs/Metagenomics/reprocessed_sequences_frequency.csv")
save(otu,file= "./Outputs/Metagenomics/reprocessed_sequences_frequency.Rdata")

otu <- load_reprocessed(frequency=F)
write.csv(otu, "./Outputs/Metagenomics/reprocessed_sequences_counts.csv")
save(otu,file= "./Outputs/Metagenomics/reprocessed_sequences_counts.Rdata")