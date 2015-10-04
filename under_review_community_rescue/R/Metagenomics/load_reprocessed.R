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
otu <- read.csv("./Data/Metagenomics/Re_processed_data/OTU_table_usearchNOchimera_97_Dalapon.csv")
otu <- otu[,-1]

#Calculate frequency
if(frequency==T){
otu[,!names(otu) %in% c("X", "X.OTU.ID")] <- apply(otu[,!names(otu) %in% c("X", "X.OTU.ID")], 2, function(x)x/sum(x, na.rm=T))}

#Melt data frame
molten_otu <- melt(otu, id.vars = c("X.OTU.ID"))

#Extract sample number and assay
molten_otu$Sample_number <- sub("X","", sapply(strsplit(as.character(molten_otu$variable), "\\."), "[", 1))
molten_otu$assay <- sapply(strsplit(as.character(molten_otu$variable), "\\."), "[", 2)

#Merge otu with metadata
otu <- merge(Sample_code,molten_otu, all=T)

otu$treatment <- as.character(otu$treatment)
otu$treatment[otu$treatment=="0.65"] <- "High Dalapon (0.65 g/L)"
otu$treatment[otu$treatment=="0.05"] <- "Low Dalapon (0.05 g/L)"
otu$treatment <- factor(otu$treatment, levels=levels(as.factor(otu$treatment))[c(3,2,1)])

return(otu)
}


otu <- load_reprocessed(frequency=T)
write.csv(otu, "./Outputs/Metagenomics/reprocessed_sequences_frequency.csv")
save(otu,file= "./Outputs/Metagenomics/reprocessed_sequences_frequency.Rdata")

otu <- load_reprocessed(frequency=F)
write.csv(otu, "./Outputs/Metagenomics/reprocessed_sequences_counts.csv")
save(otu,file= "./Outputs/Metagenomics/reprocessed_sequences_counts.Rdata")