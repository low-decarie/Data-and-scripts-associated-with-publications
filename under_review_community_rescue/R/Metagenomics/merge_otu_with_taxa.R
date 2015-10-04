load("./Outputs/Metagenomics/reprocessed_sequences_frequency.Rdata")
#load("./Outputs/Metagenomics/reprocessed_OTU_taxa.RData")
otu_taxa <- read.csv("./Data/Metagenomics/Re_processed_data/processed_OTU_taxa.csv")

otu$treatment <- as.character(otu$treatment)
otu$treatment[otu$treatment=="High Dalapon (0.65 g/L)"] <- "Lethal \n(0.65 g/L Dalapon)"
otu$treatment[otu$treatment=="Low Dalapon (0.05 g/L)"] <- "Benign \n(0.05 g/L Dalapon)"
otu$treatment[otu$treatment=="Source"] <- "Source community"
otu$treatment <- factor(otu$treatment, levels=levels(as.factor(otu$treatment))[c(3,1,2)])

names(otu)[names(otu) %in% "X.OTU.ID"] <- "OTU"
count_data <- merge(otu, otu_taxa)
count_data$Genus <- as.character(count_data$Genus)
count_data$Genus[is.na(count_data$Genus)] <- "Unclassified"
count_data$Genus <- as.factor(count_data$Genus)

save(count_data, file="./Outputs/Metagenomics/reprocessed_OTU_with_taxa.RData")