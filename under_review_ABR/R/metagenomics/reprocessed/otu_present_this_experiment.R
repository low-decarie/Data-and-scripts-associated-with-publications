#Housekeeping ####
rm(list=ls())
require(dplyr)
require(ggplot2)
require(reshape)
theme_set(theme_bw())

sequences <- read.csv("./Metagenomics data/Re_processed_data/cluster_consensus_seq_usearch.fasta.csv", sep=c(" ",";"), header = F)

load("./Metagenomics data/Re_processed_data/out_table.RData")

#Select only OTU present in bioreactor
otu_list <- unique(otu[otu$value>0 & otu$Bioreactor=="Yes",names(otu) %in% c("X.OTU.ID","assay")])
sequences <- sequences[sequences$V1 %in% otu_list,]

algae <- sequences[sequences$V1 %in% otu[otu$value>0 &
                                           otu$Bioreactor=="Yes" &
                                           otu$assay=="algae","X.OTU.ID"],]

F28 <- sequences[sequences$V1 %in% otu[otu$value>0 &
                                           otu$Bioreactor=="Yes" &
                                           otu$assay=="28F","X.OTU.ID"],]

proto <- sequences[sequences$V1 %in% otu[otu$value>0 &
                                           otu$Bioreactor=="Yes" &
                                           otu$assay=="proto300","X.OTU.ID"],]

write.csv(otu_list,"./Metagenomics data/Re_processed_data/otu_list_new.csv")
write.csv(sequences, "./Metagenomics data/Re_processed_data/sequences.csv")

write.csv(algae, "./Metagenomics data/Re_processed_data/algae_sequences.csv")
write.csv(proto, "./Metagenomics data/Re_processed_data/proto_sequences.csv")
write.csv(F28, "./Metagenomics data/Re_processed_data/28F_sequences.csv")


