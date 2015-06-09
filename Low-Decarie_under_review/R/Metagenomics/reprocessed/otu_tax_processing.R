
#Otu taxa association matrix
otu_taxa <- read.csv("./Data/Metagenomics/Re_processed_data/otu_list.csv")

otu_taxa$Domain <- as.factor(sapply(strsplit(as.character(otu_taxa$ID), "\\;"), "[", 2))
otu_taxa$Domain <- as.factor(replace(as.character(otu_taxa$Domain), is.na(as.character(otu_taxa$Domain)),"unassigned"))

otu_taxa$lineage_length <- sapply(strsplit(as.character(otu_taxa$ID), "\\;"), function(x)length(x))
otu_taxa$Phylum[otu_taxa$Domain==" Bacteria"] <- sapply(strsplit(as.character(otu_taxa$ID[otu_taxa$Domain==" Bacteria"]), "\\;"), "[", 3)
otu_taxa$Class[otu_taxa$Domain==" Bacteria"] <- sapply(strsplit(as.character(otu_taxa$ID[otu_taxa$Domain==" Bacteria"]), "\\;"), "[", 4)
otu_taxa$Order[otu_taxa$Domain==" Bacteria"] <- sapply(strsplit(as.character(otu_taxa$ID[otu_taxa$Domain==" Bacteria"]), "\\;"), "[", 5)
otu_taxa$Family[otu_taxa$Domain==" Bacteria"] <- sapply(strsplit(as.character(otu_taxa$ID[otu_taxa$Domain==" Bacteria"]), "\\;"), "[", 6)
otu_taxa$Genus[otu_taxa$Domain==" Bacteria"] <- sapply(strsplit(as.character(otu_taxa$ID[otu_taxa$Domain==" Bacteria"]), "\\;"), "[", 7)
otu_taxa$Species[otu_taxa$Domain==" Bacteria"] <- sapply(strsplit(as.character(otu_taxa$ID[otu_taxa$Domain==" Bacteria"]), "\\;"), "[",8)

otu_taxa$Kingdom[otu_taxa$Domain==" Eukaryota"] <- sapply(strsplit(as.character(otu_taxa$ID[otu_taxa$Domain==" Eukaryota"]), "\\;"), "[", 4)
otu_taxa$Subkingdom[otu_taxa$Domain==" Eukaryota"] <- sapply(strsplit(as.character(otu_taxa$ID[otu_taxa$Domain==" Eukaryota"]), "\\;"), "[", 5)
otu_taxa$Division[otu_taxa$Domain==" Eukaryota"] <- sapply(strsplit(as.character(otu_taxa$ID[otu_taxa$Domain==" Eukaryota"]), "\\;"), "[", 6)
otu_taxa$Phylum[otu_taxa$Domain==" Eukaryota"] <- sapply(strsplit(as.character(otu_taxa$ID[otu_taxa$Domain==" Eukaryota"]), "\\;"), "[", 6)
otu_taxa$Class[otu_taxa$Domain==" Eukaryota"] <- sapply(strsplit(as.character(otu_taxa$ID[otu_taxa$Domain==" Eukaryota"]), "\\;"), "[", 8)
otu_taxa$Order[otu_taxa$Domain==" Eukaryota"] <- sapply(strsplit(as.character(otu_taxa$ID[otu_taxa$Domain==" Eukaryota"]), "\\;"), "[", 9)
otu_taxa$Family[otu_taxa$Domain==" Eukaryota"] <- sapply(strsplit(as.character(otu_taxa$ID[otu_taxa$Domain==" Eukaryota"]), "\\;"), "[", 10)
otu_taxa$Genus[otu_taxa$Domain==" Eukaryota"] <- sapply(strsplit(as.character(otu_taxa$ID[otu_taxa$Domain==" Eukaryota"]), "\\;"), "[", 11)
otu_taxa$Species[otu_taxa$Domain==" Eukaryota"] <- sapply(strsplit(as.character(otu_taxa$ID[otu_taxa$Domain==" Eukaryota"]), "\\;"), "[",12)

for(i in c("Kingdom", "Subkingdom",
           "Division","Phylum", "Class", "Family", "Genus", "Species")){
  otu_taxa[,i] <- as.factor(otu_taxa[,i])}

qplot(data=otu_taxa,x=lineage_length, fill=Domain, binwidth=1)

save(otu_taxa,file="./Outputs/Metagenomics/reprocessed_OTU_taxa.RData")
write.csv(otu_taxa,file="./Data/Metagenomics/Re_processed_data/processed_OTU_taxa.csv")