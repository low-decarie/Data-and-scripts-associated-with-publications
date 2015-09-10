# #Housekeeping ####
rm(list=ls())
require(dplyr)
require(ggplot2)
require(stringr)
require(reshape)
require(Hmisc)
require(dplyr)
require(reshape)
require(ggpca)
# theme_set(theme_bw())
# 

load("./Metagenomics data/Re_processed_data/otu_table_frequency.RData")
otu_genus <- read.csv("./Metagenomics data/Re_processed_data/otu_list.csv")
otu <- otu[otu$Experiment==2,]
otu <- merge(otu, otu_genus)

otu$Genus <- as.character(otu$Genus)
otu$Genus[otu$Genus==""] <- gsub("g__","",
                                 str_extract(otu$taxonomy_Procaryota, ignore.case("g__[a-z]+")))[otu$Genus==""]


#Sum values from different OTUs but the same genus
genus <- group_by(otu, Genus, Condition, After_lab_culture,assay, Bioreactor)
genus <- summarise(genus,value=sum(value))
genus <- na.omit(genus)

#Plot only values present in at least one ABR
genus <- group_by(genus, Genus)
genus <- mutate(genus, is_in_multiple=sum(value>0)>1,
                  is_dominant=any(value>0.1))
genus <- group_by(genus, Genus, Condition)
genus <- mutate(genus, before_and_after=sum(value>0)>1)
genus <- group_by(genus, Genus)
genus <- mutate(genus, pair_count=sum(before_and_after))


genus$Treatment_Lab_culture <- paste(genus$Condition, genus$After_lab_culture)




p_otu <- qplot(data=otu,
           x=After_lab_culture,
           y=reorder(X.OTU.ID, -value),
           xlab="",
           main="",
           ylab="",
           fill=value,
           geom="tile",
           stat="identity")+facet_grid(assay~Condition,scale="free",space="free")+
  scale_fill_continuous(low = "grey", high="black")


# otu$Genus <- gsub(" [a-z]", "", otu$Genus , ignore.case =T)

genus <- droplevels(genus[genus$Condition!="Input",])
genus$Condition <- as.factor(genus$Condition)
genus$Condition <- factor(genus$Condition,levels=levels(genus$Condition)[c(4,3,1,2,5)])
genus$After_lab_culture <- factor(genus$After_lab_culture,levels=levels(genus$After_lab_culture)[c(2,1)])

#Calculations
#Is in extreme after prolongued
genus$is_extreme_after_culture <- F
genus$is_extreme_after_culture[genus$Treatment_Lab_culture %in% c("Acid After","Base After","Salt After")] <- T
genus$is_in_extreme <- F
genus$is_in_extreme[genus$is_extreme_after_culture & genus$value>0] <- T
length(unique(genus$Genus[genus$is_in_extreme]))

#Is in benign
genus$is_in_benign <-F
genus$is_in_benign[genus$Condition=="Benign" & genus$value>0] <- T
length(unique(genus$X.OTU.ID[genus$is_in_benign]))

#Is in extreme and also in lake
genus$is_in_lake <- F
genus$is_in_lake[genus$Treatment_Lab_culture=="Lake Before" & genus$value>0] <- T
genus_grouped <- group_by(genus, Genus)
genus_summary <- summarise(genus_grouped, is_in_lake_and_extreme=any(is_in_extreme)&any(is_in_lake), is_only_in_extreme=any(is_in_extreme)&!any(is_in_lake),
                           is_in_benign_and_extreme=any(is_in_extreme)&any(is_in_benign))
sum(genus_summary$is_in_lake_and_extreme)
sum(genus_summary$is_only_in_extreme)
sum(genus_summary$is_in_benign_and_extreme)



#Number of OTUs shared between Base and Salt
genus$is_in_salt <-F
genus$is_in_salt[genus$Condition=="Salt" & genus$value>0] <- T
genus$is_in_base <-F
genus$is_in_base[genus$Condition=="Base" & genus$value>0] <- T
genus_grouped <- group_by(genus, Genus)
genus_summary <- summarise(genus_grouped, is_in_both=sum(is_in_salt)==2&sum(is_in_base)==2, is_only_in_salt=sum(is_in_salt)==2&!any(is_in_base), is_only_in_base=!any(is_in_salt)&sum(is_in_base)==2)
sum(genus_summary$is_in_both)
sum(genus_summary$is_only_in_salt)
genus_summary$Genus[genus_summary$is_only_in_salt]
sum(genus_summary$is_only_in_base)

#Mean number of genera per community after prolongued culture
grouped__extreme_genus <- group_by(genus[genus$is_extreme_after_culture,],Treatment_Lab_culture)
mean(summarise(grouped__extreme_genus, genus_count=sum(value>0))$genus_count)
length(unique(genus$Genus[genus$is_in_lake]))

# genus$value[genus$value==0] <- NA

p_is_in_multiple <- qplot(data=genus[genus$is_in_multiple &
                                       genus$assay!="proto300" &
                                       genus$Genus!="Can not be resolved",],
           x=After_lab_culture,
           y=reorder(Genus, pair_count+0.1*value),
           xlab="",
           main="",
           fill=log10(value),
           ylab="",
           #fill=value>0,
           geom="tile",
           stat="identity")+facet_grid(.~Condition,scale="free",space="free")+
  #scale_fill_manual(values=c("TRUE"="black", "FALSE"="white"))
  scale_fill_continuous("Log10 Frequency",low = "white", high="black", na.value = "white")

print(p_is_in_multiple)



p_dominant <- qplot(data=genus[genus$is_dominant,],
                          x=After_lab_culture,
                          y=reorder(Genus, value, function(x)mean(x)),
                          xlab="",
                          main="",
                          fill=log10(value),
                          ylab="",
                          #fill=value>0,
                          geom="tile",
                          stat="identity")+facet_grid(assay~Condition,scale="free",space="free")+
  #scale_fill_manual(values=c("TRUE"="black", "FALSE"="white"))
  scale_fill_continuous("Frequency",low = "black", high="white", na.value = "white")

print(p_dominant)
pdf("./Plots/Reprocessed_sequencing/genus_table.pdf",width=7,height=7)
print(p_is_in_multiple)
print(p_dominant)
graphics.off()

pdf("./Plots/Reprocessed_sequencing/otu_table.pdf",width=8,height=30)
print(p_otu)
graphics.off()
# unique(otu$X.OTU.ID[(otu$X.OTU.ID %in% otu_list$X.OTU.ID) & is.na(otu$Genus)])



