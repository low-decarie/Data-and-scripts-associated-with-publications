#Restart R if plyr has been loaded


# #Housekeeping ####
rm(list=ls())
require(dplyr)
require(ggplot2)
require(reshape)
require(Hmisc)
require(dplyr)
require(reshape)
theme_set(theme_bw())
# 
# 
load("./Metagenomics data/Re_processed_data/otu_table.RData")

otu_species <- read.csv("./Metagenomics data/Re_processed_data/otu_list.csv")


#Select data and reorder
otu <- otu[otu$Experiment==2,]
otu <- droplevels(otu[otu$Condition!="Input",])
otu$Condition <- factor(otu$Condition, levels=levels(as.factor(otu$Condition))[c(4,3,1,2,5)])

#Calculate cumulative sums
count_data_by <- group_by(otu,Condition, After_lab_culture)
count_data_ordered <- arrange(count_data_by,-value)
count_data <- mutate(count_data_ordered, index=order(-value),
                     cum_sum=cumsum(value),
                     frequency=value/sum(value, na.rm=T))

#Plot
p <- qplot(data=count_data,
           x=index,
           y=value,
#            ylab=,
           xlab="Index of OTUs",
           geom="line",
#            colour=After_lab_culture,
           linetype=After_lab_culture)+
  facet_grid(.~Condition, scale="free_y")+
  scale_x_log10(breaks=c(10, 100, 1000), labels=c(10, 100, 1000))+
  scale_y_log10(name="Number of sequences")+
#   scale_color_grey("Laboratory culture")+
  scale_linetype_discrete("Laboratory culture")
print(p)


#Number of sequences per sample
count_data_by <- group_by(otu,assay, Sample_number)
summary_counts <- summarise(count_data_by, counts=sum(value))
smean.cl.boot(summary_counts$counts)

#Number of otus in extreme bioreactors after prolongued culture and lake
otu$present <- otu$value>0
otu$Treatment_Lab_culture <- paste(otu$Condition, otu$After_lab_culture)
otu$is_extreme_after_culture <- F
otu$is_extreme_after_culture[otu$Treatment_Lab_culture %in% c("Acid After","Base After","Salt After")] <- T
count_data_by <- group_by(otu,Condition, After_lab_culture, is_extreme_after_culture)
summary_counts <- summarise(count_data_by, total_OTU=sum(present))
smean.cl.boot(summary_counts$total_OTU[summary_counts$is_extreme_after_culture])
summary_counts$total_OTU[summary_counts$Condition=="Lake"]

#Is in extreme
otu$is_in_extreme_after_culture  <- F
otu$is_in_extreme_after_culture[otu$is_extreme_after_culture & otu$value>0] <- T
length(unique(otu$X.OTU.ID[otu$is_in_extreme_after_culture]))

#Is in lake
otu$is_in_lake <-F
otu$is_in_lake[otu$Condition=="Lake" & otu$value>0] <- T
length(unique(otu$X.OTU.ID[otu$is_in_lake]))

#Is in benign
otu$is_in_benign <-F
otu$is_in_benign[otu$Condition=="Benign" & otu$value>0] <- T
length(unique(otu$X.OTU.ID[otu$is_in_benign]))

#Number of OTUs that are in extreme bioreactor that are or are not found in lake
otu_grouped <- group_by(otu, X.OTU.ID)
otu_summary <- summarise(otu_grouped, is_in_lake_and_extreme=any(is_in_extreme_after_culture)&any(is_in_lake), is_only_in_extreme=any(is_in_extreme_after_culture)&!any(is_in_lake),
                         is_in_benign_and_extreme=any(is_in_extreme_after_culture)&any(is_in_benign))
sum(otu_summary$is_in_lake_and_extreme)
sum(otu_summary$is_only_in_extreme)
sum(otu_summary$is_in_benign_and_extreme)


#Number of OTUs shared between Base and Salt
otu$is_in_salt <-F
otu$is_in_salt[otu$Condition=="Salt" & otu$value>0] <- T
otu$is_in_base <-F
otu$is_in_base[otu$Condition=="Base" & otu$value>0] <- T
otu_grouped <- group_by(otu, X.OTU.ID)
otu_summary <- summarise(otu_grouped, is_in_both=sum(is_in_salt)==2&sum(is_in_base)==2, is_only_in_salt=sum(is_in_salt)==2&!any(is_in_base), is_only_in_base=!any(is_in_salt)&sum(is_in_base)==2)
sum(otu_summary$is_in_both)
sum(otu_summary$is_only_in_salt)
sum(otu_summary$is_only_in_base)




pdf("./Plots/Reprocessed_sequencing/otu_accumulation.pdf",width=10,height=3)
print(p)
graphics.off()