#Housekeeping ####
rm(list=ls())
require(dplyr)
require(ggplot2)
require(reshape)
theme_set(theme_bw())
require(scales)
require(gridExtra)

load("./Outputs/Metagenomics/reprocessed_sequences_frequency.Rdata")
load("./Outputs/Metagenomics/reprocessed_OTU_taxa.RData")

otu$treatment[otu$treatment=="0.65"] <- "High Dalapon (0.65 g/L)"
otu$treatment[otu$treatment=="0.05"] <- "Low Dalapon (0.05 g/L)"
otu$treatment <- factor(otu$treatment, levels=levels(as.factor(otu$treatment))[c(3,2,1)])

names(otu)[names(otu) %in% "X.OTU.ID"] <- "OTU"
count_data <- merge(otu, otu_taxa)
count_data$Genus <- as.character(count_data$Genus)
count_data$Genus[is.na(count_data$Genus)] <- "Unclassified"
count_data$Genus <- as.factor(count_data$Genus)


# count_data <- count_data[count_data$Soil!="B",]

pdf("./Plots/Metagenomics/Re_processed_data/Dominant genus plot.pdf",width=11, height=9)

dodge <- position_dodge(width=0.8)

count_grouped <- group_by(count_data, Soil, treatment,Genus, Domain)
summarized_count <- summarise(count_grouped, total_frequency=sum(value, na.rm=T))
summarized_count_grouped <- group_by(summarized_count,Genus, Domain)
summarized_count <- mutate(summarized_count_grouped,
                           is_in_multiple=sum(total_frequency>0)>1, 
importance_high_dalapon=mean(total_frequency[treatment=="High Dalapon (0.65 g/L)"],
                             na.rm=T),
                              dominant_species=any(total_frequency>0.10))


p_bact <- qplot(data=summarized_count[summarized_count$Domain==" Bacteria" &
                                        summarized_count$dominant_species,],
                xlab="Bacteria genera",
                fill=I("gray"),
                x=reorder(Genus,importance_high_dalapon),
                y=total_frequency,
                ylab="Frequency",
                geom="bar",
                stat="summary",
                fun.y=mean,
                width=0.8,
                position=dodge,
                main="")+
  stat_summary(fun.data=mean_cl_boot, geom = "errorbar", position = dodge, width=0.5)+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),
        plot.margin=unit(c(1,1,-0.1,1), "cm"),
        axis.text.y=element_text(vjust=0.5,face="italic"))+coord_flip()+
  facet_grid(.~treatment)


p_fungi <- qplot(data=summarized_count[summarized_count$Domain==" Eukaryota" &
                                         summarized_count$dominant_species,],
                 xlab="Fungi genera",
                 fill=I("gray"),
                 x=reorder(Genus,importance_high_dalapon),
                 y=total_frequency,
                 ylab="Frequency",
                 geom="bar",
                 stat="summary",
                 fun.y=mean,
                 width=0.8,
                 position=dodge,
                 main="") +
  stat_summary(fun.data=mean_cl_boot, geom = "errorbar", position = dodge, width=0.5)+
  facet_grid(~treatment)+
  coord_flip()+
  scale_y_continuous(labels = percent)+
  theme(plot.margin=unit(c(-0.1,1,1,1), "cm"),
        strip.background=element_blank(),
        strip.text=element_blank(),
        axis.text.y=element_text(vjust=0.5,face="italic"))


p_bact <- ggplot_gtable(ggplot_build(p_bact+theme(legend.position="none")))
p_fungi <- ggplot_gtable(ggplot_build(p_fungi+theme(legend.position="none")))
maxWidth = grid::unit.pmax(p_bact$widths[2:3], p_fungi$widths[2:3])
p_bact$widths[2:3] <- as.list(maxWidth)
p_fungi$widths[2:3] <- as.list(maxWidth)

grid.arrange(p_bact,p_fungi,ncol=1, heights=c(60,40))


graphics.off()

