#Housekeeping ####
rm(list=ls())
require(dplyr)
require(ggplot2)
require(reshape)
theme_set(theme_bw())
require(scales)
require(gridExtra)
require(Hmisc)

load("./Outputs/Metagenomics/reprocessed_OTU_with_taxa.RData")

pdf("./Plots/Metagenomics/Re_processed_data/Dominant genus plot.pdf",width=11, height=9)

dodge <- position_dodge(width=0.8)

#Sum values from OTU within a genus
count_grouped <- group_by(count_data, Soil, treatment,Genus, Domain, Sample_name, assay)
summarized_count <- summarise(count_grouped, total_frequency=sum(value, na.rm=T))

#Average of samples for a given community
count_grouped <- group_by(summarized_count, Soil, treatment,Genus, Domain, assay)
summarized_count <- summarise(count_grouped, total_frequency=mean(total_frequency, na.rm=T))

#Id common and multiples
summarized_count_grouped <- group_by(summarized_count,Genus, Domain, assay)
summarized_count <- mutate(summarized_count_grouped,
                           is_in_multiple=sum(total_frequency>0)>1, 
importance_high_dalapon=mean(total_frequency[treatment=="Lethal \n(0.65 g/L Dalapon)"],
                             na.rm=T),
                              dominant_species=any(total_frequency>0.10))





#Calculate individual values 
smean.cl.boot(summarized_count$total_frequency[summarized_count$Genus==" Pseudomonas" & summarized_count$treatment=="Source community" & summarized_count$assay=="28F"])
smean.cl.boot(summarized_count$total_frequency[summarized_count$Genus==" Serratia" & summarized_count$treatment=="Source community" & summarized_count$assay=="28F"])

smean.cl.boot(summarized_count$total_frequency[summarized_count$Genus==" Pseudomonas" & summarized_count$treatment=="Lethal \n(0.65 g/L Dalapon)" & summarized_count$assay=="28F"])
smean.cl.boot(summarized_count$total_frequency[summarized_count$Genus==" Serratia" & summarized_count$treatment=="Lethal \n(0.65 g/L Dalapon)" & summarized_count$assay=="28F"])

smean.cl.boot(summarized_count$total_frequency[summarized_count$Genus==" Burkholderia" & summarized_count$treatment=="Source community" & summarized_count$assay=="28F"])
smean.cl.boot(summarized_count$total_frequency[summarized_count$Genus==" Dyella" & summarized_count$treatment=="Source community" & summarized_count$assay=="28F"])
smean.cl.boot(summarized_count$total_frequency[summarized_count$Genus==" Yersinia" & summarized_count$treatment=="Source community" & summarized_count$assay=="28F"])

smean.cl.boot(summarized_count$total_frequency[summarized_count$Genus==" Burkholderia" & summarized_count$treatment=="Source community" & summarized_count$assay=="28F"])
smean.cl.boot(summarized_count$total_frequency[summarized_count$Genus==" Burkholderia" & summarized_count$treatment=="Benign \n(0.05 g/L Dalapon)" & summarized_count$assay=="28F"])

smean.cl.boot(summarized_count$total_frequency[summarized_count$Genus==" Dyella" & summarized_count$treatment=="Source community" & summarized_count$assay=="28F"])
smean.cl.boot(summarized_count$total_frequency[summarized_count$Genus==" Dyella" & summarized_count$treatment=="Benign \n(0.05 g/L Dalapon)" & summarized_count$assay=="28F"])

smean.cl.boot(summarized_count$total_frequency[summarized_count$Genus==" Yersinia" & summarized_count$treatment=="Source community" & summarized_count$assay=="28F"])
smean.cl.boot(summarized_count$total_frequency[summarized_count$Genus==" Yersinia" & summarized_count$treatment=="Benign \n(0.05 g/L Dalapon)" & summarized_count$assay=="28F"])



mean(summarized_count$total_frequency[summarized_count$Genus==" Pseudomonas" & summarized_count$treatment=="Lethal \n(0.65 g/L Dalapon)" & summarized_count$assay=="28F"])
sd(summarized_count$total_frequency[summarized_count$Genus==" Pseudomonas" & summarized_count$treatment=="Lethal \n(0.65 g/L Dalapon)" & summarized_count$assay=="28F"])


smean.cl.boot(summarized_count$total_frequency[summarized_count$Genus==" Stenotrophomonas" & summarized_count$treatment=="Source community" & summarized_count$assay=="28F"])
smean.cl.boot(summarized_count$total_frequency[summarized_count$Genus==" Stenotrophomonas" & summarized_count$treatment=="Lethal \n(0.65 g/L Dalapon)" & summarized_count$assay=="28F"])

smean.cl.boot(summarized_count$total_frequency[summarized_count$Genus==" Delftia" & summarized_count$treatment=="Source community" & summarized_count$assay=="28F"])
smean.cl.boot(summarized_count$total_frequency[summarized_count$Genus==" Delftia" & summarized_count$treatment=="Lethal \n(0.65 g/L Dalapon)" & summarized_count$assay=="28F"])

smean.cl.boot(summarized_count$total_frequency[summarized_count$Genus==" Duganella" & summarized_count$treatment=="Source community" & summarized_count$assay=="28F"])
smean.cl.boot(summarized_count$total_frequency[summarized_count$Genus==" Duganella" & summarized_count$treatment=="Lethal \n(0.65 g/L Dalapon)" & summarized_count$assay=="28F"])

smean.cl.boot(summarized_count$total_frequency[summarized_count$Genus==" Trichoderma" & summarized_count$assay=="SSU"])

smean.cl.boot(summarized_count$total_frequency[summarized_count$Genus==" Trichosporon" & (summarized_count$treatment %in% c("Source community","Benign \n(0.05 g/L Dalapon)")) & summarized_count$assay=="SSU"])

smean.cl.boot(summarized_count$total_frequency[summarized_count$Genus==" Cryptococcus" & summarized_count$treatment=="Source community" & summarized_count$assay=="SSU"])
smean.cl.boot(summarized_count$total_frequency[summarized_count$Genus==" Cryptococcus" & summarized_count$treatment=="Lethal \n(0.65 g/L Dalapon)" & summarized_count$assay=="SSU"])

smean.cl.boot(summarized_count$total_frequency[summarized_count$Genus==" Penicillium" & summarized_count$treatment=="Source community" & summarized_count$assay=="SSU"])
smean.cl.boot(summarized_count$total_frequency[summarized_count$Genus==" Penicillium" & summarized_count$treatment=="Lethal \n(0.65 g/L Dalapon)" & summarized_count$assay=="SSU"])

smean.cl.boot(summarized_count$total_frequency[summarized_count$Genus==" Umbelopsis" & summarized_count$treatment=="Source community" & summarized_count$assay=="SSU"])
smean.cl.boot(summarized_count$total_frequency[summarized_count$Genus==" Umbelopsis" & summarized_count$treatment=="Lethal \n(0.65 g/L Dalapon)" & summarized_count$assay=="SSU"])






summarized_count$total_frequency_log10 <- log10(summarized_count$total_frequency)

p_bact <- qplot(data=summarized_count[summarized_count$assay=="28F" &
                                      summarized_count$is_in_multiple,],
                xlab="Bacteria genera",
                fill=I("gray"),
                x=reorder(Genus,importance_high_dalapon),
                y=total_frequency,
                ylab="Frequency",
                ylim=c(0,1),
                geom="bar",
                stat="summary",
                fun.y=mean,
                width=0.8,
                position=dodge,
                main="")+
  stat_summary(fun.data=mean_cl_boot, geom = "errorbar", position = dodge, width=0.5)+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),
        plot.margin=unit(c(1,1,-0.1,1), "cm"),
        axis.text.y=element_text(vjust=0.5,face="italic"))+
  coord_flip()+
  facet_grid(.~treatment)


p_fungi <- qplot(data=summarized_count[summarized_count$assay=="SSU" &
                                       summarized_count$is_in_multiple,],
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
  scale_y_continuous(labels = percent, limits=c(0,1))+
  theme(plot.margin=unit(c(-0.1,1,1,1), "cm"),
        strip.background=element_blank(),
        strip.text=element_blank(),
        axis.text.y=element_text(vjust=0.5,face="italic"))


p_bact <- ggplot_gtable(ggplot_build(p_bact+theme(legend.position="none")))
p_fungi <- ggplot_gtable(ggplot_build(p_fungi+theme(legend.position="none")))
maxWidth = grid::unit.pmax(p_bact$widths[2:3], p_fungi$widths[2:3])
p_bact$widths[2:3] <- as.list(maxWidth)
p_fungi$widths[2:3] <- as.list(maxWidth)

grid.arrange(p_bact,p_fungi,ncol=1, heights=c(70,30))


graphics.off()

