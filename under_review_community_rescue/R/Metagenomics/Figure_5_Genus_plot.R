#Housekeeping ####
rm(list=ls())
require(dplyr)
require(ggplot2)
require(reshape)
theme_set(theme_bw())
require(scales)
require(gridExtra)

load("./Outputs/Metagenomics/reprocessed_OTU_with_taxa.RData")


  pdf("./Plots/Metagenomics/Re_processed_data/Genus plot.pdf",width=11, height=9)
  
  dodge <- position_dodge(width=0.8)
  
  count_grouped <- group_by(count_data, Soil, treatment,Genus, Domain, Sample_name,Sample_number)
  summarized_count <- summarise(count_grouped, total_frequency=sum(value, na.rm=T))

  summarized_count_grouped <- group_by(summarized_count,Genus, Domain)
  summarized_count <- mutate(summarized_count_grouped, is_in_multiple=sum(total_frequency>0)>1, importance_high_dalapon=mean(total_frequency[treatment=="Lethal \n(0.65 g/L Dalapon)"], na.rm=T))
  
  
  p_bact <- qplot(data=summarized_count[summarized_count$Domain==" Bacteria" &
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
  
  
  p_fungi <- qplot(data=summarized_count[summarized_count$Domain==" Eukaryota" &
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
  
  grid.arrange(p_bact,p_fungi,ncol=1, heights=c(0.7,0.3))


count_data_grouped <- group_by(summarized_count, Soil, treatment, Domain)
summarized_by_sample <- summarise(count_data_grouped,genera_count=sum(total_frequency>0))
summarized_by_treatment <- group_by(summarized_by_sample[summarized_by_sample$treatment=="Source community",], Soil, Domain)
summarized_by_treatment <- summarise(summarized_by_treatment,genera_count=mean(genera_count))
summarized_by_treatment_grouped <- group_by(summarized_by_treatment, Domain)
summarized_by_assay <- summarise(summarized_by_treatment_grouped,genera_count_mean=median(genera_count), genera_count_SD=sd(genera_count), min=min(genera_count), max=max(genera_count))

as.data.frame(summarized_by_assay)




count_data_grouped <- group_by(summarized_count, Soil, treatment)
summarized_by_sample <- summarise(count_data_grouped,genera_count=sum(total_frequency>0))
smean.cl.boot(data.frame(summarized_by_sample[summarized_by_sample$treatment=="Source community","genera_count"]))



count_data_grouped <- group_by(summarized_count, Soil, treatment)
summarized_by_sample <- summarise(count_data_grouped,genera_count=sum(total_frequency>0))
smean.cl.boot(data.frame(summarized_by_sample[summarized_by_sample$treatment=="Lethal \n(0.65 g/L Dalapon)","genera_count"]))

as.data.frame(summarized_by_assay)
  
  
  graphics.off()
