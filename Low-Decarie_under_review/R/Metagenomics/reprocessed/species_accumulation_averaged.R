#Housekeeping ####
rm(list=ls())
require(dplyr)
require(ggplot2)
require(reshape)
theme_set(theme_bw())
require(scales)

pdf("./Plots/Metagenomics/Re_processed_data/OTU_accumulation_averaged.pdf",width=7,height=7)

species_accumulation <- function(ylab="Frequency"){

count_data_by <- group_by(count_data,assay,treatment, Soil)
count_data_ordered <- arrange(count_data_by,-value)
count_data <- mutate(count_data_ordered, index=order(-value),
                     cum_sum=cumsum(value))

count_data$treatment[count_data$treatment==0.05] <- "Benign\n(0.05 g/L Dalapon)"
count_data$treatment[count_data$treatment==0.65] <- "Lethal\n(0.65 g/L Dalapon)"
count_data$treatment[count_data$treatment=="Source community"] <- "Source community"
count_data$assay[count_data$assay=="28F"] <- "Bacteria"
count_data$assay[count_data$assay=="SSU"] <- "Fungi"

count_data$treatment <- as.factor(count_data$treatment)
count_data$treatment <- factor(count_data$treatment,  levels(count_data$treatment)[c(3,1,2)])

p <- qplot(data=count_data,
           x=index,
           xlab="OTU index",
           y=value,
           ylab=ylab,
           geom="line",
#            log="x",
           size=I(1),
           stat="summary",
           fun.y=mean)+
  stat_summary(fun.data=mean_cl_boot, geom = "ribbon", alpha=0.4, B=100)+
  facet_grid(assay~treatment, scale="free_y")+scale_x_log10(lim=c(1,100))+
  scale_linetype_discrete("Source soil")

if(ylab=="Frequency"){
p <- p+ scale_y_continuous(labels = percent)}

print(p)

return(p)

# scale_fill_manual("Dalapon category\nduring selection\n(Phase 1)",
#                   values=c("benign"="#2FB3CA",
#                            "sub-lethal"="#F69654",
#                            "lethal"="#F1564F"))
}

load("./Outputs/Metagenomics/reprocessed_sequences_counts.Rdata")
count_data <- otu[otu$value>1,]
p <- species_accumulation(ylab="Number of sequences")

load("./Outputs/Metagenomics/reprocessed_sequences_frequency.Rdata")
count_data <- otu[otu$value>0,]
p <- species_accumulation(ylab="Frequency")



graphics.off()

print(p)
