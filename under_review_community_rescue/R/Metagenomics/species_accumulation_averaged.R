species_accumulation_averaged <- function(count_data, level="Species"){
  
  pdf(paste("./Plots/Metagenomics/By_",level,"/species_accumulation_averaged.pdf",sep=""),height=5, width=8)
  
  print(paste("Processing ",level,"/species_accumulation_averaged.pdf",sep=""))


# 
count_data_by <- group_by(count_data, Domain,treatment, Soil)
count_data_ordered <- arrange(count_data_by,-value)
count_data <- mutate(count_data_ordered, index=order(-value),
                                            cum_sum=cumsum(value))


p <- qplot(data=count_data,
           x=index,
           xlab=paste(level,"index (from most common to least common)"),
           y=value,
           ylab="Number of sequences",
           geom="line",
           log="x",
           size=I(1),
           stat="summary",
           fun.y=mean)+
  stat_summary(fun.data=mean_cl_boot, geom = "ribbon", alpha=0.4, B=100)+
  facet_grid(Domain~treatment, scale="free_y")+
  scale_linetype_discrete("Source soil")

print(p)


graphics.off()

}