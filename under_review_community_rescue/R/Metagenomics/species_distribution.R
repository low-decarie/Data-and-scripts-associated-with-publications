distribution_counts <- function(count_data,level="Species") {

pdf(paste("./Plots/Metagenomics/By_",level,"/species_distribution.pdf",sep=""),width=9)


print(paste("Processing ",level,"/Soil_diversity.pdf",sep="")) 
  
  p <- qplot(data=count_data,
             x=log10(value),
             xlab=paste("Number of sequences per",level,"(Log10)"),
             ylab=paste("Number of ",level),
                        fill=Soil,
             binwidth = 1)+
    facet_grid(Domain~treatment, scale="free")+
    scale_fill_manual("Source soil", values=c("A"="#AA3939","B"="#AA6C39","C"="#226666", "D"="#339933"))
  
  print(p)

p <- qplot(data=count_data,
           x=value,
           xlab=paste("Number of sequences per",level),
           ylab=paste("Number of ",level),
           fill=Soil,log="xy", binwidth=0.5)+
  facet_grid(Domain~treatment, scale="free")+
  scale_fill_manual("Source soil", values=c("A"="#AA3939","B"="#AA6C39","C"="#226666", "D"="#339933"))

print(p)



graphics.off()
  
}