# dominant species frequency ####
require(scales)
require(gridExtra)

plot_dominant_averaged <- function(count_data,level, dominance_criteria=0.25){
  
  pdf(paste("./Plots/Metagenomics/By_",level,"/dominant species frequency_averaged.pdf",sep=""),width=8, height=7.5)
  
  print(paste("Processing ",level,"/dominant species frequency.pdf",sep=""))
  
  dodge <- position_dodge(width=0.8)
  
  
  count_data$name <- gsub("(::)+","\n",count_data$name,
                          ignore.case = T)
  
  count_data$name <- gsub("\\(","\n\\(",count_data$name,
                          ignore.case = T)
  
  #Merge counts of types that can only be resolved to the uper taxonomic level
  
  #   count_data <- ddply(.data=count_data,
  #                       .variable="name",
  #                       function(x){
  #                         if(grepl(" sp",x$name)){x$name <- gsub("(.*::)+","",x$name,
  #                                                                   ignore.case = T)}else{
  #                         count_data$name <- gsub("(::)+","\n",count_data$name,
  #                                                 ignore.case = T)}
  #                       return(x)})
  #   
  #   
  #   count_data <- ddply(.data=count_data,
  #                              .variables=c("Sample_number", "name", "Domain",
  #                                           "Assay", "Sample_name", 
  #                                           "PlateNo", "Soil", "Dil", 
  #                                           "Dispersal", "comple_info", "plateID", 
  #                                           "Dalapon", "Glucose"),
  #                              function(x)data.frame(value=sum(x$value)))
  
  
  count_data <- ddply(.data=count_data,
                      .variables=c("Sample_number","Assay"),
                      function(x)data.frame(x,frequency=x$value/sum(x$value,na.rm=T)))
  
  count_data <- ddply(.data=count_data,
                      .variables=c("name"),
                      function(x)data.frame(x,
                                            dominant_species=any(x$frequency>dominance_criteria),
                                            median_in_source=median(x$frequency[x$treatment=="Source"])))
  
  
  
  p_bact <- qplot(data=count_data[count_data$Domain=="Bacteria" & count_data$dominant_species ,],
#                   fill=Soil,
fill=I("gray"),
                  xlab="Dominant bacteria",
                  x=reorder(name,median_in_source),
                  y=frequency,
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
          axis.text.y=element_text(vjust=0.5,face="italic"))+
     facet_grid(~treatment)+coord_flip() 
#     scale_fill_manual("Source soil", values=c("A"="#AA3939","B"="#AA6C39","C"="#226666", "D"="#339933"))
  
  
  p_fungi <- qplot(data=count_data[count_data$Domain=="Fungi" & count_data$dominant_species ,],
#                    fill=Soil,
fill=I("grey"),
                   xlab="Dominant fungi",
                   x=reorder(name,median_in_source),
                   y=frequency,
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
#     scale_fill_manual("Source soil", values=c("A"="#AA3939","B"="#AA6C39","C"="#226666", "D"="#339933"))+
    theme(plot.margin=unit(c(-0.1,1,1,1), "cm"),
          strip.background=element_blank(),
          strip.text=element_blank(),
          axis.text.y=element_text(vjust=0.5,face="italic", lineheight=0.8))
  
  
  g_legend<-function(a.gplot){
    tmp <- ggplot_gtable(ggplot_build(a.gplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    legend
  }
  
#   legend <- g_legend(p_bact+theme(legend.position="bottom"))
  
  p_bact <- ggplot_gtable(ggplot_build(p_bact+theme(legend.position="none")))
  p_fungi <- ggplot_gtable(ggplot_build(p_fungi+theme(legend.position="none")))
  maxWidth = grid::unit.pmax(p_bact$widths[2:3], p_fungi$widths[2:3])
  p_bact$widths[2:3] <- as.list(maxWidth)
  p_fungi$widths[2:3] <- as.list(maxWidth)
  
  grid.arrange(p_bact,p_fungi, ncol=1, heights=c(45,55))
  
  
  graphics.off()
  
}


