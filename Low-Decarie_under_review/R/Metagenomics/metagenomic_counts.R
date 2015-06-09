#Housekeeping ####
rm(list=ls())
require(plyr)
require(ggplot2)
require(reshape)
library(stringr)
require(vegan)
require(scales)
require(gridExtra)
require(ggpca)
require(randomForest)
require(dplyr)


source("./R/theme.R")
theme_set(theme_minimal())

classifications <- c("Species",
                     "Genus",
                     "Family",
                     "Order",
                     "Class",
#                     "OTU",
                     "Phylum")

load_and_plot <- function(level="Species"){try({
  
# load the data

print(paste("Loading data for", level))
  
pattern <- paste(".*",level,"Counts.csv",sep="")
  
file.list <- list.files(path=".",
                        recursive="T",
                        pattern=pattern)

pattern <- paste("[A-Z][a-z]*",level,sep="")

count_data <- ldply(.data=file.list,
#                     .progress="text",
                    function(x){
                      temp <- read.csv(x)
                      temp$Domain <- sub(level,"",str_extract(x, pattern))
                      temp$Assay <- sub("Analysis","",str_extract(x, "[A-Z]Analysis"))
                      names(temp) <- do.call(rbind,str_split(names(temp),"\\."))[,1]
                      return(temp)
                    })

molten.count_data <- melt(count_data,
                          id.vars=c("name",
                                    "Domain",
                                    "Assay"))

names(molten.count_data)[names(molten.count_data) %in% "variable"] <- "Sample_number"
molten.count_data$Sample_number <- as.character(molten.count_data$Sample_number)

# Sample_name code
Sample_code <- read.csv("./Data/Metagenomics/Sample_codes.csv")
Sample_code$Sample_number <- paste("X",Sample_code$Sample_number, sep="")

pruned_Sample_code <- Sample_code[Sample_code$Sample_number %in% molten.count_data$Sample_number,]

molten.count_data <- merge(molten.count_data, Sample_code)


#Merge counts of species that can not be seperated (ie that have the ::)
molten.count_data$name <- gsub("(.*::)+","",molten.count_data$name,
                               ignore.case = T)


molten.count_data <- ddply(.data=molten.count_data,
                           .variables=c("Sample_number", "name", "Domain",
                                        "Assay", "Sample_name", 
                                        "PlateNo", "Soil", "Dil", 
                                        "Dispersal", "comple_info", "plateID", 
                                        "Dalapon", "Glucose"),
                           function(x)data.frame(value=sum(x$value)))


molten.count_data <- ddply(.data=molten.count_data,
                           .variables=c("Sample_number","Assay"),
                           function(x)data.frame(x,frequency=x$value/sum(x$value,na.rm=T)))



molten.count_data <- ddply(.data=molten.count_data,
                           .variables=c("name"),
                           function(x)data.frame(x,dominant_species=any(x$frequency>0.25)))

molten.count_data$treatment <-as.character(molten.count_data$Dalapon)
molten.count_data$treatment[is.na(molten.count_data$treatment)] <- "Source"



write.csv(molten.count_data, file="./Outputs/Metagenomics/species_counts.csv")

write.csv(pruned_Sample_code, file="./Data/Metagenomics/pruned_Sample_code.csv")




print(paste("Conducting diversity calculations for ", level))



#Diversity calculations
diversity.calc <- function(count_data){
  community_matrix <- count_data[,!names(count_data) %in% c("name", "in", "pH2_s",
                                                            "pH12", "salt", "Domain", "Assay",
                                                            "pH2_r")]
  
#   community_matrix <- community_matrix[rowSums(community_matrix)>10,]
  
  species_count <- specnumber(community_matrix,MARGIN=2)
  shannon_diversity <- diversity(community_matrix,MARGIN=2)
  rarefied_diversity <- rarefy(community_matrix,sample=1000,se=T, MARGIN=2)
  
  diversity_all <- rbind(species_count,
                         shannon_diversity,
                         rarefied_diversity)
  
  diversity_all <- data.frame(t(diversity_all))
  diversity_all$Sample_number <- rownames(diversity_all)
  diversity_all <- merge(diversity_all, Sample_code)
  
  return(diversity_all)
}

div.fungi <- diversity.calc(count_data[count_data$Domain=="Fungi",])
div.fungi$Domain <- "Fungi"

div.bacteria <- diversity.calc(count_data[count_data$Domain=="Bacteria",])
div.bacteria$Domain <- "Bacteria"

div.all <- rbind(div.fungi,div.bacteria)

molten.count_data_temp <- molten.count_data[molten.count_data$value>0,]
molten.count_data_temp$Dalapon <- as.character(molten.count_data_temp$Dalapon)
molten.count_data_temp$Dalapon[is.na( molten.count_data_temp$Dalapon)] <- "Source"
molten.count_data_temp$Dalapon[molten.count_data_temp$Dalapon==0.05] <- "Low Dalapon (0.05 g/L)"
molten.count_data_temp$Dalapon[molten.count_data_temp$Dalapon==0.65] <- "High Dalapon (0.65 g/L)"

molten.count_data_temp$Dalapon <- factor(molten.count_data_temp$Dalapon)
molten.count_data_temp$Dalapon <- factor(molten.count_data_temp$Dalapon, levels=levels(molten.count_data_temp$Dalapon)[c(3,2,1)])

#Soil_diversity ####

pdf(paste("./Plots/Metagenomics/By_",level,"/Soil_diversity.pdf",sep=""),width=9)


print(paste("Processing ",level,"/Soil_diversity.pdf",sep=""))

try({
  

  
  #Rarefaction ####
#   #expand species by value times
#   molten.count_data_temp$row <- 1:nrow(molten.count_data_temp)
#   molten.count_data_temp <- ddply(.data=molten.count_data_temp,
#                                 .variables="row",
#                                 function(x)x[rep(1,x$value),])
#   
#   #sample within each sample
#   molten.count_data_temp <- ddply(.data=molten.count_data_temp,
#                                   .variables=c("Domain", "Sample_name"),
#                                   function(x)x[sample(1:nrow(x),
#                                                       size=min(500,nrow(x))),])
#   
#   #count hits for each species
# molten.count_data_temp <- ddply(.data=molten.count_data_temp,
#                                 .variables=c("row"),
#                                 function(x)data.frame(unique(x),rarefied_value=nrow(x)))
  


summary_counts <- ddply(.data=molten.count_data_temp,
                        .variable=c("Dalapon","Soil"),
                        function(x)data.frame(common_species=sum(x$value>100)))

print(summary_counts)

summary_counts <- ddply(.data=summary_counts,
                        .variable=c("Dalapon"),
                        function(x)data.frame(mean_common_species=mean(x$common_species)))

print(summary_counts)

soils <- div.all[grep("Soil", div.all$Sample_name),]
dodge <- position_dodge(width=1)
p_rarefied_soil <- qplot(data=soils,
                         x=Sample_name,
                         y=S,
                         fill=Domain,
                         ylab="Species counts (rarefied to 1000 total counts)",
                         position=dodge,
                         stat="identity",
                         geom="bar")+
  geom_errorbar(aes(ymin=S-se,ymax=S+se), width=0.5, position=dodge)+
  scale_fill_grey()


print(p_rarefied_soil)


p <- qplot(data=soils,
           x=Sample_name,
           y=species_count,
           ylab="Raw species counts",
           fill=Domain,
           position=dodge,
           stat="identity",
           geom="bar")


print(p)


p <- qplot(data=soils,
           x=Sample_name,
           y=shannon_diversity,
           ylab="Shannon diversity index",
           position=dodge,
           fill=Domain,
           stat="identity",
           geom="bar")


print(p)


p_rarefied_selection <- qplot(data=div.all[!is.na(div.all$Dalapon),],
                              x=as.factor(Dalapon),
                              xlab="Dalapon",
                              fill=Domain,
                              facets=.~Soil,
                              y=S,
                              ylab="Species counts (rarefied to 1000 total counts)",
                              position=dodge,
                              stat="summary",
                              fun.y="mean",
                              geom="bar")+
  stat_summary(fun.data=mean_sdl, geom = "errorbar", position=dodge)


print(p_rarefied_selection)


p_rarefied_soil <- qplot(data=soils,
                         x=Sample_name,
                         y=S,
                         fill=Domain,
                         ylab="Species counts (rarefied to 1000 total counts)",
                         position=dodge,
                         stat="identity",
                         geom="bar")+
  facet_grid(.~Soil,scale="free")+
  geom_errorbar(aes(ymin=S-se,ymax=S+se), width=0.5, position=dodge)


print(p_rarefied_soil)

grid.arrange(p_rarefied_soil,p_rarefied_selection)


selection_communities <- div.all[-grep("Soil", div.all$Sample_name),]
soils_temp <- soils[,names(soils) %in% c("Domain","Soil","species_count", "shannon_diversity", "S")]
names(soils_temp) <- c("species_count_soil", "shannon_diversity_soil", "S_soil","Soil","Domain")
selection_communities <- merge(selection_communities,soils_temp,all=T)

p <- qplot(data=selection_communities,
           x=S_soil,
           xlab="Rarefied diversity in source population",
           y=S,
           ylab="Rarefied diversity in community after selection",
           facets=.~Domain,
           geom="text",
           label=Soil,
           colour=as.factor(Dalapon))+
  geom_smooth(method="lm",se=F)

print(p)

p <- qplot(data=selection_communities,
           x=shannon_diversity_soil,
           xlab="Shannon diversity in source population",
           y=shannon_diversity,
           ylab="Shannon diversity in community after selection",
           facets=.~Domain,
           geom="text",
           label=Soil,
           colour=as.factor(Dalapon))+
  geom_smooth(method="lm",se=F)

print(p)



p <- qplot(data=selection_communities,
           x=species_count_soil,
           xlab="Species count in source population",
           y=species_count,
           ylab="Species count in community after selection",
           facets=.~Domain,
           geom="text",
           label=Soil,
           colour=as.factor(Dalapon))+
  geom_smooth(method="lm",se=F)

print(p)


graphics.off()

})


# diversity adaptation ####

pdf(paste("./Plots/Metagenomics/By_",level,"/diversity adaptation.pdf",sep=""),width=9)

print(paste("Processing ",level,"/diversity adaptation.pdf",sep=""))


try({

soils <- div.all[grep("Soil", div.all$Sample_name),
                 names(div.all) %in% c("species_count", "shannon_diversity", "S", 
                                       "se", "Domain","Soil")]

load(file="./Outputs/phase 2 as binary landscape.RData")

counts.from.binary <- ddply(.data=binary,
                            .variables=c("Soil",
                                         "Dil",
                                         "Dispersal"),
                            function(x){data.frame(counts.viable=sum(as.numeric(x$is.viable)),
                                                   occupancy=sum(as.numeric(x$is.viable))/length(as.numeric(x$is.viable)))})

binary_and_diversity <- merge(soils, counts.from.binary, all=T)

binary_and_diversity$treatment <- with(binary_and_diversity, paste(Dil, Dispersal))


p <- qplot(data=binary_and_diversity,
           x=S,
           y=occupancy,
           xlab="Species counts (rarefied to 1000 total counts)",
           shape=Dispersal,
           linetype=Dispersal,
           colour=as.factor(Dil))+
  geom_text(data=soils,
            aes(x=S,
                y=0.3,
                label=Soil,
                shape=NULL,
                linetype=NULL,
                colour=NULL))+
  facet_grid(.~Domain, scale="free")+
  geom_line()
#   geom_smooth(method="lm", se=F)

print(p)



p <- qplot(data=binary_and_diversity,
           x=species_count,
           y=occupancy,
           xlab="Species counts",
           shape=Dispersal,
           linetype=Dispersal,
           colour=as.factor(Dil))+
  geom_text(data=soils,
            aes(x=species_count,
                y=0.3,
                label=Soil,
                shape=NULL,
                linetype=NULL,
                colour=NULL))+
  facet_grid(.~Domain, scale="free")+
  geom_line()
#   geom_smooth(method="lm", se=F)

print(p)

p <- qplot(data=binary_and_diversity,
           x=shannon_diversity,
           y=occupancy,
           xlab="Shannon diversity",
           shape=Dispersal,
           linetype=Dispersal,
           colour=as.factor(Dil))+
  geom_text(data=soils,
            aes(x=shannon_diversity,
                y=0.3,
                label=Soil,
                shape=NULL,
                linetype=NULL,
                colour=NULL))+
  facet_grid(.~Domain, scale="free")+
  geom_line()
#   geom_smooth(method="lm", se=F)

print(p)



graphics.off()


})



})}


#load_and_plot("OTU")

# load_and_plot("Species")

l_ply(.data=classifications,
      .progress="text",
      .fun=load_and_plot)



