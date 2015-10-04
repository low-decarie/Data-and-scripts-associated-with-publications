# #Housekeeping ####
rm(list=ls())
require(dplyr)
require(ggplot2)
require(reshape)
require(Hmisc)
require(dplyr)
require(ggpca)
require(reshape)
require(vegan)
theme_set(theme_bw())
# 
# 

load("./Metagenomics data/Re_processed_data/otu_table_frequency.RData")
otu_species <- read.csv("./Metagenomics data/Re_processed_data/otu_list.csv")
 otu <- otu[otu$Experiment==2,]
otu <- droplevels(otu[otu$Condition!="Input",])

#Calculation


#Tag as in multiple or dominant
otu <- group_by(otu, X.OTU.ID)
otu <- mutate(otu, is_in_multiple=sum(value>0)>1,
                  is_dominant=any(value>0.05))
otu <- otu[otu$is_in_multiple,]


otu$Treatment_Lab_culture <- paste(otu$Condition, otu$After_lab_culture)
otu_matrix <- cast(otu, Treatment_Lab_culture~X.OTU.ID, fun.aggregate=sum)
otu_matrix <- as.matrix(otu_matrix)




pdf("./Plots/Reprocessed_sequencing/otu_CCA.pdf",width=4.5,height=3)


cca_out <- cca(X=otu_matrix)
plot(cca_out, type="n", choices = c(1, 2))
text(cca_out)
# plot(cca_out, type="n", choices = c(3, 4))
# text(cca_out, choices = c(3, 4))
# plot(cca_out, type="n", choices = c(5, 6))
# text(cca_out, choices = c(5, 6))

cca_data <- as.data.frame(summary(cca_out)[["sites"]])
cca_data$sample <- rownames(cca_data)
cca_data$treatment <- as.factor(gsub(" Before| After", "", cca_data$sample))
cca_data$treatment <- factor(cca_data$treatment , levels=levels(cca_data$treatment)[c(4,3,1,2,5)])
cca_data$laboratory_cultrue <- gsub("[A-Z]*[a-z]* ", "", cca_data$sample)
cca_data$laboratory_cultrue[cca_data$laboratory_cultrue=="After"] <- T
cca_data$laboratory_cultrue[cca_data$laboratory_cultrue=="Before"] <- F


p <- qplot(data=cca_data,
           x=CA1,
           xlab="Dimension 1",
           y=CA2,
           ylab="Dimension 2",
           size=I(3),
           shape=treatment,
           colour=treatment,
           fill=factor(ifelse(laboratory_cultrue, NA, treatment)))+ 
  scale_shape_manual("", values=c(3, 21,22,23,24))+
  scale_fill_brewer("", na.value=NA, guide="none", palette="Dark2")+
  scale_colour_brewer("", palette="Dark2")

print(p)

p <- qplot(data=cca_data,
           x=CA2,
           xlab="Dimension 2",
           y=CA3,
           ylab="Dimension 3",
           size=I(3),
           shape=treatment,
           colour=treatment,
           fill=factor(ifelse(laboratory_cultrue, NA, treatment)))+ 
  scale_shape_manual("", values=c(3, 21,22,23,24))+
  scale_fill_brewer("", na.value=NA, guide="none", palette="Dark2")+
  scale_colour_brewer("", palette="Dark2")

print(p)
  

           

extremes <- c("Extreme","Extreme",
              "Extreme","Extreme",
              "Benign", "Benign",
              "Benign", "Extreme","Extreme")
treatment <- gsub(" Before| After", "", rownames(otu_matrix))
laboratory_cultrue <- gsub("[A-Z]*[a-z]* ", "", rownames(otu_matrix))

fit <- adonis(otu_matrix~extremes+treatment+laboratory_cultrue)
fit

otu_matrix_basic_saline <- otu_matrix[grep("Acid|Benign",rownames(otu_matrix)),]
fit <- adonis(otu_matrix_basic_saline~gsub(" Before| After", "", rownames(otu_matrix_basic_saline)))
fit


otu_matrix_basic_saline <- otu_matrix[grep("Base|Salt",rownames(otu_matrix)),]
fit <- adonis(otu_matrix_basic_saline~gsub(" Before| After", "", rownames(otu_matrix_basic_saline)))
fit



graphics.off()


# rda_out <- rda(X=otu_matrix)
# biplot(rda_out)
# text(cca_out)

