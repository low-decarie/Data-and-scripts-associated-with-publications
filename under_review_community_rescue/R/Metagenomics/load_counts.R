#Housekeeping ####
require(plyr)
require(ggplot2)
require(reshape)
require(dplyr)
require(stringr)



load_counts <- function(level="Species"){
  
  # load the data
  
  print(paste("Loading data for", level))
  
  
  pattern <- paste(".*",level,"Counts.csv",sep="")
  
  file.list <- list.files(path="./Data/Metagenomics/",
                          recursive="T",
                          pattern=pattern)
  
  path.list <- paste0("./Data/Metagenomics/",file.list)
  
  pattern <- paste("[A-Z][a-z]*",level,sep="")
  
  count_data <- ldply(.data=path.list,
                      #                     .progress="text",
                      function(x){
                        temp <- read.csv(x)
                        temp$Domain <- sub(level,"",str_extract(x, pattern))
                        temp$Assay <- sub("Analysis","",str_extract(x, "[A-Z]Analysis"))
                        names(temp) <- do.call(rbind,str_split(names(temp),"\\."))[,1]
                        return(temp)
                      })
    
  
  count_data <- melt(count_data,
                            id.vars=c("name",
                                      "Domain",
                                      "Assay"))
  
  names(count_data)[names(count_data) %in% "variable"] <- "Sample_number"
  count_data$Sample_number <- as.character(count_data$Sample_number)
  
  # Sample_name code
  Sample_code <- read.csv("./Data/Metagenomics/Sample_codes.csv")
  Sample_code$Sample_number <- paste("X",Sample_code$Sample_number, sep="")
  
  pruned_Sample_code <- Sample_code[Sample_code$Sample_number %in% count_data$Sample_number,]
  
  count_data <- merge(count_data, Sample_code)
  
  count_data <- ddply(.data=count_data,
                             .variables=c("Sample_number","Assay"),
                             function(x)data.frame(x,frequency=x$value/sum(x$value,na.rm=T)))
  
  count_data$treatment <-as.character(count_data$Dalapon)
  count_data$treatment[is.na(count_data$treatment)] <- "Source"
  
  count_data$treatment[count_data$treatment=="Source"] <- "Source"
  count_data$treatment[count_data$treatment==0.05] <- "Low Dalapon (0.05 g/L)"
  count_data$treatment[count_data$treatment==0.65] <- "High Dalapon (0.65 g/L)"
  
  count_data$treatment <- as.factor(count_data$treatment)
  count_data$treatment <- factor(count_data$treatment,
                                 levels=levels(count_data$treatment)[c(3,2,1)])
  
#   return(count_data)
  
  save(count_data, file=paste("./Outputs/Metagenomics/",level,"_count.RData",sep=""))
}