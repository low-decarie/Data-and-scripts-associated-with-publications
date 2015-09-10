#Housekeeping ####
rm(list=ls())
require(plyr)
require(ggplot2)
require(reshape)
require(lubridate)
require(Hmisc)
theme_set(theme_bw())

file.list <- list.files("./Data/")
file.list <- file.list[grepl(".csv",file.list)]

legend <- read.csv("./plate_legend.csv")

od_data <- ldply(.data=file.list,
                 function(x){
                   path <- paste("./Data/",x,sep="")
                   date.char <- as.character(read.csv(path,header=F,skip=4)[1,1])
                   OD <- read.csv(path,skip=8,col.names=paste("C",1:9,sep=""))[,-9]
                   OD$row <- row.names(OD)
                   molten_OD <- melt(OD, id.vars="row",variable_name="column")
                   molten_OD$date.char <- date.char
                   molten_OD$column <- gsub("C","",molten_OD$column)
                   molten_OD$plate_ID <- as.factor(gsub("ampli_bio_recip_transplant_flask_[0-9]+-|\\.csv","",x))
                   return(molten_OD)})

od_data <- merge(od_data,legend,all=T)

od_data <- od_data[od_data$assay!="empty",]

od_data <- ddply(.data=od_data,
                 .variable="assay",
                 function(x){
                   data.frame(x,blanked_value=x$value-x$value[x$ampli_bio=="blank"],
                              upper_CI_blank=smean.cl.normal(x$value[x$ampli_bio=="blank"])[3])
                 })


od_data$date <- parse_date_time(od_data$date.char,"%m/%d/%Y %H:%M:%S %p")

od_data$time <- as.numeric(difftime(od_data$date, min(na.omit(od_data$date)), units="days"))

od_data$ampli_bio <- as.factor(as.character(od_data$ampli_bio))
od_data$selection <- as.character(od_data$ampli_bio)
od_data$selection[od_data$selection=="acid_exp_1"] <- "acid"
od_data$selection[od_data$selection=="acid_exp_2"] <- "acid"
od_data$selection[od_data$selection=="in"] <- "bolds"

#No chlamy in last assay
od_data <- od_data[od_data$ampli_bio!="Chlamydomonas",]

od_data <- od_data[od_data$ampli_bio!="empty"&!is.na(od_data$assay),]
od_data <- od_data[od_data$ampli_bio!="blank",]

od_data$blanked_value[od_data$blanked_value<0] <- 0


#Remove problem data (outlying absorbance value that prevent nls algorithm to converge)
od_data <- od_data[!(od_data$assay=="salt" & od_data$ampli_bio=="salt" & od_data$date.char=="7/3/2014 12:29:12 PM"),] 
od_data <- od_data[!(od_data$assay=="salt" & od_data$ampli_bio=="salt" & od_data$date.char=="7/3/2014 12:30:18 PM"),] 
od_data <- od_data[!(od_data$assay=="bolds" & od_data$ampli_bio=="acid_exp_2" & od_data$date.char=="6/30/2014 11:54:03 AM" & od_data$plate_ID==1),]  
od_data <- od_data[!(od_data$assay=="salt" & od_data$ampli_bio=="acid_exp_1" & od_data$date.char=="7/1/2014 3:29:03 PM" & od_data$plate_ID==1),]  

save(od_data, file="./Output/od_data.RData")
                   
