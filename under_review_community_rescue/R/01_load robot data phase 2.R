# This script reads .log robot files (found in robot_OD/log_files)
#and associates the data Without treatment variables found in a legend file

#It produces a .RData and a .csv file names "complete robot OD data"

rm(list=ls())
require(plyr)
require(stringr)

#Load legend file
legend_robot_OD<-read.csv("./Data/legend_robot_OD.csv", stringsAsFactors=F)

# #Load .log file names and append path
file.list<-list.files("./Data/Phase 2/Without recent dispersal/",recursive=T)
file.list<-file.list[grep(".txt$", file.list)]

#Test
#path<-path.list[1]


#Function for reading a .log file from the robot
read.file<-function(file){
  
    print(file)
    #Creating paths from file names
    path<-paste("./Data/Phase 2/Without recent dispersal/", file, sep="")
    
    #Reading in all the data line by line
    data.file<-data.frame(data=readLines(path))
    
    #Omit white spaces and blanks and convert to character vector
    data.file<-as.character(data.file$data[!data.file$data %in% c("", " ")])
    
    data.file <- gsub("\t", "\n", data.file) 
    
    write.table(data.file, paste(path, ".temp",sep=""),quote=F)
    
    writeLines(as.character(data.file), paste(path, ".temp",sep=""))
    
    data.file<-data.frame(data=readLines(paste(path, ".temp",sep="")),stringsAsFactors=F)
    
    #Extract the plate names and numeric data
      #Wrapped in a warning suppressor because we know NA are being produced 
      #(that is waht we are using to check the type
    data.file <- data.file$data
    
    suppressWarnings({
    read.labels<-data.file[is.na(as.numeric(data.file))]
    numeric.data<-na.omit(as.numeric(data.file))
    })
    
    read.labels.temp <- NULL
    
    for(i in seq(4, length(read.labels), by=4)){
      read.labels.temp[i/4] <- paste(read.labels[(i-3):i], collapse=",")
    }
    
    read.labels <- read.labels.temp
    
#Create a plate template (rows and columns) ####
    
      #This contains the glucose and dalapon treatments
    plate.template<-data.frame(rows=rep(LETTERS[1:8], each=12),
                               column=rep(1:12, 8),
                               dalapon=rep(c(NA,0,
                                             0.05,
                                             0.15,
                                             0.25,
                                             0.35,
                                             0.45,
                                             0.55,
                                             0.65,
                                             0.75,
                                             1.5,NA), 8),
                               glucose=rep(c(NA,5,2,1,0.5,0.25,0,NA),each=12))
    
    #Merge the labels Without the numeric data assuming each plate has 96 wells
    file.data<-data.frame(OD=numeric.data,
                          label=rep(read.labels,each=96),
                          plate.template[rep(seq_len(nrow(plate.template)), length(read.labels)), ])
    
    
#Parse the label into its value ####
    #Date
    file.data$date<-laply(file.data$label,
                                  function(x){
                                    x<-as.character(x)
        strsplit(x, split=",")[[1]][length(strsplit(x, split=",")[[1]])]})
    file.data$date<-as.Date(file.data$date,
                            format="%m/%d/%Y %H:%M:%S")
    
    #Plate ID
    file.data$plateID<-laply(file.data$label,
                          function(x){
                            x<-as.character(x)
                            strsplit(x, split=",")[[1]][1]})
    
    file.data$file <- file
    
    
return(file.data)}


#Combine all the data into a single file
file.data<-ldply(file.list,
                 function(x) read.file(x))

#Add values from legend  ####
file.data.complete<-join(file.data, legend_robot_OD)

robo.data<-file.data.complete


robo.data$transfer <- laply(.data=robo.data$file,
                            .progress="text",
                            function(x){
                              temp <- str_split(x,"/")[[1]][1]
                              temp <- str_split(temp," ")[[1]][2]
                              return(temp)})
robo.data$transfer <- as.numeric(robo.data$transfer)

robo.data$block <- laply(.data=robo.data$file,
                         .progress="text",
                         function(x){
                           temp <- str_split(x,"/")[[1]][2]
                           temp <- substring(temp,1,1)
                           return(temp)})


robo.data$wellID <- with(robo.data, paste(block,plateID,rows,column))

#Blank using blank wells  ####

robo.data$is.blank.well <- F
robo.data$is.blank.well[is.na(robo.data$dalapon) & is.na(robo.data$glucose)] <- T

robo.data <- ddply(.data=robo.data,
                   .variables=c("plateID"),
                   function(x){
                     mean.blank <- mean(x$OD[x$is.blank.well], na.rm=T)
                     sd.blank <- sd(x$OD[x$is.blank.well], na.rm=T)
                     temp <- data.frame(x, mean.blank=mean.blank,
                                        sd.blank=sd.blank)
                     return(temp)})


robo.data$wellID <- with(robo.data, paste(block,plateID,rows,column))

robo.data$OD.blanked <- robo.data$OD-robo.data$mean.blank


robo.data$is.viable <- F
# robo.data$is.viable[with(robo.data, OD > mean.blank+2*sd.blank)] <- T
robo.data$is.viable[with(robo.data, OD.blanked > 2*sd.blank)] <- T
# robo.data$is.viable[with(robo.data, OD.blanked > sd.blank)] <- T




robo.data<-robo.data[robo.data$Dispersal!="",]
robo.data<-robo.data[!is.na(robo.data$Dispersal),]
robo.data<-robo.data[!is.na(robo.data$glucose),]
robo.data<-robo.data[!is.na(robo.data$dalapon),]

robo.data <- ddply(.data=robo.data,
                   .variables=c("dalapon",
                                "glucose",
                                "Dilution",
                                "Dispersal",
                                "Soil",
                                "block"),
                   function(x){
                     data.frame(is.viable=all(x$is.viable[x$transfer %in% c(3,4)]),
                                mean.OD=mean(x$OD.blanked[x$transfer %in% c(3,4)]))})



robo.data<-robo.data[robo.data$Dispersal!="",]
robo.data<-robo.data[!is.na(robo.data$Dispersal),]
robo.data<-robo.data[!is.na(robo.data$glucose),]
robo.data<-robo.data[!is.na(robo.data$dalapon),]

write.csv(robo.data, file="./Outputs/phase 2 robot OD data without recent disp including fresh soil test.csv")
save(robo.data, file="./Outputs/phase 2 robot OD data without recent disp including fresh soil test.RData")




#Save files ####
robo.data<-robo.data[robo.data$block!="N",]
write.csv(robo.data, file="./Outputs/phase 2 robot OD data without recent disp.csv")
save(robo.data, file="./Outputs/phase 2 robot OD data without recent disp.RData")