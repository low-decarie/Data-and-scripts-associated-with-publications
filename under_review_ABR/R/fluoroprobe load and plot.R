#Clear memory
rm(list=ls())

#Load libraries
require(plyr)
require(ggplot2)
require(reshape)
require(tools)
theme_set(theme_bw())

#Store plots
pdf("./Plots/Fluoroprobe explore plots.pdf", width=4, height=4)

#Get folder names for dates
dates<-list.files("./Data/Fluoroprobe")

#Get the full path names of all files
samples<-llply(.data=dates,
               function(x) list_files_with_exts(paste("./Data/Fluoroprobe/",
                                                      x,
                                                      sep=""),
                                                exts="TXT"))

samples<-unlist(samples)


#Use file path list to get data and label it
probe.dat<-ldply(.data=samples,
                 function(x){#print(x)
                   probe.dat<-read.delim(x, header=F, skip=2)
                   if(ncol(probe.dat)==20){probe.dat<-probe.dat[,-15]}
                   probe.dat$file<-x
                   probe.dat$sampleID<-unlist(strsplit(unlist(strsplit(x, "/"))[length(unlist(strsplit(x, "/")))], "\\."))[1]
                   return(probe.dat)})


names(probe.dat)<-c("date",
                    "time",
                    "depth",
                    "temperature",
                    "nm525",
                    "nm570",
                    "nm610",
                    "nm590",
                    "nm470",
                    "UV",
                    "Chlorophytes",
                    "Cyanobacteria",
                    "Diatoms",
                    "Cryptophytes",
                    "Total_chlorophyll",
                    "Transmittance",
                    "Fluo_temperature",
                    "Led_temperature",
                    "Chi_Squared",
                    "file",
                    "sampleID")

#Convert date and time formats
probe.dat$date.time<-with(probe.dat, paste(date, time))
probe.dat$date.time<-strptime(probe.dat$date.time, format="%d.%m.%Y%H:%M:%S")
probe.dat$date<-as.Date(probe.dat$date, format="%d.%m.%Y")
probe.dat$time<-strptime(probe.dat$time, format="%H:%M:%S")

#Trim unused data
probe.dat<-probe.dat[,names(probe.dat) %in% c("sampleID",
                                              "date.time",
                                              "depth",
                                              "temperature",
                                              "Chlorophytes",
                                              "Cyanobacteria",
                                              "Diatoms",
                                              "Cryptophytes",
                                              "Yellow substances",
                                              "Total_chlorophyll",
                                              "Transmittance")]




#Remove anomalous time point
error<-strptime("2012-01-22 12:00:00", format="%Y-%m-%d %H:%M:%S")
probe.dat<-probe.dat[probe.dat$date.time>error,]

#Total chlorophyll above 1000 need to sum parts
probe.dat$Total_chlorophyll<-with(probe.dat, Chlorophytes+Cyanobacteria+Diatoms+Cryptophytes)

#Remove "Invalid"
probe.dat<-probe.dat[probe.dat$Chlorophytes!=1&probe.dat$Cyanobacteria!=1&probe.dat$Diatoms!=1,]



#Recalculate diluted samples


value.names<-c("Chlorophytes",
                 "Cyanobacteria",
                 "Diatoms",
                 "Cryptophytes",
                 "Total_chlorophyll")

probe.dat[grep("quarter", probe.dat$sampleID) ,names(probe.dat) %in% value.names]<-4*probe.dat[grep("quarter", probe.dat$sampleID),names(probe.dat) %in% value.names]                                                                                                                                        

probe.dat[grep("heigth", probe.dat$sampleID) ,names(probe.dat) %in% value.names]<-8*probe.dat[grep("heigth", probe.dat$sampleID),names(probe.dat) %in% value.names]


probe.dat$sampleID<-laply(probe.dat$sampleID,
                          function(x)strsplit(x, " ")[[1]][1])

probe.dat[probe.dat$sampleID=="raw 1"|probe.dat$sampleID=="raw"|probe.dat$sampleID=="raw 2"|probe.dat$sampleID=="input 2", "sampleID"]<-"input"


probe.dat<-probe.dat[probe.dat$sampleID!="sterile 1",]
probe.dat<-probe.dat[probe.dat$sampleID!="sterile 2",]
probe.dat<-probe.dat[probe.dat$sampleID!="sterile",]


probe.dat$input.type[probe.dat$sampleID %in% c(2,4,5,7)]<-"raw"

probe.dat$input.type[probe.dat$sampleID %in% c(1,3,6,8)]<-"sterile"
probe.dat$input.type[probe.dat$sampleID %in% "input"]<-"input"


probe.dat$treatment[probe.dat$sampleID=="input"]<-"input"
probe.dat$treatment[probe.dat$sampleID %in% c(1,2)]<-"Acid"
probe.dat$treatment[probe.dat$sampleID %in% c(3,4)]<-"Base"
probe.dat$treatment[probe.dat$sampleID %in% c(5,6)]<-"Benign"
probe.dat$treatment[probe.dat$sampleID %in% c(7,8)]<-"Salt"


probe.dat$date<-as.Date(format(probe.dat$date.time, "%Y-%m-%d"),"%Y-%m-%d")

probe.dat<-probe.dat[,!(names(probe.dat) %in% "date.time")]

# colwise.mean<-colwise(mean)
# 

# 
# probe.dat<-ddply(.data=probe.dat,
#                  .variables=c("sampleID", "date", "input.type"),
#                  .fun="colwise.mean")

#Get ratios
probe.dat$Chlorophytes.perc<-probe.dat$Chlorophytes/probe.dat$Total_chlorophyll
probe.dat$Cyanobacteria.perc<-probe.dat$Cyanobacteria/probe.dat$Total_chlorophyll
probe.dat$Diatoms.perc<-probe.dat$Diatoms/probe.dat$Total_chlorophyll
probe.dat$Cryptophytes.perc<-probe.dat$Cryptophytes/probe.dat$Total_chlorophyll

probe.dat$treatment <- as.factor(probe.dat$treatment)
probe.dat$treatment <- factor(probe.dat$treatment, levels=levels(probe.dat$treatment)[c(3,1,2,5)])

p<-qplot(data=probe.dat[probe.dat$date==max(probe.dat$date) & probe.dat$input.type %in%  "raw",],
         y=Total_chlorophyll,
         ylab=expression(paste("Chlorophyll a (",mu,"g/L)")),
         x=treatment,
         xlab="Treatment",
         stat="summary",
         fun.y = "mean",
         geom="bar")+
  theme(panel.grid=element_blank())
print(p)

by_type <- melt(probe.dat[probe.dat$date==max(probe.dat$date) & probe.dat$input.type %in%  "raw" & probe.dat$treatment!="Control",],
                measure.vars=c("Chlorophytes", "Cyanobacteria", "Diatoms", 
                               "Cryptophytes"))

p<-qplot(data=by_type,
         y=value,
         ylab="Chlorophyll a (ug/L)",
         x=treatment,
         fill=variable,
         xlab="Treatment",
         stat="summary",
         fun.y = "mean",
         geom="bar")+
  theme(panel.grid=element_blank())
print(p)


for(i in c("Total_chlorophyll", "Chlorophytes.perc","Cyanobacteria.perc",
           "Diatoms.perc","Cryptophytes.perc")){
# #Basic plot of total chlorophyll through time
p<-qplot(data=probe.dat,
         x=date,
         y=get(i),
         ylab=i,
         colour=treatment,
         linetype=input.type,
         shape=sampleID,
         stat="summary",
         fun.y = "mean")+
           scale_shape_manual(values=1:9,guide=FALSE)+
           scale_linetype_manual(values=c("raw"=1,
                                          "sterile"=2,
                                          "input"=3))

p<-p+geom_line(aes(group=sampleID),
               stat="summary",
               fun.y = "mean")

p<-p+geom_vline(xintercept=as.numeric(as.Date("01-11-2012", "%d-%m-%Y")))




print(p)}






graphics.off()
