#Housekeeping ####
rm(list=ls())
require(plyr)
require(ggplot2)
require(reshape)
require(growthcurves)
theme_set(theme_bw())

# NLS logistic growth ####

load("./Output/od_data.RData")

x <- od_data[(od_data$assay=="base" & od_data$ampli_bio=="base" & od_data$plate_ID==1),] 


fitted_od_data <- ddply(.data=od_data,
                 .variable=c("ampli_bio","assay","plate_ID","selection"),
                 function(x){
                   with(x,print(unique(paste(ampli_bio,assay,plate_ID,selection))))
                   fit <- with(x,logistic_growth_nls(time=time, abundance=blanked_value))
                   if(max(x$blanked_value)<max(x$upper_CI_blank))fit[names(fit) %in% c("N0", "K", "r", 
                                                                 "predicted", "K.lower",
                                                                 "K.upper", "r.lower", 
                                                                 "r.upper")] <- 0
                   return(fit)
                 })

save(fitted_od_data,file="./Output/fitted_od_data.RData")

growth_parameters <- ddply(.data=fitted_od_data,
                           .variable=c("ampli_bio","assay","plate_ID","selection"),
                           function(x){
                             data.frame(unique(x[,names(x) %in% c("N0", "K", "r","K.lower", 
                                                                 "K.upper", "r.lower", 
                                                                 "r.upper")]))})


save(growth_parameters,file="./Output/growth_parameters.RData")


# NLS exponential ####

load("./Output/od_data.RData")



fitted_od_data <- ddply(.data=od_data[od_data$time<8,],
                        .variable=c("ampli_bio","assay","plate_ID","selection"),
                        function(x){
                          with(x,print(unique(paste(ampli_bio,assay,plate_ID,selection))))
                  fit <- with(x,exponential_growth_nls(time=time, abundance=blanked_value))
if(max(x$blanked_value)<max(x$upper_CI_blank))fit[names(fit) %in% c("N0", "K", "r", 
                                                                     "predicted", "K.lower",
                                                                     "K.upper", "r.lower", 
                                                                     "r.upper")] <- 0
                          return(fit)
                        })

save(fitted_od_data,file="./Output/fitted_od_data_exp.RData")

growth_parameters <- ddply(.data=fitted_od_data,
                           .variable=c("ampli_bio","assay","plate_ID","selection"),
                           function(x){
                             data.frame(unique(x[,names(x) %in% c("N0", "r","r.lower", 
                                                                  "r.upper")]))})


save(growth_parameters,file="./Output/growth_parameters_exp.RData")


# MLE logistic

load("./Output/od_data.RData")



fitted_od_data <- ddply(.data=od_data,
                        .variable=c("ampli_bio","assay","plate_ID","selection"),
                        function(x){
                          with(x,print(unique(paste(ampli_bio,assay,plate_ID,selection))))
                          fit <- with(x,logistic_growth_mle_norm(time=time, abundance=blanked_value))
if(max(x$blanked_value)<max(x$upper_CI_blank))fit[names(fit) %in% c("N0", "K", "r", 
                                                           "predicted")] <- 0
                          return(fit)
                        })

save(fitted_od_data,file="./Output/fitted_od_data_MLE.RData")

growth_parameters <- ddply(.data=fitted_od_data,
                           .variable=c("ampli_bio","assay","plate_ID","selection"),
                           function(x){
                             data.frame(unique(x[,names(x) %in% c("N0","K", "r")]))})


save(growth_parameters,file="./Output/growth_parameters_MLE.RData")





