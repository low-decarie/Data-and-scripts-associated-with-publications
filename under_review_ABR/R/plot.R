#Housekeeping ####
rm(list=ls())
require(plyr)
require(ggplot2)
require(reshape)
theme_set(theme_bw())


pdf("./Plots/growth.pdf")

load("./Output/fitted_od_data.RData")

plot_logistic <- function(){
p_predicted <- qplot(data=fitted_od_data,
                     x=time,
                     y=abundance,
                     colour=plate_ID,
                     geom=c("point"))+
  geom_line(aes(y=predicted))+
  facet_grid(assay~ampli_bio)

p_r <- qplot(data=growth_parameters,
             x=ampli_bio,
             y=r,
             fill=selection,
             stat="summary",
             fun.y="mean",
             geom="bar")+
  facet_grid(assay~.)+
  scale_fill_manual(values=c("acid"="red",
                              "base"="orange",
                              "bolds"="blue",
                              "salt"="green"))+
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", mult = 1)



p_K <- qplot(data=growth_parameters,
             x=ampli_bio,
             y=K,
             fill=selection,
             stat="summary",
             fun.y="mean",
             geom="bar")+
  facet_grid(assay~.)+
  scale_fill_manual(values=c("acid"="red",
                             "base"="orange",
                             "bolds"="blue",
                             "salt"="green"))+
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", mult = 1)

print(p_predicted)
print(p_r)
print(p_K)

}

load("./Output/fitted_od_data.RData")
load("./Output/growth_parameters.RData")
plot_logistic()
load("./Output/fitted_od_data_MLE.RData")
load("./Output/growth_parameters_MLE.RData")
plot_logistic()


# NLS exponential ####


load("./Output/fitted_od_data_exp.RData")


p_predicted_exp <- qplot(data=fitted_od_data,
                     x=time,
                     y=abundance,
                     colour=plate_ID,
                     geom=c("point"))+
  geom_line(aes(y=predicted))+
  facet_grid(assay~ampli_bio)



load("./Output/growth_parameters_exp.RData")

p_r_exp <- qplot(data=growth_parameters,
                 x=ampli_bio,
                 y=r,
                 fill=selection,
                 stat="summary",
                 fun.y="mean",
                 geom="bar")+
  facet_grid(assay~.)+
  scale_fill_manual(values=c("acid"="red",
                             "base"="orange",
                             "bolds"="blue",
                             "salt"="green"))+
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", mult = 1)

print(p_predicted_exp)
print(p_r_exp)
graphics.off()