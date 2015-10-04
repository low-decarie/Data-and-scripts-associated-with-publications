#Housekeeping ####
rm(list=ls())
require(plyr)
require(ggplot2)
require(reshape)
require(gridExtra)
require(Hmisc)
theme_set(theme_bw())

pdf("./Plots/pub_plot_recip_transplant.pdf", width=3, height=4)

load("./Output/growth_parameters_exp.RData")

growth_parameters$assay <- capitalize(as.character(growth_parameters$assay))
growth_parameters$selection <- capitalize(as.character(growth_parameters$selection))

growth_parameters$selection <- factor(x=growth_parameters$selection,levels=levels(as.factor(growth_parameters$selection))[c(3,1,2,4)])


growth_parameters$r_base_e <- growth_parameters$r*log(2)

second_experiment <- growth_parameters[growth_parameters$ampli_bio!="acid_exp_1",]

mean(second_experiment$r_base_e[second_experiment$assay==second_experiment$selection])

second_experiment$selection <- as.character(second_experiment$selection)
second_experiment$assay <- as.character(second_experiment$assay)
second_experiment$selection[second_experiment$selection=="Bolds"] <- "Benign"
second_experiment$assay[second_experiment$assay=="Bolds"] <- "Benign"

second_experiment$assay <- as.factor(second_experiment$assay)
second_experiment$selection <- as.factor(second_experiment$selection)
second_experiment$assay <- factor(x=second_experiment$assay,levels=levels(as.factor(second_experiment$assay))[c(3,1,2,4)])
second_experiment$selection <- factor(x=second_experiment$selection,levels=levels(as.factor(second_experiment$selection))[c(3,1,2,4)])



p_r_exp <- qplot(data=second_experiment,
                 x=selection,
                 xlab="Selection environment",
                 y=r_base_e,
                 ylab=expression(paste("Growth rate -r- (",d^-1,")")),
                 stat="summary",
                 fun.y="mean",
                 geom="bar",
                 width=I(0.5),
                 fill=I("white"),
                 colour=I("black"))+
  facet_grid(assay~.)+
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", mult = 1, width=0.25)+
  theme(panel.grid=element_blank(),strip.background=element_blank())

print(p_r_exp)
grid.text(label="Assay environment",x=0.98,y=0.5,vjust=0.5,rot=-90)

graphics.off()