#Data from 1 Elser, J. J. et al. 2007 Global analysis of nitrogen and phosphorus limitation of primary producers in freshwater, marine and terrestrial ecosystems. Ecology letters 10, 1135–42. (doi:10.1111/j.1461-0248.2007.01113.x)
#Available at http://knb.ecoinformatics.org/knb/metacat/nceas.347/nceas



rm(list=ls())
require(plyr)
require(ggplot2)
require(reshape)

source("./R/theme.R")
theme_set(theme_minimal())


d <- read.csv("./Data/elser2007.csv")


pdf("./Plots/elser2007.pdf")





# Nutrient available in the system ####

avail.d <- melt(d, measure.vars=c("n_avail", 
                                   "p_avail"))

avail.d.summary <- ddply(.data=avail.d,
                          .progress="text",
                          .variables=c("system",
                                       "variable",
                                       "cat"),
                          function(x){
                            data.frame(mean=mean(x$value, na.rm=T),
                                 se=sd(x$value, na.rm=T)/sqrt(length(na.omit(x$value))))
                          })

avail.d.summary$element <- substr(avail.d.summary$variable, 1,1)

p <- qplot(data=avail.d.summary,
           x=cat,
           y=mean,
           ymin=mean-se,
           ymax=mean+se,
           colour=variable,
           geom="pointrange",
           stat="identity",
           log="y")

p <- p + facet_grid(.~system, scale="free")
p <- p + theme(axis.text.x=(element_text(angle=90)))



print(p)




# Relative availability ####

d$rel.n.avail <- with(d, n_avail/(p_avail+n_avail))
d$rel.p.avail <- with(d, p_avail/(p_avail+n_avail))
d$rel.n.avail.redfield <- d$rel.n.avail/16


rel.avail.d <- melt(d, measure.vars=c("rel.n.avail", 
                                  "rel.p.avail"))

rel.avail.d.summary <- ddply(.data=rel.avail.d,
                         .progress="text",
                         .variables=c("system",
                                      "variable",
                                      "cat"),
                         function(x){
                           data.frame(mean=mean(x$value, na.rm=T),
                                      se=sd(x$value, na.rm=T)/sqrt(length(na.omit(x$value))))
                         })

rel.avail.d.summary$element <- substr(avail.d.summary$variable, 1,1)

p <- qplot(data=rel.avail.d.summary,
           x=cat,
           y=mean,
           fill=variable,
           geom="bar",
           position="dodge",
           stat="identity")

p <- p + facet_grid(.~system, scale="free")
p <- p + theme(axis.text.x=(element_text(angle=90)))


p <- p + geom_errorbar(aes(ymin=mean-se, ymax=mean+se), 
                       width=.1,
                       position=position_dodge(1))



print(p)



rel.avail.d <- melt(d, measure.vars=c("rel.n.avail.redfield", 
                                      "rel.p.avail"))

rel.avail.d.summary <- ddply(.data=rel.avail.d,
                             .progress="text",
                             .variables=c("system",
                                          "variable",
                                          "cat"),
                             function(x){
                               data.frame(mean=mean(x$value, na.rm=T),
                                          se=sd(x$value, na.rm=T)/sqrt(length(na.omit(x$value))))
                             })

rel.avail.d.summary$element <- substr(avail.d.summary$variable, 1,1)

p <- qplot(data=rel.avail.d.summary,
           x=cat,
           y=mean,
           fill=variable,
           geom="bar",
           position="dodge",
           stat="identity")

p <- p + facet_grid(.~system, scale="free")
p <- p + theme(axis.text.x=(element_text(angle=90)))


p <- p + geom_errorbar(aes(ymin=mean-se, ymax=mean+se), 
                       width=.1,
                       position=position_dodge(1))



print(p)



#  Response ####

limiting.d <- melt(d, measure.vars=c("l.n.c", "l.p.c", "l.int.c"))


limiting.d.summary <- ddply(.data=limiting.d,
                          .progress="text",
                          .variables=c("system",
                                       "variable",
                                       "cat"),
                          function(x){
                            data.frame(mean=mean(x$value, na.rm=T),
                             se=sd(x$value, na.rm=T)/sqrt(length(na.omit(x$value))))
                          })

p <- qplot(data=limiting.d.summary,
           x=cat,
           y=mean,
           fill=variable,
           geom="bar",
           position="dodge",
           stat="identity")

p <- p + facet_grid(.~system, scale="free")
p <- p + theme(axis.text.x=(element_text(angle=90)))

p <- p + geom_errorbar(aes(ymin=mean-se, ymax=mean+se), 
                       width=.1,
                       position=position_dodge(1))

print(p)


#  Predictions ####

d <- d[!is.na(d$system),,drop=T]

limiting.d.summary <- limiting.d.summary[limiting.d.summary$variable!="l.int.c",]
limiting.d.summary$element <- substr(limiting.d.summary$variable, 3,3)
names(limiting.d.summary) <- c("system", "variable.response","cat", "response.mean", "se.response", "element")

names(rel.avail.d.summary) <- c("system", "variable.rel.avail","cat", "rel.avail.mean", "se.avail", "element")




comb.d <- merge(limiting.d.summary,rel.avail.d.summary)

p <- qplot(data=comb.d,
           x=rel.avail.mean,
           y=response.mean,
           colour=element,
           shape=system)+
  geom_smooth(method="lm",se=F, aes(group=element))


p <- p + geom_errorbar(aes(ymin=response.mean-se.response, ymax=response.mean+se.response), 
                                                      width=.1)

print(p)



p <- qplot(data=d[d$rel.p.avail!=0,],
           x=rel.p.avail,
           y=l.p.c,
           color=cat)+
  geom_smooth(method="lm", se=F)
p <- p + facet_grid(.~system)

print(p)

p <- qplot(data=d,
           x=rel.n.avail,
           y=l.n.c,
           color=cat)+
  geom_smooth(method="lm", se=F)
p <- p + facet_grid(.~system)

print(p)

p <- qplot(data=d,
           x=p_avail,
           y=l.p.c),
           color=cat)+
  geom_smooth(method="lm", se=F)
p <- p + facet_grid(.~system)

print(p)

p <- qplot(data=d,
           x=n_avail,
           y=l.n.c,
           color=cat)+
  geom_smooth(method="lm", se=F)

p <- p + facet_grid(.~system)


print(p)



p <- qplot(data=d,
           x=log(exp(l.n.c)+exp(l.p.c)),
           y=l.int.c,
           color=cat)+
  geom_smooth(method="lm", se=F)

p <- p +  geom_abline(slope=1)

p <- p + facet_grid(.~system)

p  <- p + theme(aspect.ratio=1)


print(p)

p <- qplot(data=d,
           x=log(exp(l.int.c)/(exp(l.n.c)+exp(l.p.c))),
           fill=cat,
           binwidth=0.2)

p <- p + geom_vline(xintercept=0)

p <- p + facet_grid(system~., scale="free")

print(p)



p <- qplot(data=d,
           x=log(exp(l.n.c)+exp(l.p.c)),
           y=log(exp(l.int.c)/(exp(l.n.c)+exp(l.p.c))),
           color=cat)+
  geom_smooth(method="lm", se=F)

p <- p +  geom_abline(slope=1)

p <- p + facet_grid(.~system)

p  <- p + theme(aspect.ratio=1)


print(p)

p <- qplot(data=d,
           x=log(exp(l.int.c)/(exp(l.n.c)+exp(l.p.c))),
           geom="density")

p <- p + geom_vline(xintercept=0)

p <- p + facet_grid(system~., scale="free")

print(p)


p <- qplot(data=d,
           color=cat,
           x=log10(duration),
           y=log10(exp(l.int.c)/(exp(l.n.c)+exp(l.p.c))))

p <- p + geom_smooth(method="lm",aes(group=system), se=F)

p <- p + facet_grid(system~., scale="free")

print(p)



p <- qplot(data=d,
           color=cat,
           x=log10(duration),
           y=l.p.c)

p <- p + geom_smooth(method="lm",aes(group=system), se=F)

p <- p + facet_grid(system~., scale="free")

print(p)

p <- qplot(data=d,
           color=cat,
           x=log10(duration),
           y=l.n.c)

p <- p + geom_smooth(method="lm",aes(group=system), se=F)

p <- p + facet_grid(system~., scale="free")

print(p)


p <- qplot(data=d,
           color=cat,
           x=log10(duration),
           y=log(exp(l.p.c)/exp(l.n.c)))

p <- p + geom_smooth(method="lm",aes(group=system), se=F)

p <- p + facet_grid(system~., scale="free")

print(p)





d$mean.response <- with(d, apply(cbind(l.n.c,l.p.c), MARGIN=1, FUN=max))

p <- qplot(data=d[d$mean.response>1.385,],
           x=log(exp(l.int.c)/(exp(l.n.c)+exp(l.p.c))),
           fill=cat,
           binwidth=0.2)

p <- p + geom_vline(xintercept=0)

p <- p + facet_grid(system~., scale="free")

print(p)

print("Total with data")
d <- with(d, d[!is.na(l.int.c) & !is.na(l.n.c) & !is.na(l.p.c),])
n.d <- nrow(d)
print("Sub additive")
n.subaditive <- nrow(with(d, (d[log(exp(l.int.c)/(exp(l.n.c)+exp(l.p.c)))<0,])))
n.subaditive/n.d
print("Synergistic response")
n.synergistic <- with(d, nrow(d[log(exp(l.int.c)/(exp(l.n.c)+exp(l.p.c)))>0,]))
n.synergistic/n.d

d$mean.response.sep <- with(d, apply(cbind(exp(l.n.c),exp(l.p.c)), MARGIN=1, FUN=mean))


p <- qplot(data=d,
           y=log(exp(l.int.c)/(exp(l.n.c)+exp(l.p.c))),
           x=log(mean.response.sep),
           color=cat)

p <- p + geom_hline(yintercept=0)

p <- p + facet_grid(system~., scale="free")

p <- p + geom_smooth(method="lm", se=F)

print(p)

graphics.off()