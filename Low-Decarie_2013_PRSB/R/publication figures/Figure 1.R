#Housekeeping
rm(list=ls())
library(ggplot2)
library(plyr)
library(reshape2)
library(devtools)
library(gridExtra)
setwd("~/Dropbox/co2_selection")
source("./R/publication figures/theme.R")
pdf("./Plots/publication figures/Figure 1.pdf", width=8, height=8)

par(mfrow=c(2,2), mar=c(3, 4, 6, 2) + 0.1)

#plot(1,1, xaxt="n", yaxt="n", type="n", ylim=c(0,6), xlim=c(0,5), axes=F, xlab="",ylab="", xaxs="i", yaxs="i")


plot(1,1, xaxt="n", yaxt="n", type="n", ylim=c(0,6), xlim=c(0,5), axes=F, xlab="",ylab="", xaxs="i", yaxs="i")
abline(h=c(2,4), col="lightgray", lwd=0.2)
text(1,4.5,labels="(a)", cex=2)

#mtext(cex=1,side=3, text="Accumulation of conditionaly", line=1)
#mtext(cex=1,side=3, text="deleterious mutations", line=0)



mtext(cex=1,side=2, text="Measure of fitness", line=0.5)

lines(x=c(1,4), y=c(2,4), col="blue", lwd=3)
lines(x=c(1,4), y=c(2,5), col="red", lty=5, lwd=6)

mtext(cex=1,side=1,text="Ambient", at=1, line=0.5, adj=0.5, col="blue")
mtext(cex=1,side=1,text="High", at=4, line=0.5, adj=0.5, col="red", lty=5)

axis(side=1, labels=F, tick=T, xaxp=c(0,5,1))
axis(side=2, labels=F, tick=T, yaxp=c(0,6,1))







plot(1,1, xaxt="n", yaxt="n", type="n", ylim=c(0,6), xlim=c(0,5), axes=F, xlab="",ylab="", xaxs="i", yaxs="i")
text(1,4.5,labels="(b)", cex=2)
abline(h=c(2,4), col="lightgray", lwd=0.2)
#mtext(cex=1,side=3, text=expression(paste("Specific adaptation to ",CO[2])), line=1)


#mtext(cex=1,side=1, text="CO2 assay environment", line=3.5, at=5)

mtext(cex=1,side=1,text="Ambient", at=1, line=0.5, adj=0.5, col="blue")
mtext(cex=1,side=1,text="High", at=4, line=0.5, adj=0.5, col="red", lty=5)

axis(side=1, labels=F, tick=T, xaxp=c(0,5,1))
axis(side=2, labels=F, tick=T, yaxp=c(0,6,1))



lines(x=c(1,4), y=c(2,4), col="blue", lwd=3)
lines(x=c(1,4), y=c(1,4), col="red", lty=5, lwd=6)


par(mar=c(6, 4, 3, 2) + 0.1)


plot(1,1, xaxt="n", yaxt="n", type="n", ylim=c(0,6), xlim=c(0,5), axes=F, xlab="",ylab="", xaxs="i", yaxs="i")
text(1,4.5,labels="(c)", cex=2)
abline(h=c(2,4), col="lightgray", lwd=0.2)
#mtext(cex=1,side=3, text=expression(paste("Specific adaptation to ",CO[2], " with tradeoff")), line=1.5)
#mtext(cex=1,side=3, text="or combination of a and b", line=0)
mtext(cex=1,side=2, text="Measure of fitness", line=0.5)
mtext(cex=1,side=1, text=expression(paste(CO[2]," assay environment")), line=2.5)





mtext(cex=1,side=1,text="Ambient", at=1, line=0.5, adj=0.5, col="blue")
mtext(cex=1,side=1,text="High", at=4, line=0.5, adj=0.5, col="red", lty=5)

axis(side=1, labels=F, tick=T, xaxp=c(0,5,1))
axis(side=2, labels=F, tick=T, yaxp=c(0,6,1))

lines(x=c(1,4), y=c(2,4), col="blue", lwd=3)
lines(x=c(1,4), y=c(1,5), col="red", lty=5, lwd=6)



plot(1,1, xaxt="n", yaxt="n", type="n", ylim=c(0,6), xlim=c(0,5), axes=F, xlab="",ylab="", xaxs="i", yaxs="i")

abline(h=c(2,4), col="lightgray", lwd=0.2)
text(1,4.5,labels="(d)", cex=2)
#mtext(cex=1,side=3, text="Non-specific adaptation", line=1)





mtext(cex=1,side=1, text=expression(paste(CO[2]," assay environment")), line=2.5)

mtext(cex=1,side=1,text="Ambient", at=1, line=0.5, adj=0.5, col="blue")
mtext(cex=1,side=1,text="High", at=4, line=0.5, adj=0.5, col="red", lty=5)

axis(side=1, labels=F, tick=T, xaxp=c(0,5,1))
axis(side=2, labels=F, tick=T, yaxp=c(0,6,1))

lines(x=c(1,4), y=c(2,4), col="blue", lwd=3)
lines(x=c(1,4), y=c(3,5), col="red", lty=5, lwd=6)

dev.off()