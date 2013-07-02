setwd("/Users/LowDecarie/Documents/MSc/Reports/Proceedings B/Figures")

pdf("Figure 1.pdf", width=24, height=5)

par(mfrow=c(1,6), mar=c(5, 0, 4, 2))

plot(1,1, xaxt="n", yaxt="n", type="n", ylim=c(0,6), xlim=c(0,5), axes=F, xlab="",ylab="", xaxs="i", yaxs="i")


plot(1,1, xaxt="n", yaxt="n", type="n", ylim=c(0,6), xlim=c(0,5), axes=F, xlab="",ylab="", xaxs="i", yaxs="i")
abline(h=c(2,4), col="lightgray", lwd=0.1)
text(1,4.5,labels="(a)", cex=2)

mtext(side=3, text="Accumulation of conditionaly deleterious mutations", line=1)

mtext(side=2, text="Measure of fitness", line=0.5)

#mtext(side=1, text="CO2 assay environment", line=1.5)

mtext(side=1,text="Ambient", at=1, line=0.5, adj=0.5)
mtext(side=1,text="High", at=4, line=0.5, adj=0.5)

axis(side=1, labels=F, tick=T, xaxp=c(0,5,1))
axis(side=2, labels=F, tick=T, yaxp=c(0,6,1))

lines(x=c(1,4), y=c(2,4), col="blue", lwd=4)
lines(x=c(1,4), y=c(1,4), col="red",lwd=2)





plot(1,1, xaxt="n", yaxt="n", type="n", ylim=c(0,6), xlim=c(0,5), axes=F, xlab="",ylab="", xaxs="i", yaxs="i")
text(1,4.5,labels="(b)", cex=2)
abline(h=c(2,4), col="lightgray", lwd=0.1)
mtext(side=3, text="Specific adaptation to CO2", line=1)


mtext(side=1, text="CO2 assay environment", line=3.5, at=5)

mtext(side=1,text="Ambient", at=1, line=0.5, adj=0.5)
mtext(side=1,text="High", at=4, line=0.5, adj=0.5)

axis(side=1, labels=F, tick=T, xaxp=c(0,5,1))
axis(side=2, labels=F, tick=T, yaxp=c(0,6,1))

lines(x=c(1,4), y=c(2,4), col="blue", lwd=4)
lines(x=c(1,4), y=c(2,5), col="red",lwd=2)






plot(1,1, xaxt="n", yaxt="n", type="n", ylim=c(0,6), xlim=c(0,5), axes=F, xlab="",ylab="", xaxs="i", yaxs="i")
text(1,4.5,labels="(c)", cex=2)
abline(h=c(2,4), col="lightgray", lwd=0.1)
mtext(side=3, text="Specific adaptation to CO2 with tradeoff", line=1.5)
mtext(side=3, text="or combination of a and b", line=0)





#mtext(side=1, text="CO2 assay environment", line=1.5)

mtext(side=1,text="Ambient", at=1, line=0.5, adj=0.5)
mtext(side=1,text="High", at=4, line=0.5, adj=0.5)

axis(side=1, labels=F, tick=T, xaxp=c(0,5,1))
axis(side=2, labels=F, tick=T, yaxp=c(0,6,1))

lines(x=c(1,4), y=c(2,4), col="blue", lwd=4)
lines(x=c(1,4), y=c(1,5), col="red",lwd=2)



plot(1,1, xaxt="n", yaxt="n", type="n", ylim=c(0,6), xlim=c(0,5), axes=F, xlab="",ylab="", xaxs="i", yaxs="i")

abline(h=c(2,4), col="lightgray", lwd=0.1)
text(1,4.5,labels="(d)", cex=2)
mtext(side=3, text="Non-specific adaptation", line=1)





#mtext(side=1, text="CO2 assay environment", line=1.5)

mtext(side=1,text="Ambient", at=1, line=0.5, adj=0.5)
mtext(side=1,text="High", at=4, line=0.5, adj=0.5)

axis(side=1, labels=F, tick=T, xaxp=c(0,5,1))
axis(side=2, labels=F, tick=T, yaxp=c(0,6,1))

lines(x=c(1,4), y=c(2,4), col="blue", lwd=4)
lines(x=c(1,4), y=c(3,5), col="red",lwd=2)

dev.off()