setwd("/Users/LowDecarie/Documents/MSc/Reports/Proceedings B/Figures")

pdf("Figure 1 presentation.pdf")

par(mar=c(5, 4, 4, 2))

#plot(1,1, xaxt="n", yaxt="n", type="n", ylim=c(0,6), xlim=c(0,5), axes=F, xlab="",ylab="", xaxs="i", yaxs="i")


plot(1,1, xaxt="n", yaxt="n", type="n", ylim=c(0,6), xlim=c(0,5), axes=F, xlab="",ylab="", xaxs="i", yaxs="i")
abline(h=c(2,4), col="lightgray", lwd=0.2)
#text(1,4.5,labels="(a)", cex=2)

mtext(cex=1.5,side=3, text="Accumulation of conditionaly deleterious mutations", line=1)
mtext(cex=1.5,side=1, text="Assay environment", line=2.5)
mtext(cex=1.5,side=2, text="Measure of fitness (eg. growth rate)", line=0.5)

legend(x=2.5,1,legend=c("Ambient","High"),lwd=1, col=c("blue","red"),title="Selection environment", xjust=0.5, yjust=0.5, cex=1.2)


mtext(cex=1.5,side=1,text="Ambient", col="blue", at=1, line=0.5, adj=0.5)
mtext(cex=1.5,side=1,text="High", col="red", at=4, line=0.5, adj=0.5)

axis(side=1, labels=F, tick=T, xaxp=c(0,5,1))
axis(side=2, labels=F, tick=T, yaxp=c(0,6,1))

lines(x=c(1,4), y=c(2,4), col="blue", lwd=4)
lines(x=c(1,4), y=c(1,4), col="red",lwd=2)





plot(1,1, xaxt="n", yaxt="n", type="n", ylim=c(0,6), xlim=c(0,5), axes=F, xlab="",ylab="", xaxs="i", yaxs="i")
#text(1,4.5,labels="(b)", cex=2)
abline(h=c(2,4), col="lightgray", lwd=0.2)
mtext(cex=1.5,side=3, text="Specific adaptation to CO2", line=1)
mtext(cex=1.5,side=1, text="Assay environment", line=2.5)
mtext(cex=1.5,side=2, text="Measure of fitness", line=0.5)
#mtext(cex=1.5,side=1, text="Assay environment", line=3.5, at=5)
legend(x=2.5,1,legend=c("Ambient","High"),lwd=1, col=c("blue","red"),title="Selection environment", xjust=0.5, yjust=0.5, cex=1.2)

mtext(cex=1.5,side=1,text="Ambient", col="blue", at=1, line=0.5, adj=0.5)
mtext(cex=1.5,side=1,text="High", col="red", at=4, line=0.5, adj=0.5)

axis(side=1, labels=F, tick=T, xaxp=c(0,5,1))
axis(side=2, labels=F, tick=T, yaxp=c(0,6,1))

lines(x=c(1,4), y=c(2,4), col="blue", lwd=4)
lines(x=c(1,4), y=c(2,5), col="red",lwd=2)






plot(1,1, xaxt="n", yaxt="n", type="n", ylim=c(0,6), xlim=c(0,5), axes=F, xlab="",ylab="", xaxs="i", yaxs="i")
#text(1,4.5,labels="(c)", cex=2)
abline(h=c(2,4), col="lightgray", lwd=0.2)
mtext(cex=1.5,side=3, text="Specific adaptation to CO2 with tradeoff", line=1.5)
mtext(cex=1.5,side=3, text="or combination of a and b", line=0)
mtext(cex=1.5,side=2, text="Measure of fitness", line=0.5)
mtext(cex=1.5,side=1, text="Assay environment", line=2.5)
legend(x=2.5,1,legend=c("Ambient","High"),lwd=1, col=c("blue","red"),title="Selection environment", xjust=0.5, yjust=0.5, cex=1.2)




mtext(cex=1.5,side=1,text="Ambient", col="blue", at=1, line=0.5, adj=0.5)
mtext(cex=1.5,side=1,text="High", col="red", at=4, line=0.5, adj=0.5)

axis(side=1, labels=F, tick=T, xaxp=c(0,5,1))
axis(side=2, labels=F, tick=T, yaxp=c(0,6,1))

lines(x=c(1,4), y=c(2,4), col="blue", lwd=4)
lines(x=c(1,4), y=c(1,5), col="red",lwd=2)



plot(1,1, xaxt="n", yaxt="n", type="n", ylim=c(0,6), xlim=c(0,5), axes=F, xlab="",ylab="", xaxs="i", yaxs="i")

abline(h=c(2,4), col="lightgray", lwd=0.2)
#text(1,4.5,labels="(d)", cex=2)
mtext(cex=1.5,side=3, text="Non-specific adaptation", line=1)

legend(x=2.5,1,legend=c("Ambient","High"),lwd=1, col=c("blue","red"),title="Selection environment", xjust=0.5, yjust=0.5, cex=1.2)



mtext(cex=1.5,side=2, text="Measure of fitness", line=0.5)
mtext(cex=1.5,side=1, text="Assay environment", line=2.5)

mtext(cex=1.5,side=1,text="Ambient", col="blue", at=1, line=0.5, adj=0.5)
mtext(cex=1.5,side=1,text="High", col="red", at=4, line=0.5, adj=0.5)

axis(side=1, labels=F, tick=T, xaxp=c(0,5,1))
axis(side=2, labels=F, tick=T, yaxp=c(0,6,1))

lines(x=c(1,4), y=c(2,4), col="blue", lwd=4)
lines(x=c(1,4), y=c(3,5), col="red",lwd=2)







plot(1,1, xaxt="n", yaxt="n", type="n", ylim=c(0,6), xlim=c(0,5), axes=F, xlab="",ylab="", xaxs="i", yaxs="i")

abline(h=c(2,4), col="lightgray", lwd=0.2)
#text(1,4.5,labels="(d)", cex=2)
mtext(cex=1.5,side=3, text="No evolutionary change", line=1)

legend(x=2.5,1,legend=c("Ambient","High"),lwd=1, col=c("blue","red"),title="Selection environment", xjust=0.5, yjust=0.5, cex=1.2)



mtext(cex=1.5,side=2, text="Measure of fitness", line=0.5)
mtext(cex=1.5,side=1, text="Assay environment", line=2.5)

mtext(cex=1.5,side=1,text="Ambient", col="blue", at=1, line=0.5, adj=0.5)
mtext(cex=1.5,side=1,text="High", col="red", at=4, line=0.5, adj=0.5)

axis(side=1, labels=F, tick=T, xaxp=c(0,5,1))
axis(side=2, labels=F, tick=T, yaxp=c(0,6,1))

lines(x=c(1,4), y=c(2.05,4.05), col="#0000FF50", lwd=6)
lines(x=c(1,4), y=c(2,4), col="#FF000050",lwd=6)

dev.off()