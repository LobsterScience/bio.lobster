#' @export
biomass.plt <- function(out.data, years, graphic='pdf',avg.line=NULL,ht=7,wd=6.5,Rymax,Bymax,TAC,pred=1,kt=T,refs=c("USR","LRP","zones"),index,USR,LRP,USR2=8000,LRP2=3000,out.data2,lab='',alpha=c(0.05,0.2),path=''){

# Fit plots
if(graphic=='pdf')pdf(file.path(path,paste(lab,"Biomass.pdf",sep='')), width = wd, height = ht)
if(graphic=='R')windows(wd,ht)

	
if(length(alpha)==1)alpha[2]<-alpha[1]
if(missing(TAC))TAC<-out.data$data$C[out.data$data$NY]
TACI<-which(out.data$data$C.p==TAC)
par(mfrow = c(2,1), mar = c(1, 4, 1, 1), omi = c(0.2, 0.1, 0, 0.2))
if(missing(Bymax))Bymax <- max(apply(out.data$sims.list$B, 2, quantile, 1-alpha[1]/2))
#browser()
plot(years, out.data$median$B, type = 'b', pch = 16, ylim = c(0, Bymax), ylab = "", las = 1, mgp = c(0.5, 0.5, 0), xlab = "", tcl = -0.3, asp = 'xy', xlim=c(min(years)-1,max(years)+max(pred)),yaxt='n' )
axis(4, lab = F, tcl = -0.3)
if(kt)axis(2, pretty(c(0, Bymax)),lab =  pretty(c(0, Bymax))/1000,las=1)
else axis(2)
#text(2007, ymax*0.95, "a", cex=1.25)
if(missing(out.data2)){
	if(kt)mtext("Fully-Recruited Biomass (kt)", 2, 3, cex=1.2)
	else mtext("Fully-Recruited Biomass (t)", 2, 3, cex=1.2)
}
lines(years, apply(out.data$sims.list$B, 2, quantile, alpha[1]/2), lty = 2)
lines(years, apply(out.data$sims.list$B, 2, quantile, 1-alpha[1]/2), lty = 2)
if(is.null(refs))if(!is.null(avg.line))abline(h=avg.line(out.data$median$B),lty=3,col='grey30')
#points(max(years)+1,out.data$median$B.p)
#points(max(years)+1,quantile(out.data$sims.list$B.p,0.5))
print(out.data$median$B[length(out.data$median$B)])
print(out.data$median$B.p[TACI])
#print(out.data$median$B.p2)
#points(max(years)+1,out.data$median$B.p[1])


# Prediction 
if(1%in%pred){
	pB<-out.data$sims.list$B.p[,TACI]
	pB.box<-pB[pB>quantile(pB,alpha[2]/2)&pB<quantile(pB,1-alpha[2]/2)]
	#pY<-max(years)+1
	boxplot(pB.box,add=T,at=max(years)+1,axes=F,range=0,lty=1)
	points(max(years)+1,median(pB.box),pch=16)
}

if(2%in%pred){
	pB2<-out.data$sims.list$B.p2[,TACI]
	pB2.box<-pB2[pB2>quantile(pB,alpha[2]/2)&pB2<quantile(pB2,1-alpha[2]/2)]
	#pY<-max(years)+1
	boxplot(pB2.box,add=T,at=max(years)+2,axes=F,range=0,lty=1)
	points(max(years)+2,median(pB2.box),pch=16)
}
if(missing(index))index<-1:length(out.data$median$B)
if(missing(USR))USR1<-mean(out.data$median$B[index])*0.8
else USR1<-USR
if(missing(LRP))LRP1<-mean(out.data$median$B[index])*0.3
else LRP1<-LRP

if("USR"%in%refs)abline(h=USR1,lty=3,col='grey')
if("LRP"%in%refs)abline(h=LRP1,lty=3,col='grey')
if("USR2"%in%refs)abline(h=USR2,lty=3,col='grey')
if("LRP2"%in%refs)abline(h=LRP2,lty=3,col='grey')
if("zones"%in%refs){
	rect(min(years)-5, -Bymax/5, max(years)+5, LRP1, border=NA, col=rgb(1,0,0,0.2) )
	rect(min(years)-5, LRP1, max(years)+5, USR1, border=NA, col=rgb(1,1,0,0.2) )
	rect(min(years)-5, USR1, max(years)+5, Bymax*1.2, border=NA, col=rgb(0,1,0,0.2) )
}
if("zones"%in%refs){
	text(min(years)+sum(diff(years))*0.75, Bymax*0.9,"HEALTHY",cex=1.5, col=rgb(0.1,0.9,0.1,0.5),pos=4 )
	text(min(years)+sum(diff(years))*0.75, LRP1+(USR1-LRP1)/2,"CAUTIOUS",cex=1.5, col=rgb(1,0.85,0,1) ,pos=4)
	text(min(years)+sum(diff(years))*0.75,LRP1- (LRP1+Bymax*0.04)/2,"CRITICAL",cex=1.5, col=rgb(1,0,0,0.4) ,pos=4)
}


if(!missing(out.data2)){
	
	if(kt)mtext("Fully-Recruited Biomass (kt)", 2, -1, cex=1.2,outer=T)
	else mtext("Fully-Recruited Biomass (t)", 2, -1, cex=1.2,outer=T)
	text(min(years), Bymax*0.95, "a", cex=1.25)
	TACI<-which(out.data2$data$C.p==TAC)
	if(missing(Bymax))Bymax <- max(c(apply(out.data2$sims.list$B, 2, quantile, 1-alpha[1]/2),quantile(out.data2$sims.list$B.p,1-alpha[2]/2)))
	#browser()
	plot(years, out.data2$median$B, type = 'b', pch = 16, ylim = c(0, Bymax), ylab = "", las = 1, mgp = c(0.5, 0.5, 0), xlab = "", tcl = -0.3, asp = 'xy', xlim=c(min(years)-1,max(years)+max(pred)),yaxt='n' )
	axis(4, lab = F, tcl = -0.3)
	if(kt)axis(2, pretty(c(0, Bymax)),lab =  pretty(c(0, Bymax))/1000,las=1)
	else axis(2)
	text(min(years), Bymax*0.95, "b", cex=1.25)
	lines(years, apply(out.data2$sims.list$B, 2, quantile, alpha[1]/2), lty = 2)
	lines(years, apply(out.data2$sims.list$B, 2, quantile, 1-alpha[1]/2), lty = 2)
	if(is.null(refs))if(!is.null(avg.line))abline(h=avg.line(out.data2$median$B),lty=3,col='grey30')
	#points(max(years)+1,out.data2$median$B.p)
	#points(max(years)+1,quantile(out.data2$sims.list$B.p,0.5))
	print(out.data2$median$B[length(out.data2$median$B)])
	print(out.data2$median$B.p[TACI])
	#print(out.data2$median$B.p2)
	#points(max(years)+1,out.data2$median$B.p[1])
	
	
	# Prediction 
	if(1%in%pred){
		pB<-out.data2$sims.list$B.p[,TACI]
		pB.box<-pB[pB>quantile(pB,alpha[2]/2)&pB<quantile(pB,1-alpha[2]/2)]
		#pY<-max(years)+1
		boxplot(pB.box,add=T,at=max(years)+1,axes=F,range=0,lty=1)
		points(max(years)+1,median(pB.box),pch=16)
	}
	
	if(2%in%pred){
		pB2<-out.data2$sims.list$B.p2
		pB2.box<-pB2[pB2>quantile(pB,alpha[2]/2)&pB2<quantile(pB2,1-alpha[2]/2)]
		#pY<-max(years)+1
		boxplot(pB2.box,add=T,at=max(years)+2,axes=F,range=0,lty=1)
		points(max(years)+2,median(pB2.box),pch=16)
	}
	if(missing(index))index<-1:length(out.data$median$B)
	if(missing(USR))USR1<-mean(out.data2$median$B[index])*0.8
	else USR1<-USR
	if(missing(LRP))LRP1<-mean(out.data2$median$B[index])*0.3
	else LRP1<-LRP
	
	if("USR"%in%refs)abline(h=USR1,lty=3,col='grey')
	if("LRP"%in%refs)abline(h=LRP1,lty=3,col='grey')
	if("USR2"%in%refs)abline(h=USR2,lty=3,col='grey')
	if("LRP2"%in%refs)abline(h=LRP2,lty=3,col='grey')
	if("zones"%in%refs){
		rect(min(years)-5, -Bymax/5, max(years)+5, LRP1, border=NA, col=rgb(1,0,0,0.2) )
		rect(min(years)-5, LRP1, max(years)+5, USR1, border=NA, col=rgb(1,1,0,0.2) )
		rect(min(years)-5, USR1, max(years)+5, Bymax*1.2, border=NA, col=rgb(0,1,0,0.2) )
	}
#arrows(pY,quantile(pB,alpha[2]/2),pY,quantile(pB,1-alpha[2]/2), angle = 90,code=3,length = 0.06,lty=1)
#polygon(c(pY-0.2,pY-0.2,pY+0.2,pY+0.2),c(quantile(pB,0.25),quantile(pB,0.75),quantile(pB,0.75),quantile(pB,0.25)),col='white')
#points(pY,median(pB))
}
else{

	if(missing(Rymax))Rymax <- max(apply(out.data$sims.list$R, 2, quantile, 1-alpha[1]/2))
	plot(years, out.data$median$R, type = 'b', pch = 16, ylim = c(0, Rymax), ylab = "", las = 1, mgp = c(0.5, 0.5, 0), xlab = "", tcl = -0.3, asp = 'xy', xlim=c(min(years)-1,max(years)+1),yaxt='n')
	axis(4, lab = F, tcl = -0.3)
	if(kt)axis(2, pretty(c(0, Rymax)),lab =  pretty(c(0, Rymax))/1000,las=1)
	else axis(2)
	#text(2007, ymax*0.95, "b", cex=1.25)
	if(kt)mtext("Recruit Biomass (kt)", 2, 3, cex=1.2)
	else mtext("Recruit Biomass (t)", 2, 3, cex=1.2)
	lines(years, apply(out.data$sims.list$R, 2, quantile, alpha[1]/2), lty = 2)
	lines(years, apply(out.data$sims.list$R, 2, quantile, 1-alpha[1]/2), lty = 2)
	if(!is.null(avg.line))abline(h=avg.line(out.data$median$R),lty=3,col='grey30')
}
print(list(USR=USR1,LRP=LRP1))

if(graphic!='R')dev.off()


}


