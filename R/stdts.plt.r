
#' @export
stdts.plt <- function(data, x=names(data[1]), y=names(data[2]), ylab=y, xlab=x, mean.line=F,graphic="R",fn='timeseries',width = 8, height = 6, labcx=1.25, ylim, xlim, col=rep(1,length(y)), pch=1:length(y), lty=1:length(y),type='o', ...){
	if(graphic=='pdf')pdf(file.path(project.figuredirectory("bio.lobster"), "figures",paste0(fn,".pdf")), width = width, height = height, pointsize = 14)
	if(graphic=="R")x11( width = width, height = height)
	meany<-colMeans(data[y],na.rm=T)
	#browser()
	if(missing(xlim))xlim=range(data[x],na.rm=T)
	if(missing(ylim))ylim=range(data[y],na.rm=T)
	par(...)
	plot(unlist(data[x[1]]),unlist(data[y[1]]), type=type, las=1, ylim=ylim, xlim=xlim, ylab="", xlab="", tcl=-0.3, mgp=c(1,0.5,0), lty=lty[1], pch=pch[1], col=col[1])
	if(length(x)==1)x<-rep(x[1],length(y))
	if(length(y)>1)for(i in 2:length(y)){lines(unlist(data[x[i]]),unlist(data[y[i]]), type='o', lty=lty[i], pch=pch[i], col=col[i])}
	if(mean.line)abline(h=meany,lty=3,lwd=1,cex=1, col=col)
	axis(4,lab=F, tcl=-0.3)
	mtext(xlab, 1, 2.5, cex=labcx)
	mtext(ylab, 2, 2.5, cex=labcx)
	if(graphic=='pdf')dev.off()
}	
