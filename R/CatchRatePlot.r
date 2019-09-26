#' @export

CatchRatePlot <- function(data,usr=NULL,lrp=NULL,lfa=NULL,fd=file.path(project.figuredirectory('bio.lobster','ReferencePoints')),title = paste('LFA',lfa),fn=paste0('CatchRateRefs',lfa),save=T,ylim,rm=T,title.line=NA,regions=F,...) {
	
	dir.create( fd, recursive = TRUE, showWarnings = FALSE )
			
		if(missing(ylim))ylim = c(0,max(c(data[,2],lrp,usr),na.rm=T))

		plot(data[,1],data[,2],xlab='Year',ylab='CPUE (kg/TH)',lwd=2,pch=16,ylim=ylim,...)
		if(regions){
			polygon(y=c(usr,max(ylim)*1.3,max(ylim)*1.3, usr),x=c(1,1,3000,3000),col='lightgreen',border=NA)
	        polygon(y=c(lrp,usr,usr, lrp),x=c(1,1,3000,3000),col='lightgoldenrod',border=NA)
	        polygon(y=c(-1,lrp,lrp, -1),x=c(1,1,3000,3000),col='darksalmon',border=NA)
	    }
		lines(data[,1],data[,2],type='o',lwd=2,pch=16,ylim=ylim,...)

		title(title,line=title.line)
		if(rm){
	    		running.median = with(rmed(data[,1],data[,2]),data.frame(YEAR=yr,running.median=x))
			data=merge(data,running.median,all=T)
			lines(data[,1],data$running.median,col='blue',lty=2,lwd=3)
		}

		if(!regions){
			if(!is.null(usr)) abline(h=usr,col='green',lwd=2,lty=2)
			if(!is.null(lrp)) abline(h=lrp,col='red',lwd=2,lty=3)
		}
		axis(1,lab=F,tcl=0.3)
		axis(2,lab=F,tcl=0.3)

	

		if(save){
			savePlot(file.path(fd,paste(fn,'.png',sep='')),type='png')
			write.csv(data,file.path(fd,paste(fn,'.csv',sep='')))
		}
		print(lfa)
}
			
