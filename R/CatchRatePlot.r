#' @export

CatchRatePlot <- function(land,usr,lrp,lfa,out.dir='bio.lobster') {
	
			fd = file.path(project.figuredirectory(out.dir),'ReferencePoints')
			dir.create( fd, recursive = TRUE, showWarnings = FALSE )
					
					xr = range(land$YR)
					cc = c('black')
					pchs = c(16)
					lls = c(1)
					ylims = range(c(land[,2],lrp,usr))
					labs = 'Wt'
						plot(land$YR,land[,2],xlab='Year',ylab='CPUE',type='b',lwd=2,pch=16,main=paste('LFA',lfa,sep=' '),ylim=ylims)
					au = rmed(land$YR,land[,2])
					lines(au$yr,au$x,col='blue',lty=2,lwd=3)
		
						if(!is.null(usr)) abline(h=usr,col='green',lwd=2,lty=2)
						if(!is.null(lrp)) abline(h=lrp,col='red',lwd=2,lty=3)
					savePlot(file.path(fd,paste('CatchRateRefs',lfa,'.png',sep='')),type='png')
					print(lfa)
}
			
