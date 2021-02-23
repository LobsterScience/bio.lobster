#' @export

LandPlot <- function(land,usr,lrp,lfa,out.dir='bio.lobster', French=F) {
	
			fd = file.path(project.figuredirectory(out.dir),'ReferencePoints')
			dir.create( fd, recursive = TRUE, showWarnings = FALSE )
					
					xr = range(land$YR)
					cc = c('black')
					pchs = c(16)
					lls = c(1)
					ylims = range(c(land[,2],lrp,usr))
					labs = 'Wt'

					if (French){
					xlab='Année'
					ylab='Débarquements (t)'
					} else{
					  xlab='Year'
					  ylab='Landings (t)' 
					}
					plot(land$YR,land[,2],xlab=xlab,ylab=ylab,type='b',lwd=2,pch=16,main=paste('LFA',lfa,sep=' '),ylim=ylims)
					au = rmed(land$YR,land[,2])
					lines(au$yr,au$x,col='blue',lty=2,lwd=3)
						abline(h=usr,col='green',lwd=2,lty=2)
						abline(h=lrp,col='red',lwd=2,lty=3)
					savePlot(file.path(fd,paste('LandingsRefs',lfa,'.png',sep='')),type='png')
					print(lfa)
}
			
