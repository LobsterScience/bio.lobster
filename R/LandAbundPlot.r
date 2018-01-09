LandAbundPlot <- function(land,abund,lfa,out.dir='bio.lobster') {
	
			fd = file.path(project.figuredirectory(out.dir),'AtSeaIndictors')
			dir.create( fd, recursive = TRUE, showWarnings = FALSE )
					par(mar=c(4,5,2,5))

					xr = range(land$YR)
					cc = c('black')
					pchs = c(16)
					lls = c(1)
					labs = 'Wt'
						plot(land$YR,land[,2],xlab='Year',ylab='Landings (t)',type='b',lwd=2,pch=16,main=paste('LFA',lfa,sep=' '))
					par(new=T)
					ll = range(do.call(rbind,abund)$N)
					if(length(abund)>0){
					with(abund[[1]], plot(YEAR,N,type='b',lty=2,xaxt='n', yaxt='n',ylab='',xlab='',col='blue',ylim=ll,lwd=2,xlim=xr)); cc = c(cc,'blue'); pchs = c(pchs,1); lls = c(lls,2); labs=c(labs,'AtSea')
					if(length(abund)>=2) {	with(abund[[2]], lines(YEAR,N,type='b',lty=3,lwd=2,pch=17,col='red')); cc = c(cc,'red'); pchs = c(pchs,17); lls = c(lls,3); labs=c(labs,'Port')}
					if(length(abund)==3) {	with(abund[[3]], lines(YEAR,N,type='b',lty=4,lwd=2,pch=18,col='green')); ; cc = c(cc,'green'); pchs = c(pchs,18); lls = c(lls,4); labs=c(labs,'FSRSComm')}
					}
							kk = round(seq(ll[1],ll[2],length=5),1)
							axis(side=4,at=kk,srt=90)
							mtext(side=4,'Landings (N x10^6)',line=3,col='black')
							legend('topleft',legend=labs,pch=pchs,lty=lls,col=cc,bty='n',cex=0.8)
					savePlot(file.path(fd,paste('LandingsAbundance',lfa,'.png',sep='')),type='png')
					print(lfa)
}
			
