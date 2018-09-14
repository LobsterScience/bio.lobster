#' @export
			
FSRSCatchRatePlot <- function(data,usr=NULL,lrp=NULL,lfa=NULL,fd=file.path(project.figuredirectory('bio.lobster','ReferencePoints')),title = paste('LFA',lfa),fn=paste0('FSRSRecruitCatchRate',lfa),...) {
	

			dir.create( fd, recursive = TRUE, showWarnings = FALSE )
					
					ylim = c(0,max(c(data[,2],lrp,usr)))

					plot(data[,1],data[,2],xlab='Year',ylab='Lobsters / Trap',type='b',lwd=2,pch=16,main=title,ylim=ylim,...)
					data$running.median = rmed(data[,1],data[,2])$x
					lines(data[,1],data$running.median,col='blue',lty=2,lwd=3)
		
						if(!is.null(usr)) abline(h=usr,col='green',lwd=2,lty=2)
						if(!is.null(lrp)) abline(h=lrp,col='red',lwd=2,lty=3)
						#readline(prompt="Press [enter] to continue")
					
					savePlot(file.path(fd,paste(fn,'png',sep='.')),type='png')
					write.csv(data,file.path(fd,paste(fn,'csv',sep='.')))
					print(lfa)
}
			
