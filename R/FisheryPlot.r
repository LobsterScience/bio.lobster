#' @export

FisheryPlot <- function(data,lfa=NULL,fd=file.path(project.figuredirectory('bio.lobster','ReferencePoints')),title = paste('LFA',lfa),fn=paste0('FisheryPlot',lfa),...) {
	
	dir.create( fd, recursive = TRUE, showWarnings = FALSE )

	par(mar=c(5.1, 4.1, 4.1, 5.1),las=1)

			
		plot(data$YEAR,data$LANDINGS,xlab='Year',ylab='Landings (t)',type='h',main=title,ylim=c(0,max(data$LANDINGS)*1.2),pch=15,col='grey',lwd=10,lend=3,...)
		par(new=T)
		plot(data$YEAR,data$EFFORT2/1000,ylab='',xlab='', type='b', pch=16, axes=F,ylim=c(0,max(data$EFFORT2/1000,na.rm=T)))
		axis(4)
		mtext("Effort ('000s Trap Hauls)", 4, 3.5, outer = F,las=0)	


		savePlot(file.path(fd,paste(fn,'.png',sep='')),type='png')
		write.csv(data,file.path(fd,paste(fn,'.csv',sep='')))
		print(lfa)



}
			
