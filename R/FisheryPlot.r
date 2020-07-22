#' @export

FisheryPlot <- function(data,lfa=NULL,fd=file.path(project.figuredirectory('bio.lobster','ReferencePoints')),title = paste('LFA',lfa),fn=paste0('FisheryPlot',lfa),preliminary=NULL,units='t',...) {
	
	dir.create( fd, recursive = TRUE, showWarnings = FALSE )

	par(mar=c(5.1, 4.1, 4.1, 5.1),las=1)

			
		if(units=='t')plot(data$YEAR,data$LANDINGS,xlab='Year',ylab='Landings (t)',type='h',main=title,ylim=c(0,max(data$LANDINGS)*1.2),pch=15,col='grey',lwd=10,lend=3,...)
		if(units=='kt')plot(data$YEAR,data$LANDINGS/1000,xlab='Year',ylab='Landings (kt)',type='h',main=title,ylim=c(0,max(data$LANDINGS/1000)*1.2),pch=15,col='grey',lwd=10,lend=3,...)
		if(!is.null(preliminary) & units == 't')lines(data$YEAR[preliminary],data$LANDINGS[preliminary],type='h',pch=21,col=rgb(1,0.6,0),lwd=10,lend=3)
		if(!is.null(preliminary) & units=='kt')lines(data$YEAR[preliminary],data$LANDINGS[preliminary]/1000,type='h',pch=21,col=rgb(1,0.6,0),lwd=10,lend=3)
		
		par(new=T)
		plot(data$YEAR,data$EFFORT2/1000,ylab='',xlab='', type='b', pch=16, axes=F,ylim=c(0,max(data$EFFORT2/1000,na.rm=T)))
		if(!is.null(preliminary))points(data$YEAR[preliminary],data$EFFORT2[preliminary]/1000, type='b', pch=21,bg=rgb(1,0.6,0))
		axis(4)
		mtext("Effort ('000s Trap Hauls)", 4, 3.5, outer = F,las=0)	

		savePlot(file.path(fd,paste(fn,'.png',sep='')),type='png')
		write.csv(data,file.path(fd,paste(fn,'.csv',sep='')))
		print(lfa)



}
			
