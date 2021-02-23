#' @export
			
FSRSCatchRatePlot <- function(recruits=NULL,legals=NULL,usr=NULL,lrp=NULL,save=T,lfa=NULL,rm=F, French=F, fd=file.path(project.figuredirectory('bio.lobster','ReferencePoints')),title = paste('LFA',lfa),fn=paste0('FSRSRecruitCatchRate',lfa),...) {
	
			dir.create( fd, recursive = TRUE, showWarnings = FALSE )
			par(las=1)
			lab=NULL
			if(!is.null(recruits) && !is.null(legals)){
				par(mfrow=c(2,1), mar = c(0, 4, 0, 2), omi = c(1, 0.25, 0.1, 0.25))
				lab=c('(a)','(b)')
			}

			if (French){
			lobtext='Homards/casier'  
			yrtext='Année'  
			}else{
			  lobtext='Lobsters / Trap'  
			  yrtext='Year'   
			  
			}
					ymaxr = ifelse(!is.null(recruits),max(recruits[,-1]),0)
					ymaxl = ifelse(!is.null(legals),max(legals[,-1]),0)

					ymax = max(c(ymaxr,ymaxl,lrp,usr))
					if(!is.null(recruits)){
						plot(recruits[,1],recruits[,2],xlab='',ylab='',type='n',main='',ylim=c(0,1.15*ymax),...)
					  title(title, line = -1.1, cex.main=1.5)
						polygon(c(recruits[,1],rev(recruits[,1])),c(recruits[,3],rev(recruits[,4])),col='grey',border=NA)
						lines(recruits[,1],recruits[,2],lwd=2,pch=16,type='b')
						if(rm){
							recruits$running.median = rmed(recruits[,1],recruits[,2])$x
							lines(recruits[,1],recruits$running.median,col='blue',lty=2,lwd=3)
										}
						axis(4,lab=F)
			
						if(!is.null(usr)) abline(h=usr,col='green',lwd=2,lty=2)
						if(!is.null(lrp)) abline(h=lrp,col='red',lwd=2,lty=3)
					mtext(lab[1], 3, -4, outer = F,adj=0.05,cex=1.25)	
					}
					
					if(!is.null(legals)){
						plot(legals[,1],legals[,2],xlab='',ylab='',type='n',main='',ylim=c(0,1.15*ymax),...)
						polygon(c(legals[,1],rev(legals[,1])),c(legals[,3],rev(legals[,4])),col='grey',border=NA)
						lines(legals[,1],legals[,2],lwd=2,pch=16,type='b')
						if(rm){
							legals$running.median = rmed(legals[,1],legals[,2])$x
							lines(legals[,1],legals$running.median,col='blue',lty=2,lwd=3)
						}
						axis(4,lab=F)
						if(!is.null(usr)) abline(h=usr,col='green',lwd=2,lty=2)
						if(!is.null(lrp)) abline(h=lrp,col='red',lwd=2,lty=3)
					mtext(lab[2], 3, -4, outer = F,adj=0.05,cex=1.25)	
					}
					mtext(lobtext, 2, -1, outer = T,las=0)	
					mtext(yrtext, 1, 2, outer = T)
					if (save){savePlot(file.path(fd,paste(fn,'png',sep='.')),type='png')}
					if(!is.null(recruits))write.csv(recruits,file.path(fd,paste(fn,'recruits','csv',sep='.')))
					if(!is.null(legals))write.csv(legals,file.path(fd,paste(fn,'legals','csv',sep='.')))
					print(lfa)
}
			
