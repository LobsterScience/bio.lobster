#' @export

CAplotsMultDataSets <- function(atSea=NULL, port=NULL, fsrs=NULL, out.dir='bio.lobster',subset=F) {
		#using the three year aggregated LCAs

			fd = file.path(project.figuredirectory(out.dir),'CohortAnalysisPlots')
			dir.create( fd, recursive = TRUE, showWarnings = FALSE )
		
		lf = c(27,29,32,33)
		
		for(i in lf){
			fname = paste('CombinedDataCohortAnalysisExploitationPlotsLFA',i,'.png',sep="")
			if(subset) fname = paste('subset',fname,sep='')
			cols = c()
			nn = c()
			yt = c()
			xp = c()
			lt = c()
			
				if(!is.null(atSea)){ o = subset(atSea,LFA==i); o$YEAR =apply(o[,c('Year.min','Year.max')],1,mean) ; cols = c(cols,'black'); nn = c(nn,'AtSea'); yt = c(yt,o$YEAR); xp = c(xp,o$expl); lt = c(lt,1)}
				if(!is.null(port)) {p = subset(port,LFA==i); p$YEAR = apply(p[,c('Year.min','Year.max')],1,mean); cols = c(cols,'red'); nn = c(nn,'Port'); yt = c(yt,p$YEAR); xp = c(xp,p$expl); lt = c(lt,2)}
				if(!is.null(fsrs)) {r = subset(fsrs,LFA==i); if(nrow(r)>0) {r$YEAR = apply(r[,c('Year.min','Year.max')],1,mean); cols = c(cols,'blue'); nn = c(nn,'FSRS'); yt = c(yt,r$YEAR); xp = c(xp,r$expl); lt = c(lt,3)}}

				png(file=file.path(fd,fname),units='in',width=15,height=12,pointsize=24, res=300,type='cairo')
				
				xr = range(yt)
				yr = range(xp)
				plot(1,1,type='n',xlab='Year',ylab = 'Exploitation Index',main=paste('LFA',i),xlim=xr,ylim=yr)
				
				if(!is.null(atSea)) with(o,lines(YEAR,expl,lty=1,col='black',lwd=2,pch=16,type='b'))
				if(!is.null(port)) with(p,lines(YEAR,expl,lty=2,col='red',lwd=2,pch=16,type='b'))
				if(!is.null(fsrs) & nrow(r)>0) with(r,lines(YEAR,expl,lty=3,col='blue',lwd=2,pch=16,type='b'))
				legend('bottomright',legend=nn,col=cols,lty=lt,lwd=2,bty='n',cex=0.8)
				dev.off()
			}
	}