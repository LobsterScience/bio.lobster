#' @export

CAplots <- function(ann , yr3,out.dir='bio.lobster',fsrs=F,port=F) {
			fd = file.path(project.figuredirectory('bio.lobster'),'CohortAnalysisPlots')
			dir.create( fd, recursive = TRUE, showWarnings = FALSE )
		
		lf = unique(ann$LFA)	
		
		for(i in lf){
			fname = paste('CohortAnalysisExploitationPlotsLFA',i,'.png',sep="")
			if(fsrs) fname = paste('FSRSCohortAnalysisExploitationPlotsLFA',i,'.png',sep="")
			if(port) fname = paste('portCohortAnalysisExploitationPlotsLFA',i,'.png',sep="")
			
				o = subset(ann,LFA==i)
				p = subset(yr3,LFA==i)
				png(file=file.path(fd,fname),units='in',width=15,height=12,pointsize=24, res=300,type='cairo')
		
				with(o,plot(YEAR,expl,type='p',pch=16,xlab='Year',ylab = 'Exploitation Index',main=paste('LFA',i)))
					for(j in 1:nrow(p)){
							with(p,arrows(x0=Year.min,x1=Year.max,y0 = expl,lwd=2,col='blue',length=0,))
						}
				dev.off()
			}
	}