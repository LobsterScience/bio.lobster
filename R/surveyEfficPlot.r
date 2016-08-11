#' @title surveyEfficPlot
#' @description uses the output of nefsc.analysis or dfo.rv.analysis where strata.efficiencies=T, to make a plot of the variance efficiencies based on the comparison of stratified random design to a simple random survey design. Based on Smith and Gavaris 1993.
#' @param \code{x} :the output file from nefsc.analysis or dfo.rv.analysis where p$strata.efficiencies=T
#' @param \code{fp} : file path for saving
#' @param \code{fn} : file name, needs to include .pdf as this is currently only setup to make pdf files.
#' @return makes a plot of survey efficiencies
#' @author  Adam Cook, \email{Adam.Cook@@dfo-mpo.gc.ca}
#' @export
#' @examples
#' require(bio.lobster)
#' p$reweight.strata = T #this subsets 
#'	p$years.to.estimate = c(1968:2015)
#'	p$length.based = T
#'	p$size.class= c(82,300)
#'	p$by.sex = T
#'	p$sex = c(1,2) # male female berried c(1,2,3)
#'	p$bootstrapped.ci=T
#'	p$strata.files.return=F
#'	p$strata.efficiencies=T
#'	p$clusters = c( rep( "localhost", 7) )
#'	p$season =c('spring')# p$series =c('spring');p$series =c('fall')
#'	p$define.by.polygons = F
#'	p$lobster.subunits=F
#'	p$area = 'LFA41'
#'	p = make.list(list(yrs=p$years.to.estimate),Y=p)
#'	aout= nefsc.analysis(DS='stratified.estimates.redo',p=p)
#' 	surveyEfficPlot(aout,'nefscSpringLFA41.pdf')


surveyEfficPlot <- function(x,fp = project.figuredirectory('bio.lobster'),fname) {
			if(!grepl('.pdf',fname)) stop('Need to include .pdf in fname')
		    pdf(file.path(fp,fname))
		      with(x[[1]],plot(yr-0.1,strat.effic.wt,type='h',col='black',xlab='Year', lwd=2,ylab='Efficiency (%)',ylim=c(-100,100)))
		      with(x[[1]],lines(yr+0.1,alloc.effic.wt,type='h',col='grey40',lwd=2,ylab='Efficiency (%)',ylim=c(-100,100)))
		      legend('topright',lty=c(1,1),lwd=2,col=c('black','grey40'),c('Strata Efficiency','Allocation Efficiency'),bty='n',cex=0.9)
		      dev.off()
}