#' @title abundanceDistributionPlot
#' @description uses the output of nefsc.analysis or dfo.rv.analysis to examine the relationship between design weighted area occupied and abundance
#' @param \code{x} :the output file from nefsc.analysis or dfo.rv.analysis
#' @param \code{m} :the output of a loglinear model of abundance and distribution.
#' @param \code{fp} : file path for saving
#' @param \code{fn} : file name, needs to include .pdf as this is currently only setup to make pdf files.
#' @return makes two plots of design weighted area occupied against N.yst and the time series of anomalies from the supplied log linear plot
#' @author  Adam Cook, \email{Adam.Cook@@dfo-mpo.gc.ca}
#' @export
#' @examples
#' require(bio.lobster)
#'	aout= nefsc.analysis(DS='stratified.estimates.redo',p=p)
#'  aout$ln.n.Yst = log(aout$n.Yst)
#'  aout$ln.dwao = log(aout$dwao)
#'  model = lm(ln.dwao~ln.n.Yst,data=aout)
#' 	abundanceDistributionPlot(aout,m=model'nefscSpringLFA41.pdf')


abundanceDistributionPlot <- function(x,m=NULL, fp = project.figuredirectory('bio.lobster'),fname) {
			if(!grepl('.pdf',fname)) stop('Need to include .pdf in fname')
		    pdf(file.path(fp,fname))
		      with(x,plot(yr,dwao,type='p',col='black',xlab='Year', lwd=2,ylab='DWAO',pch=16))
		      with(x,plot(ln.n.Yst,ln.dwao,type='p',col='black',xlab='Abundance', lwd=2,ylab='DWAO',pch=16))
		      abline(m)
		      plot(x$yr,m$residuals,type='h',lwd=2,xlab='Year',ylab='Residuals')
		      abline(h=0,col='red')
		      dev.off()
}