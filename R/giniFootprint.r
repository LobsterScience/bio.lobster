#' @title giniFootprint
#' @description gini index of patchniness from a fisheryFootprint, this assumes fishery footprint boxes are all the same size
#' @param \code{x} : footprint Metric
#' @param \code{plot} : makes the Lorenz plot if set to \code{true}
#' @return estimate of gini
#' @examples
#' x=c(0,	0,	0,	0,	0,	0.25,	1.81,	2.52,	2.6,	3.68,	7.58,	10.07,	13.33,	13.33,	24.99)
#' giniFootprint(x,plot=T)
#' @author  Adam Cook, \email{Adam.Cook@@dfo-mpo.gc.ca}
#' @export


giniFootprint <- function(x,plot=F){
		    x = x[order(x)]
		    n = length(x)
		    np = rep(1,times=n)
		    cP = cumsum(x) / sum(x)
		    cA = cumsum(np) / sum(np)
		    if(plot) {
		    	plot(cA,cP,type='l')
		    	abline(a=0,b=1,col='blue')
		    	}
		    gI = 1-(2*sum(rowMeans(embed(cP,2)) * diff(cA)))
		    return(gI)

			}