#' @title giniFootprint
#' @description gini index of patchniness from a fisheryFootprint, this assumes fishery footprint boxes can change
#' @param \code{x} : footprint metric per grid
#' @param \code{y} : area of grid
#' @param \code{plot} : makes the Lorenz plot if set to \code{true}
#' @return estimate of gini
#' @examples
#' x=c(0,	0,	0,	0,	0,	0.25,	1.81,	2.52,	2.6,	3.68,	7.58,	10.07,	13.33,	13.33,	24.99)
#' y=c(1,1,1,1,2,2,2,3,3,1,1,2,3,4,5)
#' giniFootprint(x,plot=T)
#' @author  Adam Cook, \email{Adam.Cook@@dfo-mpo.gc.ca}
#' @export


giniFootprint <- function(x,y=NULL,plot=F){
			a = order(x)
		    x = x[a]
		    y = y[a]
		
		    n = length(x)
		    if(is.null(y)) {
		    	y= rep(1,times=n)
		      }
		    if(any(c(is.na(y),is.na(x)))){
		    	h = c(which(is.na(x)),which(is.na(y)))
		    	x=x[-h]
		    	y=y[-h]
		    }
		    cP = cumsum(x) / sum(x)
		    cA = cumsum(y) / sum(y)
		    if(plot) {
		    	plot(cA,cP,type='l')
		    	abline(a=0,b=1,col='blue')
		    	}
		    gI = 1-(2*sum(rowMeans(embed(cP,2)) * diff(cA)))
		    return(gI)
			}