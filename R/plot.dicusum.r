#' @title plot.dicusum
#' @description figure decision interval CUSUM control chart analysis 
#' @param \code{x} :output of dicusum.r
#' @param \code{x.index} : index for x labels
#' @param \code{h} : decision interval
#' @return returns a plot
#' @examples
#' x = c(1,2,3,4,5,3,2,1,2,6)
#' a = dicusum(x,1:3,0.2)
#' plot.dicusum(a)
#' @author  Adam Cook, \email{Adam.Cook@@dfo-mpo.gc.ca}
#' @references Mesnil and Pettigas 2008 Aquat Living Resour 22 187-192
#' @export

plot.dicusum <- function(x,x.index,h,...) {
	y = max(abs(c(x$pos.dev,x$neg.dev)))
	ylims = c(y *-1 ,y)
	plot(x.index,x$pos.dev,ylim=c(ylims),ylab="S-|S+", lty=1,lwd=1,type='l')
	lines(x.index,x$neg.dev, lty=1,lwd=1)
	abline(h=h,lty=2,col='grey70')
	abline(h=h *-1 ,lty=2,col='grey70')
	lines(x=c(x.index[min(x$ref.index)],x.index[max(x$ref.index)]),y=c(0,0),col='red',lwd=2)
	}