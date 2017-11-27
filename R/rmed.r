#' @export

rmed <- function(yr,x) {
	# frequently used running median	
		if(any(is.na(x) | !is.finite(x))) {
		  		ik  = which(is.na(x) | !is.finite(x))
				xpo = x[-ik]
				xpy = yr[-ik]
				rmean = runmed(xpo,k=3,endrule='median')
				yp = data.frame(mean = rmean, year=xpy)
				lines(yp$year,yp$mean,lty=1,lwd=3,col='salmon')
				rmean.yr = yp$year; rmean = yp$mean
		  	} else {
      	rmean = runmed(x,k=3,endrule='median')
        rmean.yr = yr[1:length(yr)]
		
		}
		return(list(yr = rmean.yr,x=rmean))
}