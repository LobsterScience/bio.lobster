#' @title getBerried
#' @param \code{p} parameter list containing (at a minimum) lens (length bins), doy (day of year), temp (temperature at doy)
#' @param \code{gdd} logical: growth using degree-days
#' @return The probability of bearing eggs at length
#' @author  Brad Hubley, \email{Brad.Hubley@@dfo-mpo.gc.ca}
#' @export


getBerried = function(p){

	pB =  1 / (1+exp(-0.1*(p$doy-p$gestation)))
	pR =  1 / (1+exp(-0.1*(p$doy-(p$gestation+p$brood))))


	pB = pB * pMat(p, cl=p$lens)
	
	return(list(pB=pB,pR=pR))
}
