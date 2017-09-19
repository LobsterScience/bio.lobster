#' @title getIncr
#' @description Prediction of moulting based on day since last moult, size and can include temperature through a degree day metric
#' @param \code{p} :parameter list containing (at a minimum) region, doy (day of year), temp (temperature at doy)
#' @param \code{cw} : carapace width 
#' @return The predicted probability of moulting
#' @author  Brad Hubley, \email{Brad.Hubley@@dfo-mpo.gc.ca}
#' @export


getIncr = function(p,cw){

	mgf = p$GrowthFactorMean
	sdgf = p$GrowthFactorSD
	
	x = p$lens[which(p$lens==cw):length(p$lens)]

	pgf = dnorm(x,cw*mgf,cw*sdgf) # modify for decreased incr with increased size, look for data
	
	res = pgf/sum(pgf)

	return(res)
}
