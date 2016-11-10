#' @title getIncr
#' @description Prediction of moulting based on day since last moult, size and can include temperature through a degree day metric
#' @param \code{p} :parameter list containing (at a minimum) region, doy (day of year), temp (temperature at doy)
#' @param \code{cw} : carapace width 
#' @citation  Bergeron 2011 MSc thesis Lobster Age Size Relationships U of Maine
#' @return The predicted probability of moulting
#' @examples
#' require(devtools)
#' load_all('E:/git/LobsterScience/bio.lobster') #to load from directory rather than package if modifying
#' nefsc.db(DS = 'odbc.dump.redo')
#' @author  Adam Cook, \email{Adam.Cook@@dfo-mpo.gc.ca}
#' @export


getIncr = function(p,cw){

	mgf = p$GrowthFactorMean
	sdgf = p$GrowthFactorSD
	
	x = p$lens[which(p$lens==cw):length(p$lens)]

	pgf = dnorm(x,cw*mgf,cw*sdgf)
	
	res = pgf/sum(pgf)

	return(res)
}
