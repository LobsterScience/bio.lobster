#' @title pPrMolt
#' @description probability of moulting based on degreedays since last moult, size.
#' @param \code{cl} : carapace length 
#' @param \code{p} : parameter list with moltPrModel and degreedays (ddoy)
#' @return The predicted probability of moulting
#' @author  Brad Hubley, \email{Brad.Hubley@@dfo-mpo.gc.ca}
#' @export
pPrMolt = function(p,cl){ 

	pM=predict(p$moltModel$moltPrModel,data.frame(degreedays=p$ddoy,CL=cl),type='response')

return(pM)

}
