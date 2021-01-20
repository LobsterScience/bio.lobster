#' @title getGroMat
#' @description Prediction of moulting based on day since last moult, size and can include temperature through a degree day metric
#' @param \code{p} :parameter list containing (at a minimum) lens (length bins), doy (day of year), temp (temperature at doy)
#' @param \code{gdd} : logical: growth using degree-days
#' @return The transition matrix for moulting and probability of moulting at length
#' @author  Brad Hubley, \email{Brad.Hubley@@dfo-mpo.gc.ca}
#' @export


getGroMat = function(p){

	mat=matrix(NA,length(p$lens),length(p$lens))
	pM=c()
	for (i in 1:(nrow(mat)-1)){

		# get probablitiy of molting		
		pM[i] = pPrMolt(p=p,cl=p$lens[i]) 
		# use molt increment mean & sd to determine which size classes to molt into then multiply by probability of molting
		incr = na.omit(p$Incr[i,]) * pM[i] 	
		# fill in row with proportion molted by size class 
		mat[i,i:ncol(mat)] = incr
	}
	mat[is.na(mat)] = 0
	
	return(list(transMatrix=mat, pMolt=c(pM,0)))
}
