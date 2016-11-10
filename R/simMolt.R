#' @title getIncr
#' @description Prediction of moulting based on day since last moult, size and can include temperature through a degree day metric
#' @param \code{p} :parameter list containing (at a minimum) region, doy (day of year), temp (temperature at doy)
#' @param \code{cw} : carapace width 
#' @return The predicted probability of moulting
#' @examples
#' @author  Brad Hubley, \email{Brad.Hubley@@dfo-mpo.gc.ca}
#' @export

simMolt = function(p,gdd=F){

	# array N[time, length, # of molts, time since last molt]
	totalPop = array(0,dim=c(p$nt,length(p$lens),p$nt,p$nt)) 
	totalPop[1,1,1,1] = p$StartPop # start with

	for(t in 1:(p$nt-1)){ #

		for(i in 1:t){ # of molts

			for(j in 1:t){ # of days since last molt

				p$doy =  j * p$timestep # days since last molt
				gm = getGroMat(p,gdd)
				totalPop[t+1,,i,j+1] = totalPop[t,,i,j] * (1 - gm$pM) #NoMolt
				molted = totalPop[t,,i,j] %*% gm$transMatrix #Molt
				totalPop[t+1,,i+1,1] = totalPop[t+1,,i+1,1] + molted # combine newly molted into 1 timestep since last molt slot

			}
		}
	}
	finalPop = apply(totalPop,c(1,2),sum)
	
	return(list(finalPop=finalPop,totalPop=totalPop))
}

