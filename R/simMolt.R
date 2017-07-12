#' @title simMolt
#' @description Prediction of moulting based on day since last moult, size and can include temperature through a degree day metric
#' @param \code{p} :parameter list containing (at a minimum) region, doy (day of year), temp (temperature at doy)
#' @param \code{cw} : carapace width 
#' @return The predicted probability of moulting
#' @author  Brad Hubley, \email{Brad.Hubley@@dfo-mpo.gc.ca}
#' @export

simMolt = function(p,gdd=F){

	start = Sys.time()

	# array N[time, length, # of molts, time since last molt]
	totalPop = array(0,dim=c(p$nt,length(p$lens),p$nt,p$nt))
	totalBerried = totalPop
	totalRemovals = totalPop
	totalEggs = array(0,dim=c(p$nt,length(p$lens)))
	
	totalPop[1,1,1,1] = p$StartPop # start with

	p$Fl = c(rep(0,length(which(p$lens<p$LS))),rep(p$F,length(which(p$lens>p$LS))))
	p$Ml = rep(p$M,length(p$lens))

	p$Fadj = FadjSeason(p) # adjust annual F by 


	ty = p$timestep/365

	for(t in 1:(p$nt-1)){ # years
		spawners = 0

		# the molting process
		for(i in 1:t){ # of molts

			for(j in 1:t){ # of days since last molt

				p$doy =  j * p$timestep # days since last molt
				if(p$sex==2){
						#browser()
						bf = getBerried(p)
						notBerried = totalPop[t,,i,j] * (1 - bf$pB) #NotBerried
						berried = totalPop[t,,i,j] * (bf$pB) #Berried
						released = totalBerried[t,,i,j] * bf$pR
						totalBerried[t+1,,i,j+1] = totalBerried[t,,i,j] + berried - released #add to totalBerried
						totalPop[t,,i,j] = notBerried + released #Return berried that release eggs to total Pop
						spawners = spawners + released 

				}
				gm = getGroMat(p,gdd)
				totalPop[t+1,,i,j+1] = totalPop[t,,i,j] * (1 - gm$pM) #NoMolt
				molted = totalPop[t,,i,j] %*% gm$transMatrix #Molt
				totalPop[t+1,,i+1,1] = totalPop[t+1,,i+1,1] + molted # combine newly molted into 1 timestep since last molt slot

			}
		}

		# mortality
		p$Fl = p$Fl * p$Fadj[t]

		for(l in 1:length(p$lens)){

			totalRemovals[t+1,l,,] = totalPop[t+1,l,,]  * (p$Fl[l] / (p$Fl[l] + p$Ml[l])) * (1 - exp(-(p$Fl[l] + p$Ml[l]) * ty)) #Baranov
			totalPop[t+1,l,,] = totalPop[t+1,l,,] * exp(-p$Ml[l] * ty) * exp(-p$Fl[l] * ty)
			if(p$sex==2)totalBerried[t+1,l,,] = totalBerried[t+1,l,,] * exp(-p$Ml[l] * ty) 
		}

		if(p$sex==2)totalEggs[t+1,] = spawners * Fecundity(cl = p$lens)


	}
	finalPop = data.frame(apply(totalPop,c(1,2),sum))
	names(finalPop)=paste0("CL",p$lens)
	finalBerried = data.frame(apply(totalBerried,c(1,2),sum))
	names(finalBerried)=paste0("CL",p$lens)
	totalRemovals = data.frame(apply(totalRemovals,c(1,2),sum))
	names(totalRemovals)=paste0("CL",p$lens)

	print(Sys.time()-start)
	
	return(list(finalPop=finalPop,finalBerried=finalBerried,totalPop=totalPop,totalBerried=totalBerried,totalEggs=totalEggs,totalRemovals=totalRemovals))
}

