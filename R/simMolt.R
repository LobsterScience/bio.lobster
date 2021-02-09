#' @title simMolt
#' @description Prediction of moulting based on day since last moult, size and can include temperature through a degree day metric
#' @param \code{p} :parameter list containing (at a minimum) region, doy (day of year), temp (temperature at doy)
#' @param \code{gdd} : growing degree day
#' @param \code{continuous.recruitment} : T/F whether you want to add new recruits at startPop size at an annual time step
#' @param \code{recruitment.vector} : Variable Recruitment vector of length p$nt/round(365/p$timestep)
#' @return The predicted probability of moulting
#' @author  Brad Hubley, \email{Brad.Hubley@@dfo-mpo.gc.ca}
#' @export

simMolt = function(p,gdd=T,continuous.recruitment=F){

	start = Sys.time()

	# array N[time, length, # of molts, time since last molt]
	totalPop = array(0,dim=c(p$nt,length(p$lens),p$nt,p$nt))
	totalBerried = totalPop
	totalRemovals = totalPop
	totalMolts = totalPop
	moltProbs = totalPop
	totalEggs = array(0,dim=c(p$nt,length(p$lens)))
	totalPop[1,1,1,1] = p$StartPop # start with
	if(continuous.recruitment) {
			xxs = round(365/p$timestep)
			xx = seq(1,p$nt,by=xxs)
			totalPop[xx,1,1,1] = p$StartPop # start with
	}
	if(length(p$F) == 1) {
		p$Fl = getFvec(p$F,p$LS,p$lens,p$window)
		} else {
			p$Fl = p$F
		}
	p$Ml = rep(p$M,length(p$lens))

	ty = p$timestep/365
	Fty = FadjSeason(p) # adjust F time by amount of open season in timestep

	cat('|')

	for(t in 1:(p$nt-1)){ # time
		spawners = 0
		I = ifelse(t<p$maxMolts,t,p$maxMolts)

		# the molting process
		for(i in 1:I){ # of molts


			J = ifelse(t<p$maxTime,t,p$maxTime)

			for(j in 1:J){ # of days since last molt

				p$doy =  j * p$timestep # days since last molt
				p$ddoy = p$doy
				
				if(gdd) p$ddoy =  getDegreeDays	(p,t) #degreedays since last molt

				#d = t * p$timestep
				#if(gdd) p$ddoy = sum(p$dailytemps[(d-p$doy):d]) 

				if(p$sex==2){
						#browser()
						bf = getBerried(p)
						notBerried = totalPop[t,,i,j] * (1 - bf$pB) # Not Berried
						berried = totalPop[t,,i,j] * (bf$pB) # Berried
						released = totalBerried[t,,i,j] * bf$pR
						totalBerried[t+1,,i,j+1] = totalBerried[t,,i,j] + berried - released #add to totalBerried
						totalPop[t,,i,j] = notBerried + released # Return berried that release eggs to total Pop
						spawners = spawners + released 

				}
				gm = getGroMat(p)
				totalPop[t+1,,i,j+1] = totalPop[t,,i,j] * (1 - gm$pM) #NoMolt
				molted = totalPop[t,,i,j] %*% gm$transMatrix #Molt
				totalPop[t+1,,i+1,1] = totalPop[t+1,,i+1,1] + molted # combine newly molted into 1 timestep since last molt slot
				totalMolts[t,,i,j] = molted
				moltProbs[t,,i,j] = gm$pM

				
			}
		}

		# mortality

		for(l in 1:length(p$lens)){

			totalRemovals[t+1,l,,] = totalPop[t+1,l,,]  * ((p$Fl[l] * Fty[t]) / (p$Fl[l] * Fty[t] + p$Ml[l]* ty)) * (1 - exp(-(p$Fl[l] * Fty[t] + p$Ml[l] * ty))) #Baranov
			safe = totalPop[t+1,l,,] * p$reserve * exp(-p$Ml[l] * ty)
			totalPop[t+1,l,,] = totalPop[t+1,l,,] * (1 - p$reserve)
			totalPop[t+1,l,,] = totalPop[t+1,l,,] * exp(-p$Ml[l] * ty) * exp(-p$Fl[l] * Fty[t]) + safe
			if(p$sex==2)totalBerried[t+1,l,,] = totalBerried[t+1,l,,] * exp(-p$Ml[l] * ty) 
		}

		if(p$sex==2)totalEggs[t+1,] = spawners * Fecundity(cl = p$lens)

		cat('-')

	}
	cat('|')

	finalPop = data.frame(apply(totalPop,c(1,2),sum))
	names(finalPop)=paste0("CL",p$lens)
	finalBerried = data.frame(apply(totalBerried,c(1,2),sum))
	names(finalBerried)=paste0("CL",p$lens)
	totalRemovals = data.frame(apply(totalRemovals,c(1,2),sum))
	names(totalRemovals)=paste0("CL",p$lens)
	totalMolts = data.frame(apply(totalMolts,c(1,2),sum))
	names(totalMolts)=paste0("CL",p$lens)

	print(Sys.time()-start)
	
	return(list(finalPop=finalPop,finalBerried=finalBerried,totalPop=totalPop,totalBerried=totalBerried,totalEggs=totalEggs,totalRemovals=totalRemovals,totalMolts=totalMolts,moltProbs=moltProbs))
}

