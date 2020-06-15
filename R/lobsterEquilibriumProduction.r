#' @export

lobsterEquilProduction <- function(p,F=0.5, rand=T) {
stop('this is so far from complete you should not read any further')
			if(rand) {
					p$pLAA   = predLAA(p,rand=T) #rnorm
					p$pWAA   = predWAA(p,rand=F) #esimted from random lengths above
					p$pMAA   = predMAA(p,rand=T) #runif
					p$pMatAA = predMatAA(p,rand=T) #rbeta this is the number exiting the system at any given age and therefore are no longer vulnerable to F
					p$ppR    = predPRA(p,rand=T) #rbeta
					p$pfecAA = predFec(p,rand=F)
					p$pMSea  = predMSea(p,rand=T)
				}
	with(p, {
				Num = c()
				NumBerried = c()
				NumVed = c()
				SexRatio = 0.5
				Num[1]  = 1 #initiate
				NumBerried[1] = 0
				prF = F * ppR #partial recruitment x fully recruited F
				
				if(!is.null(Felv)) prF[1] = prF[1] + Felv #elver fishing mortality
				
				for (a in 2:(nAges - 1)) { 					
							Num[a] 		= Num[a-1] * exp(-(prF[a-1] + pMAA[a-1])) 
							NumExit[a] 	= Num[a-1] * pMatAA[a-1]
							Num[a] 		= Num[a] - NumExit[a] #discounting spawning emigration
							NumExit[a] 	= NumExit[a] * exp(-(pMSea)) # updating NumExiting to include natural morality
						}
				PlusSurv    	= exp(-(prF[nAges-1] + pMAA[nAges-1]))
				Num[nAges]  	= Num[nAges-1] * PlusSurv / (1-PlusSurv)
				NumExit[nAges]  = Num[nAges]  * exp(-pMSea) # assume all remaining leave the system at nAge
				Eggs        	= sum(NumExit * pfecAA)
				CA       		= Num * pWAA * (1-exp(-(prF+pMAA))) * (prF) / (prF+pMAA)
				Catch 			= sum(CA)
				Value 			= 0
				if(exists('value',p)) Value = sum(CA*value)
	result = list()
	
	result$Num 	 	= Num
	result$NumExit 	= NumExit
	result$Eggs 	= Eggs
	result$Catch 	= Catch
	result$Ca 		= CA
	result$Value 	= Value
	result$p     	= list(pLAA=pLAA, pWAA=pWAA, pMAA = pMAA,pMatAA = pMatAA,ppR = ppR, prF = prF, pfecAA = pfecAA,pMSea = pMSea)
	
		return(result)
			})
	}

