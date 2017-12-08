#' @export


cohortAnalysis <- function(lens , N , dt , M = 0.146 , termF = 0.2,Tc = 0.8){
	#Cadrin and Estrella 1995 -- with dts estimated from simLobster

	#lens = length.groups
	#N = Landings by lens
	#dt = time at lens
	#M = natural mortality
	#termF = terminalF
	#Tc = Seasonal timing (on lobster years)
	n = length(lens)
	sN = numeric(n)
	PR = mN = F = Z = fz = numeric(n-1)
	pW = lobLW(lens) / 1000

	sN[n] = N[n] * (termF + M) / termF
	for(i in (n-1):1) {
			sN[i] = sN[i+1] * exp(Tc * M * dt[i]) + N[i] * exp((1 - Tc) * M * dt[i])
			}
		fz = N[1:(n-1)] / (sN[1:(n-1)] - sN[2:n] )
		Z = M / (1 - fz)
		F = Z - M
		mN = (sN[1:(n-1)] - sN[2:n] ) / Z
		waF = sum(F * N[1:(n-1)]) / sum(N)
		u = 1-exp(-waF)
		B = sN * pW
		return(list(L = lens, F = F, M = M, termF = termF , Tc= Tc, wF = waF , expl = u, B = B))
	}



#lens = c(81,86,91,96,101,106,111,116)
#dt = c(0.803,	0.831,	0.841,	0.844,	0.845,	0.859,	0.902,	0.949)
#N = o[,2]

#M=0.2

#example from xls spreadsheet cohort.xls (from cadrin) 1993 LFA27 -- waF = 1.779 expl = 0.831
#lens = c(70,76,81,86,91,96,101)
#N = c(214.2,170.8,79.4,21.8,7,1.35,0.359)
#dt = c(0.62,0.584,0.803,0.831,0.841,0.844,0.845)
