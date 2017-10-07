getDegreeDays = function(p,t){
	
	d = t * p$timestep
	lts = p$dailytemps[(d-p$timestep+1):d]
	d2 = p$timestep-max(which(lts>p$mint))

	dd = sum(p$dailytemps[(d-p$doy):(d-d2)])

	dd
}