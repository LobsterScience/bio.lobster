FadjSeason = function(p){

	tpy = round(365/p$timestep)

	season = seq(julian(as.Date(p$season[1]),origin=as.Date(p$startDate)),julian(as.Date(p$season[2]),origin=as.Date(p$startDate)),1)

	q = 0
	pvec = c()
	
	for(i in 1:tpy){
		x = max(q) 
		q = x + (1:p$timestep)
		pvec[i] = ifelse(sum(q%in%season)/p$timestep>p$seasonThreshold,1,0)
	}

	pvec[pvec==1] = tpy/sum(pvec)
	return(rep(pvec,p$nt/tpy))
}