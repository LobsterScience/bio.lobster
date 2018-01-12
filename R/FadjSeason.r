#' @export
FadjSeason = function(p){

	tpy = round(365/p$timestep)

	season = seq(julian(as.Date(p$season[1]),origin=as.Date(p$startDate)),julian(as.Date(p$season[2]),origin=as.Date(p$startDate)),1)

	q = 0
	pvec = c()
	
	for(i in 1:tpy){
		x = max(q) 
		q = x + (1:p$timestep)
		pvec[i] = sum(season%in%q)/365

	}
	pvec = rep(pvec,p$nt/tpy)
	return(pvec)
}