#' @export
FadjSeason = function(p){

	tpy = round(365/p$timestep)

	if(length(p$season)==2) season = seq(julian(as.Date(p$season[1]),origin=as.Date(p$startDate)),julian(as.Date(p$season[2]),origin=as.Date(p$startDate)),1)
	if(length(p$season)==4) season = c(seq(julian(as.Date(p$season[1]),origin=as.Date(p$startDate)),julian(as.Date(p$season[2]),origin=as.Date(p$startDate)),1),seq(julian(as.Date(p$season[3]),origin=as.Date(p$startDate)),julian(as.Date(p$season[4]),origin=as.Date(p$startDate)),1))

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