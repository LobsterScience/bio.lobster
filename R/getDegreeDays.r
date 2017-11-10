#' @export
getDegreeDays = function(p,t){
	
	d = t * p$timestep #total days
	lts = c(p$mint+1,p$dailytemps[(d-p$doy+2):d])
	d2 = p$doy-max(which(lts>p$mint))

	dd = sum(p$dailytemps[(d-p$doy+1):(d-d2)])

	dd
}