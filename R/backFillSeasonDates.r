#' @export
backFillSeasonDates = function(SeasonDates, syr = 1980){
	
	lfas = unique(SeasonDates$LFA)
	SeasonDates$START_DATE = as.Date(SeasonDates$START_DATE)
	SeasonDates$END_DATE = as.Date(SeasonDates$END_DATE)
	slist=list()

	for(i in 1:length(lfas)){

		tmp = subset(SeasonDates,LFA==lfas[i])
		yrs = syr:(min(tmp$SYEAR)-1)
		sdates = rep(tmp$START_DATE[yday(tmp$START_DATE)==min(yday(tmp$START_DATE))][1],length(yrs))
		year(sdates)=yrs

		edates = rep(tmp$END_DATE[yday(tmp$END_DATE)==max(yday(tmp$END_DATE))][1],length(yrs))
		year(edates)=yrs

		newdates = data.frame(LFA=lfas[i],SYEAR=yrs,START_DATE=sdates,END_DATE=edates)

		slist[[i]] = merge(tmp,newdates,all=T)
	}

	extendedSeasonDates = do.call("rbind",slist)

	return(extendedSeasonDates)

}