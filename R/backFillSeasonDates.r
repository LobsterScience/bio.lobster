#' @export
backFillSeasonDates = function(SeasonDates, syr = 1980, eyr = NULL){
	
	lfas = unique(SeasonDates$LFA)
	if(any(is.na(lfas))) print('Some LFAs are NA in data set')
	lfas = na.omit(lfas)
	SeasonDates$START_DATE = as.Date(SeasonDates$START_DATE)
	SeasonDates$END_DATE = as.Date(SeasonDates$END_DATE)
	slist=list()
	for(i in 1:length(lfas)){
	print(i)
		tmp = subset(SeasonDates,LFA==lfas[i])
		yrs = syr:(min(tmp$SYEAR)-1)
		if(!is.null(eyr)&&eyr>max(tmp$SYEAR)) yrs = c(yrs,(max(tmp$SYEAR)+1):eyr)
		if(lfas[i]%in%c(35:36)){
			sdates1 = rep(tmp$START_DATE[yday(tmp$START_DATE)==min(yday(tmp$START_DATE[month(tmp$START_DATE)<9]))][1],length(yrs))
			year(sdates1)=yrs
			edates1 = rep(tmp$END_DATE[yday(tmp$END_DATE)==max(yday(tmp$END_DATE[month(tmp$START_DATE)<9]))][1],length(yrs))
			year(edates1)=yrs			
			sdates2 = rep(tmp$START_DATE[yday(tmp$START_DATE)==min(yday(tmp$START_DATE[month(tmp$START_DATE)>8]))][1],length(yrs))
			year(sdates2)=yrs-1
			edates2 = rep(tmp$END_DATE[yday(tmp$END_DATE)==max(yday(tmp$END_DATE[month(tmp$START_DATE)>8]))][1],length(yrs))
			year(edates2)=yrs-1
			newdates = data.frame(LFA=lfas[i],SYEAR=sort(rep(yrs,2)),START_DATE=sort(c(sdates2,sdates1)),END_DATE=sort(c(edates2,edates1)))
		}
		if(lfas[i]%in%c(33:34)){
			sdates = rep(tmp$START_DATE[yday(tmp$START_DATE)==min(yday(tmp$START_DATE))][1],length(yrs))
			year(sdates)=yrs-1
			edates = rep(tmp$END_DATE[yday(tmp$END_DATE)==max(yday(tmp$END_DATE))][1],length(yrs))
			year(edates)=yrs
			newdates = data.frame(LFA=lfas[i],SYEAR=yrs,START_DATE=sdates,END_DATE=edates)
		}
		else {
			sdates = rep(tmp$START_DATE[yday(tmp$START_DATE)==min(yday(tmp$START_DATE))][1],length(yrs))
			year(sdates)=yrs
			edates = rep(tmp$END_DATE[yday(tmp$END_DATE)==max(yday(tmp$END_DATE))][1],length(yrs))
			year(edates)=yrs
			newdates = data.frame(LFA=lfas[i],SYEAR=yrs,START_DATE=sdates,END_DATE=edates)
		}
		slist[[i]] = merge(tmp,newdates,all=T)
	}
	extendedSeasonDates = do.call("rbind",slist)
	return(extendedSeasonDates)

}