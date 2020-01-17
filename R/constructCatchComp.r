#' @export
constructCatchComp = function(yrs=1982:2018, sex=1:2, LS=82.5, bins=seq(0,200,5), est.den=T){
	
	# Length Frequency data
	S = CarapaceLengthFrequencies(LFAs= '34', DS='atSea', by='SEX', graphic="R",Yrs = yrs, sex=sex, vers=2, bins=bins, min.size=LS, est.den= est.den)
	P = CarapaceLengthFrequencies(LFAs= '34', DS='port',  by='SEX', graphic="R",Yrs = yrs, sex=sex, vers=2, bins=bins, est.den= est.den)

	# Length-Weight parameters
	a=c(0.000608,0.001413,0.00482)
	b=c(3.0583,2.8746,2.638)
	wv=list()
	wv[[1]] = a[1]*(bins[-length(bins)]+diff(bins)[1]*.5)^b[1]
	wv[[2]] = a[2]*(bins[-length(bins)]+diff(bins)[1]*.5)^b[2]

	# Landings
	land = lobster.db('seasonal.landings')
	land$SYEAR = as.numeric(substr(land$SYEAR,6,9))
	land34 = subset(land,SYEAR%in%yrs)$LFA34


	LC=list()
	dat=list()
	j=1
	#browser()
	for(i in 1:length(yrs)){
		LC[[i]] = S[[i]] + P[[i]]
		for(s in sex){
			sw = LC[[i]][s,] * wv[[s]]
			ps = sum(sw) / (land34[i] * 10^6)
			dat[[j]] = data.frame(year=yrs[i],sex=s,CL=bins[-length(bins)]+5,obs=round(LC[[i]][s,]/ps))
			j = j + 1
		}
	}

	CLCdata=do.call("rbind",dat)

	return(CLCdata)

}
