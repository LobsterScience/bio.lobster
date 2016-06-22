setupDepletionData<-function(lfa='34'){

	loadfunctions('lobster')
	logsInSeason<-read.csv(file.path( project.datadirectory('bio.lobster'), "data","logsInSeason.csv"))
	logs<-subset(logsInSeason,LFA==lfa,c("SYEAR","DATE_FISHED","WEIGHT_KG","NUM_OF_TRAPS"))
	if(lfa%in%c()){
		landings<-read.csv(file.path(project.datadirectory('bio.lobster'),"data","Commercial","SeasonalSlipLand.csv"))
		landings$YEAR<-as.numeric(substr(landings$SEASON,6,9))
	}
	else{
		landings<-read.csv(file.path(project.datadirectory('bio.lobster'),"data","Commercial","AnnualSlipLand.csv"))
	}

	S<-sort(unique(logs$SYEAR))
	cpue.lst<-list()
	for(i in 1:length(S)){

		tmp<-subset(logs,SYEAR==S[i],c("DATE_FISHED","WEIGHT_KG","NUM_OF_TRAPS"))
		names(tmp)<-c('time','catch','effort')
		tmp$date<-as.Date(tmp$time)
		first.day<-min(tmp$date)
		tmp$time<-julian(tmp$date,origin=first.day-1)
		cpue.lst[[i]]<-jackknife(tmp,run=T)
		total.landings.bumpup<-unlist(subset(landings,YEAR==S[i],paste0("LFA",lfa)))*1000/sum(cpue.lst[[i]]$catch)
		cpue.lst[[i]]$cumuluative.catch<-cumsum(cpue.lst[[i]]$catch)*total.landings.bumpup
		cpue.lst[[i]]$season<-S[i]
	}

	cpue.dat<-do.call("rbind",cpue.lst)
	cpue.dat$cpue.se<-cpue.dat$cpue.var^0.5
	write.csv(cpue.dat,file.path(project.datadirectory('bio.lobster'),"data",paste0("depletionData",lfa,".csv")))

	return(cpue.dat)
}
