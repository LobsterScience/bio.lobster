#' @export
CPUEplot<-function(logs,lfa=NULL,yrs,French= F, subarea=NULL,lab='',graphic='R',wd=8,ht=11,effort.min=10,export=F,path=file.path( project.figuredirectory("bio.lobster")),...){

	if(is.null(lfa)&&is.null(subarea))stop("specify LFA or subarea")

	if(missing(yrs))yrs<-unique(logData$SYEAR)
	if(!is.null(lfa))logs<-subset(logs,LFA%in%lfa&SYEAR%in%yrs,c("SYEAR","LFA","DATE_FISHED","WEIGHT_KG","NUM_OF_TRAPS","subarea"))
	logs$DATE_FISHED<-as.Date(logs$DATE_FISHED)
	dates<-data.frame(DATE=seq(min(logs$DATE_FISHED),max(logs$DATE_FISHED),1))
	daily<-list()
	annual<-list()

	if(!is.null(subarea)){
		logs$LFA = logs$subarea
		lfa = subarea
	}

	for(i in 1:length(lfa)){
		# daily
		print(lfa[i])
		#if(lfa[i]=='31A') browser()
		catch<-with(subset(logs,LFA==lfa[i]),tapply(WEIGHT_KG,DATE_FISHED,sum,na.rm=T))
		effort<-with(subset(logs,LFA==lfa[i]),tapply(NUM_OF_TRAPS,DATE_FISHED,sum,na.rm=T))
		daily[[i]]<-merge(data.frame(LFA=lfa[i],DATE=as.Date(names(catch)),CATCH=catch),data.frame(LFA=lfa[i],DATE=as.Date(names(effort)),EFFORT=effort),all=T)
		daily[[i]]$CPUE<-daily[[i]]$CATCH/daily[[i]]$EFFORT

		# annual
		day<-with(subset(logs,LFA==lfa[i]),tapply(DATE_FISHED,SYEAR,mean,na.rm=T))
		catch<-with(subset(logs,LFA==lfa[i]),tapply(WEIGHT_KG,SYEAR,sum,na.rm=T))
		effort<-with(subset(logs,LFA==lfa[i]),tapply(NUM_OF_TRAPS,SYEAR,sum,na.rm=T))
		annual[[i]]<-data.frame(LFA=lfa[i],YEAR=as.numeric(names(catch)),DATE=as.Date(day,origin="1970-01-01"),CATCH=catch,EFFORT=effort)
		annual[[i]]$CPUE<-annual[[i]]$CATCH/annual[[i]]$EFFORT

	}
	daily.dat<-do.call("rbind",daily)
	daily.dat<-subset(daily.dat,EFFORT>effort.min)
	daily.dat<-merge(daily.dat,merge(dates,data.frame(LFA=lfa)),all=T)
	annual.dat<-do.call("rbind",annual)

	if(graphic=='pdf')pdf(file.path( path,paste0("CPUE",lab,".pdf")),wd,ht)

	par(mfrow=c(length(lfa),1),mar=c(0,0,0,0),omi=c(0.5,1,0.5,0.5),las=1)

	for(i in 1:length(lfa)){

		plot(CPUE~DATE,subset(daily.dat,LFA==lfa[i]),type='l',ylim=c(0,max(daily.dat$CPUE,na.rm=T)),col=rgb(0,0,0,0.5),...)
		lines(CPUE~DATE,subset(annual.dat,LFA==lfa[i]),type='b',pch=21,bg='red')
		text(min(daily.dat$DATE,na.rm=T),max(daily.dat$CPUE,na.rm=T)*.8,paste("LFA",lfa[i]),cex=2,pos=4)
	}
		
  if (French){mtext("CPUE (kg/casier leve)", 2, 3, outer = T, cex = 1.5,las=0)}
	
	else {mtext("CPUE (kg/TH)", 2, 3, outer = T, cex = 1.5,las=0)}
	
	if(graphic=='pdf')dev.off()

	if(export)write.csv(annual.dat,file.path( project.datadirectory("bio.lobster"), "data","products","CommercialCPUE.csv"),row.names=F)


	return(list(daily.data=daily.dat,annual.data=annual.dat))
}
