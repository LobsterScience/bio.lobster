#' @export
ScallopSurveyProcess<-function(size.range=c(0,220),SPA,Yrs,bin.size=5,log=F,sex=0:3){
	
	require(lubridate)

	# import bycatch data from scallop survey
	lobster.db('scallop')
	scallop.tows$YEAR<-year(scallop.tows$TOW_DATE)

	# calculate area swept for bycatch
	scallop.tows$GEAR_WIDTH_BYCATCH<-with(scallop.tows,DRAG_WIDTH*(NUM_LINED+NUM_UNLINED)*0.3048)
	scallop.tows$GEAR_WIDTH_BYCATCH[scallop.tows$MGT_AREA_ID=='29']<-18*0.3048
	scallop.tows$AREA_SWEPT<-with(scallop.tows,GEAR_WIDTH_BYCATCH*TOW_LEN)

	# select for lobsters
	lobster.catch<-subset(scallop.catch,SPECCD_ID==2550&SEX_ID%in%sex,c(2,5:8))
	
	# merge with tow data
	ScalSurvLob.dat<-merge(scallop.tows,lobster.catch,all=T)
	
	# subset for area and years
	if(!missing(SPA))	ScalSurvLob.dat<-subset(ScalSurvLob.dat,MGT_AREA_ID%in%SPA)
	if(!missing(Yrs))	ScalSurvLob.dat<-subset(ScalSurvLob.dat,YEAR%in%Yrs)

	# 
	ScalSurvLob.dat$NLobs<-0
	ScalSurvLob.dat$NLobs[ScalSurvLob.dat$MEAS_VAL>size.range[1]&ScalSurvLob.dat$MEAS_VAL<size.range[2]]<-1
	ScalSurvLob.dat$lon<-convert.dd.dddd(ScalSurvLob.dat$START_LONG)
	ScalSurvLob.dat$lat<-convert.dd.dddd(ScalSurvLob.dat$START_LAT)


	tmp<-with(ScalSurvLob.dat,tapply(NLobs,TOW_SEQ,sum))
	d1<-subset(ScalSurvLob.dat,!duplicated(TOW_SEQ),c('TOW_SEQ','YEAR','TOW_DATE','MGT_AREA_ID','AREA_SWEPT','lon','lat'))
	d2<-data.frame(TOW_SEQ=as.numeric(names(tmp)),NLobs=as.vector(tmp))
	ScalSurvLob<-merge(d1,d2,all.x=T)

	# add columns for length bins
	bins<-seq(size.range[1],size.range[2],bin.size)
	sets<-unique(ScalSurvLob.dat$TOW_SEQ)

	CLF<-data.frame(TOW_SEQ=sets,t(sapply(sets,function(s){with(subset(ScalSurvLob.dat,TOW_SEQ==s&MEAS_VAL>=min(bins)&MEAS_VAL<max(bins)),hist(MEAS_VAL,breaks=bins,plot=F)$count)})))
	names(CLF)[-1]<-paste0("CL",bins[-1])
	
	ScalSurvLob<-merge(ScalSurvLob,CLF,all=T)

	# standardized to 4000 m^2
	ScalSurvLob$NLobsStd<-ScalSurvLob$NLobs/ScalSurvLob$AREA_SWEPT*4000
	ScalSurvLob$LobDen<-ScalSurvLob$NLobs/ScalSurvLob$AREA_SWEPT*10^6
	ScalSurvLob[,which(names(ScalSurvLob)%in%names(CLF)[-1])]<-sweep(ScalSurvLob[,which(names(ScalSurvLob)%in%names(CLF)[-1])],1,FUN="/", ScalSurvLob$AREA_SWEPT/4000)

    # add LFA column
    events <- with(ScalSurvLob,data.frame(EID=TOW_SEQ,X=lon,Y=lat))
    LFAPolys<-read.csv(file.path( project.datadirectory('bio.lobster'), "data","maps","LFAPolys.csv"))
    key <- findPolys(events,LFAPolys)
    ScalSurvLob <- merge(ScalSurvLob,with(key,data.frame(TOW_SEQ=EID,LFA=PID)),all=T)


	print("Lobster Abundance From Scallop Survey")
	print(sort(unique(ScalSurvLob$YEAR)))
	print(sort(unique(ScalSurvLob$MGT_AREA_ID)))

	return(ScalSurvLob)

}