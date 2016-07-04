#' @export
PortSamplesProcess<-function(lfa='34',min.size=80){
		
	# get port sampling data
	lobster.db('port')
	port1<-subset(port,LFA%in%lfa&L_SIZE>min.size)
	port1$SYEAR<-as.numeric(substr(port1$SEASON,6,9))
	port1$SAMPLE_ID<-as.numeric(paste(port1$SAMPLE_SEQ,port1$SAMPLE_NO,sep='.'))
	
	# individual samples
	column.select<-c("SAMPLE_ID","SAMPLE_SEQ","SAMPLE_NO", "SDATE","SEASON","SYEAR", "NTRAPS", "LATITUDE", "LONGITUDE","LFA","PORT", "COUNTY", "STAT", "PORT_CODE","PORT_LAT", "PORT_LON")
	NPS<-aggregate(subset(port1,select=c("N_MALES","N_FEM","NBF")),by=list(port1$SAMPLE_ID),FUN=sum,na.rm=T)
	names(NPS)[1]<-"SAMPLE_ID"
	NPS$TOTAL<-rowSums(NPS[,-1],na.rm=T)
	portsamples<-merge(subset(port1,!duplicated(SAMPLE_ID),column.select),NPS,all=T)
	portsamples$Q<-quarter(portsamples$SDATE)


	# individual lobsters
	males.lst<-list()
	females.lst<-list()
	berried.lst<-list()
	for(s in subset(portsamples,N_MALES>0)$SAMPLE_ID)males.lst[[s]]<-with(subset(port1,SAMPLE_ID==s&!is.na(N_MALES)&N_MALES>0),data.frame(SAMPLE_ID=s,LENGTH=rep(L_SIZE,N_MALES),SEX="M"))
	for(s in subset(portsamples,N_FEM>0)$SAMPLE_ID)females.lst[[s]]<-with(subset(port1,SAMPLE_ID==s&!is.na(N_FEM)&N_FEM>0),data.frame(SAMPLE_ID=s,LENGTH=rep(L_SIZE,N_FEM),SEX="F"))
	for(s in subset(portsamples,NBF>0)$SAMPLE_ID)berried.lst[[s]]<-with(subset(port1,SAMPLE_ID==s&!is.na(NBF)&NBF>0),data.frame(SAMPLE_ID=s,LENGTH=rep(L_SIZE,NBF),SEX="B"))
	portlengths<-rbind(do.call("rbind",males.lst),do.call("rbind",females.lst),do.call("rbind",berried.lst))
	portlengths<-merge(portsamples,portlengths,all=T)

	list(portsamples=portsamples,portlengths=portlengths)
}
