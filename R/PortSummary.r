#' @export
PortSummary<-function(lfa='34',bins=seq(50,200,5),output='graph'){
		
	loadfunctions('lobster')
	lobster.db('atSea')
	atSea<-addSYEAR(subset(atSea,LFA==lfa))
	atSea$YEAR<-year(atSea$SDATE)
	atSea$SYEAR[month(atSea$SDATE)<12&atSea$YEAR<2001]<-atSea$YEAR[month(atSea$SDATE)<12&atSea$YEAR<2001]
	atSea$SYEAR[month(atSea$SDATE)==12&atSea$YEAR<2000]<-atSea$YEAR[month(atSea$SDATE)==12&atSea$YEAR<2000]-1
		
	PortSamp<-PortSamplesProcess(lfa=lfa,min.size=0)
		
	logs<-subset(read.csv(file.path( project.datadirectory('bio.lobster'), "data","products","logsInSeason.csv")),LFA==lfa)
	allPorts<-subset(read.csv(file.path( project.datadirectory('bio.lobster'), "data","inputs","Ports.csv")),LFA==lfa)

		#PSports<-subset(PS$portsamples,!duplicated(PORT_CODE))$PORT_CODE
		#ASports<-subset(atSea,!duplicated(PORT))$PORT
		#SLports<-subset(logs,!duplicated(COMMUNITY_CODE))$COMMUNITY_CODE

	if(output=='graph'){
		ports<-allPorts$Port_Code[allPorts$Port_Code%in%logs$COMMUNITY_CODE&allPorts$Port_Code%in%PortSamp$portsamples$PORT_CODE&allPorts$Port_Code%in%atSea$PORT]
		#browser()
		
		for(i in 1:length(ports)){

			pdf(file.path(project.datadirectory('bio.lobster'),"figures","PortSummary",paste0("PortSummary",ports[i],".pdf")))
			#browser()

			# at Sea sampling
			atSeaDat<-subset(atSea,!is.na(SYEAR)&PORT==ports[i],c("SYEAR","CARLENGTH"))
			if(nrow(atSeaDat)>0){
				atSeaCLF<-CLF(atSeaDat,bins=bins,ID="atSea")
				BarPlotCLF(atSeaCLF$CLF,yrs=atSeaCLF$yrs,bins=bins,col='grey',pdf=F,rel=T,LS=83)
			}

			# port sampling
			IDs<-subset(PortSamp[[1]],PORT_CODE==ports[i])$SAMPLE_ID
			portDat<-subset(PortSamp$portlengths,SAMPLE_ID%in%IDs,c("SYEAR","LENGTH"))
			if(nrow(portDat)>0){
				portCLF<-CLF(portDat,bins=bins,ID="Port")
				BarPlotCLF(portCLF$CLF,yrs=portCLF$yrs,bins=bins,col='grey',pdf=F,rel=T,LS=83)
			}

			# logs
			catch<-with(subset(logs,COMMUNITY_CODE==ports[i]),tapply(WEIGHT_KG,SYEAR,sum))
			effort<-with(subset(logs,COMMUNITY_CODE==ports[i]),tapply(NUM_OF_TRAPS,SYEAR,sum))
			cpue.dat<-merge(data.frame(year=as.numeric(names(catch)),catch=catch),data.frame(year=as.numeric(names(effort)),effort=effort),all=T)
			cpue.dat$cpue<-cpue.dat$catch/cpue.dat$effort
			par(mfrow=c(3,1),las=1)
			plot(catch~year,cpue.dat,type='o',pch=16,col='blue',lwd=2,ylim=c(0,max(catch)),ylab="Catch (Kg)",xlab="Year")
			plot(effort~year,cpue.dat,type='o',pch=16,col='blue',lwd=2,ylim=c(0,max(effort)),ylab="Effort (TH)",xlab="Year")
			plot(cpue~year,cpue.dat,type='o',pch=16,col='blue',lwd=2,ylim=c(0,max(cpue)),ylab="CPUE (Kg/TH)",xlab="Year")

			dev.off()



		}
	}
	if(output=='table'){

		port.table<-list()

		for(i in 1:length(years)){

			AS<-with(subset(atSea,SYEAR==years[i]),tapply(CARLENGTH,PORT,length))
			PS<-with(merge(subset(PortSamp$portsamples,SYEAR==years[i],c('PORT_CODE','SAMPLE_ID')),subset(PortSamp$portlengths,SYEAR==years[i])),tapply(LENGTH,PORT_CODE,length))
			SL<-round(with(subset(logs,SYEAR==years[i]),tapply(WEIGHT_KG,COMMUNITY_CODE,sum)))



			port.table[[i]]<-merge(allPorts,merge(data.frame(Port_Code=names(SL),landed.kg=SL),merge(data.frame(Port_Code=names(AS),at.Sea=AS),data.frame(Port_Code=names(PS),at.Port=PS),all=T),all=T),all.y=T)

		}
	}
port.table


}