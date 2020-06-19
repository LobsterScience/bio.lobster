p = bio.lobster::load.environment()

la()

lobster.db('survey')

ILTS2016TowDepth = ILTS2016TowDepth[order(ILTS2016TowDepth$TTIME),]

tows = unique(ILTS2016TowDepth$TOW)

	for(i in 1:length(tows)){

		png(file.path(project.datadirectory("bio.lobster"),"figures","ILTS","Comparative2016",paste0("Depth",tows[i],".png")),12,12,units='cm',pointsize=12, res=300,type='cairo')
		
		plot(-DEPTH_M~TTIME,subset(ILTS2016TowDepth,TOW==tows[i]),type='l',ylab="Depth",main = paste("Tow",tows[i]),ylim=c(-220,0))

		dev.off()

	}

#  "surveyCatch"  "surveyMeasurements"


  SurveyCatchLobster2016 = subset(surveyCatch, YEAR==2016 & SPECCD_ID==2550)
  SurveyMeasurementsLobster2016 = subset(surveyMeasurements, SET_ID%in%SurveyCatchLobster2016$SET_ID & SPECCD_ID==2550)



 comparativeStations = sort(subset(SurveyCatchLobster2016,GEAR=="280 BALLOON")$STATION)


 NestStations = subset(SurveyCatchLobster2016,GEAR=="NEST"&STATION%in%comparativeStations)
 BalloonStations = subset(SurveyCatchLobster2016,GEAR=="280 BALLOON"&STATION%in%comparativeStations)

 NestStations = NestStations[order(NestStations$STATION),]
 BalloonStations = BalloonStations[order(BalloonStations$STATION),]

NestStations$ORDER = ifelse(BalloonStations$SET_DATE > NestStations$SET_DATE,1,2)
BalloonStations$ORDER = ifelse(BalloonStations$SET_DATE < NestStations$SET_DATE,1,2)


NestLobsters = subset(SurveyMeasurementsLobster2016,SET_ID%in%NestStations$SET_ID)
BalloonLobsters = subset(SurveyMeasurementsLobster2016,SET_ID%in%BalloonStations$SET_ID)


NestStations$SET_TIME - BalloonStations$SET_TIME

par(mfrow=c(2,1),las=1)
 hist(NestLobsters$FISH_LENGTH,breaks=seq(0,200,5),ylim=c(0,400),main="Nest",xlab='')
 hist(BalloonLobsters$FISH_LENGTH,breaks=seq(0,200,5),ylim=c(0,400),main="Balloon",xlab='Carapace Length (mm)')


 
 	## ITQ-ILTS Survey
	
	surveyLobsters34<-LobsterSurveyProcess(lfa="34",yrs=1996:2017,bin.size=5)
 	comparativeStations = sort(subset(surveyLobsters34,YEAR==2016&GEAR=="280 BALLOON")$STATION)
	ComparativeSurvey = subset(surveyLobsters34,YEAR==2016&STATION%in%comparativeStations)

	surveyLobsters34<-LobsterSurveyProcess(lfa="34",yrs=1996:2017,mths=c("Jul","Jun"),bin.size=1)
 	comparativeStations = sort(subset(surveyLobsters34,YEAR==2016&GEAR=="280 BALLOON")$STATION)
	ComparativeSurvey = subset(surveyLobsters34,YEAR==2016&STATION%in%comparativeStations)


ComparativeSurvey = merge(ComparativeSurvey,subset(rbind(NestStations,BalloonStations),select=c("SET_ID","GEAR","ORDER")),all=T)
ComparativeSurvey$FACTOR =as.factor(with(ComparativeSurvey, paste(GEAR,ORDER)))

m1=glm(LobDen ~ FACTOR,data=ComparativeSurvey,family=Gamma(link = "log"))

	save(ComparativeSurvey,file="ComparativeSurvey.rdata")	
	load(file="ComparativeSurvey.rdata")



	CarapaceLengthFrequencies(LFAs= '34', DS='LobsterSurvey',Yrs = c(2013, 2015, 2016),vers=2,rootdir=figdir)

	CarapaceLengthFrequencies(LFAs='34',DS='LobsterSurvey', Yrs=2016, gear.type='280 BALLOON', index.stations = F,pdf=F ,rel=F)
	CarapaceLengthFrequencies(LFAs='34',DS='LobsterSurvey', Yrs=2016, gear.type='NEST', index.stations = F,pdf=F )


surveyLobsters34<-LobsterSurveyProcess(lfa="34",yrs=1996:2017,mths=c("Aug","Jul","Jun"),bin.size=5,gear.type='NEST')






CarapaceLengthFrequencies(LFAs='34',DS='LobsterSurvey', Yrs=2016, gear.type='NEST', index.stations = F,graphic="png" ,rel=F,wd=8,ht=5,ymax=1600,fn="NEST",comparative=T)
CarapaceLengthFrequencies(LFAs='34',DS='LobsterSurvey', Yrs=2016, gear.type='280 BALLOON', index.stations = F,graphic="png" ,rel=F,wd=8,ht=5,ymax=1600,fn="BALLOON")



surveyLobsters34otter<-LobsterSurveyProcess(lfa="34",yrs=1996:2016,mths=c("Aug","Jul","Jun"),bin.size=5,gear.type='280 BALLOON')
surveyLobsters34nest<-LobsterSurveyProcess(lfa="34",yrs=2016:2017,mths=c("Aug","Jul","Jun"),bin.size=5,gear.type='NEST')

pdf(file.path(project.datadirectory('bio.lobster'),'figures','LobsterSurveyComparative2016.pdf'), width = 8, height = 11)
LobsterMap(ylim=c(42.65,44.95), xlim=c(-67.5,-65.2),mapRes="UR",title="",isobath=seq(50,500,50),bathcol=rgb(0,0,1,0.2),bathy.source='bathy')

ComparativeStations = subset(surveyLobsters34otter,YEAR==2016)$STATION

 points(SET_LAT~SET_LONG,subset(surveyLobsters34nest,YEAR==2016&!STATION%in%ComparativeStations),col='blue',pch=21,cex=1,bg='blue')
	#with(subset(surveyLobsters34nest,YEAR==2016),segments(SET_LONG, SET_LAT, HAUL_LONG, HAUL_LAT,col='red',lwd=2))
 points(SET_LAT~SET_LONG,subset(surveyLobsters34otter,YEAR==2016),col='blue',pch=21,cex=1,bg='red')
	#with(subset(surveyLobsters34otter,YEAR==2016),segments(SET_LONG, SET_LAT, HAUL_LONG, HAUL_LAT,col='blue',lwd=2))

dev.off()

