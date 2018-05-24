#' LobsterSurveyProcess
#' @param size.range defines the minimum and maximum value and is a filter (default is 0, 220mm CW)
#' @param lfa defines the specific LFA for the ILTS
#' @param yrs is the survey years to estimate
#' @param mnts months of the survey, defaults to the full year
#' @param bin.size aggregates the abundance into size bins (default is 5mm bins)
#' @param gear.type survey trawl net identification (if !NULL options are '280 BALLOON' or 'NEST')
#' @return data.frame of survey data called 'surveyLobsters'
#' @author Brad Hubley & Manon Cassista-Da Ros
#' @export


LobsterSurveyProcess<-function(size.range=c(0,220),lfa='34',yrs,mths=c("May","Jun","Jul","Aug","Sep","Oct"),gear.type=NULL,sex=1:3,bin.size=5,LFS=160,Net=NULL){
  
	lobster.db("survey")
	RLibrary("CircStats","PBSmapping","SpatialHub","spatstat")
	if(missing(yrs))yrs<-sort(unique(surveyCatch$YEAR))

	# add column for LFA
	LFAs<-read.csv(file.path(project.datadirectory('bio.lobster'),"data",'maps','Polygons_LFA.csv'))
	SurvLocs<-subset(surveyCatch,select=c("SET_ID","SET_LONG","SET_LAT"))
	names(SurvLocs)[2:3]<-c("X","Y")
	SurvLocs$EID<-1:nrow(SurvLocs)
	key<-findPolys(SurvLocs,LFAs)
	SurvLFAs<-merge(subset(SurvLocs,select=c("EID","SET_ID")),merge(key,subset(LFAs,!duplicated(PID),c("PID","LFA"))))
	surveyCatch<-merge(surveyCatch,subset(SurvLFAs,!duplicated(SET_ID),c("SET_ID","LFA")),all=T)
	
	# select for Lobsters
	setNames<-c("SET_ID", "TRIP_ID", "TRIPCD_ID", "SURVEY_TYPE", "CFV", "VESSEL_NAME", "BOARD_DATE", "LANDING_DATE","HAULCCD_ID", "SET_NO","GEAR","FISHSET_ID",
	        "STATION", "STRATUM_ID", "SET_LAT", "SET_LONG", "SET_DEPTH", "SET_TIME", "SET_DATE", "HAUL_LAT", "HAUL_LONG", "HAUL_DEPTH", "HAUL_TIME", 
	        "HAUL_DATE", "YEAR", "LFA")           
	surveyLobsters<-merge(subset(surveyCatch,SPECCD_ID==2550),subset(surveyCatch,!duplicated(SET_ID),setNames),all=T) #includes zeros
	# number of lobsters with detailed data
	
	NLM<-with(surveyMeasurements,tapply(SET_ID,SET_ID,length))
	surveyLobsters<-merge(surveyLobsters,data.frame(SET_ID=names(NLM),LOBSTERS_MEASURED=NLM),by='SET_ID',all=T)
	surveyLobsters <- subset(surveyLobsters,GEAR !='3/4 OTTER TRAWL') # Remove 2015 survey portion on Fundy Spray
	
	# add column for tow length
	lat1<-surveyLobsters$SET_LAT
	lat2<-surveyLobsters$HAUL_LAT
	lon1<-surveyLobsters$SET_LONG
	lon2<-surveyLobsters$HAUL_LONG
	surveyLobsters$LENGTH<-6371.3*acos(cos(rad(90-lat1))*cos(rad(90-lat2))+sin(rad(90-lat1))*sin(rad(90-lat2))*cos(rad(lon1-lon2))) #dist in km


	# Save list of tows with length outliers
	write.csv(subset(surveyLobsters,(LENGTH<0.67|LENGTH>3.5)&HAULCCD_ID==1),file.path(project.datadirectory('bio.lobster'),"data","longTows.csv"),row.names=F)

	# Add columns  NUM_STANDARDIZED and MONTH
	surveyLobsters$LENGTH[surveyLobsters$LENGTH<0.67|surveyLobsters$LENGTH>3.5]<-NA #123 NA records created JAN_2017

	
	surveyLobsters$NUM_CAUGHT[is.na(surveyLobsters$NUM_CAUGHT)]<-0 #2053 NA records of NUM_CAUGHT replaced with 0 JAN_2017
	surveyLobsters$MONTH<-as.character(month(surveyLobsters$HAUL_DATE,T))
	
	#Use trawl gear wingspread specification
	#browser()
	x = as.data.frame(with(surveyLobsters,unique(cbind(YEAR,GEAR))))
	out <- list()
	for(i in 1:nrow(x)){
	    g = subset(surveyLobsters,YEAR==x[i,1] & GEAR == x[i,2])
	    mL = mean(g$LENGTH,na.rm=T)
	    g$LENGTH[is.na(g$LENGTH)]<-mL
	    g$GEAR_WIDTH <- NA
	    if(x[i,2]=='280 BALLOON') g$GEAR_WIDTH <- 20
	    if(x[i,2]=='NEST') g$GEAR_WIDTH <- 13
	    out[[i]] <- g
	 }
	
	
	surveyLobsters = do.call(rbind,out)

	# Net mensuration data available for 2016
	
	#surveyLobsters
	
	dist = calcAreaSwept()

	surveyLobsters = merge(surveyLobsters,dist,all=T)

	surveyLobsters$DIST_KM[is.na(surveyLobsters$DIST_KM)] = surveyLobsters$LENGTH[is.na(surveyLobsters$DIST_KM)]
	surveyLobsters$WING_SPREAD[is.na(surveyLobsters$WING_SPREAD)] = surveyLobsters$GEAR_WIDTH[is.na(surveyLobsters$WING_SPREAD)]
	surveyLobsters$AREA_SWEPT<-surveyLobsters$DIST_KM*(surveyLobsters$WING_SPREAD/1000)
	surveyLobsters$SUBSAMPLE <- surveyLobsters$LOBSTERS_MEASURED/surveyLobsters$NUM_CAUGHT
	#surveyLobsters$SUBSAMPLE[is.na(surveyLobsters$SUBSAMPLE)] = 1

	LongForm = aggregate(FISH_NO~floor(FISH_LENGTH)+SEX+SET_ID,data=surveyMeasurements,FUN=length)
	names(LongForm)[1] = "FISH_LENGTH"
	x = readRDS(file=file.path(project.datadirectory('bio.lobster'),"survey","summarybootRhoNestBall.rds"))
	NetConv = with(x,data.frame(FISH_LENGTH=Length,NestCF=Median))
	LongForm = merge(LongForm,NetConv,all=T)
	LongForm$NestCF[LongForm$FISH_LENGTH<min(NetConv$FISH_LENGTH)] <- NetConv$NestCF[NetConv$FISH_LENGTH==min(NetConv$FISH_LENGTH)]
	LongForm$NestCF[LongForm$FISH_LENGTH>max(NetConv$FISH_LENGTH)] <- NetConv$NestCF[NetConv$FISH_LENGTH==max(NetConv$FISH_LENGTH)]
	LongForm = merge(surveyLobsters[,c("SET_ID","GEAR","AREA_SWEPT","SUBSAMPLE")],LongForm)
	LongForm$BalloonCF = 1/LongForm$NestCF
	LongForm$NestCF[LongForm$GEAR=="NEST"] = 1
	LongForm$BalloonCF[LongForm$GEAR=="280 BALLOON"] = 1
	LongForm$DENSITY = LongForm$FISH_NO/LongForm$AREA_SWEPT/LongForm$SUBSAMPLE
	LongForm$NEST_DENSITY = LongForm$DENSITY * LongForm$NestCF
	LongForm$BALLOON_DENSITY = LongForm$DENSITY * LongForm$BalloonCF

	# add columns for length bins
	bins<-seq(size.range[1],size.range[2],bin.size)
	LongForm$BIN = ceiling(LongForm$FISH_LENGTH/bin.size) * bin.size
	if(is.null(Net))CLF = aggregate(DENSITY~BIN+SET_ID,data=subset(LongForm,SEX%in%sex),FUN=sum)
	else {
		if(Net=="NEST")CLF = aggregate(NEST_DENSITY~BIN+SET_ID,data=subset(LongForm,SEX%in%sex),FUN=sum)
		if(Net=="280 BALLOON")CLF = aggregate(BALLOON_DENSITY~BIN+SET_ID,data=subset(LongForm,SEX%in%sex),FUN=sum)
	}
	names(CLF)[3] = "CL"
	CLF = merge(CLF,data.frame(SET_ID=CLF$SET_ID[1],BIN=bins[-1]),all=T)
	CLF = reshape(CLF[order(CLF$BIN),],idvar='SET_ID',timevar='BIN',direction='wide',sep='')
	CLF[is.na(CLF)] = 0
	surveyLobsters<-merge(surveyLobsters,CLF,all=T)
	surveyLobsters[surveyLobsters$NUM_CAUGHT==0,which(names(surveyLobsters)%in%names(CLF)[-1])] <- 0
	surveyLobsters$LobDenNC<-rowSums(surveyLobsters[,which(names(surveyLobsters)%in%names(CLF)[-1])])
	surveyLobsters$LobDen<-surveyLobsters$NUM_CAUGHT/surveyLobsters$AREA_SWEPT

	## berried females
	#with(subset(surveyMeasurements,SEX==3),tapply(SEX,SET_ID,length))->bfs
	#with(subset(surveyMeasurements,SEX==2),tapply(SEX,SET_ID,length))->fs
	#with(subset(surveyMeasurements,SEX%in%c(2,3)&FISH_LENGTH>LFS),tapply(SEX,SET_ID,length))->LargeFemales
	#with(subset(surveyMeasurements,SEX==1),tapply(SEX,SET_ID,length))->ms
	#with(subset(surveyMeasurements,SEX>0),tapply(SEX,SET_ID,length))->all
	#sets<-merge(data.frame(SET_ID=names(all),all),merge(data.frame(SET_ID=names(ms),ms),merge(data.frame(SET_ID=names(fs),fs),merge(data.frame(SET_ID=names(bfs),bfs),data.frame(SET_ID=names(LargeFemales),LargeFemales),all=T),all=T),all=T),all=T)
	#sets$bfs[is.na(sets$bfs)]<-0
	#sets$fs[is.na(sets$fs)]<-0
	#sets$ms[is.na(sets$ms)]<-0
	#sets$LargeFemales[is.na(sets$LargeFemales)]<-0
	#surveyLobsters<-merge(surveyLobsters,sets,all=T)
	#surveyLobsters$N_BERRIED_FEMALES<-surveyLobsters$NUM_STANDARDIZED*(surveyLobsters$bfs/surveyLobsters$all)
	#surveyLobsters$N_FEMALES<-surveyLobsters$NUM_STANDARDIZED*(surveyLobsters$fs/surveyLobsters$all)
	#surveyLobsters$N_MALES<-surveyLobsters$NUM_STANDARDIZED*(surveyLobsters$ms/surveyLobsters$all)
	#surveyLobsters$N_LARGE_FEMALES<-surveyLobsters$NUM_STANDARDIZED*(surveyLobsters$LargeFemales/surveyLobsters$all)

	write.csv(surveyLobsters,file.path(project.datadirectory('bio.lobster'),"data","products","surveyLobsters.csv"),row.names=F) # Save data as csv
	surveyLobsters<-subset(surveyLobsters,LFA==lfa&HAULCCD_ID==1&YEAR%in%yrs&MONTH%in%mths)
	
	if(!is.null(gear.type)) {
			surveyLobsters = subset(surveyLobsters, GEAR==gear.type)
	}
	if(lfa==34){

	#	 STATIONS assigned based on proximity
		ITQspat34<-subset(surveyLobsters,select=c("SET_ID","SET_LONG","SET_LAT","HAUL_LONG","HAUL_LAT","STATION","GEAR"))
		names(ITQspat34)[2:5]<-c("X1","Y1","X2","Y2")
		ITQspat34$EID<-1:nrow(ITQspat34)
		pdf(file.path( project.datadirectory('bio.lobster'), "figures","LFA34ITQSurveyStations.pdf"),8,11)
		ITQspat34<-ITQspat34[complete.cases(ITQspat34),]
		ITQspat34ns<-assignStation(ITQspat34,lines=T,map=T)
		dev.off()
		write.csv(ITQspat34ns$events,file.path(project.datadirectory('bio.lobster'),"data","products","surveyTows.csv"),row.names=F)
		write.csv(ITQspat34ns$stations,file.path(project.datadirectory('bio.lobster'),"data","products","surveyStations.csv"),row.names=F)

	#	 add assigned stations to data
		surveyLobsters<-merge(surveyLobsters,subset(ITQspat34ns$events,select=c("SET_ID","SID")),all=T)
		}
	
	return(surveyLobsters)
}