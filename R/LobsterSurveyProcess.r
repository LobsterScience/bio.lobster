#' LobsterSurveyProcess
#' @param size.range defines the minimum and maximum value and is a filter (default is 0, 220mm CW)
#' @param lfa defines the specific LFA for the ILTS
#' @param yrs is the survey years to estimate
#' @param mnts months of the survey, defaults to the full year
#' @param bin.size aggregates the abundance into size bins (default is 5mm bins)
#' @param gear.type survey trawl net identification (default is 280 BALLOON)
#' @return data.frame of survey data called 'surveyLobsters'
#' @author Brad Hubley & Manon Cassista-Da Ros
#' @export


LobsterSurveyProcess<-function(size.range=c(0,220),lfa='34',yrs,mths=c("May","Jun","Jul","Aug","Sep","Oct"),gear.type=c("280 BALLOON"),bin.size=5){
require('bio.lobster')
require('bio.utilities')
  
#browser()
	lobster.db("survey")
	RLibrary("CircStats","PBSmapping")
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
	surveyLobsters<-merge(subset(surveyCatch,SPECCD_ID==2550),subset(surveyCatch,!duplicated(SET_ID),setNames),all=T)
	# number of lobsters with detailed data
	NLM<-with(surveyMeasurements,tapply(SET_ID,SET_ID,length))
	surveyLobsters<-merge(surveyLobsters,data.frame(SET_ID=names(NLM),LOBSTERS_MEASURED=NLM),by='SET_ID',all=T)
	surveyLobsters <- subset(surveyLobsters,GEAR !='3/4 OTTER TRAWL') # Remove 2015 survey portion on Fundy Spray
	
	# add column for tow length
	lat1<-surveyLobsters$SET_LAT
	lat2<-surveyLobsters$HAUL_LAT
	lon1<-surveyLobsters$SET_LONG
	lon2<-surveyLobsters$HAUL_LONG
	surveyLobsters$LENGTH<-6371.3*acos(cos(rad(90-lat1))*cos(rad(90-lat2))+sin(rad(90-lat1))*sin(rad(90-lat2))*cos(rad(lon1-lon2)))

	# Save list of tows with length outliers
	write.csv(subset(surveyLobsters,(LENGTH<0.67|LENGTH>3.5)&HAULCCD_ID==1),file.path(project.datadirectory('bio.lobster'),"data","longTows.csv"),row.names=F)

	# Add columns  NUM_STANDARDIZED and MONTH
	surveyLobsters$LENGTH[surveyLobsters$LENGTH<0.67|surveyLobsters$LENGTH>3.5]<-NA #123 NA records created JAN_2017
	surveyLobsters$NUM_CAUGHT[is.na(surveyLobsters$NUM_CAUGHT)]<-0 #2053 NA records of NUM_CAUGHT replaced with 0 JAN_2017
	surveyLobsters$NUM_STANDARDIZED<-surveyLobsters$NUM_CAUGHT/surveyLobsters$LENGTH
	surveyLobsters$MONTH<-as.character(month(surveyLobsters$HAUL_DATE,T))
	
	#Use trawl gear wingspread specification
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
	surveyLobsters$AREA_SWEPT<-surveyLobsters$LENGTH*(surveyLobsters$GEAR_WIDTH/1000)
	
	surveyLobsters$LobDen<-surveyLobsters$NUM_CAUGHT/surveyLobsters$AREA_SWEPT
	surveyLobsters<-subset(surveyLobsters,!is.na(NUM_STANDARDIZED) & LFA==lfa & HAULCCD_ID==1 & YEAR %in% yrs & MONTH %in% mths)
	

	# add columns for length bins
	bins<-seq(size.range[1],size.range[2],bin.size)
	sets<-unique(surveyMeasurements$SET_ID)
	CLF<-data.frame(SET_ID=sets,t(sapply(sets,function(s){with(subset(surveyMeasurements,SET_ID==s&FISH_LENGTH>=min(bins)&FISH_LENGTH<max(bins)),hist(FISH_LENGTH,breaks=bins,plot=F)$count)})))
	names(CLF)[-1]<-paste0("CL",bins[-length(bins)])
	surveyLobsters<-merge(surveyLobsters,CLF,all=T)
	surveyLobsters[,which(names(surveyLobsters)%in%names(CLF)[-1])]<-sweep(surveyLobsters[,which(names(surveyLobsters)%in%names(CLF)[-1])],1,FUN="/", surveyLobsters$LENGTH)

	## berried females
	with(subset(surveyMeasurements,SEX==3),tapply(SEX,SET_ID,length))->bfs
	with(subset(surveyMeasurements,SEX==2),tapply(SEX,SET_ID,length))->fs
	with(subset(surveyMeasurements,SEX==1),tapply(SEX,SET_ID,length))->ms
	with(subset(surveyMeasurements,SEX>0),tapply(SEX,SET_ID,length))->all
	sets<-merge(data.frame(SET_ID=names(all),all),merge(data.frame(SET_ID=names(ms),ms),merge(data.frame(SET_ID=names(fs),fs),data.frame(SET_ID=names(bfs),bfs),all=T),all=T),all=T)
	sets$bfs[is.na(sets$bfs)]<-0
	sets$fs[is.na(sets$fs)]<-0
	sets$ms[is.na(sets$ms)]<-0
	surveyLobsters<-merge(surveyLobsters,sets,all=T)
	surveyLobsters$BERRIED_FEMALES<-surveyLobsters$NUM_STANDARDIZED*(surveyLobsters$bfs/surveyLobsters$all)
	surveyLobsters$FEMALES<-surveyLobsters$NUM_STANDARDIZED*(surveyLobsters$fs/surveyLobsters$all)
	surveyLobsters$MALES<-surveyLobsters$NUM_STANDARDIZED*(surveyLobsters$ms/surveyLobsters$all)

	write.csv(surveyLobsters,file.path(project.datadirectory('bio.lobster'),"data","products","surveyLobsters.csv"),row.names=F) # Save data as csv
	surveyLobsters<-subset(surveyLobsters,!is.na(NUM_STANDARDIZED)&LFA==lfa&HAULCCD_ID==1&YEAR%in%yrs&MONTH%in%mths)

	#if(lfa==34){

		# STATIONS assigned based on proximity
		#ITQspat34<-subset(surveyLobsters,select=c("SET_ID","SET_LONG","SET_LAT","HAUL_LONG","HAUL_LAT","STATION","GEAR"))
		#names(ITQspat34)[2:5]<-c("X1","Y1","X2","Y2")
		#ITQspat34$EID<-1:nrow(ITQspat34)
		#pdf(file.path( project.datadirectory('bio.lobster'), "figures","LFA34ITQSurveyStations.pdf"),8,11)
		#ITQspat34<-ITQspat34[complete.cases(ITQspat34),]
		#ITQspat34ns<-assignStation(ITQspat34,lines=T)
		#dev.off()
		#write.csv(ITQspat34ns$events,file.path(project.datadirectory('bio.lobster'),"data","products","surveyTows.csv"),row.names=F)
		#write.csv(ITQspat34ns$stations,file.path(project.datadirectory('bio.lobster'),"data","products","surveyStations.csv"),row.names=F)

		# add assigned stations to data
		#surveyLobsters<-merge(surveyLobsters,subset(ITQspat34ns$events,select=c("SET_ID","SID")),all=T)

	#}



	return(surveyLobsters)

}