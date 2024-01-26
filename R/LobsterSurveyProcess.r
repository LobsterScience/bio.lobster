#' LobsterSurveyProcess
#' @param size.range defines the minimum and maximum value and is a filter (default is 0, 200mm CW)
#' @param lfa defines the specific LFA for the ILTS
#' @param yrs is the survey years to estimate
#' @param mnts months of the survey, defaults to the full year
#' @param bin.size aggregates the abundance into size bins (default is 5mm bins)
#' @param gear.type survey trawl net identification (if !NULL options are '280 BALLOON' or 'NEST')
#' @return data.frame of survey data called 'surveyLobsters'
#' @author Brad Hubley & Manon Cassista-Da Ros
#' @export


LobsterSurveyProcess=function(species = 2550, size.range=c(0,200),lfa='34',yrs,mths=c("May","Jun","Jul","Aug","Sep","Oct"),gear.type=NULL,sex=c(1:3,NA),bin.size=5,LFS=160,Net=NULL,comparative=F,biomass=F){
  
	lobster.db("survey")
	RLibrary("CircStats","PBSmapping","spatstat")
	if(missing(yrs))yrs=sort(unique(surveyCatch$YEAR))
	LFAs=read.csv(file.path(project.datadirectory('bio.lobster'),"data",'maps','Polygons_LFA.csv'))
	LFAs$LFA = paste("L",LFAs$LFA,sep="")
	  surveyCatch$EID=1:nrow(surveyCatch)
		k = which(is.na(surveyCatch$LFA))
	  SurvLocs=surveyCatch[k,c("SET_ID","SET_LONG","SET_LAT","LFA",'EID')]
		names(SurvLocs)[2:3]=c("X","Y")
		key=findPolys(SurvLocs,LFAs)
		SurvLFAs=merge(subset(SurvLocs,select=c("EID","SET_ID")),merge(key,subset(LFAs,!duplicated(PID),c("PID","LFA"))))
    if(dim(SurvLFAs)[1] != length(k)) stop('Some points outside polygon bounds')
		surveyCatch=bio.utilities::fillNaDf2(surveyCatch,SurvLFAs,'EID','LFA')
	# select for Lobsters
	setNames=c("SET_ID", "TRIP_ID", "TRIPCD_ID", "SURVEY_TYPE", "CFV", "VESSEL_NAME", "BOARD_DATE", "LANDING_DATE","HAULCCD_ID", "SET_NO","GEAR","FISHSET_ID",
	        "STATION", "STRATUM_ID", "SET_LAT", "SET_LONG", "SET_DEPTH", "SET_TIME", "SET_DATE", "HAUL_LAT", "HAUL_LONG", "HAUL_DEPTH", "HAUL_TIME", 
	        "HAUL_DATE", "YEAR", "LFA")           
#	browser()
	surveyLobsters=merge(subset(surveyCatch,SPECCD_ID==species),subset(surveyCatch,!duplicated(SET_ID),setNames),all=T) #includes zeros
	# number of lobsters with detailed data
	if(species == 2550){
		surveyMeasurements = subset(surveyMeasurements,SPECCD_ID ==2550) #added Sept 2021
		NLM = with(surveyMeasurements,tapply(SET_ID,SET_ID,length))
		MEAN_LENGTH = with(surveyMeasurements,tapply(FISH_LENGTH,SET_ID,mean,na.rm=T))

		#shell
		SOFT_SHELL =  with(subset(surveyMeasurements,SHELL %in% c(1,2,3,7)),tapply(SHELL,SET_ID,length))
		HARD_SHELL =  with(subset(surveyMeasurements,SHELL %in% 4:5),tapply(SHELL,SET_ID,length))
		shell = merge(data.frame(SET_ID=names(SOFT_SHELL),SOFT_SHELL=SOFT_SHELL),data.frame(SET_ID=names(HARD_SHELL),HARD_SHELL=HARD_SHELL),by='SET_ID',all=T)
		shell[is.na(shell)]=0
		shell$pSOFT = shell$SOFT_SHELL / (shell$SOFT_SHELL + shell$HARD_SHELL)
		surveyLobsters = merge(surveyLobsters,shell[,c("SET_ID","pSOFT")],by='SET_ID',all=T)

	}
	else {
		NLM = with(subset(fishMeasurements,SPECCD_ID%in%species),tapply(NUM_AT_LENGTH,paste0(TRIP_ID,SET_NO),sum))
		MEAN_LENGTH = with(subset(fishMeasurements,SPECCD_ID%in%species),tapply(FISH_LENGTH,paste0(TRIP_ID,SET_NO),mean,na.rm=T))
	}
	surveyLobsters = merge(merge(surveyLobsters,data.frame(SET_ID=names(NLM),NUM_MEASURED=NLM),by='SET_ID',all=T),data.frame(SET_ID=names(MEAN_LENGTH),MEAN_LENGTH=MEAN_LENGTH),by='SET_ID',all=T)
	surveyLobsters = subset(surveyLobsters,GEAR !='3/4 OTTER TRAWL') # Remove 2015 survey portion on Fundy Spray
	# add column for tow length
	lat1 = surveyLobsters$SET_LAT
	lat2 = surveyLobsters$HAUL_LAT
	lon1 = surveyLobsters$SET_LONG
	lon2 = surveyLobsters$HAUL_LONG
	surveyLobsters$LENGTH = 6371.3*acos(cos(rad(90-lat1))*cos(rad(90-lat2))+sin(rad(90-lat1))*sin(rad(90-lat2))*cos(rad(lon1-lon2))) #dist in km


	# Save list of tows with length outliers
	write.csv(subset(surveyLobsters,(LENGTH<0.67|LENGTH>3.5)&HAULCCD_ID==1),file.path(project.datadirectory('bio.lobster'),"data","longTows.csv"),row.names=F)

	# Add columns  NUM_STANDARDIZED and MONTH
	surveyLobsters$LENGTH[surveyLobsters$LENGTH<0.67|surveyLobsters$LENGTH>3.5]=NA #123 NA records created JAN_2017
	surveyLobsters$NUM_CAUGHT[is.na(surveyLobsters$NUM_CAUGHT)]=0 #2053 NA records of NUM_CAUGHT replaced with 0 JAN_2017
	surveyLobsters$MONTH = as.character(month(surveyLobsters$HAUL_DATE,T))
	
	#Use trawl gear wingspread specification
	#browser()
	x = as.data.frame(with(surveyLobsters,unique(cbind(YEAR,GEAR))))
	out = list()
	for(i in 1:nrow(x)){
	    g = subset(surveyLobsters,YEAR==x[i,1] & GEAR == x[i,2])
	    mL = mean(g$LENGTH,na.rm=T)
	    g$LENGTH[is.na(g$LENGTH)]=mL
	    g$GEAR_WIDTH = NA
	    if(x[i,2]=='280 BALLOON') g$GEAR_WIDTH = 20
	    if(x[i,2]=='NEST') g$GEAR_WIDTH = 13
	    out[[i]] = g
	 }
	
	
	surveyLobsters = do.call(rbind,out)
	
	# Net mensuration data available for 2016
	#surveyLobsters
	
	dist = calcAreaSwept()

	surveyLobsters = merge(surveyLobsters,dist,all=T)

	surveyLobsters$DIST_KM[is.na(surveyLobsters$DIST_KM)] = surveyLobsters$LENGTH[is.na(surveyLobsters$DIST_KM)]
	surveyLobsters$WING_SPREAD[is.na(surveyLobsters$WING_SPREAD)] = surveyLobsters$GEAR_WIDTH[is.na(surveyLobsters$WING_SPREAD)]
	surveyLobsters$AREA_SWEPT=surveyLobsters$DIST_KM*(surveyLobsters$WING_SPREAD/1000)
	surveyLobsters$SUBSAMPLE = surveyLobsters$NUM_MEASURED/surveyLobsters$NUM_CAUGHT
	#surveyLobsters$SUBSAMPLE[is.na(surveyLobsters$SUBSAMPLE)] = 1

	#temperature
	ILTSTemp$SET_ID = paste0(ILTSTemp$TRIP_ID,ILTSTemp$SET_NO)

#	x = aggregate(TEMPC ~ SET_ID, data = ILTSTemp, median,na.rm=T)
#	surveyLobsters = merge(surveyLobsters,x,all=T)
#browser()


	if(species == 2550){
		LongForm = aggregate(FISH_NO~floor(FISH_LENGTH)+SEX+SET_ID,data=subset(surveyMeasurements,SPECCD_ID==2550),FUN=length)
		names(LongForm) = c("FISH_LENGTH","SEX","SET_ID","NUM_AT_LENGTH")
		LongForm$BM_AT_LENGTH = LongForm$NUM_AT_LENGTH * lobLW(LongForm$FISH_LENGTH, sex= LongForm$SEX)/1000
		x = readRDS(file=file.path(project.datadirectory('bio.lobster'),'data',"survey","summarybootRhoNestBall.rds"))
		NetConv = with(x,data.frame(FISH_LENGTH=Length,NestCF=Median))
		LongForm = merge(LongForm,NetConv,all=T)
		LongForm$NestCF[LongForm$FISH_LENGTH<min(NetConv$FISH_LENGTH)] = NetConv$NestCF[NetConv$FISH_LENGTH==min(NetConv$FISH_LENGTH)]
		LongForm$NestCF[LongForm$FISH_LENGTH>max(NetConv$FISH_LENGTH)] = NetConv$NestCF[NetConv$FISH_LENGTH==max(NetConv$FISH_LENGTH)]
	} else {
		LongForm = with(subset(fishMeasurements,SPECCD_ID%in%species),data.frame(FISH_LENGTH,SEX=NA,SET_ID=paste0(TRIP_ID,SET_NO),NUM_AT_LENGTH))
		LongForm$NestCF = 1
	}
	LongForm = merge(surveyLobsters[,c("SET_ID","GEAR","AREA_SWEPT","SUBSAMPLE")],LongForm)
	LongForm$BalloonCF = 1/LongForm$NestCF
	LongForm$NestCF[LongForm$GEAR=="NEST"] = 1
	LongForm$BalloonCF[LongForm$GEAR=="280 BALLOON"] = 1
	LongForm$DENSITY = LongForm$NUM_AT_LENGTH/LongForm$AREA_SWEPT/LongForm$SUBSAMPLE
	if(biomass)LongForm$DENSITY = LongForm$BM_AT_LENGTH/LongForm$AREA_SWEPT/LongForm$SUBSAMPLE
	LongForm$NEST_DENSITY = LongForm$DENSITY * LongForm$NestCF
	LongForm$BALLOON_DENSITY = LongForm$DENSITY * LongForm$BalloonCF

	# add columns for length bins
	bins=seq(size.range[1],size.range[2],bin.size)
	LongForm$BIN = bins[cut(LongForm$FISH_LENGTH,bins,labels=FALSE)]
	#LongForm$BIN = ceiling(LongForm$FISH_LENGTH/bin.size) * bin.size
	assign("LongForm", LongForm, pos = 1)
	if(is.null(Net))CLF = aggregate(DENSITY~BIN+SET_ID,data=subset(LongForm,SEX%in%sex),FUN=sum)
	else {
		if(Net=="NEST")CLF = aggregate(NEST_DENSITY~BIN+SET_ID,data=subset(LongForm,SEX%in%sex),FUN=sum)
		if(Net=="280 BALLOON")CLF = aggregate(BALLOON_DENSITY~BIN+SET_ID,data=subset(LongForm,SEX%in%sex),FUN=sum)

	}
	names(CLF)[3] = "CL"
	sids=unique(CLF$SET_ID)
	CLF = subset(CLF,BIN%in%bins)
	CLF = merge(CLF,data.frame(SET_ID=rep(sids,length(bins)-1),BIN=sort(rep(bins[-1],length(sids)))),all=T)
	CLF = reshape(CLF[order(CLF$BIN),],idvar='SET_ID',timevar='BIN',direction='wide',sep='')
	CLF[is.na(CLF)] = 0
	
	surveyLobsters=merge(surveyLobsters,CLF,all.x=T)
	# subset by time and area
	surveyLobsters=subset(surveyLobsters,LFA%in%lfa&HAULCCD_ID==1&YEAR%in%yrs&MONTH%in%mths)
	# Calculate Densities
	surveyLobsters[surveyLobsters$NUM_CAUGHT==0,which(names(surveyLobsters)%in%names(CLF)[-1])] = 0
	surveyLobsters[surveyLobsters$NUM_CAUGHT==0,which(names(surveyLobsters)%in%names(CLF)[-1])] = 0
	
	surveyLobsters$LobDenCorrected=rowSums(surveyLobsters[,which(names(surveyLobsters)%in%names(CLF)[-1])])
	surveyLobsters$LobDenNotCorrected=surveyLobsters$NUM_CAUGHT/surveyLobsters$AREA_SWEPT
	surveyLobsters$LobDen=surveyLobsters$LobDenCorrected
	surveyLobsters$pLC=surveyLobsters$LobDenCorrected/surveyLobsters$LobDenNotCorrected
	
	x=aggregate(pLC~YEAR,surveyLobsters,median,na.rm=T)
	names(x)[2]='mpLC'
	ompLC = median(surveyLobsters$pLC,na.rm=T)
	surveyLobsters = merge(surveyLobsters,x,all=T)
	surveyLobsters$mpLC[is.na(surveyLobsters$mpLC)] = ompLC
	
	surveyLobsters$LobDen[is.na(surveyLobsters$LobDenCorrected)] = surveyLobsters$LobDenNotCorrected[is.na(surveyLobsters$LobDenCorrected)]*surveyLobsters$mpLC[is.na(surveyLobsters$LobDenCorrected)]
	surveyLobsters$NUM_STANDARDIZED=surveyLobsters$NUM_CAUGHT/surveyLobsters$DIST_KM
	if(comparative){
		comparativeStations = sort(subset(surveyLobsters,YEAR==2016&GEAR=="280 BALLOON")$STATION)
		surveyLobsters =surveyLobsters[which(!(surveyLobsters$YEAR==2016&!surveyLobsters$STATION%in%comparativeStations)),]
	}
	if(!is.null(Net)) surveyLobsters =surveyLobsters[which(!(surveyLobsters$YEAR==2016&surveyLobsters$GEAR!=Net)),]

	#browser()

	## berried females
	#with(subset(surveyMeasurements,SEX==3),tapply(SEX,SET_ID,length))->bfs
	#with(subset(surveyMeasurements,SEX==2),tapply(SEX,SET_ID,length))->fs
	#with(subset(surveyMeasurements,SEX%in%c(2,3)&FISH_LENGTH>LFS),tapply(SEX,SET_ID,length))->LargeFemales
	#with(subset(surveyMeasurements,SEX==1),tapply(SEX,SET_ID,length))->ms
	#with(subset(surveyMeasurements,SEX>0),tapply(SEX,SET_ID,length))->all
	#sets=merge(data.frame(SET_ID=names(all),all),merge(data.frame(SET_ID=names(ms),ms),merge(data.frame(SET_ID=names(fs),fs),merge(data.frame(SET_ID=names(bfs),bfs),data.frame(SET_ID=names(LargeFemales),LargeFemales),all=T),all=T),all=T),all=T)
	#sets$bfs[is.na(sets$bfs)]=0
	#sets$fs[is.na(sets$fs)]=0
	#sets$ms[is.na(sets$ms)]=0
	#sets$LargeFemales[is.na(sets$LargeFemales)]=0
	#surveyLobsters=merge(surveyLobsters,sets,all=T)
	#surveyLobsters$N_BERRIED_FEMALES=surveyLobsters$NUM_STANDARDIZED*(surveyLobsters$bfs/surveyLobsters$all)
	#surveyLobsters$N_FEMALES=surveyLobsters$NUM_STANDARDIZED*(surveyLobsters$fs/surveyLobsters$all)
	#surveyLobsters$N_MALES=surveyLobsters$NUM_STANDARDIZED*(surveyLobsters$ms/surveyLobsters$all)
	#surveyLobsters$N_LARGE_FEMALES=surveyLobsters$NUM_STANDARDIZED*(surveyLobsters$LargeFemales/surveyLobsters$all)

	write.csv(surveyLobsters,file.path(project.datadirectory('bio.lobster'),"data","products","surveyLobsters.csv"),row.names=F) # Save data as csv
	
	if(all(lfa=='L34')){

	#	 STATIONS assigned based on proximity
		ITQspat34=subset(surveyLobsters,select=c("SET_ID","SET_LONG","SET_LAT","HAUL_LONG","HAUL_LAT","STATION","GEAR"))
		names(ITQspat34)[2:5]=c("X1","Y1","X2","Y2")
		ITQspat34$EID=1:nrow(ITQspat34)
		pdf(file.path( project.datadirectory('bio.lobster'), "figures","LFA34ITQSurveyStations.pdf"),8,11)
		ITQspat34=ITQspat34[complete.cases(ITQspat34),]
		#browser()
		ITQspat34ns=assignStation(ITQspat34,lines=T,map='lfa34')
		dev.off()
		write.csv(ITQspat34ns$events,file.path(project.datadirectory('bio.lobster'),"data","products","surveyTows.csv"),row.names=F)
		write.csv(ITQspat34ns$stations,file.path(project.datadirectory('bio.lobster'),"data","products","surveyStations.csv"),row.names=F)

	#	 add assigned stations to data
		surveyLobsters=merge(surveyLobsters,subset(ITQspat34ns$events,select=c("SET_ID","SID")),all=T)
		}
	if(!is.null(gear.type)) {
			surveyLobsters = subset(surveyLobsters, GEAR==gear.type)
	}
#	surveyLobsters = subset(surveyLobsters, TRIP_ID !='100058328')
	print('REMOVING A MISCODED TRIP Sept 2022, NEED TO REMOVE ONCE FIXED IN ISDB')
	return(surveyLobsters)
}
