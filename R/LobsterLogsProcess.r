#' LobsterLogsProcess
#' Processing lobster logbook data to get to clean data in a useable form
#' @param bumpup is for using the slip data to bumpup logs as a percentage basis. This defaults to false.
#' @return returns an R data object and saves the data to a .csv
#' @note Make sure you run the lobster.db('logs.redo') to get the up to date data. Need to check fishing season dates as a appropriate filter.
#' @seealso lobster.db('logs.redo')
#' @author Brad Hubley & Adam Cook
#' @export
LobsterLogsProcess<-function(bumpup=F){

	options(stringsAsFactors=F)
	#loadfunctions("lobster")
	require(lubridate)



	#Filtering by date trap and weight:
	Fish.Date <- read.csv(file.path( project.datadirectory('bio.lobster'), "data","inputs","FishingSeasonDates.csv"))
	Fish.Date2 = lobster.db('season.dates')


	lfa <- unique(Fish.Date$LFA)
	max_trap<-c(825,750,750,750,750,750,750,750,1126,1126,1126,1226)
	max_lbs<-c(2750,2750,2750,2750,2750,2750,2750,10000,30000,30000,30000,30000)
	Fish.Date$START_DATE<-as.Date(Fish.Date$START_DATE,format = "%Y-%m-%d")
	Fish.Date$END_DATE<-as.Date(Fish.Date$END_DATE,format = "%Y-%m-%d")
	Fish.Date2$START_DATE<-as.Date(Fish.Date2$START_DATE,format = "%Y-%m-%d")
	Fish.Date2$END_DATE<-as.Date(Fish.Date2$END_DATE,format = "%Y-%m-%d")


	# import logs from marfis
	lobster.db('logs')

	logs$TOTAL_NUM_TRAPS<-rowSums(logs[c('NUM_OF_TRAPS','NUM_OF_TRAPS_B','NUM_OF_TRAPS_C')],na.rm=T)
	logs$TOTAL_WEIGHT_LBS<-rowSums(logs[c('WEIGHT_LBS','WEIGHT_LBS_B','WEIGHT_LBS_C')],na.rm=T)
	logs$TOTAL_WEIGHT_KG<-logs$TOTAL_WEIGHT_LBS*0.4536

	# select for records within season
	logs$SYEAR<-NA
	logs$END_DATE <- NA
	logs$DATE_FISHED<-as.Date(logs$DATE_FISHED)
	for(i in 1:length(lfa)) {
		h <- Fish.Date[Fish.Date$LFA==lfa[i],]	
		for(j in 1:nrow(h)) {
			logs$SYEAR[logs$LFA==lfa[i]&logs$DATE_FISHED>=(h[j,'START_DATE'])&logs$DATE_FISHED<=(h[j,'END_DATE'])]<-h[j,'SYEAR']
			logs$END_DATE[logs$LFA==lfa[i]&logs$DATE_FISHED>=(h[j,'START_DATE']) & (year(logs$DATE_FISHED)==(h[j,'SYEAR']) & month(logs$DATE_FISHED)<11) | (logs$LFA==lfa[i]&logs$DATE_FISHED>=(h[j,'START_DATE']) & year(logs$DATE_FISHED)==(h[j,'SYEAR']-1) & month(logs$DATE_FISHED)>=11) ]<-h[j,'END_DATE']
		
		}

	}
	logs<-subset(logs,!is.na(SYEAR))
	#logs2<-subset(logs,!is.na(SYEAR))


	slips$SYEAR<-NA
	slips$END_DATE <- NA
	slips$DATE_FISHED<-as.Date(slips$DATE_LANDED)
	for(i in 1:length(lfa)) {
		h <- Fish.Date[Fish.Date$LFA==lfa[i],]	
		for(j in 1:nrow(h)) {
			slips$SYEAR[slips$LFA==lfa[i]&slips$DATE_FISHED>=(h[j,'START_DATE'])&slips$DATE_FISHED<=(h[j,'END_DATE'])]<-h[j,'SYEAR']
			slips$END_DATE[slips$LFA==lfa[i]&slips$DATE_FISHED>=(h[j,'START_DATE']) & (year(slips$DATE_FISHED)==(h[j,'SYEAR']) & month(slips$DATE_FISHED)<11) | (slips$LFA==lfa[i]&slips$DATE_FISHED>=(h[j,'START_DATE']) & year(slips$DATE_FISHED)==(h[j,'SYEAR']-1) & month(slips$DATE_FISHED)>=11) ]<-h[j,'END_DATE']
		
		}

	}
	
	#browser()

	# add week of season (WOS) variable
	logs$WOS<-NA
	for(i in 1:length(lfa)) {
		h <- Fish.Date[Fish.Date$LFA==lfa[i],]	
		for(j in unique(logs$SYEAR[logs$LFA==lfa[i]])){
			logs$WOS[logs$LFA==lfa[i]&logs$SYEAR==j]<-floor(as.numeric(logs$DATE_FISHED[logs$LFA==lfa[i]&logs$SYEAR==j]-min(h$START_DATE[h$SYEAR==j]))/7)+1
		}
	}

	# add quarter
	logs$quarter<-NA
	logs$quarter[month(logs$DATE_FISHED)%in%1:3]<-1
	logs$quarter[month(logs$DATE_FISHED)%in%4:6]<-2
	logs$quarter[month(logs$DATE_FISHED)%in%7:9]<-3
	logs$quarter[month(logs$DATE_FISHED)%in%10:12]<-4



	commonCols<-c("SUM_DOC_ID", "VR_NUMBER", "VESSEL_NAME", "SUBMITTER_NAME", "LICENCE_ID", "LFA", "COMMUNITY_CODE","SD_LOG_ID", "DATE_FISHED","SYEAR","WOS",'quarter',"TOTAL_NUM_TRAPS","TOTAL_WEIGHT_KG")

	logsInSeasonA<-subset(logs,!is.na(SYEAR)&!is.na(WEIGHT_LBS)&!is.na(NUM_OF_TRAPS),c(commonCols,"GRID_NUM", "WEIGHT_LBS", "NUM_OF_TRAPS"))
	logsInSeasonB<-subset(logs,!is.na(SYEAR)&!is.na(WEIGHT_LBS_B)&!is.na(NUM_OF_TRAPS_B),c(commonCols,"GRID_NUM_B", "WEIGHT_LBS_B", "NUM_OF_TRAPS_B"))
	logsInSeasonC<-subset(logs,!is.na(SYEAR)&!is.na(WEIGHT_LBS_C)&!is.na(NUM_OF_TRAPS_C),c(commonCols,"GRID_NUM_C", "WEIGHT_LBS_C", "NUM_OF_TRAPS_C"))
	names(logsInSeasonB)<-names(logsInSeasonA)
	names(logsInSeasonC)<-names(logsInSeasonA)
	logsInSeason<-rbind(logsInSeasonA,logsInSeasonB,logsInSeasonC)
	logsInSeason$WEIGHT_KG<-logsInSeason$WEIGHT_LBS*0.4536

	centgrid<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","lfa27_38_centgrid.csv"))
	grid.key<-with(centgrid,paste(LFA,GRID_NUM,sep='.'))
	logsInSeason<-subset(logsInSeason,!is.na(GRID_NUM)&paste(LFA,GRID_NUM,sep='.')%in%grid.key)
	logsInSeason$CPUE<-logsInSeason$WEIGHT_KG/logsInSeason$NUM_OF_TRAPS
	logsInSeason<-subset(logsInSeason,CPUE<20)

	subareas<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","inputs","LFA2733subarea.csv"))
	names(subareas)[2]<-"GRID_NUM"
	logsInSeason<-merge(logsInSeason,subareas,all.x=T)

	TotalLandings<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","products","TotalLandings.csv"))
		
	# add BUMPUP column: total landings/sum of logs for each year  & LFA
	if(bumpup){
		logsInSeason$BUMPUP<-NA
		lfa<-unique(TotalLandings$LFA)	
		for(i in 1:length(lfa)){
			tmplogs<-subset(logsInSeason,LFA==lfa[i])
			yrs<-sort(unique(tmplogs$SYEAR))
			for(y in 1:length(yrs)){
				logsInSeason$BUMPUP[logsInSeason$SYEAR==yrs[y]&logsInSeason$LFA==lfa[i]]<-TotalLandings$C[TotalLandings$SYEAR==yrs[y]&TotalLandings$LFA==lfa[i]]*1000/sum(tmplogs$WEIGHT_KG[tmplogs$SYEAR==yrs[y]],na.rm=T)
			}
		}
	}


	# Save logsInSeason as working data
	write.csv(logsInSeason,file.path( project.datadirectory('bio.lobster'), "data","products","logsInSeason.csv"),row.names=F)




	return(logsInSeason)

}
