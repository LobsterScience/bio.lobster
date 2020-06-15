setwd('/SpinDr/backup/bio_data/bio.lobster/analysis/LFA34-38/indicators/')
a = read.csv('LFA34DFO.restratified.All.csv')
aa = read.csv('LFA35DFO.restratified.All.csv')
aaa = read.csv('LFA36DFO.restratified.All.csv')
surveyLobsters = read.csv('LFA38DFO.restratified.All.csv')
surveyLobstersa = read.csv('LFA35-38DFO.restratified.All.csv')

a$group = ifelse(a$YEAR %in% 1970:1980,1,
			ifelse(a$YEAR %in% 1981:1990,2,
			ifelse(a$YEAR %in% 1991:1998,3,
			ifelse(a$YEAR %in% 1999:2009,4,5))))

			aggregate(cbind(Nsets,NsetswithLobster,ObsLobs)~group,data=a,FUN=sum)



aa$group = ifelse(aa$YEAR %in% 1970:1980,1,
			ifelse(aa$YEAR %in% 1981:1990,2,
			ifelse(aa$YEAR %in% 1991:1998,3,
			ifelse(aa$YEAR %in% 1999:2009,4,5))))

			aggregate(cbind(Nsets,NsetswithLobster,ObsLobs)~group,data=aa,FUN=sum)



aaa$group = ifelse(aaa$YEAR %in% 1970:1980,1,
			ifelse(aaa$YEAR %in% 1981:1990,2,
			ifelse(aaa$YEAR %in% 1991:1998,3,
			ifelse(aaa$YEAR %in% 1999:2009,4,5))))

			aggregate(cbind(Nsets,NsetswithLobster,ObsLobs)~group,data=aaa,FUN=sum)



surveyLobsters$group = ifelse(surveyLobsters$YEAR %in% 1970:1980,1,
			ifelse(surveyLobsters$YEAR %in% 1981:1990,2,
			ifelse(surveyLobsters$YEAR %in% 1991:1998,3,
			ifelse(surveyLobsters$YEAR %in% 1999:2009,4,5))))

			aggregate(cbind(Nsets,NsetswithLobster,ObsLobs)~group,data=surveyLobsters,FUN=sum)


#nefsc spring
a =      read.csv('LFA34NEFSC.Spring.Restratified.csv')
surveyLobsters =   read.csv('LFA38NEFSC.Spring.Restratified.csv')

a$group = ifelse(a$YEAR %in% 1970:1980,1,
			ifelse(a$YEAR %in% 1981:1990,2,
			ifelse(a$YEAR %in% 1991:1998,3,
			ifelse(a$YEAR %in% 1999:2009,4,5))))

			aggregate(cbind(Nsets,NsetswithLobster,ObsLobs)~group,data=a,FUN=sum)



surveyLobsters$group = ifelse(surveyLobsters$YEAR %in% 1970:1980,1,
			ifelse(surveyLobsters$YEAR %in% 1981:1990,2,
			ifelse(surveyLobsters$YEAR %in% 1991:1998,3,
			ifelse(surveyLobsters$YEAR %in% 1999:2009,4,5))))

			aggregate(cbind(Nsets,NsetswithLobster,ObsLobs)~group,data=surveyLobsters,FUN=sum)


#nefsc fall
a =      read.csv('LFA34NEFSC.Fall.Restratified.csv')
surveyLobsters =   read.csv('LFA38NEFSC.Fall.Restratified.csv')

a$group = ifelse(a$YEAR %in% 1970:1980,1,
			ifelse(a$YEAR %in% 1981:1990,2,
			ifelse(a$YEAR %in% 1991:1998,3,
			ifelse(a$YEAR %in% 1999:2009,4,5))))

			aggregate(cbind(Nsets,NsetswithLobster,ObsLobs)~group,data=a,FUN=sum)



surveyLobsters$group = ifelse(surveyLobsters$YEAR %in% 1970:1980,1,
			ifelse(surveyLobsters$YEAR %in% 1981:1990,2,
			ifelse(surveyLobsters$YEAR %in% 1991:1998,3,
			ifelse(surveyLobsters$YEAR %in% 1999:2009,4,5))))

			aggregate(cbind(Nsets,NsetswithLobster,ObsLobs)~group,data=surveyLobsters,FUN=sum)

#ILTS

			lobster.db("survey")
	RLibrary("CircStats","PBSmapping","SpatialHub","spatstat")
	if(missing(YEARs))YEARs=sort(unique(surveyCatch$YEAR))

	# add column for LFA
	LFAs=read.csv(file.path(project.datadirectory('bio.lobster'),"data",'maps','Polygons_LFA.csv'))
	SurvLocs=subset(surveyCatch,select=c("SET_ID","SET_LONG","SET_LAT"))
	names(SurvLocs)[2:3]=c("X","Y")
	SurvLocs$EID=1:nrow(SurvLocs)
	key=findPolys(SurvLocs,LFAs)
	SurvLFAs=merge(subset(SurvLocs,select=c("EID","SET_ID")),merge(key,subset(LFAs,!duplicated(PID),c("PID","LFA"))))
	surveyCatch=merge(surveyCatch,subset(SurvLFAs,!duplicated(SET_ID),c("SET_ID","LFA")),all=T)
	
	# select for Lobsters
	setNames=c("SET_ID", "TRIP_ID", "TRIPCD_ID", "SURVEY_TYPE", "CFV", "VESSEL_NAME", "BOARD_DATE", "LANDING_DATE","HAULCCD_ID", "SET_NO","GEAR","FISHSET_ID",
	        "STATION", "STRATUM_ID", "SET_LAT", "SET_LONG", "SET_DEPTH", "SET_TIME", "SET_DATE", "HAUL_LAT", "HAUL_LONG", "HAUL_DEPTH", "HAUL_TIME", 
	        "HAUL_DATE", "YEAR", "LFA")           
	surveyLobsters=merge(subset(surveyCatch,SPECCD_ID==species),subset(surveyCatch,!duplicated(SET_ID),setNames),all=T) #includes zeros

surveyLobsters$group = ifelse(surveyLobsters$YEAR %in% 1970:1980,1,
			ifelse(surveyLobsters$YEAR %in% 1981:1990,2,
			ifelse(surveyLobsters$YEAR %in% 1991:1998,3,
			ifelse(surveyLobsters$YEAR %in% 1999:2009,4,5))))


aggregate(SET_ID~group,data=subset(surveyLobsters,LFA==34),FUN=length)