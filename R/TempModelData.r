#' @export
TempModelData = function(){

	p = bio.lobster::load.environment()

	library("bio.bathymetry")
	library("bio.spacetime")
	library("bio.temperature")

	##################### Temperature Data from FSRS traps

	lobster.db('fsrs')
	fsrs$SYEAR<-fsrs$HAUL_YEAR
	fsrs$HAUL_DATE<-as.Date(fsrs$HAUL_DATE)
	fsrs$SYEAR[fsrs$LFA%in%c("33","34")]<-as.numeric(substr(fsrs$S_LABEL[fsrs$LFA%in%c("33","34")],6,9))

	fsrsT = subset(fsrs,TEMP>-90)
	fsrsT$Dloc = paste(fsrsT$HAUL_DATE,fsrsT$LATITUDE,fsrsT$LONGITUDE)

	fsrsT = subset(fsrsT,!duplicated(Dloc))

	LobsterTemp = fsrsT[,c("HAUL_DATE","LONG_DD","LAT_DD","TEMP","DEPTH")]

	# Temperature Data from scallop surveys
	InshoreScallop = read.csv(file.path(project.datadirectory('bio.lobster'),'Temperature Data','InshoreScallopTemp.csv'))
	InshoreScallop$DATE = as.Date(InshoreScallop$DATE,"%d/%m/%Y")
	InshoreScallop$LONGITUDE = convert.dd.dddd(InshoreScallop$LONGITUDE)
	InshoreScallop$LATITUDE = convert.dd.dddd(InshoreScallop$LATITUDE)
	OffshoreScallop = read.csv(file.path(project.datadirectory('bio.lobster'),'Temperature Data','OffshoreScallopTemp.csv'))
	OffshoreScallop$DATE = as.Date(OffshoreScallop$DATE,"%d/%m/%Y")
	OffshoreScallop$LONGITUDE = convert.dd.dddd(OffshoreScallop$LONGITUDE)
	OffshoreScallop$LATITUDE = convert.dd.dddd(OffshoreScallop$LATITUDE)
	ScallopTemp = rbind(InshoreScallop,OffshoreScallop)

	TempData = rbind(
		with(ScallopTemp,data.frame(DATE=DATE,LONGITUDE=LONGITUDE,LATITUDE=LATITUDE,TEMPERATURE=TEMPERATURE,DEPTH=NA)),
		with(LobsterTemp,data.frame(DATE=HAUL_DATE,LONGITUDE=LONG_DD,LATITUDE=LAT_DD,TEMPERATURE=TEMP,DEPTH=DEPTH)))

	p = spatial_parameters( type = "canada.east" ) 

	TempData = lonlat2planar(TempData, input_names=c("LONGITUDE", "LATITUDE"),proj.type = p$internal.projection)

	Complete = bathymetry.db(p=p, DS="complete")
	
	# identify locations of data relative to baseline for envionmental data
	locsmap = match( 
	lbm::array_map( "xy->1", TempData[,c("plon","plat")], gridparams=p$gridparams ), 
	lbm::array_map( "xy->1", Complete[,c("plon","plat")], gridparams=p$gridparams ) )
	
	
	TempData$z = Complete$z[locsmap]
	TempData$DEPTH = TempData$DEPTH * 1.8288
	TempData$DEPTH[is.na(TempData$DEPTH)] = TempData$z[is.na(TempData$DEPTH)]
	TempData$DEPTH[TempData$DEPTH<1] = 1
	
	TempData = assignArea(TempData)
	TempData = assignSubArea2733(TempData)

 	# Jae's temp data
 	B = hydro.db( p=p, DS="bottom.all" ) 

 	moreTempData = subset(B,date>'1977-01-01')
	moreTempData = assignArea(moreTempData)
	moreTempData = assignSubArea2733(moreTempData)
	moreTempData$DEPTH = moreTempData$z
	moreTempData = rename.df(moreTempData,n0=c('t','date'),n1=c('TEMPERATURE',"DATE"))

 	TempData = rbind(TempData,moreTempData[,c("LFA", "LFA_GRID", "EID", "DATE", "X", "Y", "TEMPERATURE", "DEPTH", "plon", "plat", "z",  "subarea")])

 	TempData = subset(TempData,DATE<Sys.time())
	
	write.csv(TempData,file.path( project.datadirectory("bio.lobster"),"Temperature Data","TempData.csv"),row.names=F)



	return(TempData)
}