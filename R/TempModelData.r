#' @export
TempModelData = function(redo=F, save=T){
if(redo){
	p = bio.lobster::load.environment()

	##################### Temperature Data from FSRS traps

	
	p = spatial_parameters( type = "canada.east" ) 
  TempData = lobster.db('temperature.data')
	TempData = lonlat2planar(TempData, input_names=c("LON_DD", "LAT_DD"),proj.type = p$internal.projection)

	Complete = lobster.db('bathymetry')
	
	# identify locations of data relative to baseline for envionmental data
	locsmap = match( 
	array_map( "xy->1", TempData[,c("plon","plat")], gridparams=p$gridparams ), 
	array_map( "xy->1", Complete[,c("plon","plat")], gridparams=p$gridparams ) )
	
	
	TempData$z = Complete$z[locsmap]
	TempData$DEPTH = TempData$DEPTH_M
	TempData$DEPTH[is.na(TempData$DEPTH)] = TempData$z[is.na(TempData$DEPTH)]
	TempData$DEPTH[TempData$DEPTH<1] = 1
	
	TempData = subset(TempData, !is.na(LON_DD) | !is.na(LAT_DD))
	
	TempData = assignArea(TempData, coords = c('LON_DD','LAT_DD'))
	TempData = assignSubArea2733(TempData)
	TempData$DATE = TempData$T_DATE
	TempData$TEMPERATURE = TempData$TEMP
	
	TempData = TempData[,c('LFA','LFA_GRID','EID','DATE','X','Y','TEMPERATURE','DEPTH','plon','plat','z','subarea')]
 	TempData = subset(TempData,DATE<Sys.time())
	if(save)write.csv(TempData,file.path( project.datadirectory("bio.lobster"),"Temperature Data","TempData.csv"),row.names=F)
} else {
  TempData = read.csv(file.path( project.datadirectory("bio.lobster"),"Temperature Data","TempData.csv"))

	return(TempData)
}
}