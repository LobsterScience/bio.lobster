#' @export
TempModel = function(areas = 'subarea'){

	require(mgcv)

	# read in temperature data        
 	TempData = read.csv(file.path( project.datadirectory("bio.lobster"),"Temperature Data","TempData.csv"))
 	#TempData$subarea[TempData$subarea==311] = "31A"
 	#TempData$subarea[TempData$subarea==312] = "31B"
 	#write.csv(TempData,file.path( project.datadirectory("bio.lobster"),"Temperature Data","TempData.csv"),row.names=F)

	# create time and harmonic variables
	TempData$y = decimal_date(as.Date(TempData$DATE))
	TempData$cos.y = cos(2*pi*TempData$y)
	TempData$sin.y = sin(2*pi*TempData$y)
	
	if(areas == 'subarea')	TempData$area = as.factor(TempData$subarea)
	if(areas == 'lfa')		TempData$area = as.factor(TempData$LFA)

		# fit GAM model 
		TempModel <- gam(TEMPERATURE ~ sin.y + cos.y + s(y) + area + s(DEPTH, cos.y, sin.y), data=TempData)
	
		print(summary(TempModel))

	return(list(Model=TempModel,Data=TempData))
}