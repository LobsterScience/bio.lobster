TempModel = function(areas = c("27N","27S","28","29","30","31A","31B","32","33E","33W")){

	require(mgcv)

	# read in temperature data        
 	TempData = read.csv(file.path( project.datadirectory("bio.lobster"),"Temperature Data","TempData.csv"))

	# create time and harmonic variables
	TempData$y = decimal_date(as.Date(TempData$DATE))
	TempData$cos.y = cos(2*pi*TempData$y)
	TempData$sin.y = sin(2*pi*TempData$y)
	TempData$subarea = as.factor(TempData$subarea)

	# fit GAM model 
	TempModel <- gam(TEMPERATURE ~ sin.y + cos.y + s(y) + subarea + s(DEPTH, cos.y, sin.y), data=TempData)
	print(summary(TempModel))

	return(list(Model=TempModel,Data=TempData))
}