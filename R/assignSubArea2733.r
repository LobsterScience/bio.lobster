#' @export
assignSubArea2733 = function(Data){
	
	#Data = Data[,-which(names(Data)=="subarea")]
	if(any(names(Data) %in% c('GRID'))) Data <- rename.df(Data,n0=c('GRID'),n1=c('LFA_GRID'))

	subareas = read.csv(file.path( project.datadirectory("bio.lobster"), "data","inputs","LFA2733subarea.csv"))
	Data = merge(Data,subareas,all.x=T)

	Data$subarea[is.na(Data$subarea)] <- as.character(Data$LFA[is.na(Data$subarea)])
	Data$subarea[Data$subarea == "27 North"] <- "27N"
	Data$subarea[Data$subarea == "27 South"] <- "27S"
	Data$subarea[Data$subarea == "33 East"] <- "33E"
	Data$subarea[Data$subarea == "33 West"] <- "33W"
	Data$subarea[Data$subarea == "31.1"] <- "31A"
	Data$subarea[Data$subarea == "31.2"] <- "31B"

	return(Data)

}

