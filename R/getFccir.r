#' @export
getFccir=function(p){
	
	ccir = load(file.path(project.datadirectory("bio.lobster"),"data","exploitationccir.rdata"))

	ccir$LFA[ccir$LFA == "LFA 27 South"] = "27S"
	ccir$LFA[ccir$LFA == "LFA 27 North"] = "27N"
	ccir$LFA[ccir$LFA == "LFA 29"] = "29"
	ccir$LFA[ccir$LFA == "LFA 30"] = "30"
	ccir$LFA[ccir$LFA == "LFA 31A"] = "31A"
	ccir$LFA[ccir$LFA == "LFA 31B"] = "31B"
	ccir$LFA[ccir$LFA == "LFA 32"] = "32"
	ccir$LFA[ccir$LFA == "LFA 33 East"] = "33E"
	ccir$LFA[ccir$LFA == "LFA 33 West"] = "33W"
	
	e = median(subset(o,LFA==p$Area)$ERfm)

	t = as.numeric(as.Date(p$season[2])-as.Date(p$season[1]))/365

	F = -log(1-e)/t

	return(F)

}