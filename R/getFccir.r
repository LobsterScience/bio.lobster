#' @export
getFccir=function(p){
	
	load(file.path(project.datadirectory("bio.lobster"),"data","exploitationccir.rdata"))
	load(file.path(project.datadirectory('bio.lobster'),'outputs','ccir','summary','compiledBinomialModels33.rdata'))
	r33=ouBin[,c("LFA","Yr","ERfm")]
	load(file.path(project.datadirectory('bio.lobster'),'outputs','ccir','summary','compiledBinomialModels34.rdata'))
	r34=ouBin[,c("LFA","Yr","ERfm")]
	load(file.path(project.datadirectory('bio.lobster'),'outputs','ccir','summary','compiledBinomialModels2732.rdata'))
	r2732=ouBin[,c("LFA","Yr","ERfm")]

	
	ccir = rbind(r2732,r33,r34)
	
	ccir$LFA[ccir$LFA == "LFA 27 South"] = "27S"
	ccir$LFA[ccir$LFA == "LFA 27 North"] = "27N"
	ccir$LFA[ccir$LFA == "LFA 29"]       = "29"
	ccir$LFA[ccir$LFA == "LFA 30"]       = "30"
	ccir$LFA[ccir$LFA == "LFA 31A"]      = "31A"
	ccir$LFA[ccir$LFA == "LFA 31B"]      = "31B"
	ccir$LFA[ccir$LFA == "LFA 32"]       = "32"
	ccir$LFA[ccir$LFA == "LFA 33 East"]  = "33E"
	ccir$LFA[ccir$LFA == "LFA 33 West"]  = "33W"

	if(p$Area == "27N" || p$Area == "27S")  p$season = c("2000-05-16","2000-07-15") # 27
	if(p$Area == "29")                      p$season = c("2000-05-01","2000-06-30") # 29
	if(p$Area == "30")                      p$season = c("2000-05-20","2000-07-20") # 30
	if(p$Area == "31A")                     p$season = c("2000-04-30","2000-06-30") # 31A
	if(p$Area == "31B" || p$Area == "32")   p$season = c("2000-04-20","2000-06-20") # 31B & 32
	if(p$Area == "33E" || p$Area == "33W")  p$season = c("1999-11-28","2000-05-31") # 33
	if(p$Area == "34")                      p$season = c("1999-11-28","2000-05-31") # 34
	if(p$Area == "35")                      p$season = c("1999-10-15","1999-12-31","2000-03-01","2000-07-31") # 35
	if(p$Area == "36")                      p$season = c("1999-11-12","2000-01-15","2000-04-01","2000-06-29") # 36
	if(p$Area == "38")                      p$season = c("1999-11-12","2000-06-25") # 38


	
	e = mean(subset(ccir,LFA==p$Area)$ERfm)

	t = as.numeric(as.Date(p$season[2])-as.Date(p$season[1]))/365

	F = -log(1-e)/t

	return(F)

}