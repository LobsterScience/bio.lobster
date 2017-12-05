#' @export
getSimList = function(p,sex=1, LS=82.5, Fadj = 1, Sadj = 1, Sclose='end', window=NULL){

	plist = list()

	if(!is.null(window))Fadj = Fadj * getWindow(LS=LS,window=window,lens)


	for(i in 1:length(p$lfas)){
	
		p$sex = sex					# run sexes seperately
		p$LS = LS					# legal size (mm)
		
		p$Area = p$lfas[i]
		p$Depth = 15
	
		p$StartPop = 1000
		p$startDate = as.Date("1999-12-01")
	
		p$nt = 60					# number of timesteps
		p$lens = seq(50,200,5)		# carapace length bins (mm)
		p$timestep = 91  			# in days
	
		#reproduction
		p$gestation = 320
		p$brood = 360
	
		#season timing
		if(p$Area == "27N" || p$Area == "27S" || p$Area == "27")  p$season = c("2000-05-16","2000-07-15") # 27
		if(p$Area == "29")                                        p$season = c("2000-05-01","2000-06-30") # 29
		if(p$Area == "30")                                        p$season = c("2000-05-20","2000-07-20") # 30
		if(p$Area == "31A")                                       p$season = c("2000-04-30","2000-06-30") # 31A
		if(p$Area == "31B" || p$Area == "32")                     p$season = c("2000-04-20","2000-06-20") # 31B & 32
		if(p$Area == "33E" || p$Area == "33W" || p$Area == "33")  p$season = c("1999-11-28","2000-05-31") # 33

		#season adjustment
		#browser()
		p$season = as.Date(p$season)
		if(Sclose=='end')p$season[2] = p$season[2] - (p$season[2]-p$season[1])*(1-Sadj)
		if(Sclose=='start')p$season[1] = p$season[1] + (p$season[2]-p$season[1])*(1-Sadj)
	
		#mortality
		p$M = 0.1
		p$F = getFccir(p)
		
		if(!is.null(window))Fadj = Fadj * getWindow(LS=LS,window=window,lens=p$lens)

		# fishing mortality adjustment
		p$F = p$F * Fadj
	
		p$reserve = 0.1 # % of lobsters that don't trap
	
		# rough temperature generator (for degree day growth)
		#coldestday = as.numeric(as.Date("2017-02-23") - as.Date(p$startDate) )
		#p$dailytemps = rDailyTemps(x=1:(p$nt*p$timestep),b=10,m=10,s=coldestday)
	
		# predicted temperature from temp model (for degree day growth)
		p$dailytemps = TempModelPredict(p)

		p$mint = 10 # minimum temperature for a lobster to molt
	
		#growth 
		p$GrowthFactorMean = 1.15	
		p$GrowthFactorSD = 0.05
	
		p$maxMolts = 15
		p$maxTime = 20

		plist[[i]] = p

	}

	return(plist)
}