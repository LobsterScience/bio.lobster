#' @export
getSimList = function(p,sex=1, LS=82.5, Fadj = 1, Sadj = 1, Sclose='end', window=NULL){

	plist = list()

	for(i in 1:length(p$lfas)){
	
		p$sex = sex					# run sexes seperately
		p$LS = LS					# legal size (mm)
		
		p$Area = p$lfas[i]
		p$Depth = 15
	
		p$StartPop = 1000
		
		p$nt = 60					# number of timesteps
		p$lens = seq(50,200,5)		# carapace length bins (mm)
		p$timestep = 91  			# in days
	
		#reproduction
		p$gestation = 320
		p$brood = 360
	
		#season timing
		if(p$Area == "27N" || p$Area == "27S" || p$Area == "27")  p$season = c(as.Date("2000-05-16"),as.Date("2000-07-15")) # 27
		if(p$Area == "29")                                        p$season = c(as.Date("2000-05-01"),as.Date("2000-06-30")) # 29
		if(p$Area == "30")                                        p$season = c(as.Date("2000-05-20"),as.Date("2000-07-20")) # 30
		if(p$Area == "31A")                                       p$season = c(as.Date("2000-04-30"),as.Date("2000-06-30")) # 31A
		if(p$Area == "31B" || p$Area == "32")                     p$season = c(as.Date("2000-04-20"),as.Date("2000-06-20")) # 31B & 32
		if(p$Area == "33E" || p$Area == "33W" || p$Area == "33")  p$season = c(as.Date("1999-11-28"),as.Date("2000-05-31")) # 33
		if(p$Area == "34")                                        p$season = c(as.Date("1999-11-28"),as.Date("2000-05-31")) # 34
		if(p$Area == "35")                                        p$season = c(as.Date("1999-10-15"),as.Date("1999-12-31"),as.Date("2000-03-01"),as.Date("2000-07-31")) # 35
		if(p$Area == "36")                                        p$season = c(as.Date("1999-11-12"),as.Date("2000-01-15"),as.Date("2000-04-01"),as.Date("2000-06-29")) # 36
		if(p$Area == "38")                                        p$season = c(as.Date("1999-11-12"),as.Date("2000-06-25")) # 38

		if(p$Area %in%  c("27N",'27S', "27",'29','30','31A','31B','32','33','34')) p$startDate = as.Date("1999-12-01")
		if(p$Area %in%  c('35','36','38')) p$startDate = as.Date("1999-10-01")
		

		#season adjustment
		#browser()
		p$season = as.Date(p$season)
		if(length(p$season)==2){
			if(Sclose=='end')p$season[2] = p$season[2] - (p$season[2]-p$season[1])*(1-Sadj)
			if(Sclose=='start')p$season[1] = p$season[1] + (p$season[2]-p$season[1])*(1-Sadj)
		}
		
		if(length(p$season)==4){
			if(Sclose=='end'){p$season[2] = p$season[2] - (p$season[2]-p$season[1])*(1-Sadj);p$season[4] = p$season[4] - (p$season[4]-p$season[3])*(1-Sadj)} 
			if(Sclose=='start'){p$season[1] = p$season[1] + (p$season[2]-p$season[1])*(1-Sadj); p$season[3] = p$season[3] + (p$season[4]-p$season[3])*(1-Sadj)}
		}


		#mortality
		p$M = 0.1
		if(is.null(p$F))p$F = getFccir(p)

		#window
		p$window = window
		

		# fishing mortality adjustment
		p$F = p$F * Fadj
	
		p$reserve = 0.1 # % of lobsters that don't trap
	
		# rough temperature generator (for degree day growth)
		#coldestday = as.numeric(as.Date("2017-02-23") - as.Date(p$startDate) )
		#p$dailytemps = rDailyTemps(x=1:(p$nt*p$timestep),b=10,m=10,s=coldestday)
	
		# predicted temperature from temp model (for degree day growth)
		if(p$Area == '31A') p$Area = 311
		p$dailytemps = TempModelPredict(p)
		p$Area = '31A'
		p$mint = 10 # minimum temperature for a lobster to molt
	
		#growth 		
	p$Incr = getIncr(p)
		
		p$maxMolts = 15
		p$maxTime = 20

		plist[[i]] = p

	}

	return(plist)
}