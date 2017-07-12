
# Lobster population simulation 

p = bio.lobster::load.environment()

p$LS = 82.5					# legal size (mm)

p$StartPop = 1000
p$startDate = "2016-12-01"

p$nt = 80					# number of timesteps
p$lens = seq(50,200,5)		# carapace length bins (mm)
p$timestep = 91  			# in days

#growth 
p$GrowthFactorMean = 1.15	
p$GrowthFactorSD = 0.05
p$moltPr = list(a=-5,b=0.013)

#reproduction
p$gestation = 320
p$brood = 360

#season timing
p$season = c("2016-11-28","2017-05-31")
p$seasonThreshold = 0.33

#mortality
p$M = 0.1
p$F = 0.4


#run sexes seperately for now
p$sex = 2

females = simMolt(p)

p$sex = 1

males = simMolt(p)

#results
round(males$finalPop)
round(females$finalPop)
round(females$finalBerried)

round(males$totalRemovals)
round(females$totalRemovals)

y = round(males$finalPop)
x = round(females$finalPop)
z = round(females$finalBerried)

y = thinMatrix(y,4)
x = thinMatrix(x,4)
z = thinMatrix(z,4)

BubblePlotCLF(list(x),bins=seq(47.5,202.5,5),yrs=1:20,log.trans=T,filen='',prop=F,LS=82.5,inch=0.2,bg=rgb(0,0,1,0.1),graphic="R")
BubblePlotCLF(list(y),bins=seq(47.5,202.5,5),yrs=1:20,log.trans=T,filen='',prop=F,LS=82.5,inch=0.2,bg=rgb(1,0,0,0.1),graphic="R")
BubblePlotCLF(list(z),bins=seq(47.5,202.5,5),yrs=1:20,log.trans=T,filen='',prop=F,LS=82.5,inch=0.2,bg=rgb(1,0,1,0.1),graphic="R")



p$nt = 20					# number of timesteps
p$lens = seq(50,200,5)		# carapace length bins (mm)
p$timestep = 365  			# in days


#run sexes seperately for now
p$sex = 2

females1 = simMolt(p)

p$sex = 1

males1 = simMolt(p)

y = round(males1$finalPop)
x = round(females1$finalPop)
z = round(females1$finalBerried)

BubblePlotCLF(list(x),bins=seq(47.5,202.5,5),yrs=1:20,log.trans=T,filen='',prop=F,LS=82.5,inch=0.2,bg=rgb(0,0,1,0.1),graphic="R")
BubblePlotCLF(list(y),bins=seq(47.5,202.5,5),yrs=1:20,log.trans=T,filen='',prop=F,LS=82.5,inch=0.2,bg=rgb(1,0,0,0.1),graphic="R")
BubblePlotCLF(list(z),bins=seq(47.5,202.5,5),yrs=1:20,log.trans=T,filen='',prop=F,LS=82.5,inch=0.2,bg=rgb(1,0,1,0.1),graphic="R")

