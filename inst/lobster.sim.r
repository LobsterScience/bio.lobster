
# Lobster population simulation 

p = bio.lobster::load.environment()

la()

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
#p$moltPr = list(a=-5,b=0.003) # degree day growth

#reproduction
p$gestation = 320
p$brood = 360

#season timing
p$season = c("2016-11-28","2017-05-31")
p$seasonThreshold = 0.33

#mortality
p$M = 0.1
p$F = 0.4

# rough temperature generator (for degree day growth)
p$dailytemps = rDailyTemps(x=1:(p$nt*p$timestep),b=10,m=10,s=50)

#run sexes seperately for now
p$sex = 2

females = simMolt(p)

p$sex = 1

males = simMolt(p)

p$moltPr = list(a=-5,b=0.003) # degree day growth
males2 = simMolt(p,gdd=T)


p$sex = 2
females2 = simMolt(p,gdd=T)

#results
round(males$finalPop)
round(females$finalPop)
round(females$finalBerried)

round(males$totalRemovals)
round(females$totalRemovals)

plot(rowSums(males$finalPop))
plot(rowSums(males2$finalPop))



y = round(males$finalPop)
y2 = round(males2$finalPop)
x = round(females$finalPop)
x2 = round(females2$finalPop)
z = round(females$finalBerried)
z2 = round(females2$finalBerried)	
m = round(males2$totalMolts)



y = thinMatrix(y,4)
x = thinMatrix(x,4)
z = thinMatrix(z,4)
		

		# VB
		Linf=c(281,207)
		k=c(0.065,0.089)
		t0=c(0.76,0.42)

		age=seq(1,23,0.1)

BubblePlotCLF(list(x),bins=seq(47.5,202.5,5),yrs=1:20,log.trans=T,filen='',prop=F,LS=82.5,inch=0.2,bg=rgb(1,0,0,0.1),graphic="R")

		lines(age-2.5,lvb(age,Linf[2],k[2],t0[2]))

BubblePlotCLF(list(y2),bins=seq(47.5,202.5,5),yrs=seq(0.25,20,0.25),log.trans=T,filen='',prop=F,LS=82.5,inch=0.2,bg=rgb(0,0,1,0.1),graphic="R")
		
		lines(age-2.8,lvb(age,Linf[1],k[1],t0[1]))


BubblePlotCLF(list(z),bins=seq(47.5,202.5,5),yrs=1:20,log.trans=T,filen='',prop=F,LS=82.5,inch=0.2,bg=rgb(1,0,1,0.1),graphic="R")

BubblePlotCLF(list(x+z),bins=seq(47.5,202.5,5),yrs=seq(0.25,20,0.25),log.trans=T,filen='',prop=F,LS=82.5,inch=0.2,bg=rgb(1,0,0,0.1),graphic="R")
BubblePlotCLF(list(m),bins=seq(47.5,202.5,5),yrs=seq(0.25,20,0.25),log.trans=T,filen='',prop=F,LS=82.5,inch=0.2,bg=rgb(1,0,0,0.1),graphic="R")

	



################################


bpCLF = 

BarPlotCLF2(bpCLF,yrs=1:20,bins=p$lens,filen=,LS=p$LS )

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

	

	####### Growth Parameters 
	#######

		# [1=male, 2=female, 3=berried]
		# length-weight 
		a=c(0.000608,0.001413,0.00482)
		b=c(3.0583,2.8746,2.638)

		# VB
		Linf=c(281,207)
		k=c(0.065,0.089)
		t0=c(0.76,0.42)

		age=seq(4,23,0.1)
		lines(age-3,lvb(age,Linf[1],k[1],t0[1])



	### Molt probs


