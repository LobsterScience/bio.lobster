
# Lobster population simulation 

p = bio.lobster::load.environment()

la()

p$LS = 82.5					# legal size (mm)
p$Area = "27N"
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
p$season = c("2000-05-16","2000-07-15") # 27
p$season = c("2000-05-01","2000-06-30") # 29
p$season = c("2000-05-20","2000-07-20") # 30
p$season = c("2000-04-30","2000-06-30") # 31A
p$season = c("2000-04-20","2000-06-20") # 31B & 32
p$season = c("1999-11-28","2000-05-31") # 33

#mortality
p$M = 0.1
p$F = 0.7

# rough temperature generator (for degree day growth)
#coldestday = as.numeric(as.Date("2017-02-23") - as.Date(p$startDate) )
#p$dailytemps = rDailyTemps(x=1:(p$nt*p$timestep),b=10,m=10,s=coldestday)

# predicted temperature from temp model (for degree day growth)
TempModelling = TempModel()
p$TempModel = TempModelling$Model
p$dailytemps = TempModelPredict(p)


p$mint = 10 # minimum temperature for a lobster to molt

#growth 
p$GrowthFactorMean = 1.15	
p$GrowthFactorSD = 0.05
p$moltPrModel = moltPrModel(p,redo.dd=F) # degree day growth

#run sexes seperately 
p$sex = 1
males = simMolt(p,gdd=T)

p$sex = 2
females = simMolt(p,gdd=T)

#results


y = round(males$finalPop)
x = round(females$finalPop)
z = round(females$finalBerried)
bins = c(p$lens[1]-2.5,p$lens-2.5)
yrs = seq(p$timestep,p$nt*p$timestep,p$timestep)/365


plot(yrs,rowSums(males$finalPop),type='l')
lines(yrs,rowSums(females$finalPop+females$finalBerried),lty=2)

# VB
Linf=c(281,207)
k=c(0.065,0.089)
t0=c(0.76,0.42)
age=seq(1,23,0.1)


BubblePlotCLF(list(x),bins=bins,yrs=yrs,log.trans=T,filen='',prop=F,LS=82.5,inch=0.2,bg=rgb(1,0,0,0.1),graphic="R")

		lines(age-3.2,lvb(age,Linf[2],k[2],t0[2]))

BubblePlotCLF(list(y),bins=bins,yrs=yrs,log.trans=T,filen='',prop=F,LS=82.5,inch=0.2,bg=rgb(0,0,1,0.1),graphic="R")
		
		lines(age-3.5,lvb(age,Linf[1],k[1],t0[1]))

BubblePlotCLF(list(z),bins=bins,yrs=yrs,log.trans=T,filen='',prop=F,LS=82.5,inch=0.2,bg=rgb(1,0,1,0.1),graphic="R")

BubblePlotCLF(list(x+z),bins=bins,yrs=yrs,log.trans=T,filen='',prop=F,LS=82.5,inch=0.2,bg=rgb(1,0,0,0.1),graphic="R")

	



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

	x11()

	par(mfrow=c(2,1))

	p$moltPr = list(a=-9,b=0.02,x=0.5)

	moltProbPlot(p)

	p$moltPr = list(a=-15,b=0.002,x=0.5) # degree day growth

	moltProbPlot(p,gdd=T)


	x11()

	as=c(-25,-20,-15)
	bs=c(0.0025,0.003,0.0035)

	l=length(as)
	par(mfrow=c(l,l))

	for(i in 1:l){
		for(j in 1:l){
			p$moltPr = list(a=as[i],b=bs[j],x=0.7) # degree day growth

			moltProbPlot(p,gdd=T,main=paste('a =',as[i],', b =',bs[j]))

		}
	}


	x11()

	as=c(-15,-10,-5)
	bs=c(0.015,0.02,0.025)

	l=length(as)
	par(mfrow=c(l,l))

	for(i in 1:l){
		for(j in 1:l){
			p$moltPr = list(a=as[i],b=bs[j],x=0.5) # degree day growth

			moltProbPlot(p,gdd=F,main=paste('a =',as[i],', b =',bs[j]))

		}
	}


	p$moltPr = list(a=-9,b=0.0013,x=1.2) # degree day growth

	moltProbPlot(p,gdd=T)
