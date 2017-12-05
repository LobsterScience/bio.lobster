
# Lobster population simulation 

p = bio.lobster::load.environment()

la()

	p$lfas = c("27N", "29", "30", "31A", "31B", "32", "33W") # specify lfas for data summary
	
	TempModelling = TempModel(areas = 'subarea')
	TempModelPlot(TempModelling,xlim=c(2000,2017),depths=c(5,25,50))
	p$TempModel = TempModelling$Model

	moltModel = moltPrModel(p,redo.dd=F)
	p$moltPrModel = moltModel$moltPrModel # degree day growth


	####### Base
	plist = getSimList(p,sex=1)
	names(plist) = p$lfas

	mlist = mclapply(X = plist, FUN = simMolt, mc.cores=length(p$lfas))
	names(mlist) = p$lfas

	plist = getSimList(p,sex=2)
	names(plist) = p$lfas

	flist = mclapply(X = plist, FUN = simMolt, mc.cores=length(p$lfas))
	names(flist) = p$lfas

	rlist = list(plist=plist,mlist=mlist,flist=flist)
	save("rlist",file=file.path(project.datadirectory("bio.lobster"),"outputs","sim","simResultsBase.rdata"))

##### Legal Size

	####### LS70
	plist = getSimList(p,sex=1,LS=70)
	names(plist) = p$lfas

	mlist = mclapply(X = plist, FUN = simMolt, mc.cores=length(p$lfas))
	names(mlist) = p$lfas

	plist = getSimList(p,sex=2,LS=70)
	names(plist) = p$lfas

	flist = mclapply(X = plist, FUN = simMolt, mc.cores=length(p$lfas))
	names(flist) = p$lfas

	rlist = list(plist=plist,mlist=mlist,flist=flist)
	save("rlist",file=file.path(project.datadirectory("bio.lobster"),"outputs","sim","simResultsLS70.rdata"))


	####### LS72.5
	plist = getSimList(p,sex=1,LS=72.5)
	names(plist) = p$lfas

	mlist = mclapply(X = plist, FUN = simMolt, mc.cores=length(p$lfas))
	names(mlist) = p$lfas

	plist = getSimList(p,sex=2,LS=72.5)
	names(plist) = p$lfas

	flist = mclapply(X = plist, FUN = simMolt, mc.cores=length(p$lfas))
	names(flist) = p$lfas

	rlist = list(plist=plist,mlist=mlist,flist=flist)
	save("rlist",file=file.path(project.datadirectory("bio.lobster"),"outputs","sim","simResultsLS72.5.rdata"))


	####### LS75
	plist = getSimList(p,sex=1,LS=75)
	names(plist) = p$lfas

	mlist = mclapply(X = plist, FUN = simMolt, mc.cores=length(p$lfas))
	names(mlist) = p$lfas

	plist = getSimList(p,sex=2,LS=75)
	names(plist) = p$lfas

	flist = mclapply(X = plist, FUN = simMolt, mc.cores=length(p$lfas))
	names(flist) = p$lfas

	rlist = list(plist=plist,mlist=mlist,flist=flist)
	save("rlist",file=file.path(project.datadirectory("bio.lobster"),"outputs","sim","simResultsLS75.rdata"))

	####### LS77.5
	plist = getSimList(p,sex=1,LS=77.5)
	names(plist) = p$lfas

	mlist = mclapply(X = plist, FUN = simMolt, mc.cores=length(p$lfas))
	names(mlist) = p$lfas

	plist = getSimList(p,sex=2,LS=77.5)
	names(plist) = p$lfas

	flist = mclapply(X = plist, FUN = simMolt, mc.cores=length(p$lfas))
	names(flist) = p$lfas

	rlist = list(plist=plist,mlist=mlist,flist=flist)
	save("rlist",file=file.path(project.datadirectory("bio.lobster"),"outputs","sim","simResultsLS77.5.rdata"))



	####### LS80
	plist = getSimList(p,sex=1,LS=80)
	names(plist) = p$lfas

	mlist = mclapply(X = plist, FUN = simMolt, mc.cores=length(p$lfas))
	names(mlist) = p$lfas

	plist = getSimList(p,sex=2,LS=80)
	names(plist) = p$lfas

	flist = mclapply(X = plist, FUN = simMolt, mc.cores=length(p$lfas))
	names(flist) = p$lfas

	rlist = list(plist=plist,mlist=mlist,flist=flist)
	save("rlist",file=file.path(project.datadirectory("bio.lobster"),"outputs","sim","simResultsLS80.rdata"))



	####### LS85
	plist = getSimList(p,sex=1,LS=85)
	names(plist) = p$lfas

	mlist = mclapply(X = plist, FUN = simMolt, mc.cores=length(p$lfas))
	names(mlist) = p$lfas

	plist = getSimList(p,sex=2,LS=85)
	names(plist) = p$lfas

	flist = mclapply(X = plist, FUN = simMolt, mc.cores=length(p$lfas))
	names(flist) = p$lfas

	rlist = list(plist=plist,mlist=mlist,flist=flist)
	save("rlist",file=file.path(project.datadirectory("bio.lobster"),"outputs","sim","simResultsLS85.rdata"))


	####### LS87.5
	plist = getSimList(p,sex=1,LS=87.5)
	names(plist) = p$lfas

	mlist = mclapply(X = plist, FUN = simMolt, mc.cores=length(p$lfas))
	names(mlist) = p$lfas

	plist = getSimList(p,sex=2,LS=87.5)
	names(plist) = p$lfas

	flist = mclapply(X = plist, FUN = simMolt, mc.cores=length(p$lfas))
	names(flist) = p$lfas

	rlist = list(plist=plist,mlist=mlist,flist=flist)
	save("rlist",file=file.path(project.datadirectory("bio.lobster"),"outputs","sim","simResultsLS87.5.rdata"))

	
	####### LS90
	plist = getSimList(p,sex=1,LS=90)
	names(plist) = p$lfas

	mlist = mclapply(X = plist, FUN = simMolt, mc.cores=length(p$lfas))
	names(mlist) = p$lfas

	plist = getSimList(p,sex=2,LS=90)
	names(plist) = p$lfas

	flist = mclapply(X = plist, FUN = simMolt, mc.cores=length(p$lfas))
	names(flist) = p$lfas

	rlist = list(plist=plist,mlist=mlist,flist=flist)
	save("rlist",file=file.path(project.datadirectory("bio.lobster"),"outputs","sim","simResultsLS90.rdata"))


############ Season reduction


	####### 10% shorter season (end)
	plist = getSimList(p,sex=1,Sadj=0.9)
	names(plist) = p$lfas

	mlist = mclapply(X = plist, FUN = simMolt, mc.cores=length(p$lfas))
	names(mlist) = p$lfas

	plist = getSimList(p,sex=2,Sadj=0.9)
	names(plist) = p$lfas

	flist = mclapply(X = plist, FUN = simMolt, mc.cores=length(p$lfas))
	names(flist) = p$lfas

	rlist = list(plist=plist,mlist=mlist,flist=flist)
	save("rlist",file=file.path(project.datadirectory("bio.lobster"),"outputs","sim","simResultsSS9.rdata"))


	####### 20% shorter season (end)
	plist = getSimList(p,sex=1,Sadj=0.8)
	names(plist) = p$lfas

	mlist = mclapply(X = plist, FUN = simMolt, mc.cores=length(p$lfas))
	names(mlist) = p$lfas

	plist = getSimList(p,sex=2,Sadj=0.8)
	names(plist) = p$lfas

	flist = mclapply(X = plist, FUN = simMolt, mc.cores=length(p$lfas))
	names(flist) = p$lfas

	rlist = list(plist=plist,mlist=mlist,flist=flist)
	save("rlist",file=file.path(project.datadirectory("bio.lobster"),"outputs","sim","simResultsSS8.rdata"))


	####### 30% shorter season 
	plist = getSimList(p,sex=1,Sadj=0.7, Sclose='start')
	names(plist) = p$lfas

	mlist = mclapply(X = plist, FUN = simMolt, mc.cores=length(p$lfas))
	names(mlist) = p$lfas

	plist = getSimList(p,sex=2,Sadj=0.7)
	names(plist) = p$lfas

	flist = mclapply(X = plist, FUN = simMolt, mc.cores=length(p$lfas))
	names(flist) = p$lfas

	rlist = list(plist=plist,mlist=mlist,flist=flist)
	save("rlist",file=file.path(project.datadirectory("bio.lobster"),"outputs","sim","simResultsSS7.rdata"))


	####### 40% shorter season 
	plist = getSimList(p,sex=1,Sadj=0.6, Sclose='start')
	names(plist) = p$lfas

	mlist = mclapply(X = plist, FUN = simMolt, mc.cores=length(p$lfas))
	names(mlist) = p$lfas

	plist = getSimList(p,sex=2,Sadj=0.6)
	names(plist) = p$lfas

	flist = mclapply(X = plist, FUN = simMolt, mc.cores=length(p$lfas))
	names(flist) = p$lfas

	rlist = list(plist=plist,mlist=mlist,flist=flist)
	save("rlist",file=file.path(project.datadirectory("bio.lobster"),"outputs","sim","simResultsSS6.rdata"))


	####### 50% shorter season 
	plist = getSimList(p,sex=1,Sadj=0.5, Sclose='start')
	names(plist) = p$lfas

	mlist = mclapply(X = plist, FUN = simMolt, mc.cores=length(p$lfas))
	names(mlist) = p$lfas

	plist = getSimList(p,sex=2,Sadj=0.5)
	names(plist) = p$lfas

	flist = mclapply(X = plist, FUN = simMolt, mc.cores=length(p$lfas))
	names(flist) = p$lfas

	rlist = list(plist=plist,mlist=mlist,flist=flist)
	save("rlist",file=file.path(project.datadirectory("bio.lobster"),"outputs","sim","simResultsSS5.rdata"))








	load(file=file.path(project.datadirectory("bio.lobster"),"outputs","sim","simResultsSS9.rdata"))
	simBubPlot(rlist,graphic='png',path=file.path(project.datadirectory("bio.lobster"),"figures","LFA2733Framework2018","sim"),fn='SS9')
	

	load(file=file.path(project.datadirectory("bio.lobster"),"outputs","sim","simResultsSS8.rdata"))
	simBubPlot(rlist,graphic='png',path=file.path(project.datadirectory("bio.lobster"),"figures","LFA2733Framework2018","sim"),fn='SS8')
	

	load(file=file.path(project.datadirectory("bio.lobster"),"outputs","sim","simResultsSS7.rdata"))
	simBubPlot(rlist,graphic='png',path=file.path(project.datadirectory("bio.lobster"),"figures","LFA2733Framework2018","sim"),fn='SS7')


	load(file=file.path(project.datadirectory("bio.lobster"),"outputs","sim","simResultsSS6.rdata"))
	simBubPlot(rlist,graphic='png',path=file.path(project.datadirectory("bio.lobster"),"figures","LFA2733Framework2018","sim"),fn='SS6')


	load(file=file.path(project.datadirectory("bio.lobster"),"outputs","sim","simResultsSS5.rdata"))
	simBubPlot(rlist,graphic='png',path=file.path(project.datadirectory("bio.lobster"),"figures","LFA2733Framework2018","sim"),fn='SS5')


	load(file=file.path(project.datadirectory("bio.lobster"),"outputs","sim","simResultsBase.rdata"))
	simBubPlot(rlist,graphic='png',path=file.path(project.datadirectory("bio.lobster"),"figures","LFA2733Framework2018","sim"),fn='Base')
	

	load(file=file.path(project.datadirectory("bio.lobster"),"outputs","sim","simResultsLS70.rdata"))
	simBubPlot(rlist,graphic='png',path=file.path(project.datadirectory("bio.lobster"),"figures","LFA2733Framework2018","sim"),fn='LS70')
	

	load(file=file.path(project.datadirectory("bio.lobster"),"outputs","sim","simResultsLS72.5.rdata"))
	simBubPlot(rlist,graphic='png',path=file.path(project.datadirectory("bio.lobster"),"figures","LFA2733Framework2018","sim"),fn='LS72.5')
	

	load(file=file.path(project.datadirectory("bio.lobster"),"outputs","sim","simResultsLS75.rdata"))
	simBubPlot(rlist,graphic='png',path=file.path(project.datadirectory("bio.lobster"),"figures","LFA2733Framework2018","sim"),fn='LS75')

	
	load(file=file.path(project.datadirectory("bio.lobster"),"outputs","sim","simResultsLS77.5.rdata"))
	simBubPlot(rlist,graphic='png',path=file.path(project.datadirectory("bio.lobster"),"figures","LFA2733Framework2018","sim"),fn='LS77.5')


	load(file=file.path(project.datadirectory("bio.lobster"),"outputs","sim","simResultsLS80.rdata"))
	simBubPlot(rlist,graphic='png',path=file.path(project.datadirectory("bio.lobster"),"figures","LFA2733Framework2018","sim"),fn='LS80')

	
	load(file=file.path(project.datadirectory("bio.lobster"),"outputs","sim","simResultsLS85.rdata"))
	simBubPlot(rlist,graphic='png',path=file.path(project.datadirectory("bio.lobster"),"figures","LFA2733Framework2018","sim"),fn='LS85')


	load(file=file.path(project.datadirectory("bio.lobster"),"outputs","sim","simResultsLS87.5.rdata"))
	simBubPlot(rlist,graphic='png',path=file.path(project.datadirectory("bio.lobster"),"figures","LFA2733Framework2018","sim"),fn='LS87.5')
	

	load(file=file.path(project.datadirectory("bio.lobster"),"outputs","sim","simResultsLS90.rdata"))
	simBubPlot(rlist,graphic='png',path=file.path(project.datadirectory("bio.lobster"),"figures","LFA2733Framework2018","sim"),fn='LS90')



simSumLegalSize = simSummary(runs=c("LS70","LS72.5","LS75","LS77.5","LS80","Base","LS85","LS87.5","LS90"))
cols=1:7
ltys=1:7
x11()
par(mfrow=c(3,1),mar=c(0,7,0,0),omi=c(0.5,0,0.5,0.5),las=1)

matplot(x=seq(70,90,2.5),simSumLegalSize$landnum,type = 'b', pch=16,col=cols,lty=ltys,ylab='Catch (#s)',ylim=c(0,2000))
matplot(x=seq(70,90,2.5),simSumLegalSize$landkg,type = 'b', pch=16,col=cols,lty=ltys,ylab='Catch (kg)',ylim=c(0,1500))
matplot(x=seq(70,90,2.5),simSumLegalSize$eggs/1000,type = 'b', pch=16,col=cols,lty=ltys,ylab='Eggs (000s)',ylim=c(0,20000))
legend('topleft',p$lfas,col=cols,lty=ltys,inset=0.05,bty='n')




simSumSeason = simSummary(runs=c("Base","SS9","SS8","SS7","SS6","SS5"))
cols=1:7
ltys=1:7
x11()
par(mfrow=c(3,1),mar=c(0,7,0,0),omi=c(0.5,0,0.5,0.5),las=1)

matplot(x=seq(100,50,-10),simSumSeason$landnum,type = 'b', pch=16,col=cols,lty=ltys,ylab='Catch (#s)',ylim=c(0,2000))
matplot(x=seq(100,50,-10),simSumSeason$landkg,type = 'b', pch=16,col=cols,lty=ltys,ylab='Catch (kg)',ylim=c(0,2000))
legend('bottomleft',p$lfas,col=cols,lty=ltys,inset=0.05,bty='n')
matplot(x=seq(100,50,-10),simSumSeason$eggs/1000,type = 'b', pch=16,col=cols,lty=ltys,ylab='Eggs (000s)',ylim=c(0,40000))





	p$lfas = c("27N","27S", "28", "29", "30", "31A", "31B", "32", "33E", "33W") 

								# carapace length bins (mm)
	TempModelling = TempModel(areas = 'subarea')
	p$TempModel = TempModelling$Model
	moltModel = moltPrModel(p,redo.dd=F)
	p$moltPrModel = moltModel # degree day growth


	plist = getSimList(p,sex=1)

	
	DTs = list()
	dt = c()

for(l in 1:length(p$lfas)){

	plist[[l]]$ddoy = cumsum(plist[[l]]$dailytemps)
	for(i in 1:length(plist[[l]]$lens))	{
		dt[i] = min(which(pPrMolt(plist[[l]],cl=plist[[l]]$lens[i])>0.5))
	}
	names(dt) = plist[[l]]$lens

	DTs[[l]] = dt

}

names(DTs) = p$lfas

save(DTs,file="deltaTs.rdata")

	
 plot(p$lens,dt2,type='l',ylim=c(0,1000),xlab='CL (mm)',ylab='days')
 lines(p$lens,dt1,lty=2)
 lines(p$lens,dt3,lty=2)


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

	
#BubblePlotCLF(list(males$finalPop),bins=bins,yrs=yrs,log.trans=T,filen='1',prop=F,LS=82.5,inch=0.2,bg=rgb(0,0,1,0.1),graphic="pdf",ylim=c(40,150),xlim=c(0,10))
BubblePlotCLF(list(y),bins=bins,yrs=yrs,log.trans=T,filen='2',prop=F,LS=82.5,inch=0.2,bg=rgb(0,0,1,0.1),graphic="pdf",ylim=c(40,150),xlim=c(0,10))



################################


bpCLF = 

BarPlotCLF2(bpCLF,yrs=1:20,bins=p$lens,filen=,LS=p$LS )
	

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
