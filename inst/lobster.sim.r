
# Lobster population simulation 

p = bio.lobster::load.environment()

la()

	TempModelling = TempModel(areas = 'subarea')
	TempModelPlot(TempModelling,xlim=c(2000,2017),depths=c(5,25,50),Area=c("33W","27N"),graphic='png')
	p$TempModel = TempModelling$Model

	p$moltModel = moltModel(p,redo.dd=F)
	moltModelPlot(p$moltModel,graphic='png')

	p$lfas = c("27N", "29", "30", "31A", "31B", "32", "33W") # specify lfas for data summary
	
	p$lfas = c("27N","27S", "28", "29", "30") # specify lfas for data summary
	p$lfas = c("31A","31B", "32", "33E", "33W") # specify lfas for data summary



	####### Base
	p$lfas = c("27N","27S", "28", "29", "30") # specify lfas for data summary
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


