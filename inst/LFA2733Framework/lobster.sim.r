
# Lobster population simulation 

p = bio.lobster::load.environment()
require(fields)

la()

	SoMplot(cols=tim.colors(5),ltys=2:6,graphic='png')


	TempModelling = TempModel(areas = 'subarea')
	TempModelPlot(TempModelling,xlim=c(1980,2017),depths=c(5,25,50),Area=c("27N","27S", "29", "30","31A","31B", "32", "33E", "33W"),graphic='R')
	p$TempModel = TempModelling$Model

	MoltModelling = moltModel(p,redo.dd=F)
	p$moltModel = MoltModelling
	moltModelPlot(p$moltModel,graphic='png')


p$lfas = c("27N","27S", "29", "30") # specify lfas in 2 batches

	plist = getSimList(p,sex=1)
	names(plist) = p$lfas

	mlist = mclapply(X = plist, FUN = simMolt, mc.cores=length(p$lfas))
	names(mlist) = p$lfas


	plist = getSimList(p,sex=2)
	names(plist) = p$lfas

	flist = mclapply(X = plist, FUN = simMolt, mc.cores=length(p$lfas))
	names(flist) = p$lfas

	rlist = list(plist=plist,mlist=mlist,flist=flist)
	save("rlist",file=file.path(project.datadirectory("bio.lobster"),"outputs","sim","sim1ResultsBase.rdata"))

##### Legal Size

	LegalSize = c(70,72.5,75,77.5,80,85,87.5,90)
	names(LegalSize) = paste0("LS",LegalSize)

	for(i in 1:length(LegalSize)){
		
		plist = getSimList(p,sex=1,LS=LegalSize[i])
		names(plist) = p$lfas

		mlist = mclapply(X = plist, FUN = simMolt, mc.cores=length(p$lfas))
		names(mlist) = p$lfas

		plist = getSimList(p,sex=2,LS=LegalSize[i])
		names(plist) = p$lfas

		flist = mclapply(X = plist, FUN = simMolt, mc.cores=length(p$lfas))
		names(flist) = p$lfas

		rlist = list(plist=plist,mlist=mlist,flist=flist)
		save("rlist",file=file.path(project.datadirectory("bio.lobster"),"outputs","sim",paste0("sim1Results",names(LegalSize)[i],".rdata")))
	}


############ Season reduction

	ShorterSeason = c(0.9,0.8,0.7,0.6,0.5)
	names(ShorterSeason) = paste0("SS",9:5)
	
	for(i in 1:length(ShorterSeason)){

		plist = getSimList(p,sex=1,Sadj=ShorterSeason[i], Sclose='start')
		names(plist) = p$lfas

		mlist = mclapply(X = plist, FUN = simMolt, mc.cores=length(p$lfas))
		names(mlist) = p$lfas

		plist = getSimList(p,sex=2,Sadj=ShorterSeason[i])
		names(plist) = p$lfas

		flist = mclapply(X = plist, FUN = simMolt, mc.cores=length(p$lfas))
		names(flist) = p$lfas

		rlist = list(plist=plist,mlist=mlist,flist=flist)
		save("rlist",file=file.path(project.datadirectory("bio.lobster"),"outputs","sim",paste0("sim1Results",names(ShorterSeason)[i],".rdata")))

	}



p$lfas = c("31A","31B", "32", "33E", "33W") # specify lfas in 2 batches

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
	save("rlist",file=file.path(project.datadirectory("bio.lobster"),"outputs","sim","sim2ResultsBase.rdata"))

##### Legal Size

	LegalSize = c(70,72.5,75,77.5,80,85,87.5,90)
	names(LegalSize) = paste0("LS",LegalSize)

	for(i in 1:length(LegalSize)){
		
		plist = getSimList(p,sex=1,LS=LegalSize[i])
		names(plist) = p$lfas

		mlist = mclapply(X = plist, FUN = simMolt, mc.cores=length(p$lfas))
		names(mlist) = p$lfas

		plist = getSimList(p,sex=2,LS=LegalSize[i])
		names(plist) = p$lfas

		flist = mclapply(X = plist, FUN = simMolt, mc.cores=length(p$lfas))
		names(flist) = p$lfas

		rlist = list(plist=plist,mlist=mlist,flist=flist)
		save("rlist",file=file.path(project.datadirectory("bio.lobster"),"outputs","sim",paste0("sim2Results",names(LegalSize)[i],".rdata")))
	}


############ Season reduction

	ShorterSeason = c(0.9,0.8,0.7,0.6,0.5)
	names(ShorterSeason) = paste0("SS",9:5)
	
	for(i in 1:length(ShorterSeason)){

		plist = getSimList(p,sex=1,Sadj=ShorterSeason[i], Sclose='start')
		names(plist) = p$lfas

		mlist = mclapply(X = plist, FUN = simMolt, mc.cores=length(p$lfas))
		names(mlist) = p$lfas

		plist = getSimList(p,sex=2,Sadj=ShorterSeason[i])
		names(plist) = p$lfas

		flist = mclapply(X = plist, FUN = simMolt, mc.cores=length(p$lfas))
		names(flist) = p$lfas

		rlist = list(plist=plist,mlist=mlist,flist=flist)
		save("rlist",file=file.path(project.datadirectory("bio.lobster"),"outputs","sim",paste0("sim2Results",names(ShorterSeason)[i],".rdata")))



	}

###### combine all areas to one rdata object

	load(file=file.path(project.datadirectory("bio.lobster"),"outputs","sim","sim1ResultsBase.rdata"))
	rlist1 = rlist
	load(file=file.path(project.datadirectory("bio.lobster"),"outputs","sim","sim2ResultsBase.rdata"))
	rlist2 = rlist
	rlist = list(plist=c(rlist1$plist[c(1,2,4,5)],rlist2$plist),mlist=c(rlist1$mlist[c(1,2,4,5)],rlist2$mlist),flist=c(rlist1$flist[c(1,2,4,5)],rlist2$flist))

	save("rlist",file=file.path(project.datadirectory("bio.lobster"),"outputs","sim","simResultsBase.rdata"))

	for(i in 1:length(ShorterSeason)){

		load(file=file.path(project.datadirectory("bio.lobster"),"outputs","sim",paste0("sim1Results",names(ShorterSeason)[i],".rdata")))
		rlist1 = rlist
		load(file=file.path(project.datadirectory("bio.lobster"),"outputs","sim",paste0("sim2Results",names(ShorterSeason)[i],".rdata")))
		rlist2 = rlist
		rlist = list(plist=c(rlist1$plist[c(1,2,4,5)],rlist2$plist),mlist=c(rlist1$mlist[c(1,2,4,5)],rlist2$mlist),flist=c(rlist1$flist[c(1,2,4,5)],rlist2$flist))
		save("rlist",file=file.path(project.datadirectory("bio.lobster"),"outputs","sim",paste0("simResults",names(ShorterSeason)[i],".rdata")))

	}

	for(i in 1:length(LegalSize)){

		load(file=file.path(project.datadirectory("bio.lobster"),"outputs","sim",paste0("sim1Results",names(LegalSize)[i],".rdata")))
		rlist1 = rlist
		load(file=file.path(project.datadirectory("bio.lobster"),"outputs","sim",paste0("sim2Results",names(LegalSize)[i],".rdata")))
		rlist2 = rlist
		rlist = list(plist=c(rlist1$plist[c(1,2,4,5)],rlist2$plist),mlist=c(rlist1$mlist[c(1,2,4,5)],rlist2$mlist),flist=c(rlist1$flist[c(1,2,4,5)],rlist2$flist))
		save("rlist",file=file.path(project.datadirectory("bio.lobster"),"outputs","sim",paste0("simResults",names(LegalSize)[i],".rdata")))

	}


############# Plots

	load(file=file.path(project.datadirectory("bio.lobster"),"outputs","sim","sim1ResultsBase.rdata"))
	simBubPlot(rlist,graphic='png',path=file.path(project.datadirectory("bio.lobster"),"figures","LFA2733Framework2018","sim"),fn='Base')


	for(i in 1:length(ShorterSeason)){

		load(file=file.path(project.datadirectory("bio.lobster"),"outputs","sim",paste0("simResults",names(ShorterSeason)[i],".rdata")))
		simBubPlot(rlist,graphic='png',path=file.path(project.datadirectory("bio.lobster"),"figures","LFA2733Framework2018","sim"),fn=names(ShorterSeason)[i])

	}

	for(i in 1:length(LegalSize)){

		load(file=file.path(project.datadirectory("bio.lobster"),"outputs","sim",paste0("simResults",names(LegalSize)[i],".rdata")))
		simBubPlot(rlist,graphic='png',path=file.path(project.datadirectory("bio.lobster"),"figures","LFA2733Framework2018","sim"),fn=names(LegalSize)[i])

	}

	LFAs = c("27N","27S",  "29", "30","31A","31B", "32", "33E", "33W")

	simSumLegalSize = simSummary(runs=c("LS70","LS72.5","LS75","LS77.5","LS80","Base","LS85","LS87.5","LS90"),lfas=LFAs)
	simSumSeason = simSummary(runs=c("Base","SS9","SS8","SS7","SS6","SS5"),lfas=LFAs)


	cols=tim.colors(9)
	ltys=1:9
	pchs=c(1:6,15:17)
	

	#x11()
	png(file.path(project.datadirectory('bio.lobster'),'figures','LFA2733Framework2018','simSumLegalSize.png'),width=8,height=8,units='in',res=200)
	par(mfrow=c(3,1),mar=c(0,5,0,0),omi=c(0.75,0,0.5,0.5),las=1)

	matplot(x=seq(70,90,2.5),simSumLegalSize$landnum,type = 'b', pch=pchs,col=cols,lty=ltys,ylab='Catch (#s)',ylim=c(0,2000))
	abline(v=82.5,lty=3,col='grey')
	legend('bottomleft',LFAs,col=cols,lty=ltys,inset=0.03,bty='n', pch=pchs)
	matplot(x=seq(70,90,2.5),simSumLegalSize$landkg,type = 'b', pch=pchs,col=cols,lty=ltys,ylab='Catch (kg)',ylim=c(0,2000))
	abline(v=82.5,lty=3,col='grey')
	matplot(x=seq(70,90,2.5),simSumLegalSize$eggs/10^6,type = 'b', pch=pchs,col=cols,lty=ltys,ylab='Eggs (millions)',ylim=c(0,25))
	abline(v=82.5,lty=3,col='grey')
	mtext("Minimum Legal Size (mm)",1,3)

	dev.off()

	#x11()
	png(file.path(project.datadirectory('bio.lobster'),'figures','LFA2733Framework2018','simSumSeason.png'),width=8,height=8,units='in',res=200)
	par(mfrow=c(3,1),mar=c(0,5,0,0),omi=c(0.75,0,0.5,0.5),las=1)

	matplot(x=seq(0,50,10),simSumSeason$landnum,type = 'b', pch=16,col=cols,lty=ltys,ylab='Catch (#s)',ylim=c(0,2000))
	#abline(v=0,lty=3,col='grey')
	legend('bottomleft',LFAs,col=cols,lty=ltys,inset=0.03,bty='n', pch=pchs)
	matplot(x=seq(0,50,10),simSumSeason$landkg,type = 'b', pch=16,col=cols,lty=ltys,ylab='Catch (kg)',ylim=c(0,2000))
	#abline(v=0,lty=3,col='grey')
	matplot(x=seq(0,50,10),simSumSeason$eggs/10^6,type = 'b', pch=16,col=cols,lty=ltys,ylab='Eggs (millions)',ylim=c(0,25))
	#abline(v=0,lty=3,col='grey')
	mtext("Season reduction (%)",1,3)

	dev.off()
