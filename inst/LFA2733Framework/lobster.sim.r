
# Lobster population simulation 

p = bio.lobster::load.environment()
require(fields)

la()

	SoMplot(cols=tim.colors(5),ltys=2:6,graphic='png')


	TempModelling = TempModel(areas = 'subarea')
	#TempModelPlot(TempModelling,xlim=c(1980,2017),depths=c(5,25,50),Area=c("27N","27S", "29", "30","31A","31B", "32", "33E", "33W"),graphic='R')
	p$TempModel = TempModelling$Model

	MoltModelling = moltModel(p,redo.dd=F)
	p$moltModel = MoltModelling
	#moltModelPlot(p$moltModel,graphic='png')


p$lfas = c("27N","27S", "29", "30") # specify lfas in 2 batches

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
	save("rlist",file=file.path(project.datadirectory("bio.lobster"),"outputs","sim","sim1ResultsBase.rdata"))


		simBubPlot(rlist,graphic='R',cex.lab=2,cex.axis=1.5)
		round(rlist$mlist$'27N'$moltProb[,,1,1],2)


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


##### Window size 

	WindowSize = list(c(115,125),c(105,125))
	names(WindowSize) = c("SmallWin","BigWin")

	for(i in 1:length(WindowSize)){
		
		plist = getSimList(p,sex=1,window=WindowSize[[i]])
		names(plist) = p$lfas

		mlist = mclapply(X = plist, FUN = simMolt, mc.cores=length(p$lfas))
		names(mlist) = p$lfas

		plist = getSimList(p,sex=2,window=WindowSize[[i]])
		names(plist) = p$lfas

		flist = mclapply(X = plist, FUN = simMolt, mc.cores=length(p$lfas))
		names(flist) = p$lfas

		rlist = list(plist=plist,mlist=mlist,flist=flist)
		save("rlist",file=file.path(project.datadirectory("bio.lobster"),"outputs","sim",paste0("sim1Results",names(WindowSize)[i],".rdata")))
	}

##### Max size 

	MaxSize = list(c(135,210),c(130,210),c(125,210))
	names(MaxSize) = paste0("Max",lapply(MaxSize,min))

	for(i in 1:length(MaxSize)){
		
		plist = getSimList(p,sex=1,window=MaxSize[[i]])
		names(plist) = p$lfas

		mlist = mclapply(X = plist, FUN = simMolt, mc.cores=length(p$lfas))
		names(mlist) = p$lfas

		plist = getSimList(p,sex=2,window=MaxSize[[i]])
		names(plist) = p$lfas

		flist = mclapply(X = plist, FUN = simMolt, mc.cores=length(p$lfas))
		names(flist) = p$lfas

		rlist = list(plist=plist,mlist=mlist,flist=flist)
		save("rlist",file=file.path(project.datadirectory("bio.lobster"),"outputs","sim",paste0("sim1Results",names(MaxSize)[i],".rdata")))
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



##### Window size 

	WindowSize = list(c(115,125),c(105,125))
	names(WindowSize) = c("SmallWin","BigWin")

	for(i in 1:length(WindowSize)){
		
		plist = getSimList(p,sex=1,window=WindowSize[[i]])
		names(plist) = p$lfas

		mlist = mclapply(X = plist, FUN = simMolt, mc.cores=length(p$lfas))
		names(mlist) = p$lfas

		plist = getSimList(p,sex=2,window=WindowSize[[i]])
		names(plist) = p$lfas

		flist = mclapply(X = plist, FUN = simMolt, mc.cores=length(p$lfas))
		names(flist) = p$lfas

		rlist = list(plist=plist,mlist=mlist,flist=flist)
		save("rlist",file=file.path(project.datadirectory("bio.lobster"),"outputs","sim",paste0("sim2Results",names(WindowSize)[[i]],".rdata")))
	}


##### Max size 

	MaxSize = list(c(135,210),c(130,210),c(125,210))
	names(MaxSize) = paste0("Max",lapply(MaxSize,min))

	for(i in 1:length(MaxSize)){
		
		plist = getSimList(p,sex=1,window=MaxSize[[i]])
		names(plist) = p$lfas

		mlist = mclapply(X = plist, FUN = simMolt, mc.cores=length(p$lfas))
		names(mlist) = p$lfas

		plist = getSimList(p,sex=2,window=MaxSize[[i]])
		names(plist) = p$lfas

		flist = mclapply(X = plist, FUN = simMolt, mc.cores=length(p$lfas))
		names(flist) = p$lfas

		rlist = list(plist=plist,mlist=mlist,flist=flist)
		save("rlist",file=file.path(project.datadirectory("bio.lobster"),"outputs","sim",paste0("sim2Results",names(MaxSize)[i],".rdata")))
	}



###### combine all areas to one rdata object

	load(file=file.path(project.datadirectory("bio.lobster"),"outputs","sim","sim1ResultsBase.rdata"))
	rlist1 = rlist
	load(file=file.path(project.datadirectory("bio.lobster"),"outputs","sim","sim2ResultsBase.rdata"))
	rlist2 = rlist
	rlist = list(plist=c(rlist1$plist,rlist2$plist),mlist=c(rlist1$mlist,rlist2$mlist),flist=c(rlist1$flist,rlist2$flist))

	save("rlist",file=file.path(project.datadirectory("bio.lobster"),"outputs","sim","simResultsBase.rdata"))

	for(i in 1:length(ShorterSeason)){

		load(file=file.path(project.datadirectory("bio.lobster"),"outputs","sim",paste0("sim1Results",names(ShorterSeason)[i],".rdata")))
		rlist1 = rlist
		load(file=file.path(project.datadirectory("bio.lobster"),"outputs","sim",paste0("sim2Results",names(ShorterSeason)[i],".rdata")))
		rlist2 = rlist
		rlist = list(plist=c(rlist1$plist,rlist2$plist),mlist=c(rlist1$mlist,rlist2$mlist),flist=c(rlist1$flist,rlist2$flist))
		save("rlist",file=file.path(project.datadirectory("bio.lobster"),"outputs","sim",paste0("simResults",names(ShorterSeason)[i],".rdata")))

	}

	for(i in 1:length(LegalSize)){

		load(file=file.path(project.datadirectory("bio.lobster"),"outputs","sim",paste0("sim1Results",names(LegalSize)[i],".rdata")))
		rlist1 = rlist
		load(file=file.path(project.datadirectory("bio.lobster"),"outputs","sim",paste0("sim2Results",names(LegalSize)[i],".rdata")))
		rlist2 = rlist
		rlist = list(plist=c(rlist1$plist,rlist2$plist),mlist=c(rlist1$mlist,rlist2$mlist),flist=c(rlist1$flist,rlist2$flist))
		save("rlist",file=file.path(project.datadirectory("bio.lobster"),"outputs","sim",paste0("simResults",names(LegalSize)[i],".rdata")))

	}

	for(i in 1:length(WindowSize)){

		load(file=file.path(project.datadirectory("bio.lobster"),"outputs","sim",paste0("sim1Results",names(WindowSize)[i],".rdata")))
		rlist1 = rlist
		load(file=file.path(project.datadirectory("bio.lobster"),"outputs","sim",paste0("sim2Results",names(WindowSize)[i],".rdata")))
		rlist2 = rlist
		rlist = list(plist=c(rlist1$plist,rlist2$plist),mlist=c(rlist1$mlist,rlist2$mlist),flist=c(rlist1$flist,rlist2$flist))
		save("rlist",file=file.path(project.datadirectory("bio.lobster"),"outputs","sim",paste0("simResults",names(WindowSize)[i],".rdata")))

		#females only
		load(file=file.path(project.datadirectory("bio.lobster"),"outputs","sim","sim1ResultsBase.rdata"))
		rlistb1 = rlist
		load(file=file.path(project.datadirectory("bio.lobster"),"outputs","sim","sim2ResultsBase.rdata"))
		rlistb2 = rlist
		rlist = list(plist=c(rlist1$plist,rlist2$plist),mlist=c(rlistb1$mlist,rlistb2$mlist),flist=c(rlist1$flist,rlist2$flist))
		save("rlist",file=file.path(project.datadirectory("bio.lobster"),"outputs","sim",paste0("simResultsF",names(WindowSize)[i],".rdata")))



	}

	for(i in 1:length(MaxSize)){

		load(file=file.path(project.datadirectory("bio.lobster"),"outputs","sim",paste0("sim1Results",names(MaxSize)[i],".rdata")))
		rlist1 = rlist
		load(file=file.path(project.datadirectory("bio.lobster"),"outputs","sim",paste0("sim2Results",names(MaxSize)[i],".rdata")))
		rlist2 = rlist
		rlist = list(plist=c(rlist1$plist,rlist2$plist),mlist=c(rlist1$mlist,rlist2$mlist),flist=c(rlist1$flist,rlist2$flist))
		save("rlist",file=file.path(project.datadirectory("bio.lobster"),"outputs","sim",paste0("simResults",names(MaxSize)[i],".rdata")))

		#females only
		load(file=file.path(project.datadirectory("bio.lobster"),"outputs","sim","sim1ResultsBase.rdata"))
		rlistb1 = rlist
		load(file=file.path(project.datadirectory("bio.lobster"),"outputs","sim","sim2ResultsBase.rdata"))
		rlistb2 = rlist
		rlist = list(plist=c(rlist1$plist,rlist2$plist),mlist=c(rlistb1$mlist,rlistb2$mlist),flist=c(rlist1$flist,rlist2$flist))
		save("rlist",file=file.path(project.datadirectory("bio.lobster"),"outputs","sim",paste0("simResultsF",names(MaxSize)[i],".rdata")))
	}


############# Plots

	load(file=file.path(project.datadirectory("bio.lobster"),"outputs","sim","simResultsBase.rdata"))
	simBubPlot(rlist,graphic='png',path=file.path(project.datadirectory("bio.lobster"),"figures","LFA2733Framework2018","sim"),fn='Base',cex.lab=2,cex.axis=1.5)


	for(i in 1:length(ShorterSeason)){

		load(file=file.path(project.datadirectory("bio.lobster"),"outputs","sim",paste0("simResults",names(ShorterSeason)[i],".rdata")))
		simBubPlot(rlist,graphic='png',path=file.path(project.datadirectory("bio.lobster"),"figures","LFA2733Framework2018","sim"),fn=names(ShorterSeason)[i],cex.lab=2,cex.axis=1.5)

	}

	for(i in 1:length(LegalSize)){

		load(file=file.path(project.datadirectory("bio.lobster"),"outputs","sim",paste0("simResults",names(LegalSize)[i],".rdata")))
		simBubPlot(rlist,graphic='png',path=file.path(project.datadirectory("bio.lobster"),"figures","LFA2733Framework2018","sim"),fn=names(LegalSize)[i],cex.lab=2,cex.axis=1.5)

	}

	for(i in 1:length(WindowSize)){

		load(file=file.path(project.datadirectory("bio.lobster"),"outputs","sim",paste0("simResults",names(WindowSize)[i],".rdata")))
		simBubPlot(rlist,graphic='png',path=file.path(project.datadirectory("bio.lobster"),"figures","LFA2733Framework2018","sim"),fn=names(WindowSize)[i],cex.lab=2,cex.axis=1.5)
		load(file=file.path(project.datadirectory("bio.lobster"),"outputs","sim",paste0("simResultsF",names(WindowSize)[i],".rdata")))
		simBubPlot(rlist,graphic='png',path=file.path(project.datadirectory("bio.lobster"),"figures","LFA2733Framework2018","sim"),fn=paste0("F",names(WindowSize)[i]),cex.lab=2,cex.axis=1.5)

	}

	for(i in 1:length(MaxSize)){

		load(file=file.path(project.datadirectory("bio.lobster"),"outputs","sim",paste0("simResults",names(MaxSize)[i],".rdata")))
		simBubPlot(rlist,graphic='png',path=file.path(project.datadirectory("bio.lobster"),"figures","LFA2733Framework2018","sim"),fn=names(MaxSize)[i],cex.lab=2,cex.axis=1.5)
		load(file=file.path(project.datadirectory("bio.lobster"),"outputs","sim",paste0("simResultsF",names(MaxSize)[i],".rdata")))
		simBubPlot(rlist,graphic='png',path=file.path(project.datadirectory("bio.lobster"),"figures","LFA2733Framework2018","sim"),fn=paste0("F",names(MaxSize)[i]),cex.lab=2,cex.axis=1.5)

	}

	LFAs = c("27N","27S",  "29", "30","31A","31B", "32", "33E", "33W")

	simSumLegalSize = simSummary(runs=c("LS70","LS72.5","LS75","LS77.5","LS80","Base","LS85","LS87.5","LS90"),lfas=LFAs)
	simSumSeason = simSummary(runs=c("Base","SS9","SS8","SS7","SS6","SS5"),lfas=LFAs)
	simSumWindow = simSummary(runs=c("Base","FSmallWin","FBigWin","SmallWin","BigWin"),lfas=LFAs)
	simSumMaxSize = simSummary(runs=c("Base","FMax125","FMax130","FMax135","Max125","Max130","Max135"),lfas=LFAs)


	LStab1 = data.frame(rbind(
		round(100*(simSumLegalSize[[1]][9,]/simSumLegalSize[[1]][6,]-1)),
		round(100*(simSumLegalSize[[1]][8,]/simSumLegalSize[[1]][6,]-1)),
		round(100*(simSumLegalSize[[1]][7,]/simSumLegalSize[[1]][6,]-1))
		))
	names(LStab1) = LFAs
	write.csv(LStab1,file.path(project.datadirectory("bio.lobster"),"outputs","sim","LStab1.csv"),row.names=F)

	LStab2 = data.frame(rbind(
		round(100*(simSumLegalSize[[2]][9,]/simSumLegalSize[[2]][6,]-1)),
		round(100*(simSumLegalSize[[2]][8,]/simSumLegalSize[[2]][6,]-1)),
		round(100*(simSumLegalSize[[2]][7,]/simSumLegalSize[[2]][6,]-1))
		))
	names(LStab2) = LFAs
	write.csv(LStab2,file.path(project.datadirectory("bio.lobster"),"outputs","sim","LStab2.csv"),row.names=F)

	LStab3 = data.frame(rbind(
		round(100*(simSumLegalSize[[3]][9,]/simSumLegalSize[[3]][6,]-1)),
		round(100*(simSumLegalSize[[3]][8,]/simSumLegalSize[[3]][6,]-1)),
		round(100*(simSumLegalSize[[3]][7,]/simSumLegalSize[[3]][6,]-1))
		))
	names(LStab3) = LFAs
	write.csv(LStab3,file.path(project.datadirectory("bio.lobster"),"outputs","sim","LStab3.csv"),row.names=F)

	OStab1 = data.frame(rbind(
		round(100*(simSumSeason[[1]][6,]/simSumSeason[[1]][1,]-1)),
		round(100*(simSumSeason[[1]][5,]/simSumSeason[[1]][1,]-1)),
		round(100*(simSumSeason[[1]][4,]/simSumSeason[[1]][1,]-1)),
		round(100*(simSumSeason[[1]][3,]/simSumSeason[[1]][1,]-1)),
		round(100*(simSumSeason[[1]][2,]/simSumSeason[[1]][1,]-1))
		))
	names(OStab1) = LFAs
	write.csv(OStab1,file.path(project.datadirectory("bio.lobster"),"outputs","sim","OStab1.csv"),row.names=F)
	OStab2 = data.frame(rbind(
		round(100*(simSumSeason[[2]][6,]/simSumSeason[[2]][1,]-1)),
		round(100*(simSumSeason[[2]][5,]/simSumSeason[[2]][1,]-1)),
		round(100*(simSumSeason[[2]][4,]/simSumSeason[[2]][1,]-1)),
		round(100*(simSumSeason[[2]][3,]/simSumSeason[[2]][1,]-1)),
		round(100*(simSumSeason[[2]][2,]/simSumSeason[[2]][1,]-1))
		))
	names(OStab2) = LFAs
	write.csv(OStab2,file.path(project.datadirectory("bio.lobster"),"outputs","sim","OStab2.csv"),row.names=F)
	OStab3 = data.frame(rbind(
		round(100*(simSumSeason[[3]][6,]/simSumSeason[[3]][1,]-1)),
		round(100*(simSumSeason[[3]][5,]/simSumSeason[[3]][1,]-1)),
		round(100*(simSumSeason[[3]][4,]/simSumSeason[[3]][1,]-1)),
		round(100*(simSumSeason[[3]][3,]/simSumSeason[[3]][1,]-1)),
		round(100*(simSumSeason[[3]][2,]/simSumSeason[[3]][1,]-1))
		))
	names(OStab3) = LFAs
	write.csv(OStab3,file.path(project.datadirectory("bio.lobster"),"outputs","sim","OStab3.csv"),row.names=F)


	Wintab1 = data.frame(rbind(
		round(100*(simSumWindow[[1]][5,]/simSumWindow[[1]][1,]-1)),
		round(100*(simSumWindow[[1]][4,]/simSumWindow[[1]][1,]-1)),
		round(100*(simSumWindow[[1]][3,]/simSumWindow[[1]][1,]-1)),
		round(100*(simSumWindow[[1]][2,]/simSumWindow[[1]][1,]-1))
		))
	names(Wintab1) = LFAs
	write.csv(Wintab1,file.path(project.datadirectory("bio.lobster"),"outputs","sim","Wintab1.csv"),row.names=F)
	Wintab2 = data.frame(rbind(
		round(100*(simSumWindow[[2]][5,]/simSumWindow[[2]][1,]-1)),
		round(100*(simSumWindow[[2]][4,]/simSumWindow[[2]][1,]-1)),
		round(100*(simSumWindow[[2]][3,]/simSumWindow[[2]][1,]-1)),
		round(100*(simSumWindow[[2]][2,]/simSumWindow[[2]][1,]-1))
		))
	names(Wintab2) = LFAs
	write.csv(Wintab2,file.path(project.datadirectory("bio.lobster"),"outputs","sim","Wintab2.csv"),row.names=F)
	Wintab3 = data.frame(rbind(
		round(100*(simSumWindow[[3]][5,]/simSumWindow[[3]][1,]-1)),
		round(100*(simSumWindow[[3]][4,]/simSumWindow[[3]][1,]-1)),
		round(100*(simSumWindow[[3]][3,]/simSumWindow[[3]][1,]-1)),
		round(100*(simSumWindow[[3]][2,]/simSumWindow[[3]][1,]-1))
		))
	names(Wintab3) = LFAs
	write.csv(Wintab3,file.path(project.datadirectory("bio.lobster"),"outputs","sim","Wintab3.csv"),row.names=F)



	Maxtab1 = data.frame(rbind(
		round(100*(simSumMaxSize[[1]][7,]/simSumMaxSize[[1]][1,]-1)),
		round(100*(simSumMaxSize[[1]][6,]/simSumMaxSize[[1]][1,]-1)),
		round(100*(simSumMaxSize[[1]][5,]/simSumMaxSize[[1]][1,]-1)),
		round(100*(simSumMaxSize[[1]][4,]/simSumMaxSize[[1]][1,]-1)),
		round(100*(simSumMaxSize[[1]][3,]/simSumMaxSize[[1]][1,]-1)),
		round(100*(simSumMaxSize[[1]][2,]/simSumMaxSize[[1]][1,]-1))
		))
	names(Maxtab1) = LFAs
	write.csv(Maxtab1,file.path(project.datadirectory("bio.lobster"),"outputs","sim","Maxtab1.csv"),row.names=F)
	Maxtab2 = data.frame(rbind(
		round(100*(simSumMaxSize[[2]][7,]/simSumMaxSize[[2]][1,]-1)),
		round(100*(simSumMaxSize[[2]][6,]/simSumMaxSize[[2]][1,]-1)),
		round(100*(simSumMaxSize[[2]][5,]/simSumMaxSize[[2]][1,]-1)),
		round(100*(simSumMaxSize[[2]][4,]/simSumMaxSize[[2]][1,]-1)),
		round(100*(simSumMaxSize[[2]][3,]/simSumMaxSize[[2]][1,]-1)),
		round(100*(simSumMaxSize[[2]][2,]/simSumMaxSize[[2]][1,]-1))
		))
	names(Maxtab2) = LFAs
	write.csv(Maxtab2,file.path(project.datadirectory("bio.lobster"),"outputs","sim","Maxtab2.csv"),row.names=F)
	Maxtab3 = data.frame(rbind(
		round(100*(simSumMaxSize[[3]][7,]/simSumMaxSize[[3]][1,]-1)),
		round(100*(simSumMaxSize[[3]][6,]/simSumMaxSize[[3]][1,]-1)),
		round(100*(simSumMaxSize[[3]][5,]/simSumMaxSize[[3]][1,]-1)),
		round(100*(simSumMaxSize[[3]][4,]/simSumMaxSize[[3]][1,]-1)),
		round(100*(simSumMaxSize[[3]][3,]/simSumMaxSize[[3]][1,]-1)),
		round(100*(simSumMaxSize[[3]][2,]/simSumMaxSize[[3]][1,]-1))
		))
	names(Maxtab3) = LFAs
	write.csv(Maxtab3,file.path(project.datadirectory("bio.lobster"),"outputs","sim","Maxtab3.csv"),row.names=F)


	cols=tim.colors(9)
	ltys=1:9
	pchs=c(1:6,15:17)
	

	#x11()
	png(file.path(project.datadirectory('bio.lobster'),'figures','LFA2733Framework2018','simSumLegalSize.png'),width=8,height=8,units='in',res=200)
	par(mfrow=c(3,1),mar=c(0,5,0,0),omi=c(0.75,0,0.5,0.5),las=1)

	matplot(x=seq(70,90,2.5),simSumLegalSize$landnum,type = 'b', pch=pchs,col=cols,lty=ltys,ylab='Catch (#s)',ylim=c(0,2000))
	axis(4)
	abline(v=82.5,lty=3,col='grey')
	legend('bottomleft',LFAs,col=cols,lty=ltys,inset=0.03,bty='n', pch=pchs)
	matplot(x=seq(70,90,2.5),simSumLegalSize$landkg,type = 'b', pch=pchs,col=cols,lty=ltys,ylab='Catch (kg)',ylim=c(0,2000))
	axis(4)
	abline(v=82.5,lty=3,col='grey')
	matplot(x=seq(70,90,2.5),simSumLegalSize$eggs/10^6,type = 'b', pch=pchs,col=cols,lty=ltys,ylab='Eggs (millions)',ylim=c(0,25))
	axis(4)
	abline(v=82.5,lty=3,col='grey')
	mtext("Minimum Legal Size (mm)",1,3)

	dev.off()

	#x11()
	png(file.path(project.datadirectory('bio.lobster'),'figures','LFA2733Framework2018','simSumSeason.png'),width=8,height=8,units='in',res=200)
	par(mfrow=c(3,1),mar=c(0,5,0,0),omi=c(0.75,0,0.5,0.5),las=1)

	matplot(x=seq(0,50,10),simSumSeason$landnum,type = 'b', pch=pchs,col=cols,lty=ltys,ylab='Catch (#s)',ylim=c(0,2000))
	axis(4)
	#abline(v=0,lty=3,col='grey')
	legend('bottomleft',LFAs,col=cols,lty=ltys,inset=0.03,bty='n', pch=pchs)
	matplot(x=seq(0,50,10),simSumSeason$landkg,type = 'b', pch=pchs,col=cols,lty=ltys,ylab='Catch (kg)',ylim=c(0,2000))
	axis(4)
	#abline(v=0,lty=3,col='grey')
	matplot(x=seq(0,50,10),simSumSeason$eggs/10^6,type = 'b', pch=pchs,col=cols,lty=ltys,ylab='Eggs (millions)',ylim=c(0,25))
	axis(4)
	#abline(v=0,lty=3,col='grey')
	mtext("Season reduction (%)",1,3)

	dev.off()



	#####
	x11()
	#png(file.path(project.datadirectory('bio.lobster'),'figures','LFA2733Framework2018','simSumLegalSize.png'),width=8,height=8,units='in',res=200)
	par(mfrow=c(3,1),mar=c(0,5,0,0),omi=c(0.75,0,0.5,0.5),las=1)

	matplot(x=seq(82.5,90,2.5),simSumLegalSize$landnum[6:9,],type = 'b', pch=pchs,col=cols,lty=ltys,ylab='Catch (#s)',ylim=c(0,2000))
	par(new=T)
	matplot(x=seq(0,50,10),simSumSeason$landnum,type = 'b', pch=pchs,col=cols,lty=ltys,ylab='Catch (#s)',ylim=c(0,2000))
	


	#abline(v=82.5,lty=3,col='grey')
	#legend('bottomleft',LFAs,col=cols,lty=ltys,inset=0.03,bty='n', pch=pchs)
	matplot(x=seq(82.5,90,2.5),simSumLegalSize$landkg[6:9,],type = 'b', pch=pchs,col=cols,lty=ltys,ylab='Catch (kg)',ylim=c(0,2000))
	par(new=T)
	matplot(x=seq(0,50,10),simSumSeason$landkg,type = 'b', pch=pchs,col=cols,lty=ltys,ylab='Catch (kg)',ylim=c(0,2000))
	#abline(v=82.5,lty=3,col='grey')
	matplot(x=seq(82.5,90,2.5),simSumLegalSize$eggs[6:9,]/10^6,type = 'b', pch=pchs,col=cols,lty=ltys,ylab='Eggs (millions)',ylim=c(0,25))
	par(new=T)
	matplot(x=seq(0,50,10),simSumSeason$eggs/10^6,type = 'b', pch=pchs,col=cols,lty=ltys,ylab='Eggs (millions)',ylim=c(0,25))
	#abline(v=82.5,lty=3,col='grey')
	mtext("Minimum Legal Size (mm)",1,3)

	#dev.off()
	par(new=T)
	#x11()
	#png(file.path(project.datadirectory('bio.lobster'),'figures','LFA2733Framework2018','simSumSeason.png'),width=8,height=8,units='in',res=200)
	par(mfrow=c(3,1),mar=c(0,5,0,0),omi=c(0.75,0,0.5,0.5),las=1)

	matplot(x=seq(0,50,10),simSumSeason$landnum,type = 'b', pch=pchs,col=cols,lty=ltys,ylab='Catch (#s)',ylim=c(0,2000))
	#abline(v=0,lty=3,col='grey')
	legend('bottomleft',LFAs,col=cols,lty=ltys,inset=0.03,bty='n', pch=pchs)
	matplot(x=seq(0,50,10),simSumSeason$landkg,type = 'b', pch=pchs,col=cols,lty=ltys,ylab='Catch (kg)',ylim=c(0,2000))
	#abline(v=0,lty=3,col='grey')
	matplot(x=seq(0,50,10),simSumSeason$eggs/10^6,type = 'b', pch=pchs,col=cols,lty=ltys,ylab='Eggs (millions)',ylim=c(0,25))
	#abline(v=0,lty=3,col='grey')
	mtext("Season reduction (%)",1,3)

	dev.off()


# from 1.IndicatorEstimation.CohortAnalysis.r
ad
outS
bins=seq(80,150,5)

### LFA 27

is=which(ad$LFA==27&ad$YEAR>2008)
CLF.lst=list()
lens=NULL
for(i in is) lens = c(lens,outS[[i]])

CLF.lst[[1]]=hist(lens[lens>82.5&lens<max(bins)],breaks=bins,plot=F)$counts

load(file=file.path(project.datadirectory("bio.lobster"),"outputs","sim","simResultsBase.rdata"))

CLF.lst[[2]]=
colSums(rlist$mlist$'27N'$totalRemovals[,7:31])+
colSums(rlist$mlist$'27S'$totalRemovals[,7:31])+
colSums(rlist$flist$'27N'$totalRemovals[,7:31])+
colSums(rlist$flist$'27S'$totalRemovals[,7:31])


compareCLF(CLF.lst,bins=bins,col='grey',rel=T,graphic='pdf',filen=file.path(project.datadirectory('bio.lobster'),'figures','LFA2733Framework2018',"CLFcompareLFA27a.pdf"),LS=c(82.5,82.5),xl=c(0,length(bins)),title="LFA 27 (2009-2015)",labels=c("At Sea Sampling","Simulation"))

### LFA 33

is=which(ad$LFA==33&ad$YEAR>2001)
CLF.lst=list()
lens=NULL
for(i in is) lens = c(lens,outS[[i]])

CLF.lst[[1]]=hist(lens[lens>82.5&lens<max(bins)],breaks=bins,plot=F)$counts

#load(file=file.path(project.datadirectory("bio.lobster"),"outputs","sim","simResultsBase.rdata"))

CLF.lst[[2]]=
colSums(rlist$mlist$'33W'$totalRemovals[,7:31])+
colSums(rlist$mlist$'33E'$totalRemovals[,7:31])+
colSums(rlist$flist$'33W'$totalRemovals[,7:31])+
colSums(rlist$flist$'33E'$totalRemovals[,7:31])

compareCLF(CLF.lst,bins=bins,col='grey',rel=T,graphic='pdf',filen=file.path(project.datadirectory('bio.lobster'),'figures','LFA2733Framework2018',"CLFcompareLFA33.pdf"),LS=c(82.5,82.5),xl=c(0,length(bins)),title="LFA 33",labels=c("At Sea Sampling","Simulation"))




### LFA 30

is=which(ad$LFA==30&ad$YEAR>1999)
CLF.lst=list()
lens=NULL
for(i in is) lens = c(lens,outS[[i]])

CLF.lst[[1]]=hist(lens[lens>82.5&lens<max(bins)],breaks=bins,plot=F)$counts

#load(file=file.path(project.datadirectory("bio.lobster"),"outputs","sim","simResultsBase.rdata"))

CLF.lst[[2]]=
colSums(rlist$mlist$'30'$totalRemovals[,7:31])+
colSums(rlist$flist$'30'$totalRemovals[,7:31])

compareCLF(CLF.lst,bins=bins,col='grey',rel=T,graphic='pdf',filen=file.path(project.datadirectory('bio.lobster'),'figures','LFA2733Framework2018',"CLFcompareLFA30.pdf"),LS=c(82.5,82.5),xl=c(0,length(bins)),title="LFA 30",labels=c("At Sea Sampling","Simulation"))



### LFA 27
bins=seq(70,150,5)

is=which(ad$LFA==27&ad$YEAR<2001)
CLF.lst=list()
lens=NULL
for(i in is) lens = c(lens,outS[[i]])

CLF.lst[[1]]=hist(lens[lens>70&lens<max(bins)],breaks=bins,plot=F)$counts

load(file=file.path(project.datadirectory("bio.lobster"),"outputs","sim","simResultsLS70.rdata"))

CLF.lst[[2]]=
colSums(rlist$mlist$'27N'$totalRemovals[,7:31])+
colSums(rlist$mlist$'27S'$totalRemovals[,7:31])+
colSums(rlist$flist$'27N'$totalRemovals[,7:31])+
colSums(rlist$flist$'27S'$totalRemovals[,7:31])


compareCLF(CLF.lst,bins=bins,col='grey',rel=T,graphic='pdf',filen=file.path(project.datadirectory('bio.lobster'),'figures','LFA2733Framework2018',"CLFcompareLFA27b.pdf"),LS=c(70,70),xl=c(0,length(bins)),title="LFA 27 (1985-1999)",labels=c("At Sea Sampling","Simulation"))



