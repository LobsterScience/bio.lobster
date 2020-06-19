
# Lobster population simulation 
require(bio.lobster)
la()
p = bio.lobster::load.environment()
require(fields)

la()

	SoMplot(Areas='36',graphic='png',fp=file.path(project.datadirectory('bio.lobster'),'figures','LFA3438Framework2019'),LS=82.5,legend=F)


	TempModelling = TempModel(areas = 'subarea')
	#TempModelPlot(TempModelling,xlim=c(1980,2017),depths=c(5,25,50),Area=c("27N","27S", "29", "30","31A","31B", "32", "33E", "33W"),graphic='R')
	p$TempModel = TempModelling$Model
	TempModelling = TempModel(areas = 'subarea',annual.by.area=T)
	tempModel=TempModelPlot(TempModelling,xlim=c(1980,2018),depths=c(5,25,50),Area=p$subareas,graphic='R',type=1:3)

	MoltModelling = moltModel(p,redo.dd=F)
	p$moltModel = MoltModelling
	#moltModelPlot(p$moltModel,graphic='png')


p$lfas = c("36") # specify lfa

#low E
 p$season = c("1999-11-15","2000-01-15","2000-03-31","2000-06-29") 
 e= 0.6789005
 t = as.numeric(as.Date(p$season[2])-as.Date(p$season[1])+as.Date(p$season[4])-as.Date(p$season[3]))/365
 p$F = -log(1-e)/t


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
	save("rlist",file=file.path(project.datadirectory("bio.lobster"),"outputs","sim","sim36ResultsBase.rdata"))


		simBubPlot(rlist,graphic='R',cex.lab=2,cex.axis=1.5)
		round(rlist$mlist$'36'$moltProb[,,1,1],2)


##### Legal Size

	LegalSize = c(85,87.5,90)
	names(LegalSize) = paste0("LS",LegalSize)

	for(i in 1:length(LegalSize)){
		
		plist = getSimList(p,sex=1,LS=LegalSize[i])
		names(plist) = p$lfas

		#simMolt(plist)
		mlist = mclapply(X = plist, FUN = simMolt, mc.cores=length(p$lfas))
		names(mlist) = p$lfas

		plist = getSimList(p,sex=2,LS=LegalSize[i])
		names(plist) = p$lfas

		flist = mclapply(X = plist, FUN = simMolt, mc.cores=length(p$lfas))
		names(flist) = p$lfas

		rlist = list(plist=plist,mlist=mlist,flist=flist)
		save("rlist",file=file.path(project.datadirectory("bio.lobster"),"outputs","sim",paste0("sim36Results",names(LegalSize)[i],".rdata")))
	}


############ Season reduction

	ShorterSeason = c(0.9,0.8,0.7,0.6,0.5)
	names(ShorterSeason) = paste0("SS",9:5)
	
	for(i in 1:length(ShorterSeason)){

		plist = getSimList(p,sex=1,Sadj=ShorterSeason[i], Sclose='start')
		names(plist) = p$lfas

		#mlist = simMolt(plist[[1]])

		mlist = mclapply(X = plist, FUN = simMolt, mc.cores=length(p$lfas))
		names(mlist) = p$lfas

		plist = getSimList(p,sex=2,Sadj=ShorterSeason[i])
		names(plist) = p$lfas

		flist = mclapply(X = plist, FUN = simMolt, mc.cores=length(p$lfas))
		names(flist) = p$lfas

		rlist = list(plist=plist,mlist=mlist,flist=flist)
		save("rlist",file=file.path(project.datadirectory("bio.lobster"),"outputs","sim",paste0("sim36Results",names(ShorterSeason)[i],".rdata")))

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
		save("rlist",file=file.path(project.datadirectory("bio.lobster"),"outputs","sim",paste0("sim36Results",names(WindowSize)[i],".rdata")))
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
		save("rlist",file=file.path(project.datadirectory("bio.lobster"),"outputs","sim",paste0("sim36Results",names(MaxSize)[i],".rdata")))
	}




	WindowSize = list(c(115,125),c(105,125))
	names(WindowSize) = c("SmallWin","BigWin")


	for(i in 1:length(WindowSize)){

		load(file=file.path(project.datadirectory("bio.lobster"),"outputs","sim","sim36ResultsBase.rdata"))
		rlistb = rlist
		load(file=file.path(project.datadirectory("bio.lobster"),"outputs","sim",paste0("sim36Results",names(WindowSize)[i],".rdata")))
		rlist1 = rlist

		#females only
		rlist = list(plist=rlist1$plist,mlist=rlistb$mlist,flist=rlist1$flist)
		save("rlist",file=file.path(project.datadirectory("bio.lobster"),"outputs","sim",paste0("sim36ResultsF",names(WindowSize)[i],".rdata")))



	}


	MaxSize = list(c(135,210),c(130,210),c(125,210))
	names(MaxSize) = paste0("Max",lapply(MaxSize,min))

	for(i in 1:length(MaxSize)){

		load(file=file.path(project.datadirectory("bio.lobster"),"outputs","sim","sim36ResultsBase.rdata"))
		rlistb = rlist
		load(file=file.path(project.datadirectory("bio.lobster"),"outputs","sim",paste0("sim36Results",names(MaxSize)[i],".rdata")))
		rlist1 = rlist

		#females only
		rlist = list(plist=rlist1$plist,mlist=rlistb$mlist,flist=rlist1$flist)
		save("rlist",file=file.path(project.datadirectory("bio.lobster"),"outputs","sim",paste0("sim36ResultsF",names(MaxSize)[i],".rdata")))

	}


############# Plots

	load(file=file.path(project.datadirectory("bio.lobster"),"outputs","sim","sim36ResultsBase.rdata"))
	simBubPlot(rlist,graphic='R',path=file.path(project.datadirectory("bio.lobster"),"figures","LFA3438Framework2019","sim"),fn='Base',cex.lab=2,cex.axis=1.5)


	for(i in 1:length(ShorterSeason)){

		load(file=file.path(project.datadirectory("bio.lobster"),"outputs","sim",paste0("sim36Results",names(ShorterSeason)[i],".rdata")))
		simBubPlot(rlist,graphic='png',path=file.path(project.datadirectory("bio.lobster"),"figures","LFA3438Framework2019","sim"),fn=names(ShorterSeason)[i],cex.lab=2,cex.axis=1.5)

	}

	for(i in 1:length(LegalSize)){

		load(file=file.path(project.datadirectory("bio.lobster"),"outputs","sim",paste0("sim36Results",names(LegalSize)[i],".rdata")))
		simBubPlot(rlist,graphic='png',path=file.path(project.datadirectory("bio.lobster"),"figures","LFA3438Framework2019","sim"),fn=names(LegalSize)[i],cex.lab=2,cex.axis=1.5)

	}

	for(i in 1:length(WindowSize)){

		load(file=file.path(project.datadirectory("bio.lobster"),"outputs","sim",paste0("sim36Results",names(WindowSize)[i],".rdata")))
		simBubPlot(rlist,graphic='png',path=file.path(project.datadirectory("bio.lobster"),"figures","LFA3438Framework2019","sim"),fn=names(WindowSize)[i],cex.lab=2,cex.axis=1.5)
		load(file=file.path(project.datadirectory("bio.lobster"),"outputs","sim",paste0("sim36ResultsF",names(WindowSize)[i],".rdata")))
		simBubPlot(rlist,graphic='png',path=file.path(project.datadirectory("bio.lobster"),"figures","LFA3438Framework2019","sim"),fn=paste0("F",names(WindowSize)[i]),cex.lab=2,cex.axis=1.5)

	}

	for(i in 1:length(MaxSize)){

		load(file=file.path(project.datadirectory("bio.lobster"),"outputs","sim",paste0("sim36Results",names(MaxSize)[i],".rdata")))
		simBubPlot(rlist,graphic='png',path=file.path(project.datadirectory("bio.lobster"),"figures","LFA3438Framework2019","sim"),fn=names(MaxSize)[i],cex.lab=2,cex.axis=1.5)
		load(file=file.path(project.datadirectory("bio.lobster"),"outputs","sim",paste0("sim36ResultsF",names(MaxSize)[i],".rdata")))
		simBubPlot(rlist,graphic='png',path=file.path(project.datadirectory("bio.lobster"),"figures","LFA3438Framework2019","sim"),fn=paste0("F",names(MaxSize)[i]),cex.lab=2,cex.axis=1.5)

	}

	LFAs = c("36")

	simSumLegalSize = simSummary(runs=c("Base","LS85","LS87.5","LS90"),lfas=LFAs,lab=36)
	simSumSeason = simSummary(runs=c("Base","SS9","SS8","SS7","SS6","SS5"),lfas=LFAs,lab=36)
	simSumWindow = simSummary(runs=c("Base","FSmallWin","FBigWin","SmallWin","BigWin"),lfas=LFAs,lab=36)
	simSumMaxSize = simSummary(runs=c("Base","FMax125","FMax130","FMax135","Max125","Max130","Max135"),lfas=LFAs,lab=36)


	LStabRP = data.frame(rbind(
		round(100*(simSumLegalSize[[1]][4,]/simSumLegalSize[[1]][1,]-1)),
		round(100*(simSumLegalSize[[1]][3,]/simSumLegalSize[[1]][1,]-1)),
		round(100*(simSumLegalSize[[1]][2,]/simSumLegalSize[[1]][1,]-1))
		))
	names(LStabRP) = LFAs
	rownames(LStabRP) = c("LS82.5", "LS80", "LS78.5")
	write.csv(LStabRP,file.path(project.datadirectory("bio.lobster"),"outputs","sim","LStab36RP70.csv"),row.names=T)

	LStabNo = data.frame(rbind(
		round(100*(simSumLegalSize[[2]][4,]/simSumLegalSize[[2]][1,]-1)),
		round(100*(simSumLegalSize[[2]][3,]/simSumLegalSize[[2]][1,]-1)),
		round(100*(simSumLegalSize[[2]][2,]/simSumLegalSize[[2]][1,]-1))
		))
	names(LStabNo) = LFAs
	rownames(LStabNo) = c("LS82.5", "LS80", "LS78.5")
	write.csv(LStabNo,file.path(project.datadirectory("bio.lobster"),"outputs","sim","LStab36No70.csv"),row.names=T)

	LStabKg = data.frame(rbind(
		round(100*(simSumLegalSize[[3]][4,]/simSumLegalSize[[3]][1,]-1)),
		round(100*(simSumLegalSize[[3]][3,]/simSumLegalSize[[3]][1,]-1)),
		round(100*(simSumLegalSize[[3]][2,]/simSumLegalSize[[3]][1,]-1))
		))
	names(LStabKg) = LFAs
	rownames(LStabKg) = c("LS82.5", "LS80", "LS78.5")
	write.csv(LStabKg,file.path(project.datadirectory("bio.lobster"),"outputs","sim","LStab36Kg70.csv"),row.names=T)

	OStabRP = data.frame(rbind(
		round(100*(simSumSeason[[1]][6,]/simSumSeason[[1]][1,]-1)),
		round(100*(simSumSeason[[1]][5,]/simSumSeason[[1]][1,]-1)),
		round(100*(simSumSeason[[1]][4,]/simSumSeason[[1]][1,]-1)),
		round(100*(simSumSeason[[1]][3,]/simSumSeason[[1]][1,]-1)),
		round(100*(simSumSeason[[1]][2,]/simSumSeason[[1]][1,]-1))
		))
	names(OStabRP) = LFAs
	rownames(OStabRP) = c("SS5", "SS6", "SS7", "SS8", "SS9")
	write.csv(OStabRP,file.path(project.datadirectory("bio.lobster"),"outputs","sim","OStab36RP.csv"),row.names=T)
	OStabNo = data.frame(rbind(
		round(100*(simSumSeason[[2]][6,]/simSumSeason[[2]][1,]-1)),
		round(100*(simSumSeason[[2]][5,]/simSumSeason[[2]][1,]-1)),
		round(100*(simSumSeason[[2]][4,]/simSumSeason[[2]][1,]-1)),
		round(100*(simSumSeason[[2]][3,]/simSumSeason[[2]][1,]-1)),
		round(100*(simSumSeason[[2]][2,]/simSumSeason[[2]][1,]-1))
		))
	names(OStabNo) = LFAs
	rownames(OStabNo) = c("SS5", "SS6", "SS7", "SS8", "SS9")
	write.csv(OStabNo,file.path(project.datadirectory("bio.lobster"),"outputs","sim","OStab36No.csv"),row.names=T)
	OStabKg = data.frame(rbind(
		round(100*(simSumSeason[[3]][6,]/simSumSeason[[3]][1,]-1)),
		round(100*(simSumSeason[[3]][5,]/simSumSeason[[3]][1,]-1)),
		round(100*(simSumSeason[[3]][4,]/simSumSeason[[3]][1,]-1)),
		round(100*(simSumSeason[[3]][3,]/simSumSeason[[3]][1,]-1)),
		round(100*(simSumSeason[[3]][2,]/simSumSeason[[3]][1,]-1))
		))
	names(OStabKg) = LFAs
	rownames(OStabNo) = c("SS5", "SS6", "SS7", "SS8", "SS9")
	write.csv(OStabKg,file.path(project.datadirectory("bio.lobster"),"outputs","sim","OStab36Kg.csv"),row.names=T)

	WintabRP = data.frame(rbind(
		round(100*(simSumWindow[[1]][5,]/simSumWindow[[1]][1,]-1)),
		round(100*(simSumWindow[[1]][4,]/simSumWindow[[1]][1,]-1)),
		round(100*(simSumWindow[[1]][3,]/simSumWindow[[1]][1,]-1)),
		round(100*(simSumWindow[[1]][2,]/simSumWindow[[1]][1,]-1))
		))
	names(WintabRP) = LFAs
	rownames(WintabRP) = c("BigWin", "SmallWin","FBigWin", "FSmallWin")
	write.csv(WintabRP,file.path(project.datadirectory("bio.lobster"),"outputs","sim","Wintab36RP.csv"),row.names=T)
	WintabNo = data.frame(rbind(
		round(100*(simSumWindow[[2]][5,]/simSumWindow[[2]][1,]-1)),
		round(100*(simSumWindow[[2]][4,]/simSumWindow[[2]][1,]-1)),
		round(100*(simSumWindow[[2]][3,]/simSumWindow[[2]][1,]-1)),
		round(100*(simSumWindow[[2]][2,]/simSumWindow[[2]][1,]-1))
		))
	names(WintabNo) = LFAs
	rownames(WintabNo) = c("BigWin", "SmallWin","FBigWin", "FSmallWin")
	write.csv(WintabNo,file.path(project.datadirectory("bio.lobster"),"outputs","sim","Wintab36No.csv"),row.names=T)
	WintabKg = data.frame(rbind(
		round(100*(simSumWindow[[3]][5,]/simSumWindow[[3]][1,]-1)),
		round(100*(simSumWindow[[3]][4,]/simSumWindow[[3]][1,]-1)),
		round(100*(simSumWindow[[3]][3,]/simSumWindow[[3]][1,]-1)),
		round(100*(simSumWindow[[3]][2,]/simSumWindow[[3]][1,]-1))
		))
	names(WintabKg) = LFAs
	rownames(WintabKg) = c("BigWin", "SmallWin","FBigWin", "FSmallWin")
	write.csv(WintabKg,file.path(project.datadirectory("bio.lobster"),"outputs","sim","Wintab36Kg.csv"),row.names=T)




	MaxtabRP = data.frame(rbind(
		round(100*(simSumMaxSize[[1]][7,]/simSumMaxSize[[1]][1,]-1)),
		round(100*(simSumMaxSize[[1]][6,]/simSumMaxSize[[1]][1,]-1)),
		round(100*(simSumMaxSize[[1]][5,]/simSumMaxSize[[1]][1,]-1)),
		round(100*(simSumMaxSize[[1]][4,]/simSumMaxSize[[1]][1,]-1)),
		round(100*(simSumMaxSize[[1]][3,]/simSumMaxSize[[1]][1,]-1)),
		round(100*(simSumMaxSize[[1]][2,]/simSumMaxSize[[1]][1,]-1))
		))
	names(MaxtabRP) = LFAs
	rownames(MaxtabRP) = c("Max135", "Max130", "Max125", "FMax135", "FMax130", "FMax125")
	write.csv(MaxtabRP,file.path(project.datadirectory("bio.lobster"),"outputs","sim","Maxtab36RP.csv"),row.names=T)
	MaxtabNo = data.frame(rbind(
		round(100*(simSumMaxSize[[2]][7,]/simSumMaxSize[[2]][1,]-1)),
		round(100*(simSumMaxSize[[2]][6,]/simSumMaxSize[[2]][1,]-1)),
		round(100*(simSumMaxSize[[2]][5,]/simSumMaxSize[[2]][1,]-1)),
		round(100*(simSumMaxSize[[2]][4,]/simSumMaxSize[[2]][1,]-1)),
		round(100*(simSumMaxSize[[2]][3,]/simSumMaxSize[[2]][1,]-1)),
		round(100*(simSumMaxSize[[2]][2,]/simSumMaxSize[[2]][1,]-1))
		))
	names(MaxtabNo) = LFAs
	rownames(MaxtabNo) = c("Max135", "Max130", "Max125", "FMax135", "FMax130", "FMax125")
	write.csv(MaxtabNo,file.path(project.datadirectory("bio.lobster"),"outputs","sim","Maxtab36No.csv"),row.names=T)
	MaxtabKg = data.frame(rbind(
		round(100*(simSumMaxSize[[3]][7,]/simSumMaxSize[[3]][1,]-1)),
		round(100*(simSumMaxSize[[3]][6,]/simSumMaxSize[[3]][1,]-1)),
		round(100*(simSumMaxSize[[3]][5,]/simSumMaxSize[[3]][1,]-1)),
		round(100*(simSumMaxSize[[3]][4,]/simSumMaxSize[[3]][1,]-1)),
		round(100*(simSumMaxSize[[3]][3,]/simSumMaxSize[[3]][1,]-1)),
		round(100*(simSumMaxSize[[3]][2,]/simSumMaxSize[[3]][1,]-1))
		))
	names(MaxtabKg) = LFAs
	rownames(MaxtabKg) = c("Max135", "Max130", "Max125", "FMax135", "FMax130", "FMax125")
	write.csv(MaxtabKg,file.path(project.datadirectory("bio.lobster"),"outputs","sim","Maxtab36Kg.csv"),row.names=T)

	for(i in 1){

		LS=data.frame(Eggs=LStabRP[,i],Numbers=LStabNo[,i],Weight=LStabKg[,i],row.names= c("LS90", "LS87.5", "LS85"))
		OS=data.frame(Eggs=OStabRP[,i],Numbers=OStabNo[,i],Weight=OStabKg[,i],row.names= c("SS5", "SS6", "SS7", "SS8", "SS9"))
		Win=data.frame(Eggs=WintabRP[,i],Numbers=WintabNo[,i],Weight=WintabKg[,i],row.names= c("BigWin", "SmallWin","FBigWin", "FSmallWin"))
		Max=data.frame(Eggs=MaxtabRP[,i],Numbers=MaxtabNo[,i],Weight=MaxtabKg[,i],row.names= c("Max135", "Max130", "Max125", "FMax135", "FMax130", "FMax125"))
		write.csv(rbind(LS,OS,Win,Max),file.path(project.datadirectory("bio.lobster"),"outputs","sim",paste0("LFA",LFAs[i],".csv")),row.names=T)
	}
##################################################################################################################################################################
#high E
#################################################################################################################################################################

 p$season = c("1999-11-15","2000-01-15","2000-03-31","2000-06-29") 
 e= 0.82
 t = as.numeric(as.Date(p$season[2])-as.Date(p$season[1])+as.Date(p$season[4])-as.Date(p$season[3]))/365
 p$F = -log(1-e)/t


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
	save("rlist",file=file.path(project.datadirectory("bio.lobster"),"outputs","sim","sim36ResultsBaseHighF.rdata"))


		simBubPlot(rlist,graphic='R',cex.lab=2,cex.axis=1.5)
		round(rlist$mlist$'36'$moltProb[,,1,1],2)


##### Legal Size

	LegalSize = c(85,87.5,90)
	names(LegalSize) = paste0("LS",LegalSize)

	for(i in 1:length(LegalSize)){
		
		plist = getSimList(p,sex=1,LS=LegalSize[i])
		names(plist) = p$lfas

		#simMolt(plist)
		mlist = mclapply(X = plist, FUN = simMolt, mc.cores=length(p$lfas))
		names(mlist) = p$lfas

		plist = getSimList(p,sex=2,LS=LegalSize[i])
		names(plist) = p$lfas

		flist = mclapply(X = plist, FUN = simMolt, mc.cores=length(p$lfas))
		names(flist) = p$lfas

		rlist = list(plist=plist,mlist=mlist,flist=flist)
		save("rlist",file=file.path(project.datadirectory("bio.lobster"),"outputs","sim",paste0("sim36Results",names(LegalSize)[i],"HighF.rdata")))
	}


############ Season reduction

	ShorterSeason = c(0.9,0.8,0.7,0.6,0.5)
	names(ShorterSeason) = paste0("SS",9:5)
	
	for(i in 1:length(ShorterSeason)){

		plist = getSimList(p,sex=1,Sadj=ShorterSeason[i], Sclose='start')
		names(plist) = p$lfas

		#mlist = simMolt(plist[[1]])

		mlist = mclapply(X = plist, FUN = simMolt, mc.cores=length(p$lfas))
		names(mlist) = p$lfas

		plist = getSimList(p,sex=2,Sadj=ShorterSeason[i])
		names(plist) = p$lfas

		flist = mclapply(X = plist, FUN = simMolt, mc.cores=length(p$lfas))
		names(flist) = p$lfas

		rlist = list(plist=plist,mlist=mlist,flist=flist)
		save("rlist",file=file.path(project.datadirectory("bio.lobster"),"outputs","sim",paste0("sim36Results",names(ShorterSeason)[i],"HighF.rdata")))

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
		save("rlist",file=file.path(project.datadirectory("bio.lobster"),"outputs","sim",paste0("sim36Results",names(WindowSize)[i],"HighF.rdata")))
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
		save("rlist",file=file.path(project.datadirectory("bio.lobster"),"outputs","sim",paste0("sim36Results",names(MaxSize)[i],"HighF.rdata")))
	}




	WindowSize = list(c(115,125),c(105,125))
	names(WindowSize) = c("SmallWin","BigWin")


	for(i in 1:length(WindowSize)){

		load(file=file.path(project.datadirectory("bio.lobster"),"outputs","sim","sim36ResultsBaseHighF.rdata"))
		rlistb = rlist
		load(file=file.path(project.datadirectory("bio.lobster"),"outputs","sim",paste0("sim36Results",names(WindowSize)[i],"HighF.rdata")))
		rlist1 = rlist

		#females only
		rlist = list(plist=rlist1$plist,mlist=rlistb$mlist,flist=rlist1$flist)
		save("rlist",file=file.path(project.datadirectory("bio.lobster"),"outputs","sim",paste0("sim36ResultsF",names(WindowSize)[i],"HighF.rdata")))



	}


	MaxSize = list(c(135,210),c(130,210),c(125,210))
	names(MaxSize) = paste0("Max",lapply(MaxSize,min))

	for(i in 1:length(MaxSize)){

		load(file=file.path(project.datadirectory("bio.lobster"),"outputs","sim","sim36ResultsBaseHighF.rdata"))
		rlistb = rlist
		load(file=file.path(project.datadirectory("bio.lobster"),"outputs","sim",paste0("sim36Results",names(MaxSize)[i],"HighF.rdata")))
		rlist1 = rlist

		#females only
		rlist = list(plist=rlist1$plist,mlist=rlistb$mlist,flist=rlist1$flist)
		save("rlist",file=file.path(project.datadirectory("bio.lobster"),"outputs","sim",paste0("sim36ResultsF",names(MaxSize)[i],"HighF.rdata")))

	}


############# Plots

	load(file=file.path(project.datadirectory("bio.lobster"),"outputs","sim","sim36ResultsBaseHighF.rdata"))
	simBubPlot(rlist,graphic='R',path=file.path(project.datadirectory("bio.lobster"),"figures","LFA3438Framework2019","sim"),fn='Base',cex.lab=2,cex.axis=1.5)


	for(i in 1:length(ShorterSeason)){

		load(file=file.path(project.datadirectory("bio.lobster"),"outputs","sim",paste0("sim36Results",names(ShorterSeason)[i],"HighF.rdata")))
		simBubPlot(rlist,graphic='png',path=file.path(project.datadirectory("bio.lobster"),"figures","LFA3438Framework2019","sim"),fn=names(ShorterSeason)[i],cex.lab=2,cex.axis=1.5)

	}

	for(i in 1:length(LegalSize)){

		load(file=file.path(project.datadirectory("bio.lobster"),"outputs","sim",paste0("sim36Results",names(LegalSize)[i],"HighF.rdata")))
		simBubPlot(rlist,graphic='png',path=file.path(project.datadirectory("bio.lobster"),"figures","LFA3438Framework2019","sim"),fn=names(LegalSize)[i],cex.lab=2,cex.axis=1.5)

	}

	for(i in 1:length(WindowSize)){

		load(file=file.path(project.datadirectory("bio.lobster"),"outputs","sim",paste0("sim36Results",names(WindowSize)[i],"HighF.rdata")))
		simBubPlot(rlist,graphic='png',path=file.path(project.datadirectory("bio.lobster"),"figures","LFA3438Framework2019","sim"),fn=names(WindowSize)[i],cex.lab=2,cex.axis=1.5)
		load(file=file.path(project.datadirectory("bio.lobster"),"outputs","sim",paste0("sim36ResultsF",names(WindowSize)[i],"HighF.rdata")))
		simBubPlot(rlist,graphic='png',path=file.path(project.datadirectory("bio.lobster"),"figures","LFA3438Framework2019","sim"),fn=paste0("F",names(WindowSize)[i]),cex.lab=2,cex.axis=1.5)

	}

	for(i in 1:length(MaxSize)){

		load(file=file.path(project.datadirectory("bio.lobster"),"outputs","sim",paste0("sim36Results",names(MaxSize)[i],"HighF.rdata")))
		simBubPlot(rlist,graphic='png',path=file.path(project.datadirectory("bio.lobster"),"figures","LFA3438Framework2019","sim"),fn=names(MaxSize)[i],cex.lab=2,cex.axis=1.5)
		load(file=file.path(project.datadirectory("bio.lobster"),"outputs","sim",paste0("sim36ResultsF",names(MaxSize)[i],"HighF.rdata")))
		simBubPlot(rlist,graphic='png',path=file.path(project.datadirectory("bio.lobster"),"figures","LFA3438Framework2019","sim"),fn=paste0("F",names(MaxSize)[i]),cex.lab=2,cex.axis=1.5)

	}

	LFAs = c("36")

	simSumLegalSize = simSummary(runs=c("BaseHighF","LS85HighF","LS87.5HighF","LS90HighF"),lfas=LFAs,lab=36)
	simSumSeason = simSummary(runs=c("BaseHighF","SS9HighF","SS8HighF","SS7HighF","SS6HighF","SS5HighF"),lfas=LFAs,lab=36)
	simSumWindow = simSummary(runs=c("BaseHighF","FSmallWinHighF","FBigWinHighF","SmallWinHighF","BigWinHighF"),lfas=LFAs,lab=36)
	simSumMaxSize = simSummary(runs=c("BaseHighF","FMax125HighF","FMax130HighF","FMax135HighF","Max125HighF","Max130HighF","Max135HighF"),lfas=LFAs,lab=36)


	LStabRP = data.frame(rbind(
		round(100*(simSumLegalSize[[1]][4,]/simSumLegalSize[[1]][1,]-1)),
		round(100*(simSumLegalSize[[1]][3,]/simSumLegalSize[[1]][1,]-1)),
		round(100*(simSumLegalSize[[1]][2,]/simSumLegalSize[[1]][1,]-1))
		))
	names(LStabRP) = LFAs
	rownames(LStabRP) = c("LS82.5", "LS80", "LS78.5")
	write.csv(LStabRP,file.path(project.datadirectory("bio.lobster"),"outputs","sim","LStab36RP70HighF.csv"),row.names=T)

	LStabNo = data.frame(rbind(
		round(100*(simSumLegalSize[[2]][4,]/simSumLegalSize[[2]][1,]-1)),
		round(100*(simSumLegalSize[[2]][3,]/simSumLegalSize[[2]][1,]-1)),
		round(100*(simSumLegalSize[[2]][2,]/simSumLegalSize[[2]][1,]-1))
		))
	names(LStabNo) = LFAs
	rownames(LStabNo) = c("LS82.5", "LS80", "LS78.5")
	write.csv(LStabNo,file.path(project.datadirectory("bio.lobster"),"outputs","sim","LStab36No70HighF.csv"),row.names=T)

	LStabKg = data.frame(rbind(
		round(100*(simSumLegalSize[[3]][4,]/simSumLegalSize[[3]][1,]-1)),
		round(100*(simSumLegalSize[[3]][3,]/simSumLegalSize[[3]][1,]-1)),
		round(100*(simSumLegalSize[[3]][2,]/simSumLegalSize[[3]][1,]-1))
		))
	names(LStabKg) = LFAs
	rownames(LStabKg) = c("LS82.5", "LS80", "LS78.5")
	write.csv(LStabKg,file.path(project.datadirectory("bio.lobster"),"outputs","sim","LStab36Kg70HighF.csv"),row.names=T)

	OStabRP = data.frame(rbind(
		round(100*(simSumSeason[[1]][6,]/simSumSeason[[1]][1,]-1)),
		round(100*(simSumSeason[[1]][5,]/simSumSeason[[1]][1,]-1)),
		round(100*(simSumSeason[[1]][4,]/simSumSeason[[1]][1,]-1)),
		round(100*(simSumSeason[[1]][3,]/simSumSeason[[1]][1,]-1)),
		round(100*(simSumSeason[[1]][2,]/simSumSeason[[1]][1,]-1))
		))
	names(OStabRP) = LFAs
	rownames(OStabRP) = c("SS5", "SS6", "SS7", "SS8", "SS9")
	write.csv(OStabRP,file.path(project.datadirectory("bio.lobster"),"outputs","sim","OStab36RPHighF.csv"),row.names=T)
	OStabNo = data.frame(rbind(
		round(100*(simSumSeason[[2]][6,]/simSumSeason[[2]][1,]-1)),
		round(100*(simSumSeason[[2]][5,]/simSumSeason[[2]][1,]-1)),
		round(100*(simSumSeason[[2]][4,]/simSumSeason[[2]][1,]-1)),
		round(100*(simSumSeason[[2]][3,]/simSumSeason[[2]][1,]-1)),
		round(100*(simSumSeason[[2]][2,]/simSumSeason[[2]][1,]-1))
		))
	names(OStabNo) = LFAs
	rownames(OStabNo) = c("SS5", "SS6", "SS7", "SS8", "SS9")
	write.csv(OStabNo,file.path(project.datadirectory("bio.lobster"),"outputs","sim","OStab36NoHighF.csv"),row.names=T)
	OStabKg = data.frame(rbind(
		round(100*(simSumSeason[[3]][6,]/simSumSeason[[3]][1,]-1)),
		round(100*(simSumSeason[[3]][5,]/simSumSeason[[3]][1,]-1)),
		round(100*(simSumSeason[[3]][4,]/simSumSeason[[3]][1,]-1)),
		round(100*(simSumSeason[[3]][3,]/simSumSeason[[3]][1,]-1)),
		round(100*(simSumSeason[[3]][2,]/simSumSeason[[3]][1,]-1))
		))
	names(OStabKg) = LFAs
	rownames(OStabNo) = c("SS5", "SS6", "SS7", "SS8", "SS9")
	write.csv(OStabKg,file.path(project.datadirectory("bio.lobster"),"outputs","sim","OStab36KgHighF.csv"),row.names=T)

	WintabRP = data.frame(rbind(
		round(100*(simSumWindow[[1]][5,]/simSumWindow[[1]][1,]-1)),
		round(100*(simSumWindow[[1]][4,]/simSumWindow[[1]][1,]-1)),
		round(100*(simSumWindow[[1]][3,]/simSumWindow[[1]][1,]-1)),
		round(100*(simSumWindow[[1]][2,]/simSumWindow[[1]][1,]-1))
		))
	names(WintabRP) = LFAs
	rownames(WintabRP) = c("BigWin", "SmallWin","FBigWin", "FSmallWin")
	write.csv(WintabRP,file.path(project.datadirectory("bio.lobster"),"outputs","sim","Wintab36RPHighF.csv"),row.names=T)
	WintabNo = data.frame(rbind(
		round(100*(simSumWindow[[2]][5,]/simSumWindow[[2]][1,]-1)),
		round(100*(simSumWindow[[2]][4,]/simSumWindow[[2]][1,]-1)),
		round(100*(simSumWindow[[2]][3,]/simSumWindow[[2]][1,]-1)),
		round(100*(simSumWindow[[2]][2,]/simSumWindow[[2]][1,]-1))
		))
	names(WintabNo) = LFAs
	rownames(WintabNo) = c("BigWin", "SmallWin","FBigWin", "FSmallWin")
	write.csv(WintabNo,file.path(project.datadirectory("bio.lobster"),"outputs","sim","Wintab36NoHighF.csv"),row.names=T)
	WintabKg = data.frame(rbind(
		round(100*(simSumWindow[[3]][5,]/simSumWindow[[3]][1,]-1)),
		round(100*(simSumWindow[[3]][4,]/simSumWindow[[3]][1,]-1)),
		round(100*(simSumWindow[[3]][3,]/simSumWindow[[3]][1,]-1)),
		round(100*(simSumWindow[[3]][2,]/simSumWindow[[3]][1,]-1))
		))
	names(WintabKg) = LFAs
	rownames(WintabKg) = c("BigWin", "SmallWin","FBigWin", "FSmallWin")
	write.csv(WintabKg,file.path(project.datadirectory("bio.lobster"),"outputs","sim","Wintab36KgHighF.csv"),row.names=T)




	MaxtabRP = data.frame(rbind(
		round(100*(simSumMaxSize[[1]][7,]/simSumMaxSize[[1]][1,]-1)),
		round(100*(simSumMaxSize[[1]][6,]/simSumMaxSize[[1]][1,]-1)),
		round(100*(simSumMaxSize[[1]][5,]/simSumMaxSize[[1]][1,]-1)),
		round(100*(simSumMaxSize[[1]][4,]/simSumMaxSize[[1]][1,]-1)),
		round(100*(simSumMaxSize[[1]][3,]/simSumMaxSize[[1]][1,]-1)),
		round(100*(simSumMaxSize[[1]][2,]/simSumMaxSize[[1]][1,]-1))
		))
	names(MaxtabRP) = LFAs
	rownames(MaxtabRP) = c("Max135", "Max130", "Max125", "FMax135", "FMax130", "FMax125")
	write.csv(MaxtabRP,file.path(project.datadirectory("bio.lobster"),"outputs","sim","Maxtab36RPHighF.csv"),row.names=T)
	MaxtabNo = data.frame(rbind(
		round(100*(simSumMaxSize[[2]][7,]/simSumMaxSize[[2]][1,]-1)),
		round(100*(simSumMaxSize[[2]][6,]/simSumMaxSize[[2]][1,]-1)),
		round(100*(simSumMaxSize[[2]][5,]/simSumMaxSize[[2]][1,]-1)),
		round(100*(simSumMaxSize[[2]][4,]/simSumMaxSize[[2]][1,]-1)),
		round(100*(simSumMaxSize[[2]][3,]/simSumMaxSize[[2]][1,]-1)),
		round(100*(simSumMaxSize[[2]][2,]/simSumMaxSize[[2]][1,]-1))
		))
	names(MaxtabNo) = LFAs
	rownames(MaxtabNo) = c("Max135", "Max130", "Max125", "FMax135", "FMax130", "FMax125")
	write.csv(MaxtabNo,file.path(project.datadirectory("bio.lobster"),"outputs","sim","Maxtab36NoHighF.csv"),row.names=T)
	MaxtabKg = data.frame(rbind(
		round(100*(simSumMaxSize[[3]][7,]/simSumMaxSize[[3]][1,]-1)),
		round(100*(simSumMaxSize[[3]][6,]/simSumMaxSize[[3]][1,]-1)),
		round(100*(simSumMaxSize[[3]][5,]/simSumMaxSize[[3]][1,]-1)),
		round(100*(simSumMaxSize[[3]][4,]/simSumMaxSize[[3]][1,]-1)),
		round(100*(simSumMaxSize[[3]][3,]/simSumMaxSize[[3]][1,]-1)),
		round(100*(simSumMaxSize[[3]][2,]/simSumMaxSize[[3]][1,]-1))
		))
	names(MaxtabKg) = LFAs
	rownames(MaxtabKg) = c("Max135", "Max130", "Max125", "FMax135", "FMax130", "FMax125")
	write.csv(MaxtabKg,file.path(project.datadirectory("bio.lobster"),"outputs","sim","Maxtab36KgHighF.csv"),row.names=T)

	for(i in 1){

		LS=data.frame(Eggs=LStabRP[,i],Numbers=LStabNo[,i],Weight=LStabKg[,i],row.names= c("LS90", "LS87.5", "LS85"))
		OS=data.frame(Eggs=OStabRP[,i],Numbers=OStabNo[,i],Weight=OStabKg[,i],row.names= c("SS5", "SS6", "SS7", "SS8", "SS9"))
		Win=data.frame(Eggs=WintabRP[,i],Numbers=WintabNo[,i],Weight=WintabKg[,i],row.names= c("BigWin", "SmallWin","FBigWin", "FSmallWin"))
		Max=data.frame(Eggs=MaxtabRP[,i],Numbers=MaxtabNo[,i],Weight=MaxtabKg[,i],row.names= c("Max135", "Max130", "Max125", "FMax135", "FMax130", "FMax125"))
		write.csv(rbind(LS,OS,Win,Max),file.path(project.datadirectory("bio.lobster"),"outputs","sim",paste0("LFA",LFAs[i],"HighF.csv")),row.names=T)
	}



######################################################################################################################################################################################




	cols=tim.colors(9)
	ltys=1:9
	pchs=c(1:6,15:17)
	#########################################################################################################################################################################################

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


### LFA 33

is=which(ad$LFA==33&ad$YEAR>2001)
CLF.lst=list()
lens=NULL
for(i in is) lens = c(lens,outS[[i]])

CLF.lst[[1]]=hist(lens[lens>82.5&lens<max(bins)],breaks=bins,plot=F)$counts

#load(file=file.path(project.datadirectory("bio.lobster"),"outputs","sim","sim34ResultsBase.rdata"))

CLF.lst[[2]]=
colSums(rlist$mlist$'33W'$totalRemovals[,7:31])+
colSums(rlist$mlist$'33E'$totalRemovals[,7:31])+
colSums(rlist$flist$'33W'$totalRemovals[,7:31])+
colSums(rlist$flist$'33E'$totalRemovals[,7:31])

compareCLF(CLF.lst,bins=bins,col='grey',rel=T,graphic='pdf',filen=file.path(project.datadirectory('bio.lobster'),'figures','LFA2733Framework2018',"CLFcompareLFA33.pdf"),LS=c(82.5,82.5),xl=c(0,length(bins)),title="LFA 33",labels=c("At Sea Sampling","Simulation"))







##############



	load(file=file.path(project.datadirectory("bio.lobster"),"outputs","sim","sim34ResultsBase.rdata"))

	transMat = getTransMatrix(rlist)
