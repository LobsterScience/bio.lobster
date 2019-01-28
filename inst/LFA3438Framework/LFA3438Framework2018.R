    
  
	p = bio.lobster::load.environment()
	la()


    p$syr = 2005
    p$yrs = p$syr:p$current.assessment.year

    figdir = file.path(project.datadirectory("bio.lobster"),"figures","LFA3438Framework2019")

    p$lfas = c('33',"34", "35", "36", "38") # specify lfas for data summary
  

### LobsterSurvey


	surveyLobsters34<-LobsterSurveyProcess(lfa="34",yrs=1996:2018,mths=c("Aug","Jul","Jun"),bin.size=5,gear.type='NEST')


	CarapaceLengthFrequencies(LFAs='34',DS='LobsterSurvey', Yrs=2016, gear.type='NEST', index.stations = F,graphic="png" ,rel=F,wd=8,ht=5,ymax=1600,fn="NEST",comparative=T)
	CarapaceLengthFrequencies(LFAs='34',DS='LobsterSurvey', Yrs=2016, gear.type='280 BALLOON', index.stations = F,graphic="png" ,rel=F,wd=8,ht=5,ymax=1600,fn="BALLOON")


	CarapaceLengthFrequencies(LFAs='34',DS='LobsterSurvey', Yrs=2005:2017, Net='280 BALLOON', index.stations = T,graphic="R" )
	CarapaceLengthFrequencies(LFAs='34',DS='LobsterSurvey', Yrs=2017, gear.type='NEST', Net='NEST', index.stations = F,graphic="R",fn="ILTS2017",wd=8,ht=8,rel=F )


	## Plot Distribution

		# interpolate abundance
		interp.data<-na.omit(subset(surveyLobsters34,YEAR==2017,c('SET_ID','SET_LONG','SET_LAT','LobDen')))
		lob.contours<-interpolation(interp.data,ticks='define',place=3,nstrata=5,str.min=0,interp.method='gstat',blank=T,res=0.005,smooth=F,idp=3.5,blank.dist=0.2)

		# define contour lines
		print(lob.contours$str.def)
		lvls=c(1, 50, 100, 500, 1000, 5000, 10000)

		# generate contour lines
		LFAs<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolys.csv"))

		cont.lst<-contourGen(lob.contours$image.dat,lvls,subset(LFAs,PID==34),col="YlGn",colorAdj=1)

		# plot Map
		pdf(file.path( project.datadirectory("bio.lobster"), "figures","Distribution2017.pdf"),8,11)
		#png(file.path( project.datadirectory("bio.lobster"), "figures","Distribution2017.png"),8,11,units='in',pointsize=12, res=300,type='cairo')
		LobsterMap(ylim=c(42.8,44.6), xlim=c(-67.15,-65.2),mapRes="UR",contours=cont.lst,title="LFA 34 Lobster Density",isobath=seq(50,500,50),bathcol=rgb(0,0,1,0.2),bathy.source='bathy')
		points(SET_LAT~SET_LONG,surveyLobsters34,subset=YEAR==2017,pch=21,cex=0.5,bg='red')#,col=rgb(0,0,0,0.5))
		contLegend("topright",lvls=lvls,Cont.data=cont.lst$Cont.data,title="#/square km",inset=0.02,cex=0.8,bg='white')
		dev.off()


	surveyLobsters34<-LobsterSurveyProcess(lfa="34",yrs=1996:2017,mths=c("Aug","Jul","Jun"),bin.size=5,gear.type='NEST')
	surveyLobsters34<-LobsterSurveyProcess(lfa="34",yrs=1996:2017,mths=c("Aug","Jul","Jun"),bin.size=5,Net='NEST')
		plotSurveyIndex(surveyLobsters34,yrs=2005:2017,se=T,graphic="R",index.variable="LobDen")

	surveyLobsters34<-LobsterSurveyProcess(lfa="34",yrs=1996:2017,mths=c("Aug","Jul","Jun"),size.range=c(85,210),bin.size=2.5,Net='NEST')
		plotSurveyIndex(surveyLobsters34,yrs=2005:2017,se=T,graphic="R",index.variable="LobDen")

	surveyLobsters34<-LobsterSurveyProcess(lfa="34",yrs=1996:2017,mths=c("Aug","Jul","Jun"),size.range=c(77.5,82.5),bin.size=2.5,Net='NEST')
		plotSurveyIndex(surveyLobsters34,yrs=2005:2017,se=T,graphic="R",index.variable="LobDen")

	surveyLobsters34<-LobsterSurveyProcess(lfa="34",yrs=1996:2017,mths=c("Aug","Jul","Jun"),bin.size=5,Net='NEST')

	surveyLobsters34 = LobsterSurveyProcess(lfa="34",yrs=1996:2017,mths=c("Aug","Jul","Jun"),bin.size=5,Net='NEST')
	surveyLobsters34m = LobsterSurveyProcess(lfa="34",yrs=1996:2017,mths=c("Aug","Jul","Jun"),bin.size=5,Net='NEST',sex=1)
	surveyLobsters34f = LobsterSurveyProcess(lfa="34",yrs=1996:2017,mths=c("Aug","Jul","Jun"),bin.size=5,Net='NEST',sex=2)
	surveyLobsters34b = LobsterSurveyProcess(lfa="34",yrs=1996:2017,mths=c("Aug","Jul","Jun"),bin.size=5,Net='NEST',sex=3)
	
	data = subset(surveyLobsters34,YEAR==2017,c("SET_LONG","SET_LAT","LobDen","MEAN_LENGTH"))
	datam = subset(surveyLobsters34m,YEAR==2017,c("SET_LONG","SET_LAT","LobDen","MEAN_LENGTH"))
	dataf = subset(surveyLobsters34f,YEAR==2017,c("SET_LONG","SET_LAT","LobDen","MEAN_LENGTH"))
	datab = subset(surveyLobsters34b,YEAR==2017,c("SET_LONG","SET_LAT","LobDen","MEAN_LENGTH"))
	names(data) = c("x","y","den","ml")
	data$denm = datam$LobDen
	data$denf = dataf$LobDen
	data$denb = datab$LobDen
	
	zeros = subset(data,den==0)
	data = subset(data,den>0)
	
	# map
		pdf(file.path( figdir,"LobsterSurveyBubbles2017.pdf"),8,11)
	LobsterMap(ylim=c(42.8,44.6), xlim=c(-67.25,-65.2),mapRes="UR",title="LFA 34 Lobster Survey 2017",isobath=seq(50,500,50),bathcol=rgb(0,0,1,0.2),bathy.source='bathy')
	points(y~x,zeros,pch=4)
	surveyBubbles(data,scaler=0.1,pie=T)
	dev.off()

	LobsterMap(ylim=c(42.8,44.6), xlim=c(-67.25,-65.2),mapRes="UR",title="LFA 34 Lobster Survey",isobath=seq(50,500,50),bathcol=rgb(0,0,1,0.2),bathy.source='bathy')
	surveyLobsters34index = calcIndexStations(surveyLobsters34)
	surveyLobsters34index$X = surveyLobsters34index$SET_LONG
	surveyLobsters34index$Y = surveyLobsters34index$SET_LAT
	surveyHistMap(surveyLobsters34index)

### ScallopSurvey


	CarapaceLengthFrequencies(LFAs='34',DS='ScallopSurvey', Yrs=2006:2017,graphic="pdf", rootdir= figdir, fn="LFA34" )
	CarapaceLengthFrequencies(LFAs='35',DS='ScallopSurvey', Yrs=2006:2017,graphic="pdf", rootdir= figdir, fn="LFA35" )
	CarapaceLengthFrequencies(LFAs='36',DS='ScallopSurvey', Yrs=2006:2017,graphic="pdf", rootdir= figdir, fn="LFA36"  )
	CarapaceLengthFrequencies(LFAs='38',DS='ScallopSurvey', Yrs=2006:2017,graphic="pdf", rootdir= figdir, fn="LFA38"  )


	scalSurv<-ScallopSurveyProcess()
	scalSurvm<-ScallopSurveyProcess(sex=1)
	scalSurvf<-ScallopSurveyProcess(sex=2)
	scalSurvb<-ScallopSurveyProcess(sex=3)


	data = subset(scalSurv,YEAR==2017,c("lon","lat","LobDen"))
	datam = subset(scalSurvm,YEAR==2017,c("lon","lat","LobDen"))
	dataf = subset(scalSurvf,YEAR==2017,c("lon","lat","LobDen"))
	datab = subset(scalSurvb,YEAR==2017,c("lon","lat","LobDen"))
	names(data) = c("x","y","den")
	data$denm = datam$LobDen
	data$denf = dataf$LobDen
	data$denb = datab$LobDen
	
	zeros = subset(data,den==0)
	data = subset(data,den>0)
	
	# map
		pdf(file.path( figdir,"ScallopSurveyBubblesLFA342017.pdf"),8,11)
	LobsterMap(ylim=c(42.8,44.6), xlim=c(-67.25,-65.2),mapRes="UR",title="LFA 34 Scallop Survey",isobath=seq(50,500,50),bathcol=rgb(0,0,1,0.2),bathy.source='bathy')
	points(y~x,zeros,pch=4)
	surveyBubbles(data,scaler=0.08,pie=T)
	dev.off()

	# map
		pdf(file.path( figdir,"ScallopSurveyBubblesBoF2017.pdf"),11,8)
	LobsterMap("BoF",mapRes="UR",title="BoF Scallop Survey",isobath=seq(50,500,50),bathcol=rgb(0,0,1,0.2),bathy.source='bathy')
	points(y~x,zeros,pch=4)
	surveyBubbles(data,scaler=0.08,pie=T)
	dev.off()


	data = subset(scalSurv,YEAR==2017&LFA==35,c("lon","lat","LobDen"))
	datam = subset(scalSurvm,YEAR==2017&LFA==35,c("lon","lat","LobDen"))
	dataf = subset(scalSurvf,YEAR==2017&LFA==35,c("lon","lat","LobDen"))
	datab = subset(scalSurvb,YEAR==2017&LFA==35,c("lon","lat","LobDen"))
	names(data) = c("x","y","den")
	data$denm = datam$LobDen
	data$denf = dataf$LobDen
	data$denb = datab$LobDen
	
	zeros = subset(data,den==0)
	data = subset(data,den>0)
	
	# map
	LobsterMap("35",mapRes="UR",title="LFA 35 Scallop Survey",isobath=seq(50,500,50),bathcol=rgb(0,0,1,0.2),bathy.source='bathy')
	points(y~x,zeros,pch=4)
	surveyBubbles(data,scaler=0.1,pie=T)




		# interpolate abundance
		interp.data<-na.omit(subset(scalSurv,YEAR==2017&LFA==34,c("TOW_SEQ","lon","lat","LobDen")))
		lob.contours<-interpolation(interp.data,ticks='define',place=3,nstrata=5,str.min=0,interp.method='gstat',blank=T,res=0.005,smooth=F,idp=3.5,blank.dist=0.2)

		# define contour lines
		print(lob.contours$str.def)
		lvls=c(1, 50, 100, 500, 1000, 5000, 10000)

		# generate contour lines
		LFAs<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolys.csv"))

		cont.lst<-contourGen(lob.contours$image.dat,lvls,subset(LFAs,PID==34),col="YlGn",colorAdj=1)

		# plot Map
		pdf(file.path( project.datadirectory("bio.lobster"), "figures","LFA34Distribution2017scal.pdf"),8,11)
		#png(file.path( project.datadirectory("bio.lobster"), "figures","Distribution2017.png"),8,11,units='in',pointsize=12, res=300,type='cairo')
		LobsterMap(ylim=c(42.8,44.6), xlim=c(-67.15,-65.2),mapRes="UR",contours=cont.lst,title="LFA 34 Lobster Density",isobath=seq(50,500,50),bathcol=rgb(0,0,1,0.2),bathy.source='bathy')
		points(lat~lon,scalSurv,subset=YEAR==2017,pch=21,cex=0.5,bg='red')#,col=rgb(0,0,0,0.5))
		contLegend("topright",lvls=lvls,Cont.data=cont.lst$Cont.data,title="#/square km",inset=0.02,cex=0.8,bg='white')
		dev.off()



	scalSurv<-ScallopSurveyProcess()

LobsterMap(ylim=c(43,46),xlim=c(-67.8,-64),isobath=seq(50,500,50),bathcol=rgb(0,0,1,0.2),bathy.source='bathy',title="Scallop Survey Locations 2017")
points(lat~lon,subset(scalSurv,YEAR==2017),pch=21,cex=0.8,bg='red')




    ## Carapace Length Frequency Plots
	

	
	# at Sea Sampling
	CarapaceLengthFrequencies(LFAs= '34', DS='atSea', by='SEX', fn='34',Yrs = c(2011:2017),vers=2,rootdir=figdir)
	CarapaceLengthFrequencies(LFAs= '36', DS='atSea', by='SEX', fn='36',Yrs = c(2013, 2015, 2017),vers=2,rootdir=figdir)
	CarapaceLengthFrequencies(LFAs= '38', DS='atSea', by='SEX', fn='38',Yrs = c(2012),vers=2,rootdir=figdir) 
	
	
	# FSRS recruitment traps
	CarapaceLengthFrequencies(LFAs= p$lfas, DS='fsrs', by="LFA", bins=seq(0,140,10),rootdir=figdir)
	


logsInSeason=lobster.db("process.logs")


	## Fishery Footprint - Landings
	catchLevels = c(0,100000,200000,300000,400000,500000,600000,700000,800000)
	yrs = 2011:2017
	for(i in 1:length(yrs)){
		catchgrids = lobGridPlot(subset(logsInSeason,LFA%in%p$lfas&SYEAR==2016,c("LFA","GRID_NUM","TOTAL_WEIGHT_KG")),FUN=sum,lvls=catchLevels)
		pdf(file.path(figdir,paste0("FisheryFootprint",yrs[i],".pdf")))
		LobsterMap('34-38',poly.lst=catchgrids)
	  	title(yrs[i],line=-3,cex.main=2,adj=0.3)
	    SpatialHub::contLegend('bottomright',lvls=catchgrids$lvls/1000,Cont.data=catchgrids,title="Catch (tons)",inset=0.02,cex=0.8,bg='white')
	    dev.off()
	}

	## Fishery Footprint - CPUE
	
	cpueLevels = c(0,0.2,0.4,0.6,0.8,0.9,1,2,3)
	yrs = 2011:2017
	#logsInSeason$logCPUE = log(logsInSeason$CPUE+1)
	for(i in 1:length(yrs)){
	  cpuegrids = lobGridPlot(subset(logsInSeason,LFA%in%p$lfas&SYEAR == yrs[i],c("LFA","GRID_NUM","CPUE")),FUN=median,lvls=cpueLevels)	
	  pdf(file.path(figdir,paste0("FishFootcpue", yrs[i],".pdf")))
	  LobsterMap('34-38',poly.lst=cpuegrids)
	  	title(yrs[i],line=-3,cex.main=2,adj=0.3)
	  SpatialHub::contLegend('bottomright',lvls=cpuegrids$lvls,Cont.data=cpuegrids,title="CPUE (kg/TH)",inset=0.02,cex=0.8,bg='white')
	  dev.off()
	}
	
	## Fishery Footprint - Mean Pots Hauled 
	
	potLevels = c (0,1000,100000,200000,300000,400000,500000,600000)
	yrs = 2011:2017
	for(i in 1:length(yrs)){
	potgrids = lobGridPlot(subset(logsInSeason,LFA%in%p$lfas&SYEAR == yrs[i],c("LFA","GRID_NUM","NUM_OF_TRAPS")),FUN=sum,lvls=potLevels) 
	pdf(file.path(figdir,paste0("FishFootpot", yrs[i],".pdf")))
	LobsterMap('34-38',poly.lst=potgrids)
	  	title(yrs[i],line=-3,cex.main=2,adj=0.3)
	SpatialHub::contLegend('bottomright',lvls=potgrids$lvls/1000,Cont.data=potgrids,title="Pots Hauled (000s)",inset=0.02,cex=0.8,bg='white')
	dev.off()
	}
	 
	
	## Fishery Footprint - Days Fished
	daysLevels = c(0,500,1000,1500,2000,2500,3000)
	daysFished<-aggregate(DATE_FISHED ~ SYEAR + LFA + GRID_NUM + LICENCE_ID, data=logsInSeason,FUN= function(x) length(unique(x)))	
	yrs = 2011:2017
	for (i in 1: length(yrs)){
	  daysgrids = lobGridPlot(subset(daysFished, LFA%in%p$lfas&SYEAR == yrs[i],c("LFA", "GRID_NUM", "DATE_FISHED")),FUN=sum, lvls= daysLevels)
	  pdf(file.path(figdir,paste0("FishFootDaysFished", yrs[i],".pdf")))
	  LobsterMap('34-38',poly.lst=daysgrids)
	  	title(yrs[i],line=-3,cex.main=2,adj=0.3)
	  SpatialHub::contLegend('bottomright',lvls=daysgrids$lvls,Cont.data=daysgrids,title="Total Days Fished",inset=0.02,cex=0.8,bg='white')
	  dev.off()
	}
	
	
	## Fishery Footprint - Licences Fished
	
	licenceLevels = c(0,15,30,45,60,75,90,105,120)
	yrs=2011:2017
	daysFished$LICENCE<-1
	for(i in 1: length(yrs)){
	  licencegrids = lobGridPlot(subset(daysFished, LFA%in%p$lfas&SYEAR==yrs[i], c("LFA", "GRID_NUM", "LICENCE")), FUN=sum, lvls= licenceLevels)
	 pdf(file.path(figdir,paste0("FishFootLicenceFished", yrs[i],".pdf")))
	 LobsterMap('34-38', poly.lst=licencegrids)
	  	title(yrs[i],line=-3,cex.main=2,adj=0.3)
	 SpatialHub::contLegend('bottomright', lvls=licencegrids$lvls, Cont.data=licencegrids, title= "Number of Licence Fished", inset =0.02,cex=0.8,bg='white')
	  dev.off()
	  }
	

    ## CPUE


    logsInSeason<-lobster.db('process.logs.redo')
    logsInSeason<-lobster.db('process.logs')

    cpueLFA.dat = CPUEplot(logsInSeason,lfa= p$lfas,yrs=2006:2017,graphic='R',export=T)
    cpueLFA.dat = CPUEplot(logsInSeason,lfa= p$lfas,yrs=2006:2017,graphic='pdf',path=figdir)
    cpueSubArea.dat = CPUEplot(logsInSeason,subarea= p$subareas,yrs=2006:2017,graphic='R')



	## Commercial CPUE MOdels
	mf1 = formula(logWEIGHT ~ fYEAR + DOS + TEMP + DOS * TEMP)
	#mf2 = formula(logWEIGHT ~ fYEAR + DOS + TEMP)
	#mf3 = formula(logWEIGHT ~ fYEAR + DOS)
	#mf4 = formula(logWEIGHT ~ fYEAR + TEMP)
	#mf5 = formula(logWEIGHT ~ fYEAR + DOS + TEMP + (1 | fYEAR/fAREA)) # combined


	TempModelling = TempModel( annual.by.area=F)
	#CPUE.data<-CPUEModelData(p,redo=T,TempModelling)
	CPUE.data<-CPUEModelData(p,redo=F)
	
	#CPUE.data$WEIGHT_KG = CPUE.data$TOTAL_WEIGHT_KG
    cpueSubArea.dat = CPUEplot(CPUE.data,subarea= p$subareas,yrs=1981:2017,graphic='R')


	CPUEModelResults1 = list()
	#CPUEModelResults2 = list()
	#CPUEModelResults3 = list()
	#CPUEModelResults4 = list()
	#AICs1 = c()
	#AICs2 = c()
	#AICs3 = c()
	#AICs4 = c()
	for(i in 1:length( p$subareas)){
	#for(i in 1:length( p$lfas)){

		mdata = subset(CPUE.data,subarea==p$subareas[i])
		#mdata = subset(CPUE.data,LFA==p$lfas[i])
		#CPUEModelResults1[[i]] = CPUEmodel(mf1,mdata,lfa=p$lfas[i])
		CPUEModelResults1[[i]] = CPUEmodel(mf1,mdata)
		#CPUEModelResults2[[i]] = CPUEmodel(mf2,mdata)
		#CPUEModelResults3[[i]] = CPUEmodel(mf3,mdata)
		#CPUEModelResults4[[i]] = CPUEmodel(mf4,mdata)
		#AICs1[i] = CPUEModelResults1[[i]]$model$aic
		#AICs2[i] = CPUEModelResults2[[i]]$model$aic
		#AICs3[i] = CPUEModelResults3[[i]]$model$aic
		#AICs4[i] = CPUEModelResults4[[i]]$model$aic


	}
	names(CPUEModelResults1) = p$subareas
	#names(CPUEModelResults2) = p$subareas
	#names(CPUEModelResults3) = p$subareas
	#names(CPUEModelResults4) = p$subareas
	
	#AICs = data.frame(rbind(AICs1,AICs2,AICs3,AICs4))
	#names(AICs) = p$subareas
	#AICs
	#sweep(AICs,2,FUN='-',apply(AICs,2,min))



	#CPUECombinedModelResults = CPUEmodel(mf5,CPUE.data,combined=T)	

	#cpue1c=CPUEModelPlot(CPUECombinedModelResults,TempModelling,combined=T,lfa = c("27N","27S", "28", "29", "30"),xlim=c(2010,2017.4),ylim=c(0,10.5),graphic='R',path=figdir,lab='1c')
	#cpue2c=CPUEModelPlot(CPUECombinedModelResults,TempModelling,combined=T,lfa = c("31A", "31B", "32", "33E", "33W"),xlim=c(2010,2017.4),ylim=c(0,10.5),graphic='pdf',path=figdir,lab='2c')

	#out=CPUEModelPlot(CPUEModelResults,TempModelling,lfa = c("33W","33E"),xlim=c(2014,2017.5),ylim=c(0,20),wd=15)
	#out1=CPUEModelPlot(CPUEModelResults,TempModelling,lfa = c("27N","27S", "28", "29", "30"),xlim=c(2010,2017.4),ylim=c(0,10.5))
	#out2=CPUEModelPlot(CPUEModelResults,TempModelling,lfa = c("31A", "31B", "32", "33E", "33W"),xlim=c(2010,2017.4),ylim=c(0,10.5))
	cpue1=CPUEModelPlot(CPUEModelResults1,TempModelling,lfa = c("34", "35", "36", "38"),xlim=c(2010,2017.4),ylim=c(0,10.5),graphic='png',path=figdir,lab=1)
	#cpue1=CPUEModelPlot(CPUEModelResults1,TempModelling,lfa = c("27", "28", "29", "30"),xlim=c(2010,2017.4),ylim=c(0,10.5),graphic='R',path=figdir,lab=1)
	#cpue2=CPUEModelPlot(CPUEModelResults1,TempModelling,lfa = c("31A", "31B", "32", "33"),xlim=c(2010,2017.4),ylim=c(0,10.5),graphic='R',path=figdir,lab=2)

	cpue.annual=list()
	for(i in 1:length(p$subareas)){

	 	mu=with(subset(cpue,LFA==p$subareas[i]),tapply(mu,YEAR,mean))
	 	mu.sd=with(subset(cpue,LFA==p$subareas[i]),tapply(mu,YEAR,sd))
	 	cpue.annual[[i]] = data.frame(Area=p$subareas[i],Year=as.numeric(names(mu)),CPUE=mu,CPUE.sd=mu.sd)
   	
	}
	cpueModel = subset(do.call("rbind",cpue.annual),Year<2017)
	x11()
	pdf(file.path( figdir,"CPUEmodelAnnualIndex.pdf"),8, 10)
	par(mfrow=c(length(p$subareas),1),mar=c(0,0,0,0),omi=c(0.5,1,0.5,0.5),las=1)

	for(i in 1:length(p$subareas)){

		plot(CPUE~Year,subset(cpueModel,Area==p$subareas[i]),type='b',pch=21,bg='red',ylim=c(0,max(cpueModel$CPUE+cpueModel$CPUE.sd,na.rm=T)),xlim=c(min(cpueModel$Year),max(cpueModel$Year)),xaxt='n')
		points(CPUE~YEAR,subset(cpueSubArea.dat$annual.dat,LFA==p$subareas[i]&YEAR<2017),pch=16,col='blue',cex=0.9)
		lines(CPUE+CPUE.sd~Year,subset(cpueModel,Area==p$subareas[i]),lty=2)
		lines(CPUE-CPUE.sd~Year,subset(cpueModel,Area==p$subareas[i]),lty=2)
		axis(1,lab=F)
		axis(4)
		if(i==length(p$subareas))axis(1)
		
		text(min(cpueModel$Year,na.rm=T),max(cpueModel$CPUE,na.rm=T)*.8,paste(p$subareas[i]),cex=2,pos=4)
	}
	mtext("CPUE (kg/TH)", 2, 3, outer = T, cex = 1,las=0)	
	dev.off()
	
	cpueData2=    CPUEplot(CPUE.data,lfa= p$lfas,yrs=1981:2017,graphic='R')$annual.data

	save(list=c("cpueModel","cpueData"),file=file.path(project.datadirectory("bio.lobster"),"outputs","cpueIndicators.rdata"))
	save(cpueData2,file=file.path(project.datadirectory("bio.lobster"),"outputs","cpueIndicators2.rdata"))
	#write.csv(cpueLFA.dat$annual.data,"CPUEannualData.csv",row.names=F)
	#write.csv(na.omit(cpueLFA.dat$daily.data),"CPUEdailyData.csv",row.names=F)


	## FSRS MOdels

	#Base

	FSRSvesday<-FSRSModelData()
	FSRSvesdayComm<-FSRSModelData(trap.type="commercial")
	FSRSModelResultsRecruit = list()
	FSRSModelResultsShort = list()
	FSRSModelResultsLegal = list()
	shorts.lst = list()
	legals.lst = list()
	recruit.lst = list()

	for(i in 1:length( p$subareas)){

		mdata = subset(FSRSvesday,subarea==p$subareas[i])

		FSRSModelResultsShort[[i]]=FSRSmodel(mdata, response="SHORTS",interaction=F)
		pdata	= 	FSRSModelResultsShort[[i]]$pData
		pdata$Area = p$subareas[i]
		shorts.lst[[i]] = pdata

		FSRSModelResultsLegal[[i]]=FSRSmodel(mdata, response="LEGALS",interaction=F)
		pdata	= 	FSRSModelResultsLegal[[i]]$pData
		pdata$Area = p$subareas[i]
		legals.lst[[i]] = pdata

		FSRSModelResultsRecruit[[i]]=FSRSmodel(mdata, response="RECRUITS",interaction=F)
		pdata	= 	FSRSModelResultsRecruit[[i]]$pData
		pdata$Area = p$subareas[i]
		recruit.lst[[i]] = pdata


	}

	names(FSRSModelResultsShort) = p$subareas
	names(FSRSModelResultsLegal) = p$subareas
	names(FSRSModelResultsRecruit) = p$subareas
	
	shorts = do.call("rbind",shorts.lst)
	legals = do.call("rbind",legals.lst)
	recruit = do.call("rbind",recruit.lst)

	library(ggplot2)

	pdf(file.path( figdir,"FSRSmodelBase.pdf"),8, 10)

	sp <- ggplot()
	sp <- sp + geom_point(data = shorts, aes(y = mu, x = YEAR), shape = 16, size = 2)
	sp <- sp + xlab("Year") + ylab("Lobsters / Trap")
	sp <- sp + theme(text = element_text(size=15)) + theme_bw()
	sp <- sp + geom_line(data = shorts, aes(x = YEAR, y = mu), colour = "black")
	sp <- sp + geom_ribbon(data = shorts, aes(x = YEAR, ymax = ub, ymin = lb ), alpha = 0.5)
	sp <- sp + facet_wrap(  ~Area, ncol=2,scales = "fixed")
	sp

	lp <- ggplot()
	lp <- lp + geom_point(data = legals, aes(y = mu, x = YEAR), shape = 16, size = 2)
	lp <- lp + xlab("Year") + ylab("Lobsters / Trap")
	lp <- lp + theme(text = element_text(size=15)) + theme_bw()
	lp <- lp + geom_line(data = legals, aes(x = YEAR, y = mu), colour = "black")
	lp <- lp + geom_ribbon(data = legals, aes(x = YEAR, ymax = ub, ymin = lb ), alpha = 0.5)
	lp <- lp + facet_wrap(  ~Area, ncol=2,scales = "fixed")
	lp

	rp <- ggplot()
	rp <- rp + geom_point(data = recruit, aes(y = mu, x = YEAR), shape = 16, size = 2)
	rp <- rp + xlab("Year") + ylab("Lobsters / Trap")
	rp <- rp + theme(text = element_text(size=15)) + theme_bw()
	rp <- rp + geom_line(data = recruit, aes(x = YEAR, y = mu), colour = "black")
	rp <- rp + geom_ribbon(data = recruit, aes(x = YEAR, ymax = ub, ymin = lb ), alpha = 0.5)
	rp <- rp + facet_wrap(  ~Area, ncol=2,scales = "fixed")
	rp

	dev.off()



	#Bayes

	FSRSvesday<-FSRSModelData()
	#FSRSvesdayComm<-FSRSModelData(trap.type="commercial")
	FSRSModelResultsRecruit = list()
	FSRSModelResultsShort = list()
	FSRSModelResultsLegal = list()
	shorts.lst = list()
	legals.lst = list()
	recruit.lst = list()

	for(i in 1:length( p$subareas)){
	st = Sys.time()

		mdata = subset(FSRSvesday,subarea==p$subareas[i])

		FSRSModelResultsShort[[i]]=FSRSmodel(mdata, response="SHORTS",interaction=F,type="bayesian",iter=5000,redo=T,ptraps=1000)
		pdata	= 	FSRSModelResultsShort[[i]]$pData
		pdata$Area = p$subareas[i]
		shorts.lst[[i]] = pdata
		
		FSRSModelResultsLegal[[i]]=FSRSmodel(mdata, response="LEGALS",interaction=F,type="bayesian",iter=5000,redo=T,ptraps=1000)
		pdata	= 	FSRSModelResultsLegal[[i]]$pData
		pdata$Area = p$subareas[i]
		legals.lst[[i]] = pdata

		FSRSModelResultsRecruit[[i]]=FSRSmodel(mdata, response="RECRUITS",interaction=F,type="bayesian",iter=5000,redo=T,ptraps=1000)
		pdata	= 	FSRSModelResultsRecruit[[i]]$pData
		pdata$Area = p$subareas[i]
		recruit.lst[[i]] = pdata
		print( Sys.time() - st)


	}

	names(FSRSModelResultsShort) = p$subareas
	names(FSRSModelResultsLegal) = p$subareas
	names(FSRSModelResultsRecruit) = p$subareas
	
	shorts = do.call("rbind",shorts.lst)
	legals = do.call("rbind",legals.lst)
	recruit = do.call("rbind",recruit.lst)

	library(ggplot2)

	#pdf(file.path( figdir,"FSRSmodelBayesShorts.pdf"),8, 10)
	png(file.path(figdir,"FSRSmodelBayesShorts.png"),width=8,height=10,units='in',res=200)
	sp <- ggplot()
	sp <- sp + geom_point(data = shorts, aes(y = median, x = YEAR), shape = 16, size = 2)
	sp <- sp + xlab("Year") + ylab("Lobsters / Trap")
	sp <- sp + theme(text = element_text(size=15)) + theme_bw()
	sp <- sp + geom_line(data = shorts, aes(x = YEAR, y = median), colour = "black")
	sp <- sp + geom_ribbon(data = shorts, aes(x = YEAR, ymax = ub, ymin = lb ), alpha = 0.5)
	sp <- sp + facet_wrap(  ~Area, ncol=2,scales = "fixed")
	sp
	dev.off()

	#pdf(file.path( figdir,"FSRSmodelBayesLegals.pdf"),8, 10)
	png(file.path(figdir,"FSRSmodelBayesLegals.png"),width=8,height=10,units='in',res=200)
	lp <- ggplot()
	lp <- lp + geom_point(data = legals, aes(y = median, x = YEAR), shape = 16, size = 2)
	lp <- lp + xlab("Year") + ylab("Lobsters / Trap")
	lp <- lp + theme(text = element_text(size=15)) + theme_bw()
	lp <- lp + geom_line(data = legals, aes(x = YEAR, y = median), colour = "black")
	lp <- lp + geom_ribbon(data = legals, aes(x = YEAR, ymax = ub, ymin = lb ), alpha = 0.5)
	lp <- lp + facet_wrap(  ~Area, ncol=2,scales = "fixed")
	lp
	dev.off()

	#pdf(file.path( figdir,"FSRSmodelBayesRecruits.pdf"),8, 10)
	png(file.path(figdir,"FSRSmodelBayesRecruits.png"),width=8,height=10,units='in',res=200)
	rp <- ggplot()
	rp <- rp + geom_point(data = recruit, aes(y = median, x = YEAR), shape = 16, size = 2)
	rp <- rp + xlab("Year") + ylab("Lobsters / Trap")
	rp <- rp + theme(text = element_text(size=15)) + theme_bw()
	rp <- rp + geom_line(data = recruit, aes(x = YEAR, y = median), colour = "black")
	rp <- rp + geom_ribbon(data = recruit, aes(x = YEAR, ymax = ub, ymin = lb ), alpha = 0.5)
	rp <- rp + facet_wrap(  ~Area, ncol=2,scales = "fixed")
	rp

	dev.off()


	FSRSvesdayComm = FSRSModelData(trap.type="commercial")

	cssa = c("33W","33E")
	FSRSModelResultsRecruitComm = list()
	FSRSModelResultsShortComm = list()
	FSRSModelResultsLegalComm = list()
	shortsComm.lst = list()
	legalsComm.lst = list()
	recruitComm.lst = list()

	for(i in 1:2){
		st = Sys.time()

		mdata = subset(FSRSvesdayComm,subarea==cssa[i])

		FSRSModelResultsShortComm[[i]]=FSRSmodel(mdata, response="SHORTS",interaction=F,type="bayesian",iter=5000,redo=F,tag="Comm",ptraps=1000)
		pdata	= 	FSRSModelResultsShortComm[[i]]$pData
		pdata$Area = cssa[i]
		shortsComm.lst[[i]] = pdata

		FSRSModelResultsLegalComm[[i]]=FSRSmodel(mdata, response="LEGALS",interaction=F,type="bayesian",iter=5000,redo=F,tag="Comm",ptraps=1000)
		pdata	= 	FSRSModelResultsLegalComm[[i]]$pData
		pdata$Area = cssa[i]
		legalsComm.lst[[i]] = pdata

		FSRSModelResultsRecruitComm[[i]]=FSRSmodel(mdata, response="RECRUITS",interaction=F,type="bayesian",iter=5000,redo=F,tag="Comm",ptraps=1000)
		pdata	= 	FSRSModelResultsRecruitComm[[i]]$pData
		pdata$Area = cssa[i]
		recruitComm.lst[[i]] = pdata
		print( Sys.time() - st)
	}


	names(FSRSModelResultsShortComm) = cssa
	names(FSRSModelResultsLegalComm) = cssa
	names(FSRSModelResultsRecruitComm) = cssa
	
	shortsComm = do.call("rbind",shortsComm.lst)
	legalsComm = do.call("rbind",legalsComm.lst)
	recruitComm = do.call("rbind",recruitComm.lst)

	library(ggplot2)

	#pdf(file.path( figdir,"FSRSmodelBayesCommShorts.pdf"),8, 2.5)
	png(file.path(figdir,"FSRSmodelBayesCommShorts.png"),width=8,height=2.5,units='in',res=200)

	sp <- ggplot()
	sp <- sp + geom_point(data = shortsComm, aes(y = median, x = YEAR), shape = 16, size = 2)
	sp <- sp + xlab("") + ylab("") + xlim(1999,2017)
	sp <- sp + theme(text = element_text(size=15)) + theme_bw()
	sp <- sp + geom_line(data = shortsComm, aes(x = YEAR, y = median), colour = "black")
	sp <- sp + geom_ribbon(data = shortsComm, aes(x = YEAR, ymax = ub, ymin = lb ), alpha = 0.5)
	sp <- sp + facet_wrap(  ~Area, ncol=2,scales = "fixed")
	sp
	dev.off()

	#pdf(file.path( figdir,"FSRSmodelBayesCommLegals.pdf"),8, 2.5)
	png(file.path(figdir,"FSRSmodelBayesCommLegals.png"),width=8,height=2.5,units='in',res=200)
	lp <- ggplot()
	lp <- lp + geom_point(data = legalsComm, aes(y = median, x = YEAR), shape = 16, size = 2)
	lp <- lp + xlab("") + ylab("Lobsters / Trap") + xlim(1999,2017)
	lp <- lp + theme(text = element_text(size=15)) + theme_bw()
	lp <- lp + geom_line(data = legalsComm, aes(x = YEAR, y = median), colour = "black")
	lp <- lp + geom_ribbon(data = legalsComm, aes(x = YEAR, ymax = ub, ymin = lb ), alpha = 0.5)
	lp <- lp + facet_wrap(  ~Area, ncol=2,scales = "fixed")
	lp
	dev.off()

	#pdf(file.path( figdir,"FSRSmodelBayesCommRecruits.pdf"),8, 2.5)
	png(file.path(figdir,"FSRSmodelBayesCommRecruits.png"),width=8,height=2.5,units='in',res=200)
	rp <- ggplot()
	rp <- rp + geom_point(data = recruitComm, aes(y = median, x = YEAR), shape = 16, size = 2)
	rp <- rp + xlab("Year") + ylab("") + xlim(1999,2017)
	rp <- rp + theme(text = element_text(size=15)) + theme_bw()
	rp <- rp + geom_line(data = recruitComm, aes(x = YEAR, y = median), colour = "black")
	rp <- rp + geom_ribbon(data = recruitComm, aes(x = YEAR, ymax = ub, ymin = lb ), alpha = 0.5)
	rp <- rp + facet_wrap(  ~Area, ncol=2,scales = "fixed")
	rp

	dev.off()
	
	FSRSvesday<-FSRSModelData()
	FSRSModelResultsRecruitLFA = list()
	FSRSModelResultsShortLFA = list()
	FSRSModelResultsLegalLFA = list()
	shortsLFA.lst = list()
	legalsLFA.lst = list()
	recruitLFA.lst = list()

	for(i in 1:length( p$lfas)){
	#for(i in c(1,8)){
	st = Sys.time()

		mdata = subset(FSRSvesday,LFA==p$lfas[i])
		FSRSModelResultsShortLFA[[i]]=FSRSmodel(mdata, lfa=p$lfa[i],response="SHORTS",interaction=F,type="bayesian",iter=5000,redo=F,ptraps=1000)
		pdata	= 	FSRSModelResultsShortLFA[[i]]$pData
		pdata$Area = p$lfas[i]
		shortsLFA.lst[[i]] = pdata
		print( st <- Sys.time() - st)
		
		FSRSModelResultsLegalLFA[[i]]=FSRSmodel(mdata, lfa=p$lfa[i], response="LEGALS",interaction=F,type="bayesian",iter=5000,redo=F,ptraps=1000)
		pdata	= 	FSRSModelResultsLegalLFA[[i]]$pData
		pdata$Area = p$lfas[i]
		legalsLFA.lst[[i]] = pdata
		print( st <- Sys.time() - st)

		FSRSModelResultsRecruitLFA[[i]]=FSRSmodel(mdata, lfa=p$lfa[i], response="RECRUITS",interaction=F,type="bayesian",iter=5000,redo=F,ptraps=1000)
		pdata	= 	FSRSModelResultsRecruitLFA[[i]]$pData
		pdata$Area = p$lfas[i]
		recruitLFA.lst[[i]] = pdata
		print( st <- Sys.time() - st)


	}

	names(FSRSModelResultsShortLFA) = p$lfas
	names(FSRSModelResultsLegalLFA) = p$lfas
	names(FSRSModelResultsRecruitLFA) = p$lfas
	
	shortsLFA = do.call("rbind",shortsLFA.lst)
	legalsLFA = do.call("rbind",legalsLFA.lst)
	recruitLFA = do.call("rbind",recruitLFA.lst)

 	save(list=c("shortsLFA","legalsLFA","recruitLFA"),file=file.path(project.datadirectory("bio.lobster"),"outputs","fsrsModelIndicators.rdata"))

	pdf(file.path( figdir,"FSRSmodelBayesLFAShorts.pdf"),8, 2.5)

	sp <- ggplot()
	sp <- sp + geom_point(data = shortsLFA, aes(y = median, x = YEAR), shape = 16, size = 2)
	sp <- sp + xlab("") + ylab("") + xlim(1999,2017)
	sp <- sp + theme(text = element_text(size=15)) + theme_bw()
	sp <- sp + geom_line(data = shortsLFA, aes(x = YEAR, y = median), colour = "black")
	sp <- sp + geom_ribbon(data = shortsLFA, aes(x = YEAR, ymax = ub, ymin = lb ), alpha = 0.5)
	sp <- sp + facet_wrap(  ~Area, ncol=2,scales = "fixed")
	sp
	dev.off()

	pdf(file.path( figdir,"FSRSmodelBayesLFALegals.pdf"),8, 2.5)
	lp <- ggplot()
	lp <- lp + geom_point(data = legalsLFA, aes(y = median, x = YEAR), shape = 16, size = 2)
	lp <- lp + xlab("") + ylab("Lobsters / Trap") + xlim(1999,2017)
	lp <- lp + theme(text = element_text(size=15)) + theme_bw()
	lp <- lp + geom_line(data = legalsLFA, aes(x = YEAR, y = median), colour = "black")
	lp <- lp + geom_ribbon(data = legalsLFA, aes(x = YEAR, ymax = ub, ymin = lb ), alpha = 0.5)
	lp <- lp + facet_wrap(  ~Area, ncol=2,scales = "fixed")
	lp
	dev.off()

	pdf(file.path( figdir,"FSRSmodelBayesLFARecruits.pdf"),8, 2.5)
	rp <- ggplot()
	rp <- rp + geom_point(data = recruitLFA, aes(y = median, x = YEAR), shape = 16, size = 2)
	rp <- rp + xlab("Year") + ylab("") + xlim(1999,2017)
	rp <- rp + theme(text = element_text(size=15)) + theme_bw()
	rp <- rp + geom_line(data = recruitLFA, aes(x = YEAR, y = median), colour = "black")
	rp <- rp + geom_ribbon(data = recruitLFA, aes(x = YEAR, ymax = ub, ymin = lb ), alpha = 0.5)
	rp <- rp + facet_wrap(  ~Area, ncol=2,scales = "fixed")
	rp

	dev.off()


	TempModelling = TempModel(areas = 'subarea')
	TempModelPlot(TempModelling,xlim=c(1980,2017),depths=c(5,25,50),Area=c("27N","27S", "29", "30","31A","31B", "32", "33E", "33W"),graphic='png',type=1:2)

 tempModel=TempModelPlot(TempModelling,xlim=c(1980,2017),depths=c(5,25,50),Area=c("27N","27S", "29", "30","31A","31B", "32", "33E", "33W"),graphic='png',type=3)
 tempData=TempModelPlot(TempModelling,xlim=c(1980,2017),depths=c(5,25,50),Area=c("27N","27S", "29", "30","31A","31B", "32", "33E", "33W"),graphic='png',type=4)
 save(list=c("tempModel","tempData"),file=file.path(project.datadirectory("bio.lobster"),"outputs","tempIndicators.rdata"))

 tempData2=TempModelPlot(TempModelling,xlim=c(1980,2017),depths=c(5,25,50),Area=c("27", "33"),lfa=T,graphic='R',type=4)
 save(tempData2,file=file.path(project.datadirectory("bio.lobster"),"outputs","tempIndicators2.rdata"))



######################## sim Molt
