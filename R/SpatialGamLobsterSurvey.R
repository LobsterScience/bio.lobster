#' @export
SpatialGamLobsterSurvey = function(surveyLobsters,Years = 2000:2018,legend.scale,lab=NULL,fd=file.path(figdir,"SpatialGam"),surface.plots=c("PA","AB"),ylab="Lobster (millions)"){

	require(mgcv)

	# Spatial temporal parameters
	LFAs<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolys.csv"))
	LFAs = lonlat2planar(LFAs,"utm20", input_names=c("X", "Y"))
	LFAs = LFAs[,-which(names(LFAs)%in%c('X','Y'))]
	LFAs = rename.df(LFAs,c('plon','plat'),c('X','Y'))
	if(missing(legend.scale))legend.scale=quantile(surveyLobsters$LobDen[surveyLobsters$LobDen>0],c(0.025,0.975))
	

	# Survey Data
	surveyLobsters = lonlat2planar(surveyLobsters,"utm20", input_names=c("SET_LONG", "SET_LAT"))
	surveyLobsters$dyear = decimal_date(as.Date(surveyLobsters$SET_DATE))
	surveyLobsters$PA = as.numeric(surveyLobsters$LobDen>0)
	surveyLobsters$LogDen = log(surveyLobsters$LobDen)


    # Presence-Absence model
    paMf = formula( PA ~ as.factor(YEAR) + s(SET_DEPTH)  + s(plon, plat,k=100)  )
    paMd = subset(surveyLobsters,YEAR>=min(Years),c('PA','plon','plat','dyear','YEAR','SET_DEPTH'))
	paMo = gam( paMf, data=paMd, family=binomial())
	print(summary(paMo))
	
    # Abundance model
    abMf = formula( LogDen ~ as.factor(YEAR) + s(SET_DEPTH) + s(plon, plat,k=100)  )
    abMd = subset(surveyLobsters,YEAR>=min(Years)&LobDen>0,c('LogDen','plon','plat','dyear','YEAR','SET_DEPTH'))
	abMo = gam( abMf, data=abMd, family=gaussian(link='log'))
	print(summary(abMo))
	
	# Prediction Surface
	load(file.path(project.codedirectory("EA"),"data","predspace.rdata")) # predSpace
	Ps = data.frame(EID=1:nrow(predSpace),predSpace[,c('plon','plat','z')])
	Ps = rename.df(Ps,c('plon','plat','z'),c('X','Y','SET_DEPTH'))
	key=findPolys(Ps,subset(LFAs,PID==34))
	Ps = subset(Ps,EID%in%key$EID)
	Ps = rename.df(Ps,c('X','Y'),c('plon','plat'))


	# annual predictions
	index=c()
	surface=list()

	for(i in 1:length(Years)){
	require(mgcv)
			
		Ps$YEAR =Years[i]

		plo = predict(paMo,Ps,type='response') 
		xyz = data.frame(Ps[,c('plon','plat')],z=plo)
		corners = data.frame(lon=c(-67.8,-65),lat=c(42.5,45))
		if("PA"%in%surface.plots)planarMap( xyz, fn=paste0("gamSurveyPA",lab,Years[i]), annot=Years[i],loc=fd, corners=corners, save=T)


		ldp = predict(abMo,Ps,type='response') 
		xyz = data.frame(Ps[,c('plon','plat')],z=exp(ldp)*plo)
		corners = data.frame(lon=c(-67.8,-65),lat=c(42.5,45))
		if("AB"%in%surface.plots)planarMap( xyz, fn=paste0("gamSurveyAB",lab,Years[i]), annot=Years[i],loc=fd, corners=corners, datascale=seq(legend.scale[1],legend.scale[2],l=50),save=T,log.variable=T)

		surface[[i]]= xyz
		index[i]= sum(xyz$z)

	}
	save(surface,file=file.path(project.datadirectory("bio.lobster"),"outputs",paste0(lab,"surface.rdata")))
	save(index,file=file.path(project.datadirectory("bio.lobster"),"outputs",paste0(lab,"index.rdata")))

	x11()
	plot(Years,index/10^6,type='b',ylab=ylab,ylim=c(0,max(index)/10^6),main=lab)

	return(index)
}


