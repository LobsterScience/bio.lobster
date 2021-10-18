#Lobster BoF ILTS extension


	
	require(mgcv)
	require(bio.lobster)
	require(bio.utilities)
	require(SpatialHub)
	require(lubridate)
	la()

ff = "LFA36Survey"
fp1 = file.path(project.datadirectory('bio.lobster'),"analysis",ff)
dir.create(fp1)
fpf1 = file.path(project.figuredirectory('bio.lobster'),ff)
dir.create(fpf1)

##Commercial
	# Survey Data
	surveyLobstersBOFindex<-LobsterSurveyProcess(lfa=c(35,36,37),yrs=2019:2020, mths=c("Aug","Sep"), bin.size=2.5, Net='NEST',size.range=c(82.5,200),biomass=T)
	surveyLobstersBOFindex = lonlat2planar(surveyLobstersBOFindex,"utm20", input_names=c("SET_LONG", "SET_LAT"))

	surveyLobstersBOFindex$dyear = decimal_date(as.Date(surveyLobstersBOFindex$SET_DATE))

	# Spatial temporal parameters
	
Years = 2019:2020
	LFAs<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolys.csv"))
	LFAs = lonlat2planar(LFAs,"utm20", input_names=c("X", "Y"))
	LFAs = LFAs[,-which(names(LFAs)%in%c('X','Y'))]
	LFAs = rename.df(LFAs,c('plon','plat'),c('X','Y'))

 sL = surveyLobstersBOFindex
 sL = rename.df(sL,c('SET_LAT','SET_LONG'),c('Y','X'))

 	Years = unique(sL$YEAR)

#model
	f1 = formula(LobDen~as.factor(YEAR) + s(SET_DEPTH) + s(plon, plat,bs='ts' ,k=100)) 
	aa = gam(f1,data=sL, family = Tweedie(p=1.25,link=power(.1))) ##



#Predictions from full model
	load(file.path(project.datadirectory('bio.lobster'),'data','predspace.rdata')) #sq km
	Ps = data.frame(EID=1:nrow(predSpace),predSpace[,c('plon','plat','z')])
	Ps = rename.df(Ps,c('plon','plat','z'),c('X','Y','SET_DEPTH'))
	key=findPolys(Ps,subset(LFAs,PID %in% c(35,36)))
	Ps = subset(Ps,EID%in%key$EID)
	Ps = rename.df(Ps,c('X','Y'),c('plon','plat'))
	Ps$pSOFT = .1
	
	# annual predictions
	R1index=c()
	R1area = list()
	R1surface=list()
	ilink <- family(aa)$linkinv   # this is the inverse of the link function

	for(i in 1:length(Years)){
	require(mgcv)
			
		#Ps$dyear =Years[i]+.5
		Ps$YEAR =Years[i]
		Ps$AREA_SWEPT = mean(sL$AREA_SWEPT)

		plo = as.data.frame(predict(aa,Ps,type='link',se.fit=TRUE))
		plo$upper = ilink(plo$fit - (1.96 * plo$se.fit))  
		plo$lower = ilink(plo$fit - (1.96 * plo$se.fit))
        plo$fitted = ilink(plo$fit)


		xyz = data.frame(Ps[,c('plon','plat')],z=ilink(plo$fit))
		corners = data.frame(lon=c(-67.3,-64.1),lat=c(44.5,45.75))

		R1area[[i]] = c(Years[i],length(which(xyz$z<5)))
		planarMap( xyz, save=T,fn=paste("gamtwPAR1",Years[i],sep='.'), datascale=seq(0.1,10000,l=30), annot=Years[i],loc=fpf1, corners=corners,log.variable=T)
		#planarMap( xyz, fn=paste("lobster.gambi.pred",Years[i],sep='.'), annot=Years[i],loc="output",corners=corners)
		#planarMap( xyz, corners=corners)
		R1surface[[i]]=xyz
		R1index[i]= sum(xyz$z)
	}
