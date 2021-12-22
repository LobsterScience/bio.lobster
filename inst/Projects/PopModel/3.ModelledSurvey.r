#SpatialSurveyIndexTweedie
  
	
	require(mgcv)
	require(bio.lobster)
	require(bio.utilities)
	require(SpatialHub)
	require(lubridate)
	la()


wd = file.path(project.datadirectory('bio.lobster'),'PopModelInputs')
dir.create(wd,showWarnings = F)

##Commercial
	# Survey Data
	surveyLobsters34index<-LobsterSurveyProcess(lfa="34", yrs=1996:2021, mths=c("Aug","Jul","Jun"), bin.size=5, Net='NEST',size.range=c(53,223),biomass=F)
	surveyLobsters34index = lonlat2planar(surveyLobsters34index,"utm20", input_names=c("SET_LONG", "SET_LAT"))

	surveyLobsters34index$dyear = decimal_date(as.Date(surveyLobsters34index$SET_DATE))

	# Spatial temporal parameters
	Years = 1996:2021
	LFAs<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolys.csv"))
	LFAs = lonlat2planar(LFAs,"utm20", input_names=c("X", "Y"))
	LFAs = LFAs[,-which(names(LFAs)%in%c('X','Y'))]
	LFAs = rename.df(LFAs,c('plon','plat'),c('X','Y'))


	surveyLobsters34index$Quant.by.year = NA
	for(i in 1:length(Years)){
		k = which(surveyLobsters34index$YEAR==Years[i])
		surveyLobsters34index$Quant.by.year[k] = findInterval(surveyLobsters34index$LobDen[k],quantile(surveyLobsters34index$LobDen[k],seq(.01,.99, length.out=10)))
	}

#tweedie
	#using the mpLC as a prob that the num caught is within the size class

	sL = surveyLobsters34index
	Years = unique(sL$YEAR)

#model
	f1 = formula(NUM_STANDARDIZED~as.factor(YEAR) + s(SET_DEPTH) + s(plon, plat,bs='ts' ,k=100)) 
	aa = gam(f1,data=sL, family = Tweedie(p=1.25,link=power(.1))) ##



#Predictions from full model
	load(file.path(project.datadirectory('bio.lobster'),'data','predspace.rdata')) #sq km
	Ps = data.frame(EID=1:nrow(predSpace),predSpace[,c('plon','plat','z')])
	Ps = rename.df(Ps,c('plon','plat','z'),c('X','Y','SET_DEPTH'))
	key=findPolys(Ps,subset(LFAs,PID==34))
	Ps = subset(Ps,EID%in%key$EID)
	Ps = rename.df(Ps,c('X','Y'),c('plon','plat'))
	Ps$pSOFT = .1
	
	# annual predictions
	R1index=c()
	R1area = list()
	R1surface=list()
	R1index.se = c()
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
		corners = data.frame(lon=c(-67.8,-65),lat=c(42.5,45))

		R1area[[i]] = c(Years[i],length(which(xyz$z<5)))
		#planarMap( xyz, save=T,fn=paste("gamtwPAR1",Years[i],sep='.'), datascale=seq(0.1,10000,l=30), annot=Years[i],loc='', corners=corners,log.variable=T)
		#planarMap( xyz, fn=paste("lobster.gambi.pred",Years[i],sep='.'), annot=Years[i],loc="output",corners=corners)
		#planarMap( xyz, corners=corners)
		R1surface[[i]]=xyz
		R1index[i]= sum(xyz$z)
		R1index.se[i] = sum(plot$se.fit)
	}

Years = 1996:2021


#using the posterior distribution model coefs

	f1 = formula(LobDen~as.factor(YEAR) + s(SET_DEPTH) + s(plon, plat,bs='ts' ,k=100)) 
	aa = gam(f1,data=sL, family = Tweedie(p=1.25,link=power(.1))) ##

		set.seed(1000)
		n_sims =1000	
		Pss = list()

	
	for(i in 1:length(Years)){
			Pst = Ps
			Pst$YEAR = Years[i]
			Pst$AREA_SWEPT = mean(subset(sL,YEAR==Years[i],AREA_SWEPT)[,1])
			Pss[[i]] = Pst
		} 

		Ps = do.call(rbind,Pss)

		a_lp_matrix = predict(object = aa, Ps,
		               type = "lpmatrix")

		a_coef_mean = coef(aa)
		a_vcov = vcov(aa)
		a_par_coef_posterior = rmvn(n = n_sims, 
                                    mu = a_coef_mean,
          	                        V = a_vcov)
		ilink = family(aa)$linkinv

		preds = ilink(a_lp_matrix %*% t(a_par_coef_posterior))
 		apreds = as.data.frame(preds)

		apreds$YEAR = Ps$YEAR 



		asa = as.data.frame(aggregate(.~YEAR,data=apreds,FUN=sum))
		aout = as.data.frame(cbind(asa$YEAR,apply(asa[,2:1001],1,quantile,0.5)/1000, apply(asa[,2:1001],1,quantile,0.025)/1000,apply(asa[,2:1001],1,quantile,0.975)/1000,apply(asa[,2:1001],1,sd)/1000))
		names(aout) = c('Year','B','lB','uB','sd')
	
		
	kk = grep('CL',names(sL))
	ky = grep('YEAR',names(sL))
	
	sLr = sL[,c(ky,kk)]
	N = aggregate(SET_ID~YEAR,data=surveyLobsters34index,FUN=length)
	
	sLR = aggregate(.~YEAR,data=sLr, FUN=mean,na.rm=T)
	r = 2:ncol(sLR)
	sLR[,r] = sLR[,r]/apply(sLR[,r],1,sum) 
	sLR[is.na(sLR)]<- -1
	
	aoutF = merge(aout,sLR,by.x='Year',by.y='YEAR')
	aoutF = merge(aoutF,N,by.x='Year',by.y='YEAR')
	aoutF$CV =aoutF$sd / aoutF$B
	write.csv(aoutF,file=file.path(wd,paste('EGOM','ILTS34.csv',sep="-")))
	
	