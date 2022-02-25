#SpatialSurveyIndexTweedie
  
	
	require(mgcv)
	require(bio.lobster)
	require(bio.utilities)
	require(SpatialHub)
	require(lubridate)
  require(rgdal)
		la()


wd = file.path(project.datadirectory('bio.lobster'),'PopModelInputs')
dir.create(wd,showWarnings = F)


	
	#SEX BASED
	s34M<-LobsterSurveyProcess(lfa="L34", yrs=2013:2021, mths=c("Aug","Jul","Jun"), bin.size=5, Net='NEST',size.range=c(53,223),biomass=F,sex=1)
	s34F<-LobsterSurveyProcess(lfa="L34", yrs=2013:2021, mths=c("Aug","Jul","Jun"), bin.size=5, Net='NEST',size.range=c(53,223),biomass=F,sex=2:3)
	s34M = lonlat2planar(s34M,"utm20", input_names=c("SET_LONG", "SET_LAT"))
	s34F = lonlat2planar(s34F,"utm20", input_names=c("SET_LONG", "SET_LAT"))
	
	s34F$dyear = decimal_date(as.Date(s34F$SET_DATE))
	s34M$dyear = decimal_date(as.Date(s34M$SET_DATE))
	
	i = which(grepl('CL',names(s34M)))
	M = s34M[,i]
	F = s34F[,i]
	#M = na.zero(M)
	#F = na.zero(F)
	
	Ma = apply(M,2,mean,na.rm=T)
	Fa = apply(F,2,mean,na.rm=T)
	
	plot(Sz,Fa/(Ma+Fa))
	
	
	
		#Predictions from full model
	load(file.path(project.datadirectory('bio.lobster'),'data','predspace.rdata')) #sq km
	Ps = data.frame(EID=1:nrow(predSpace),predSpace[,c('plon','plat','z')])
	Ps = rename.df(Ps,c('plon','plat','z'),c('X','Y','SET_DEPTH'))
	LFAs<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolys.csv"))
	LFAs = lonlat2planar(LFAs,"utm20", input_names=c("X", "Y"))
	LFAs = LFAs[,-which(names(LFAs)%in%c('X','Y'))]
	LFAs = rename.df(LFAs,c('plon','plat'),c('X','Y'))
	
	key=findPolys(Ps,subset(LFAs,PID==34))
	Ps = subset(Ps,EID%in%key$EID)
	Ps = rename.df(Ps,c('X','Y'),c('plon','plat'))
	Ps$pSOFT = .1
	
	Years = 2013:2021
	
	#Female
	
	#using the posterior distribution model coefs
	
	f1 = formula(LobDen~as.factor(YEAR) + s(SET_DEPTH) + s(plon, plat,bs='ts' ,k=100)) 
	aa = gam(f1,data=s34F, family = Tweedie(p=1.25,link=power(.1))) ##
	
	set.seed(1000)
	n_sims =1000	
	Pss = list()
	
	
	for(i in 1:length(Years)){
	  Pst = Ps
	  Pst$YEAR = Years[i]
	  Pst$AREA_SWEPT = mean(subset(s34F,YEAR==Years[i],AREA_SWEPT)[,1])
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
	
	
	kk = grep('CL',names(s34F))
	ky = grep('YEAR',names(s34F))
	
	sLr = s34F[,c(ky,kk)]
	N = aggregate(SET_ID~YEAR,data=s34F,FUN=length)
	
	sLR = aggregate(.~YEAR,data=sLr, FUN=mean,na.rm=T)
	r = 2:ncol(sLR)
	sLR[,r] = sLR[,r]/apply(sLR[,r],1,sum) 
	sLR[is.na(sLR)]<- -1
	
	aoutF = merge(aout,sLR,by.x='Year',by.y='YEAR')
	aoutF = merge(aoutF,N,by.x='Year',by.y='YEAR')
	aoutF$CV =aoutF$sd / aoutF$B
	
	write.csv(aoutF,file='EGOMITLSFematSize.csv')
	
	
	####################male
	
	
	
	#Female
	
	#using the posterior distribution model coefs
	
	f1 = formula(LobDen~as.factor(YEAR) + s(SET_DEPTH) + s(plon, plat,bs='ts' ,k=100)) 
	aa = gam(f1,data=s34M, family = Tweedie(p=1.25,link=power(.1))) ##
	
	set.seed(1000)
	n_sims =1000	
	Pss = list()
	
	
	for(i in 1:length(Years)){
	  Pst = Ps
	  Pst$YEAR = Years[i]
	  Pst$AREA_SWEPT = mean(subset(s34F,YEAR==Years[i],AREA_SWEPT)[,1])
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
	
	
	kk = grep('CL',names(s34M))
	ky = grep('YEAR',names(s34M))
	
	sLr = s34M[,c(ky,kk)]
	N = aggregate(SET_ID~YEAR,data=s34M,FUN=length)
	
	sLR = aggregate(.~YEAR,data=sLr, FUN=mean,na.rm=T)
	r = 2:ncol(sLR)
	sLR[,r] = sLR[,r]/apply(sLR[,r],1,sum) 
	sLR[is.na(sLR)]<- -1
	
	aoutF = merge(aout,sLR,by.x='Year',by.y='YEAR')
	aoutF = merge(aoutF,N,by.x='Year',by.y='YEAR')
	aoutF$CV =aoutF$sd / aoutF$B
	
	write.csv(aoutF,file='EGOMITLSMalatSize.csv')
	