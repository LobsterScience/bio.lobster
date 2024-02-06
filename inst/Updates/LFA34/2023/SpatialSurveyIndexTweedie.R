#SpatialSurveyIndexTweedie

	
	require(mgcv)
	require(bio.lobster)
	require(bio.utilities)
	require(SpatialHub)
	require(lubridate)
	la()

ff = "LFA34Update"
fp1 = file.path(project.datadirectory('bio.lobster'),"analysis",ff)
fpf1 = file.path(project.figuredirectory('bio.lobster'),ff)
dir.create(fp1)
dir.create(fpf1)
##Commercial
	# Survey Data
	surveyLobsters34index<-LobsterSurveyProcess(lfa="L34", yrs=1996:2023, mths=c("Aug","Jul","Jun"), bin.size=2.5, Net='NEST',size.range=c(82.5,200),biomass=T)
	surveyLobsters34index$X = surveyLobsters34index$SET_LONG
	surveyLobsters34index$Y = surveyLobsters34index$SET_LAT
	attr(surveyLobsters34index,'projection') <- "LL"
	#surveyLobsters34index = convUL(surveyLobsters34index)
	surveyLobsters34index = lonlat2planar(surveyLobsters34index, input_names=c("SET_LONG", "SET_LAT"))
	surveyLobsters34index$dyear = decimal_date(as.Date(surveyLobsters34index$SET_DATE))

	surveyLobsters34index$plat = surveyLobsters34index$plat/1000
	surveyLobsters34index$plon = surveyLobsters34index$plon/1000
	
	# Spatial temporal parameters
	Years = 1996:2023
	LFAs<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolys.csv"))
	attr(LFAs,'projection')<- "LL" 
	#LFAs = convUL(LFAs)
	LFAs = lonlat2planar(LFAs,input_names=c("X", "Y"))
	
	LFAs = LFAs[,-which(names(LFAs)%in%c('X','Y'))]
	LFAs = rename.df(LFAs,c('plon','plat'),c('X','Y'))
  LFAs$X = LFAs$X/1000
  LFAs$Y = LFAs$Y/1000
  
	surveyLobsters34index$Quant.by.year = NA
	for(i in 1:length(Years)){
		k = which(surveyLobsters34index$YEAR==Years[i])
		surveyLobsters34index$Quant.by.year[k] = findInterval(surveyLobsters34index$LobDen[k],quantile(surveyLobsters34index$LobDen[k],seq(.01,.99, length.out=10)))
	}

	
	nstations = aggregate(FISHSET_ID~YEAR+LFA,data=surveyLobsters34index,FUN=function(x) length(unique(x)))
	
#tweedie
	#using the mpLC as a prob that the num caught is within the size class

	sL = surveyLobsters34index
	Years = unique(sL$YEAR)

#examining data
	
	
	
#model
	f1 = formula(LobDen~as.factor(YEAR) + s(SET_DEPTH) + s(plon, plat,bs='ts' ,k=100)) 
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
	#	planarMap( xyz, save=T,fn=paste("gamtwPAR1",Years[i],sep='.'), datascale=seq(0.1,10000,l=30), annot=Years[i],loc=fpf1, corners=corners,log.variable=T)
		#planarMap( xyz, fn=paste("lobster.gambi.pred",Years[i],sep='.'), annot=Years[i],loc="output",corners=corners)
		#planarMap( xyz, corners=corners)
		R1surface[[i]]=xyz
		R1index[i]= sum(xyz$z)
	}

Years = 1996:2023


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
		ag = apply(asa[,2:1001],1,quantile,0.5)/1000
		
		aout = as.data.frame(cbind(asa$YEAR,apply(asa[,2:1001],1,quantile,0.5)/1000, apply(asa[,2:1001],1,quantile,0.025)/1000,apply(asa[,2:1001],1,quantile,0.975)/1000))
		names(aout) = c('Year','B','lB','uB')
		R0 = aout
		write.csv(aout,file=file.path(fpf1,'ILTSCommB.csv'))

			png(file=file.path(fpf1,'ILTSCommB.png'),units='in',width=10,height=8,pointsize=18, res=300,type='cairo')
			plot(asa$YEAR,apply(asa[,2:1001],1,quantile,0.5)/1000,xlab='Year',ylab='Commercial Biomass',pch=16,ylim=c(0,32000))
			arrows(asa$YEAR, y0 = apply(asa[,2:1001],1,quantile,0.025)/1000, y1 =apply(asa[,2:1001],1,quantile,0.975)/1000,length=0 )
			lines(asa$YEAR,runmed(ag,k=3),col='salmon',lwd=2)
			dev.off()


#area 

 R1area1 = as.data.frame(do.call(rbind,R1area))
 names(R1area1) = c('Year','Area')
area =  nrow(xyz)
R1area1$Prop = (area - R1area1$Area)/area
R1area1 = R1area1[order(R1area1$Year),]
png(file=file.path(fpf1,'ILTSPropArea5perkm.png'),units='in',width=10,height=8,pointsize=18, res=300,type='cairo')
 plot(R1area1$Year,R1area1$Prop,pch=16,xlab='Year',ylab='Proportion of Total Area')
 lines(R1area1$Year,runmed(R1area1$Prop,k=3),col='salmon',lwd=2)
dev.off()




################################################
###Recruits
	# Survey Data
	surveyLobsters34index<-LobsterSurveyProcess(lfa="L34", yrs=1996:2022, mths=c("Aug","Jul","Jun"), bin.size=2.5, Net='NEST',size.range=c(70,82.5),biomass=F)
	surveyLobsters34index = lonlat2planar(surveyLobsters34index,"utm20", input_names=c("SET_LONG", "SET_LAT"))
	surveyLobsters34index$dyear = decimal_date(as.Date(surveyLobsters34index$SET_DATE))

	# Spatial temporal parameters
	Years = 1996:2022
	LFAs<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolys.csv"))
	LFAs = lonlat2planar(LFAs,"utm20", input_names=c("X", "Y"))
	LFAs = LFAs[,-which(names(LFAs)%in%c('X','Y'))]
	LFAs = rename.df(LFAs,c('plon','plat'),c('X','Y'))

#tweedie
	#using the mpLC as a prob that the num caught is within the size class

	sL = surveyLobsters34index
	Years = unique(sL$YEAR)
	#compound poisson regression with area swept as an offset

#model
	f1 = formula(LobDen~as.factor(YEAR) + s(SET_DEPTH) + s(plon, plat,bs='ts' ,k=100)) 
	aa = gam(f1,data=sL, family = Tweedie(p=1.25,link=power(.1))) ##



#Predictions from full model
	load(file.path(project.datadirectory('bio.lobster'),'data','predspace.rdata')) #sq km
	Ps = data.frame(EID=1:nrow(predSpace),predSpace[,c('plon','plat','z')])
	Ps = rename.df(Ps,c('plon','plat','z'),c('X','Y','SET_DEPTH'))
	key=findPolys(Ps,subset(LFAs,PID==34))
	Ps = subset(Ps,EID%in%key$EID)
	Ps = rename.df(Ps,c('X','Y'),c('plon','plat'))

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
		corners = data.frame(lon=c(-67.8,-65),lat=c(42.5,45))

		R1area[[i]] = c(Years[i],length(which(xyz$z<5)))
		planarMap( xyz, fn=paste("gamtwPAR1",Years[i],sep='.'), datascale=seq(0.1,10000,l=30), annot=Years[i],loc=fpf1, corners=corners,log.variable=T)
		#planarMap( xyz, fn=paste("lobster.gambi.pred",Years[i],sep='.'), annot=Years[i],loc="output",corners=corners)
		#planarMap( xyz, corners=corners)
		R1surface[[i]]=xyz
		R1index[i]= sum(xyz$z)
	}

Years = 1996:2022


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
		ag = apply(asa[,2:1001],1,quantile,0.5)/1000
		aout = as.data.frame(cbind(asa$YEAR,apply(asa[,2:1001],1,quantile,0.5)/1000, apply(asa[,2:1001],1,quantile,0.025)/1000,apply(asa[,2:1001],1,quantile,0.975)/1000))
		
		names(aout) = c('Year','B','lB','uB')
		rec = aout
		write.csv(aout,file=file.path(fpf1,'ILTSRecruitN.csv'))

			png(file=file.path(fpf1,'ILTSRecruitN.png'),units='in',width=10,height=8,pointsize=18, res=300,type='cairo')
			plot(asa$YEAR,apply(asa[,2:1001],1,quantile,0.5)/1000,xlab='Year',ylab='Recruit abundance',pch=16,ylim=c(0,55000))
			arrows(asa$YEAR, y0 = apply(asa[,2:1001],1,quantile,0.025)/1000, y1 =apply(asa[,2:1001],1,quantile,0.975)/1000,length=0 )
			lines(asa$YEAR,runmed(ag,k=3),col='salmon',lwd=2)
			dev.off()
