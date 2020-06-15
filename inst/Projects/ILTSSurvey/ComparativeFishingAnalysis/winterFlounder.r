#winter flounder analysis
options(stringsAsFactors=F)
require(bio.lobster)
require(bio.utilities)
require(lubridate)
require(PBSmapping)
require(mgcv)

lf = read.csv(file.path(project.datadirectory('bio.groundfish'),'ITQ_ILTS_winter_flounder_lfs.csv'))
ca = read.csv(file.path(project.datadirectory('bio.groundfish'),'ITQ_ILTS_winter_flounder_catch.csv'))
can = read.csv(file.path(project.datadirectory('bio.groundfish'),'ITQ_ILTS_winter_flounder_nocatch_updated_2.csv'))
cand = can[!duplicated(can$FISHSET_ID),]
cand = subset(cand, !FISHSET_ID %in% unique(ca$FISHSET_ID)) 

ca = as.data.frame(rbind(ca,cand))
attr(ca,'projection') <- "LL"
ca$Dist = calcGCdist(ca$SET_LON*-1,ca$SET_LAT, ca$HAUL_LON*-1, ca$HAUL_LAT )$d
ca$YEAR = year(ca$SET_DATE)
ca$MON = month(ca$SET_DATE)

ca = subset(ca,Dist<3 & Dist>.4)

#there are weights for some but no numbers using a mean wt per gear to get numbers
			ca= toNums(ca,c('EST_KEPT_WT','EST_DISCARD_WT','NUM_CAUGHT'))
			ca$EST_DISCARD_WT[is.na(ca$EST_DISCARD_WT)] <- 0
			ca$EST_KEPT_WT[is.na(ca$EST_KEPT_WT)] <- 0
			ca$TOTWGT = ca$EST_DISCARD_WT+ca$EST_KEPT_WT
			aggregate(cbind(NUM_CAUGHT,TOTWGT)~GEAR, data=subset(ca,!is.na(NUM_CAUGHT)&!is.na(TOTWGT)),FUN=sum)

			#weights per individual
			bW = 0.3729392 
			nW = 0.2270291

			ca$mW = NA
			ca$mW[which(ca$GEAR=='NEST')] <- nW
			ca$mW[which(ca$GEAR=='280 BALLOON')] <- bW

			i = which(is.na(ca$NUM_CAUGHT) & (ca$TOTWGT)>0)
			ca$NUM_CAUGHT[i] = ca$TOTWGT[i]/ca$mW[i]

			ca$NUM_CAUGHT[which(is.na(ca$NUM_CAUGHT))]<-0

#Correct by tow distance

ca$cNC = round(ca$NUM_CAUGHT/ca$Dist)

#prune to comparative stations for bulk correction factors
		Cca = subset(ca, YEAR %in% c(2016,2019))
		Ccb = aggregate(cNC~STATION+GEAR+YEAR,data=Cca,FUN=sum)
		Ccb$ID = paste(Ccb$STATION, Ccb$YEAR, sep="-")
		ccb = reshape(Ccb[,c('ID','GEAR','cNC')], idvar = 'ID', timevar='GEAR', direction='wide')
		names(ccb)[2:3] = c('Bal','Nest')
		ccb = subset(ccb,!is.na(Bal) )
		ccb1 = subset(ccb,!is.na(Bal) & (Bal>0 &Nest>0))

#ratio estimator
		P = sum(ccb$Nest) / sum(ccb$Bal)

#Betabinomial model non length
			setwd('~/git/bio.lobster/inst/IP/ILTSNonLobs/')
			library(TMB)
			n_i = nrow(ccb1)
			data = list(
			  n_A = ccb1$Bal,
			  n_B = ccb1$Nest)
			parameters = list(
			  log_alpha = 0,
			  log_beta = 0,
			  log_rho = rep(0, n_i))

			version <- "beta_binomial"
			compile(paste0(version,".cpp"))
			dyn.load(dynlib(version))
			obj = MakeADFun(data=data, 
			                parameters=parameters,
			                random=c("log_rho"),
			                DLL=version,
			                silent = F)
			opt <- nlminb(obj$par,obj$fn,obj$gr,
			              control = list(),
			              lower = c(log(0.1),log(0.1)),
			              upper = c(log(100),log(100)))
			rep <- sdreport(obj)
			mu = rep$value[grep('mu',names(rep$value))]
			Pbb = mu/(1-mu) #Calibration Factor to multiply Balloon By

##Applying Ballon 2 Nest Conversion
ca$cNCNest = NA
i = which(ca$GEAR=='280 BALLOON')
ca$cNCNest[i] = ca$cNC[i] * Pbb 
i = which(ca$GEAR=='NEST')
ca$cNCNest[i] = ca$cNC[i]  
attr(ca,'projection') <- "LL"
ca = makePBS(ca,polygon=F)
ca$X = ca$HAUL_LONG * -1
ca$Y = ca$HAUL_LAT  
ca = convUL(ca)
#######Tweedie Modelling
caS = subset(ca, !is.na(cNCNest) & MON %in% c(6,7,8))

	f1 = formula(cNCNest~as.factor(YEAR) + s(SET_DEPTH) + s(X, Y,bs='ts' ,k=100)) 
	aa = gam(f1,data=caS, family = Tweedie(p=1.25,link=power(.1))) ##


#root mean square error

calc_RMSE <- function(pred, obs){
  	RMSE <- round(sqrt(mean((pred-obs)^2)),3)
  	return(RMSE)
	}

oo = ca$cNCNest

#cross validation

	xVTweedie <- function(data=sL, model=f1, prop.training=.85,nruns=100) {

		nsamp = round(nrow(data)*prop.training)
		vec = 1:nrow(data)
		RMSE = c()
		test.data = list()
		for(i in 1:nruns) {
					a = sample(vec,nsamp)
					training = data[a,]
					test = data[which(!vec %in% a),]
					mod1 = gam(model,data=training,family = Tweedie(p=1.25,link=power(.1)))
					test$pred = predict(mod1,newdata = test,type='response')
					RMSE[i] =  calc_RMSE(test$pred,test$Lobs)
					test.data[[i]] = test
		}
		return(RMSE)
	}

	xVresults = xVTweedie()
	mean(xVresults)
	

#Overall model
	oo = sL$LobDen
	pp = predict(aa,type='response')

	#normalized RMSE
	calc_RMSE(pp,oo) / mean(oo) # 

	plot(pp,oo)
	abline(a=0,b=1)

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

Years = 1996:2018



######################################

if(LengthBasedCoversion){
			##Length based 
			 lf$ID=paste(lf$TRIP_ID,lf$SET_NO,sep='-')
			 ca$ID=paste(ca$TRIP_ID, ca$SET_NO,sep='-')

			lf1 =merge(lf,ca[,c('ID','GEAR','YEAR','MON','STATION', 'NUM_CAUGHT')],by='ID')

			#no comparative data for 2016--we did not measure winter flounder but we did in 2019 BoF

			lfb = subset(lf1,GEAR=='280 BALLOON' &YEAR==2019)
			lfn = subset(lf1,GEAR=='NEST'&YEAR==2019)

			#check for subsampling
				ab = aggregate(NUM_AT_LENGTH~ID+NUM_CAUGHT+STATION,data=lfb,FUN=sum) #no subsampling all good
				an = aggregate(NUM_AT_LENGTH~ID+NUM_CAUGHT,data=lfn,FUN=sum) #subsampling in one set

			#only a few stations where there were actually >10 fish caught, lets examine those before moving forward
				i = unique(ab$STATION[ab$NUM_CAUGHT>10])

			#building a data set for length based vessel comps
					lb = subset(lfb,STATION %in% i)
					ln = subset(lfn,STATION %in% i)

					lb$b3 = floor(lb$FISH_LENGTH/3)*3
					ln$b3= floor(ln$FISH_LENGTH/3)*3


					lb$b5 = floor(lb$FISH_LENGTH/5)*5
					ln$b5= floor(ln$FISH_LENGTH/5)*5


					lb5 = aggregate(NUM_AT_LENGTH ~ STATION+b5, data=lb,FUN=sum)
					ln5 = aggregate(NUM_AT_LENGTH ~ STATION+b5, data=ln,FUN=sum)

					lb3 = aggregate(NUM_AT_LENGTH ~ STATION+b3, data=lb,FUN=sum)
					ln3 = aggregate(NUM_AT_LENGTH ~ STATION+b3, data=ln,FUN=sum)

					names(lb3)[3] = 'Ball'
					names(ln3)[3] = 'Nest'

					lbn3 = merge(lb3, ln3, by=c('STATION', 'b3'), all=T)
					lbn3 = na.zero(lbn3)
			#Length based conversion Coeffs
					require(gamlss)
					lbn3$C = lbn3$Nest / lbn3$Ball
					out = gamlss(cbind(Nest,Ball)~cs(b3,df=3),data=lbn3,family=BB())
					newd = data.frame(b3=13:45)
					mu = predict(out,what='mu',type='response',newdata=newd)
					rho = mu / (1-mu)

					with(lbn3,plot(b3,C))
					lines(newd[,1],rho)
					#there is some evidence of length based differences, but we don't have lengths for the full time series, ways to deal with this, but for now we will ignore
				}
