#SpatialSurveyIndexTweedie

	
	require(mgcv)
	require(bio.lobster)
	require(bio.utilities)
	require(SpatialHub)
	require(lubridate)
	la()

	# Survey Data
	surveyLobsters34index<-LobsterSurveyProcess(lfa="34", yrs=1996:2018, mths=c("Aug","Jul","Jun"), bin.size=2.5, Net='NEST',size.range=c(70,82.5))
	surveyLobsters34index = lonlat2planar(surveyLobsters34index,"utm20", input_names=c("SET_LONG", "SET_LAT"))
	surveyLobsters34index$dyear = decimal_date(as.Date(surveyLobsters34index$SET_DATE))

	# Spatial temporal parameters
	Years = 1996:2018
	LFAs<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolys.csv"))
	LFAs = lonlat2planar(LFAs,"utm20", input_names=c("X", "Y"))
	LFAs = LFAs[,-which(names(LFAs)%in%c('X','Y'))]
	LFAs = rename.df(LFAs,c('plon','plat'),c('X','Y'))

#tweedie
	#using the mpLC as a prob that the num caught is within the size class

	sL = surveyLobsters34index
	sL$Lobs = round(sL$LobDen*sL$AREA_SWEPT	)
	Years = unique(sL$YEAR)
	#compound poisson regression with area swept as an offset

#model
	f1 = formula(Lobs~as.factor(YEAR) + s(SET_DEPTH) + s(plon, plat,bs='ts' ,k=100)+offset(AREA_SWEPT)) 
	aa = gam(f1,data=sL, family = Tweedie(p=1.25,link=power(.1))) ##


#root mean square error

calc_RMSE <- function(pred, obs){
  	RMSE <- round(sqrt(mean((pred-obs)^2)),3)
  	return(RMSE)
	}



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
	oo = sL$Lobs
	pp = predict(aa,type='response')

	#normalized RMSE
	calc_RMSE(pp,oo) / mean(oo) # 

	plot(pp,oo)
	abline(a=0,b=1)

#Predictions from full model
	load(file.path(project.datadirectory('bio.lobster'),'data','predspace.rdata'))
	Ps = data.frame(EID=1:nrow(predSpace),predSpace[,c('plon','plat','z')])
	Ps = rename.df(Ps,c('plon','plat','z'),c('X','Y','SET_DEPTH'))
	key=findPolys(Ps,subset(LFAs,PID==34))
	Ps = subset(Ps,EID%in%key$EID)
	Ps = rename.df(Ps,c('X','Y'),c('plon','plat'))

	# annual predictions
	R1index=c()
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
		planarMap( xyz, fn=paste("gamtwPAR1",Years[i],sep='.'), datascale=seq(0.1,400,l=30), annot=Years[i],loc=figdir, corners=corners,log.variable=T)
		#planarMap( xyz, fn=paste("lobster.gambi.pred",Years[i],sep='.'), annot=Years[i],loc="output",corners=corners)
		#planarMap( xyz, corners=corners)
		R1surface[[i]]=xyz
		R1index[i]= sum(xyz$z)
	}

Years = 1996:2018


tweedie_boot <- function(data,i, pred.data=Ps,years=Years) {
						#if(is.null(nsamples)) nsamples = nrow(data)
						# i = sample(1:nsamples, nsamples, replace=T )
  						data_boot <- data[i, ]
						aa = gam(Lobs~as.factor(YEAR) + s(SET_DEPTH) + s(plon, plat, k=100)+offset(AREA_SWEPT),data=data_boot, family = Tweedie(p=1.25,link=power(.1)))
						ilink <- family(aa)$linkinv   # this is the inverse of the link function
		R1index	= c()	
	for(g in 1:length(years)){
				pred.data$YEAR =years[g]
				pred.data$AREA_SWEPT = mean(data_boot$AREA_SWEPT)
				plo = predict(aa,pred.data,type='response')
				R1index[g]= sum(plo)
			}
	return(R1index)
  		}


#bootstrapping statistics
b <- boot(data=sL, statistic = tweedie_boot, R = 5)
plot(apply(b$t/10000,2,mean))



#using the posterior distribution model coefs

	f1 = formula(Lobs~as.factor(YEAR) + s(SET_DEPTH) + s(plon, plat,bs='ts' ,k=100)+offset(AREA_SWEPT)) 
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
plot(asa$YEAR,apply(asa[,2:1001],1,quantile,0.5)/1000,)
lines(asa$YEAR,apply(asa[,2:1001],1,quantile,0.025)/1000)
lines(asa$YEAR,apply(asa[,2:1001],1,quantile,0.975)/1000)

