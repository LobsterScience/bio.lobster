
	require(mgcv)
	require(bio.lobster)
	require(bio.utilities)
	require(SpatialHub)
	require(lubridate)
	la()

	# Survey Data
	surveyLobsters34index<-LobsterSurveyProcess(lfa="34", yrs=1996:2018, mths=c("Aug","Jul","Jun"), bin.size=2.5, Net='NEST',size.range=c(0,200))
	surveyLobsters34index = lonlat2planar(surveyLobsters34index,"utm20", input_names=c("SET_LONG", "SET_LAT"))
	surveyLobsters34index$dyear = decimal_date(as.Date(surveyLobsters34index$SET_DATE))

	# Spatial temporal parameters
	Years = 1996:2018
	LFAs<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolys.csv"))
	LFAs = lonlat2planar(LFAs,"utm20", input_names=c("X", "Y"))
	LFAs = LFAs[,-which(names(LFAs)%in%c('X','Y'))]
	LFAs = rename.df(LFAs,c('plon','plat'),c('X','Y'))
	
	sL = surveyLobsters34index
	Years = unique(sL$YEAR)
sL$SET_DEPTH = log(sL$SET_DEPTH)


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
					RMSE[i] =  calc_RMSE(test$pred,test$LobDen) / mean(test$LobDen) #
					test.data[[i]] = test
		}
		return(RMSE)
	}

aicc <- function(aic, logLik,n,k){
	aic + (2*k^2+2*k) / (n-k-1)
}

#model
	f1 = formula(LobDen~as.factor(YEAR) + s(SET_DEPTH) + s(plon, plat,bs='ts' ,k=100)) 
	f2 = formula(LobDen~as.factor(YEAR)  + s(plon, plat,bs='ts' ,k=100)) 
	f3 = formula(LobDen~as.factor(YEAR) + s(SET_DEPTH) ) 
	f4 = formula(LobDen~as.factor(YEAR) )
mm = c(f1,f2,f3,f4)
model.data = data.frame()


for(i in 1:length(mm)){
	aa = gam(mm[[i]],data=sL, family = Tweedie(p=1.25,link=power(.1))) 
	xV = mean(xVTweedie(model=mm[[i]]))
	nParams = 	attr(logLik(aa),'df')
	logLiks = logLik(aa)[1]
	aaa = 2*nParams-2*logLiks
	AICc = aicc(aaa,logLiks,nrow(sL),nParams)
	model.data = rbind(model.data,
	data.frame(Model=i,nP = nParams, ll = logLiks, cv = xV,Aicc = AICc, stringsAsFactors=F
	))
}

testingMCMCCV==F
if(testingMCMCCV){
nruns=c(10,100,250,500,750,1000)
out=c()
for(i in nruns){
	print(i)
	out=c(out,mean(xVTweedie(model=f1,nruns=i)))

}
} ##no biases

###gam hurdle
#cross validation

	xVHurdle <- function(data=sL, model=f1, prop.training=.85,nruns=100) {

		nsamp = round(nrow(data)*prop.training)
		vec = 1:nrow(data)
		RMSE = c()
		test.data = list()
		for(i in 1:nruns) {
					a = sample(vec,nsamp)

					training = data[a,]
					test = data[which(!vec %in% a),]
					training$R = training$PA
					test$id = 1:nrow(test)
					test$R = test$PA
					mod1 = gam(model,data=training,family = binomial())
					test$predPA = predict(mod1,newdata = test,type='response')
					test2 = subset(test,LobDen>0)
					test2$R = log(test2$LobDen)
					training2 = subset(training,LobDen>0)
					training2$R = log(training2$LobDen)
					mod2 = gam(model,data=training,family = gaussian(link='log'))
					test2$predAb = predict(mod2,newdata = test2,type='response')
					test = merge(test,test2[,c('id','predAb')],by='id',all.x=T)
					test$pred = exp(test$predAb)*test$predPA
					test$pred[which(is.na(test$pred))]<- 0 #forces solution to exact LobDen -- biased 
					RMSE[i] =  calc_RMSE(test$pred,test$LobDen) / mean(test$LobDen) #
					test.data[[i]] = test
		}
		return(RMSE)
	}

aicc = function(aic, logLik,n,k){
	aic + (2*k^2+2*k) / (n-k-1)
}

#model
	f1 = formula(R~as.factor(YEAR) + s(SET_DEPTH) + s(plon, plat,bs='ts' ,k=100)) 
	f2 = formula(R~as.factor(YEAR)  + s(plon, plat,bs='ts' ,k=100)) 
	f3 = formula(LobDen~as.factor(YEAR) + s(SET_DEPTH) )
	f4 = formula(R~as.factor(YEAR) )
mm = c(f1,f2,f3,f4)
model.data = data.frame()
sL$PA = as.numeric(sL$LobDen>0)


for(i in 1:length(mm)){
	sL$R = sL$PA
	aa = gam(mm[[i]],data=sL, family = binomial())
	 j = which(sL$LobDen>0)
	 sLL = sL[j,]
	 sLL$R = log(sLL$LobDen)
	ab = gam(mm[[i]],data=sLL, family = gaussian(link='log')) 
	xV = mean(xVHurdle(model=mm[[i]]))
	nParamsa = 	attr(logLik(aa),'df')
	nParamsb = 	attr(logLik(ab),'df')
	nParams = max(c(nParamsb,nParamsa))
	logLiks = logLik(aa)[1]+logLik(ab)[1]
	aaa = 2*nParams+2*logLiks
	AICc = aicc(aaa,logLiks,nrow(sL),nParams)
	model.data = rbind(model.data,
	data.frame(Model=i,nP = nParams, ll = logLiks, cv = xV,Aicc = AICc, stringsAsFactors=F
	))
}

