#SoakTime stuff
require(bio.lobster)
require(bio.utilities)

 setwd('/SpinDr/backup/bio_data/bio.lobster/LFA41SoakTimeData/')
options(stringsAsFactors=F)
#logs = read.csv('CW\ LOG_DATA\ 2016.11.15.csv')

#logs$Y = round((((logs$ENT_LATITUDE /100/100-trunc(logs$ENT_LATITUDE/100/100))*100)/60)+trunc(logs$ENT_LATITUDE/100/100),4)
#logs$X = round((((logs$ENT_LONGITUDE/100/100-trunc(logs$ENT_LONGITUDE/100/100))*100)/60)+trunc(logs$ENT_LONGITUDE/100/100),4)*-1 
#obs  = read.csv('CW\ OBSERVER_DATA\ 2016.11.15.csv')
#obs = subset(obs,!is.na(NUM_HOOK_HAUL))
#mats = read.csv('Observer_Log\ Match.csv')


#obs = obs[,c('TRIP_ID','NUM_HOOK_HAUL','FISHSET_ID','SET_NO','SPECCD_ID','EST_DISCARD_WT','LATITUDE','LONGITUDE')]
#obs$X = obs$LONGITUDE*-1
#obs$Y = obs$LATITUDE

##match log strings to obs strings
	out = list()
#for(i in 1:nrow(mats)){

#	
#		j = mats[i,'TRIP_ID']
#		k = mats[i,'MON_DOC_ID']
#
#		ll = subset(logs,MON_DOC_ID==k)
#		oo = aggregate(SPECCD_ID~FISHSET_ID+X+Y+TRIP_ID,data=subset(obs,TRIP_ID==j),FUN=length)
#		ooo = unique(oo$FISHSET_ID)
#		ou = list()
#		for(o in 1:length(ooo)){
#				oooo = subset(oo,FISHSET_ID==ooo[o])
#				aa = (geosphere::distGeo(oooo[,c('X','Y')],ll[,c('X','Y')]))
#				oooo$LOG_EFRT_STD_INFO_ID = NA
#				if(min(aa)<5000) oooo$LOG_EFRT_STD_INFO_ID = ll$LOG_EFRT_STD_INFO_ID[which.min(aa)]
#				ou[[o]] <- oooo
#		}
#		out[[i]] <- do.call(rbind,ou)
#


###new data
	obs1  = read.csv('CW\ OBSERVER_DATA_SETDATE\ 2019.02.01.csv')
#source=0 for observer
	obs1 = subset(obs1,SOURCE==0)
	obs1$SOAK_DAYS =as.numeric( as.Date(obs1$HAUL_DATE) - as.Date(obs1$SET_DATE))
	obs = obs1[,c('TRIP_ID','NUM_HOOK_HAUL','FISHSET_ID','SET_NO','SPECCD_ID','EST_DISCARD_WT','LATITUDE','LONGITUDE','SOAK_DAYS')]
	obs$X = obs$LONGITUDE*-1
	obs$Y = obs$LATITUDE
	obs$LONGITUDE <- obs$LATITUDE <- NULL


ad = reshape(obs,idvar=c('TRIP_ID','NUM_HOOK_HAUL','FISHSET_ID','SET_NO','Y','X','SOAK_DAYS'),timevar='SPECCD_ID',direction='wide')
ad = na.zero(ad)
ad = subset(ad,SOAK_DAYS>0)
 ad$TOT = rowSums(ad[,8:53])
ad$Cod = with(ad,EST_DISCARD_WT.10/NUM_HOOK_HAUL)
ad$WpTH = ad$TOT/ad$NUM_HOOK_HAUL
ad$F1 = ifelse(ad$SOAK_DAYS<7,1,ifelse(ad$SOAK_DAYS>=7 & ad$SOAK_DAYS<14,2,3))
ad$F2 = ifelse(ad$SOAK_DAYS<8,1,ifelse(ad$SOAK_DAYS>=8 & ad$SOAK_DAYS<15,2,3))
ad$F3 = ifelse(ad$SOAK_DAYS<9,1,ifelse(ad$SOAK_DAYS>=10 & ad$SOAK_DAYS<16,2,3))

aa = aggregate(Cod~F2, data=ad,FUN=function(x) c(mean(x),sd(x)))
####cod specific analyses
###lets treat this data as a zip and 
ccd = aggregate(EST_DISCARD_WT.10/NUM_HOOK_HAUL~SOAK_DAYS,data=ad,FUN=mean)
names(ccd)[2] = 'CodCatchRate'
aaa = plot(CodCatchRate~(SOAK_DAYS),data=subset(ccd,SOAK_DAYS<25),type='l')

				    x <- as.ts(ccd$CodCatchRate[1:17])
				    x = mave(x,c(1,1,1,1,1))
				  #  x <- residuals(tslm(x ~ trend)) #removing time series trend
				    n.freq <- 500
				    spec <- spectrum(c(na.contiguous(x)),  plot = T) # spectral analysis of cycle
				    period <- floor(1/spec$freq[which.max(spec$spec)] + 0.5) #determiing the most likely period

				    plot( 1/spec$freq,spec$spec,type= 'l',xlim=c(0,20),xlab='Frequency', ylab='Spectral Density')
				    abline(h=max(spec$spec),v=1/spec$freq[which.max(spec$spec)],col='red',lwd=2)
				    Period = round(1/spec$freq[which.max(spec$spec)])
			



 ad$TOT = rowSums(ad[,8:53])
plot(aggregate(TOT/NUM_HOOK_HAUL~SOAK_DAYS,data=ad,FUN=mean),type='l',ylim=c(0,2),xlim=c(0,30))

ad$WpTH = ad$TOT/ad$NUM_HOOK_HAUL
ad$F1 = ifelse(ad$SOAK_DAYS<7,1,ifelse(ad$SOAK_DAYS>=7 & ad$SOAK_DAYS<14,2,3))
ad$F2 = ifelse(ad$SOAK_DAYS<8,1,ifelse(ad$SOAK_DAYS>=8 & ad$SOAK_DAYS<15,2,3))
ad$F3 = ifelse(ad$SOAK_DAYS<9,1,ifelse(ad$SOAK_DAYS>=10 & ad$SOAK_DAYS<16,2,3))


#lines(aggregate(TOT/NUM_HOOK_HAUL~SOAK_DAYS,data=ad,FUN=mean),type='l',ylim=c(0,2),xlim=c(0,30),col='red')
aggregate(NUM_HOOK_HAUL~SOAK_DAYS,data=ad,FUN=length)

#kept weights
	obs = obs1[,c('TRIP_ID','NUM_HOOK_HAUL','FISHSET_ID','SET_NO','SPECCD_ID','EST_KEPT_WT','LATITUDE','LONGITUDE','SOAK_DAYS')]
	obs$X = obs$LONGITUDE*-1
	obs$Y = obs$LATITUDE
	obs$LONGITUDE <- obs$LATITUDE <- NULL


ad = reshape(obs,idvar=c('TRIP_ID','NUM_HOOK_HAUL','FISHSET_ID','SET_NO','Y','X','SOAK_DAYS'),timevar='SPECCD_ID',direction='wide')
ad = na.zero(ad)
ad = subset(ad,SOAK_DAYS>0)

aggregate(EST_DISCARD_WT.10/NUM_HOOK_HAUL~SOAK_DAYS,data=ad,FUN=mean)

 ad$TOT = rowSums(ad[,8:53])
plot(aggregate(TOT/NUM_HOOK_HAUL~SOAK_DAYS,data=ad,FUN=mean),type='l',ylim=c(0,2),xlim=c(0,30))


#power analysis
require(pwr)
	obs1  = read.csv('CW\ OBSERVER_DATA_SETDATE\ 2019.02.01.csv')
#source=0 for observer
	obs1 = subset(obs1,SOURCE==0)
	obs1$SOAK_DAYS =as.numeric( as.Date(obs1$HAUL_DATE) - as.Date(obs1$SET_DATE))
	obs = obs1[,c('TRIP_ID','NUM_HOOK_HAUL','FISHSET_ID','SET_NO','SPECCD_ID','EST_DISCARD_WT','LATITUDE','LONGITUDE','SOAK_DAYS')]
	obs$X = obs$LONGITUDE*-1
	obs$Y = obs$LATITUDE
	obs$LONGITUDE <- obs$LATITUDE <- NULL


ad = reshape(obs,idvar=c('TRIP_ID','NUM_HOOK_HAUL','FISHSET_ID','SET_NO','Y','X','SOAK_DAYS'),timevar='SPECCD_ID',direction='wide')
ad = na.zero(ad)
ad = subset(ad,SOAK_DAYS>0)
ad$TOT = rowSums(ad[,8:53])
ad$Cod = with(ad,EST_DISCARD_WT.10/NUM_HOOK_HAUL)*100
ad$Cusk = with(ad,EST_DISCARD_WT.15/NUM_HOOK_HAUL)*100
ad$WpTH = ad$TOT/ad$NUM_HOOK_HAUL*100
ad$F1 = ifelse(ad$SOAK_DAYS<7,1,ifelse(ad$SOAK_DAYS>=7 & ad$SOAK_DAYS<14,2,3))
ad$F2 = ifelse(ad$SOAK_DAYS<8,1,ifelse(ad$SOAK_DAYS>=8 & ad$SOAK_DAYS<15,2,3))
ad$F3 = ifelse(ad$SOAK_DAYS<9,1,ifelse(ad$SOAK_DAYS>=10 & ad$SOAK_DAYS<16,2,3))

aa = aggregate(Cod~F2, data=ad,FUN=function(x) c(mean(x),sd(x)))

aa2 = aggregate(Cusk~F2, data=ad,FUN=function(x) c(mean(x),sd(x)))



##effect size and sample size
PercentChangeWithPower <- function(M1=4,M2=NULL, S1=5, S2=6, perch = 0.5){
if(is.null(M2)) M2 = M1+M1*perch
 	effsz = (M1 - M2)/sqrt(((S1^2) + (S2^2))/2)  
	a = pwr.t.test(d=effsz,sig.level=0.05,power=0.9, type='two.sample',alternative='two.sided')
	a$n
}


plot(1,1,type='n',xlim=c(0,1),ylim=c(0,4000),xlab='Percent Change Detectable',ylab='N Strings')

per = seq(0.1,0.9, by=0.01)
Spp = list(	c(33, 25, 33), 	#lobster
			c(12, 14, 15), 	#jonah
			c(5, 5, 7),   	#Cusk
			c(4, 5, 6), 	#Cod
			c(4, 5, 5),		#whitehake
			c(1, 1, 1)		#rockcrab
	)


ff = list()
for(j in 1:length(Spp)){
out = c()
	for(i in 1:length(per)){
	out = c(out,PercentChangeWithPower(M1=Spp[[j]][1],S1=Spp[[j]][2],S2=Spp[[j]][3], perch=per[i]))
	}
lines(per, out, lty=j,col=j)
ff[[j]] = (c(out[16],out[41]))
}

abline(v=0.5,col='black',lwd=2)
abline(v=0.25,col='black',lwd=2)
savePlot('PercentDifferenceandSampleSize.png',type='png')

ff = as.data.frame(do.call(rbind,ff))
ff$Species= c('Lobster','Jonah','Cusk','Cod','White hake','Rock crab')
names(ff)[1:2] <- c('Change of 25%','Change of 50%')
ff[1:2] = round(ff[1:2])

xtable(ff)


########################################################################################################
##March 27 2020 Simulating a ZIP and exploring variances
############getting the mean and variance, pi and lamba of a zip process data set
meansd2ZIP = function(x, return.mean.sig2=T){
	library(MASS)
	dzip <- function (x, pie, lam) {
		#zero inflated poisson process #https://en.wikipedia.org/wiki/Zero-inflated_model
		  ifelse((x == 0), (pie + (1 - pie) * exp(-lam)), ((1 - pie) * (exp(-lam) * lam^x))/factorial(x)) 
		}

	pies = sum(x==0)/length(x) #initial value for pie ...pie not pi as pi is reserved for 3.1415..
	means = mean(x)
	zipr = try(fitdistr(x, dzip, start = list(lam = 0.05, pie = pies), lower = list(p = 0.00001)),silent=T)[[1]]
	if(grepl('Error',zipr)) zipr = try(fitdistr(x, dzip, start = list(lam = means, pie = pies), lower = list(p = 0.00001)),silent=T)[[1]]
	if(grepl('Error',zipr)) zipr = try(fitdistr(x, dzip, start = list(lam = .3, pie = pies), lower = list(p = 0.00001)),silent=T)[[1]]
	if(grepl('Error',zipr)) zipr = try(fitdistr(x, dzip, start = list(lam = means, pie = .9), lower = list(p = 0.00001)),silent=T)[[1]]
	if(grepl('Error',zipr)) zipr = try(fitdistr(x, dzip, start = list(lam = 2, pie = .8), lower = list(p = 0.00001)),silent=T)[[1]]
	
	out = zipr
	if(return.mean.sig2) out = c(mu = (1-zipr[2])*zipr[1], sig2 = zipr[1]*(1-zipr[2])*(1+zipr[1]*zipr[2]),pi=zipr[2],lambda=zipr[1])
	return(out) 
}

dznib = function (x, mu, size, zprob, log = FALSE) {
    logv = log(1 - zprob) + dnbinom(x, mu = mu, size = size, 
        log = TRUE)
    logv = ifelse(x == 0, log(zprob + exp(logv)), logv)
    if (log) 
        logv
    else exp(logv)
}

#ratio estimators are biased, you can account for this by the following (assuming it is following a zip here)
  unbiasedRatioEstimatorMuSig2 = function(x,y){
  					r = sum(x,na.rm=T) / sum(y,na.rm=T)
  					rho = abs(cor(x,y))
  					sxy = abs(cov(x,y))
  					n = length(x)
  					##ignore FPC
  					zipx = meansd2ZIP(x)
  					cxy = sxy/(zipx[1]*mean(y))
  					cx2 = zipx[2]/(zipx[1]^2)
  					rc = r*(1-1/n*(cxy-cx2)) #Tins BC
  					#rc = as.numeric(r+1/n*((r*zipx[2]-rho*sqrt(zipx[2])*sd(y))/zipx[1]^2))
  					#rc = as.numeric(r+((zipx[2]/zipx[1]^2)*mean(y)/zipx[1]-sxy/zipx[1]^2))
  					
  					vrc = as.numeric(1/n*(rc^2*zipx[2] + var(y) - 2*rc*rho*sqrt(zipx[2])*sd(y))/zipx[1]^2)
  					return(c(ratio = rc, variance.of.ratio = vrc, pi = zipx[3],lambda=zipx[4]))
  			}

#unbiasedRatioEstimatorMuSig2(Codtrap$CODWEIGHT, Codtrap$Total)

#how many rows of data for each soak haul
aggregate(NUM_HOOK_HAUL~SOAK_DAYS,data=ad,FUN=length)
#prune ad
ad = subset(ad,SOAK_DAYS>3 & SOAK_DAYS<20)
ou = list()
ah = split(ad[,c('SOAK_DAYS','EST_DISCARD_WT.10','NUM_HOOK_HAUL')],f=ad$SOAK_DAYS)
	 for(i in 1:length(ah)){
	 		h = ah[[i]]
	 		ou[[i]] = c(unique(h$SOAK_DAYS), unbiasedRatioEstimatorMuSig2(x=h$EST_DISCARD_WT.10/h$NUM_HOOK_HAUL*100,y=h$NUM_HOOK_HAUL/h$NUM_HOOK_HAUL*100))
	 }
#bias corrected
ah = do.call(rbind,ou)
ah = data.frame(ah)
names(ah) = c('SOAK_DAYS','Ratio','Var.Ratio','pi','lambda')
plot(ah$SOAK_DAYS,ah$Ratio,type='b', pch=16)
arrows(x0=ah$SOAK_DAYS, y0=ah$Ratio-sqrt(ah$Var.Ratio),y1=ah$Ratio+sqrt(ah$Var.Ratio),length=0)
#biased
uah = aggregate(EST_DISCARD_WT.10/NUM_HOOK_HAUL~SOAK_DAYS,data=ad,FUN=mean)


rzip = function(n=1000,p=.76, lambda=9) {
ifelse(rbinom(n, size = 1, prob = p) > 0, 0, rpois(n, lambda = lambda))
}


# lets treat this through modelling 
require(mgcv)
ad$Tot = rowSums(ad[,8:ncol(ad)])
ad$TotnCod = ad$Tot - ad$EST_DISCARD_WT.10
ads = subset(ad,SOAK_DAYS>2 & SOAK_DAYS<20,select=c(SOAK_DAYS, NUM_HOOK_HAUL, EST_DISCARD_WT.10, TotnCod))
ads$NUM_HOOK_HAULR = ads$NUM_HOOK_HAUL/100

	f1 = formula(EST_DISCARD_WT.10~ s(SOAK_DAYS) + s(TotnCod) + offset(NUM_HOOK_HAULR)) 
	aa = gam(f1,data=ads, family = Tweedie(p=1.5,link=power(.1)),method='REML') ##
	newd = data.frame(SOAK_DAYS=c(3:20),TotnCod=rep(300,18),NUM_HOOK_HAULR=rep(1,18))


set.seed(1000)
n_sims =1000	


Ps = newd

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


apreds$SOAK_DAYS= newd$SOAK_DAYS 



asa = as.data.frame(aggregate(.~SOAK_DAYS,data=apreds,FUN=sum))
ag = apply(asa[,2:1001],1,quantile,0.5)/1000

#root mean square error

calc_RMSE <- function(pred, obs){
  	RMSE <- round(sqrt(mean((pred-obs)^2)),3)
  	return(RMSE)
	}



#cross validation

	xVTweedie <- function(data=ads, model=f1, prop.training=.85,nruns=100,response='EST_DISCARD_WT.10') {

		nsamp = round(nrow(data)*prop.training)
		vec = 1:nrow(data)
		RMSE = c()
		test.data = list()
		for(i in 1:nruns) {
					a = sample(vec,nsamp)
					training = data[a,]
					test = data[which(!vec %in% a),]
					mod1 = try(gam(model,data=training,family = Tweedie(p=1.25,link=power(.1))))
					if(grepl('Error',mod1[1])) next()
					test$pred = predict(mod1,newdata = test,type='response')
					RMSE[i] =  calc_RMSE(test$pred,test[,response])
					test.data[[i]] = test
		}
		return(RMSE)
	}
	f1 = formula(EST_DISCARD_WT.10~ s(SOAK_DAYS) + s(TotnCod) + offset(NUM_HOOK_HAULR)) 
	f2 = formula(EST_DISCARD_WT.10~ s(SOAK_DAYS) + offset(NUM_HOOK_HAULR)) 
	f3 = formula(EST_DISCARD_WT.10~ offset(NUM_HOOK_HAULR)) 
	
	xVresults = xVTweedie(model=f1)
	xVresults2 = xVTweedie(model=f2)
xVresults2 = xVTweedie(model=f2)

	mean




	(xVresults)
	