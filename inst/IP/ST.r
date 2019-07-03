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

aggregate(EST_DISCARD_WT.10/NUM_HOOK_HAUL~SOAK_DAYS,data=ad,FUN=mean)


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



### 
require(VGAM)

shape1 <- 1; shape2 <- 1; y <- 0:30
proby <- dbetageom(y, shape1, shape2, log = FALSE)
plot(y, proby, type = "h", col = "blue", ylab = "P[Y=y]", main = paste("Y ~ Beta-geometric(shape1=", shape1,", shape2=", shape2, ")", sep = ""))
dat1 = rep(y,times=proby*100)
dat1 = data.frame(Grp=1,Data=dat1)
shape1 <- 1; shape2 <- 2; y <- 0:30
proby <- dbetageom(y, shape1, shape2, log = FALSE)
plot(y, proby, type = "h", col = "blue", ylab = "P[Y=y]", main = paste("Y ~ Beta-geometric(shape1=", shape1,", shape2=", shape2, ")", sep = ""))
dat2 = rep(y,times=proby*100)
dat2 = data.frame(Grp=2,Data=dat2)
dat3 = as.data.frame(rbind(dat1,dat2))
vglm(Data~Grp,data=dat3,family='betageometric')
