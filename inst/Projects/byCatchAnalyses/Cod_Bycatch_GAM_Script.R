#Bank of packages needed to complete the following analysis:#

library(mgcv)
library(dplyr)
library(ggplot2)
library(tidyverse)
require(devtools)
require(SpatialHub)
library(sf)
library(lubridate)
library(MASS)
library(sp)
library(rgdal)


#Pull the data needed from atSS#
Cod2 <- atSS %>% dplyr::select(UID,STARTDATE, LFA, DEPTH, GRIDNO, SPECIESCODE, SPECIES, EST_NUM_CAUGHT, EST_KEPT_WT, EST_DISCARD_WT, Sampled, Total, Nempty, Y, X)

#Remove NA's from weight and # caught data# 
Cod2$EST_KEPT_WT[is.na(Cod2$EST_KEPT_WT)] <- 0
Cod2$EST_DISCARD_WT[is.na(Cod2$EST_DISCARD_WT)] <- 0
Cod2$EST_NUM_CAUGHT[is.na(Cod2$EST_NUM_CAUGHT)] <- 0

#Get total weight and number caught into respective columns#
Cod2$CODWEIGHT <- ifelse(Cod2$SPECIESCODE == 10,Cod2$EST_DISCARD_WT+Cod2$EST_KEPT_WT,0)
Cod2$CODNUMBER <- ifelse(Cod2$SPECIESCODE == 10,Cod2$EST_NUM_CAUGHT,0)

#Select unique IUDs based on cod caught or no cod caught# 
Codfiltered <-  Cod2 %>% group_by(UID) %>% filter(CODWEIGHT==max(CODWEIGHT))
Codfiltered2 <- Codfiltered %>% distinct(UID, .keep_all = TRUE)

#Leave base dataset alone and manipulate variables 
Codtot <- Codfiltered2
Codtot <- Codtot %>% mutate(Month = month(STARTDATE, label = TRUE))
Codtot$LFA <- as.factor((Codtot$LFA))
Codtot$Month <- as.factor(Codtot$Month)
Codtot <- Codtot %>% mutate(Year = year(STARTDATE)) %>% mutate(Day = day(STARTDATE))
Codtot$Year <- as.factor(Codtot$Year)

#Before removing all NA data select only the variables necessary for the model#
totdat3 <- Codtot %>% dplyr::select(CODWEIGHT,CODNUMBER, Year, DEPTH, X, Y, Sampled)
totdat3 <- na.omit(totdat3)
totdat3 <- totdat3 %>% rename("SET_DEPTH" = "DEPTH")

#Transform Decimal degree coordinates into UTM projection#
latlon <- totdat3 %>% dplyr::select(X,Y)
coordinates(latlon) <- c("X", "Y")
proj4string(latlon) <- CRS("+proj=longlat +datum=WGS84")
res <- spTransform(latlon, CRS("+proj=utm +zone=20 ellps=WGS84"))
planar <-as.data.frame(res)
planar <- planar %>% rename("plon" = "X") %>% rename("plat" = "Y")

#Further analysis needs km instead of meters#
planar <- planar %>% mutate(plon = plon/1000) %>% mutate(plat = plat/1000)
totdat3 <- merge(totdat3,planar,by="UID")

#Build GAM formula and model# 
#CTF = formula(CODWEIGHT~ as.factor(Year) + s(SET_DEPTH, k = 20) + s(plon, plat, bs='ts',k=100))
#CTM = gam(CTF,data=totdat3, family = Tweedie(p=1.25,link=power(.1)), method = "REML")

CTF = formula(CODWEIGHT~ as.factor(Year) + s(SET_DEPTH, k = 20) + s(plon, plat, bs='ts',k=100), offset = Sampled)
CTM = gam(CTF,data=totdat3, family = Tweedie(p=1.25,link=power(.1)), method = "REML")

#Run diagnostics#
summary(CTM)
gam.check(CTM)
plot(CTM, scheme = 2)

#Cross-validate the model#
calc_RMSE <- function(pred, obs){
  RMSE <- round(sqrt(mean((pred-obs)^2)),3)
  return(RMSE)
}

totTweedie <- function(data=totdat3, model=CTF, prop.training=.85,nruns=100) {
  
  nsamp = round(nrow(data)*prop.training)
  vec = 1:nrow(data)
  RMSE = c()
  test.data = list()
  for(i in 1:nruns) {
    iterror = tryCatch({
      a = sample(vec,nsamp)
      training = data[a,]
      test = data[which(!vec %in% a),]
      mod1 = gam(model,data=training,family = Tweedie(p=1.25,link=power(.1)), method = "REML")
      test$pred = predict(mod1,newdata = test,type='response')
      RMSE[i] =  calc_RMSE(test$pred,test$CODWEIGHT)
      test.data[[i]] = test}
      , error=function(e) e)
    if(inherits(iterror, "error")) next
    
  }
  return(RMSE)
}

totresults = totTweedie()

totresults

NNtotresults = na.omit(totresults)
mean(NNtotresults)

#Overall model
oo = totdat3$CODWEIGHT
pp = predict(CTM,type='response')

#normalized RMSE
calc_RMSE(pp,oo) / mean(oo)

plot(pp,oo)
abline(a=0,b=1)

#pull spatial data frame#
Ps = data.frame(EID=1:nrow(predSpace),predSpace[,c('plon','plat','z')])
Ps <- Ps %>% rename("SET_DEPTH" = "z")

# annual predictions
R1index=c()
R1area = list()
R1surface=list()
ilink <- family(CTM)$linkinv # this is the inverse of the link function


Years = unique(totdat3$Year)

for(i in 1:length(Years)){
  require(mgcv)
  
  #Ps$dyear =Years[i]+.5
  Ps$Year = Years[i]
  Ps$Sampled = mean(totdat3$Sampled)
  
  plo = as.data.frame(predict(CTM,Ps,type='link',se.fit=TRUE))
  plo$upper = ilink(plo$fit - (1.96 * plo$se.fit))  
  plo$lower = ilink(plo$fit - (1.96 * plo$se.fit))
  plo$fitted = ilink(plo$fit)
  
  
  xyz = data.frame(Ps[,c('plon','plat')],z=ilink(plo$fit))
  corners = data.frame(lon=c(-67.8,-62),lat=c(41,46))
  
  R1area[[i]] = c(Years[i],length(which(xyz$z<5)))
  planarMap(xyz, fn=paste("gamtwPAR1",Years[i],sep='.'), datascale=seq(0.1,1000,l=30), annot=Years[i],loc=fpf1, corners=corners,log.variable=T)
  #planarMap( xyz, fn=paste("lobster.gambi.pred",Years[i],sep='.'), annot=Years[i],loc="output",corners=corners)
  #planarMap( xyz, corners=corners)
  R1surface[[i]]=xyz
  R1index[i]= sum(xyz$z)
}

#Currently this is still not converging (June 15th 2020) May not be necessary though#
tweedie_boot <- function(data,i, pred.data=Ps,years=Years) {
  #if(is.null(nsamples)) nsamples = nrow(data)
  # i = sample(1:nsamples, nsamples, replace=T )
  data_boot <- data[i, ]
  CTM = gam(CODWEIGHT~as.factor(Year) + s(SET_DEPTH, k=25) + s(X, Y, bs="ts", k=100) + offset(Sampled),data=data_boot, family = Tweedie(p=1.25,link=power(.1)), method = "REML")
  ilink <- family(CTM)$linkinv   # this is the inverse of the link function
  R1index	= c()	
  for(g in 1:length(years)){
    pred.data$Year =years[g]
    pred.data$Sampled = mean(data_boot$Sampled)
    plo = predict(CTM,pred.data,type='response')
    R1index[g]= sum(plo)
  }
  return(R1index)
}

#bootstrapping statistics
install.packages("boot")
library("boot")

b <- boot(data=totdat3, statistic = tweedie_boot, R = 5)

plot(apply(b$t/10000,2,mean))

#No convergence, NEED TO FIX THIS#

#Add base model here again

CTF2 = formula(CODWEIGHT~ as.factor(Year) + s(SET_DEPTH, k = 20) + s(plon, plat, bs='ts',k=100))
CTM2 = gam(CTF2,data=totdat3, family = Tweedie(p=1.25,link=power(.1)), method = "REML")
memory.limit(size=16000)

set.seed(1000)
n_sims =1000	


Pss = list()

for(i in 1:length(Years)){
  Pst = Ps
  Pst$YEAR = Years[i]
  Pst$Sampled = mean(subset(totdat3,Year==Years[i],Sampled)[,1])
  Pss[[i]] = Pst
} 


Ps = do.call(rbind,Pss)



a_lp_matrix = predict(object = CTM2, Ps,
                      type = "lpmatrix")

a_coef_mean = coef(CTM2)
a_vcov = vcov(CTM2)
a_par_coef_posterior = rmvn(n = n_sims, 
                            mu = a_coef_mean,
                            V = a_vcov)
ilink = family(CTM2)$linkinv

preds = ilink(a_lp_matrix %*% t(a_par_coef_posterior))
apreds = as.data.frame(preds)


apreds$YEAR = Ps$YEAR 



asa = as.data.frame(aggregate(.~YEAR,data=apreds,FUN=sum))
ag = apply(asa[,2:1001],1,quantile,0.5)/1000

png(file=file.path(fpf1,'ILTSrecruitabund.png'),units='in',width=15,height=12,pointsize=18, res=300,type='cairo')
plot(asa$YEAR,apply(asa[,2:1001],1,quantile,0.5)/1000,xlab='Year',ylab='Recruit abundance',pch=16,ylim=c(0,55000))
arrows(asa$YEAR, y0 = apply(asa[,2:1001],1,quantile,0.025)/1000, y1 =apply(asa[,2:1001],1,quantile,0.975)/1000,length=0 )
lines(asa$YEAR,runmed(ag,k=3),col='salmon',lwd=2)
dev.off()