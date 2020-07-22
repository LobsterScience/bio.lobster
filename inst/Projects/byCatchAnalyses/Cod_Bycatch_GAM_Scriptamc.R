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

load('~/tmp/atSeaData.rdata')
load('~/tmp/AtSeaDataAggregatedWithEmptyTrapsCoates.rdata')

load('/SpinDr/backup/bio_data/bio.lobster/data/predspace.rdata')

    #Pull the data needed from atSS#
    Cod2 <- atSS %>% dplyr::select(UID,STARTDATE, LFA, DEPTH, GRIDNO, SPECIESCODE, SPECIES, EST_NUM_CAUGHT, EST_KEPT_WT, EST_DISCARD_WT, NUM_HOOK_HAUL, Sampled, Total, Nempty, Y, X)

    #Remove NA's from weight and # caught data# 
    Cod2$EST_KEPT_WT[is.na(Cod2$EST_KEPT_WT)] <- 0
    Cod2$EST_DISCARD_WT[is.na(Cod2$EST_DISCARD_WT)] <- 0
    Cod2$EST_NUM_CAUGHT[is.na(Cod2$EST_NUM_CAUGHT)] <- 0
    Cod2$NUM_HOOK_HAUL[is.na(Cod2$NUM_HOOK_HAUL)] <- 0

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
    totdat3 <- Codtot %>% dplyr::select(CODWEIGHT,CODNUMBER, Year, DEPTH, X, Y, Sampled,NUM_HOOK_HAUL)
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

    #AMC
    totdat3 = subset(totdat3, NUM_HOOK_HAUL>1)
    totdat3$lHook = log(totdat3$NUM_HOOK_HAUL)
    save(totdat3,file='~/tmp/totdat3.rdata')
    CTF = formula(CODWEIGHT~ as.factor(Year) + s(SET_DEPTH, k = 4) + s(plon, plat, bs='ts',k=100)+offset(lHook))

    #want the offset to be same as link
    CTM = gam(CTF,data=totdat3, family = Tweedie(p=1.25,link=log), method = "REML")


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

    #predict on 10 traps

    Ps$lHook = log(10)
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

load('~/tmp/totdat3.rdata')

totdat3 = subset(totdat3, Year %in% c(2004:2016))

#CTF2 = formula(CODWEIGHT~ as.factor(Year) + s(SET_DEPTH, k = 4) + s(plon, plat, bs='ts',k=100)+offset(lHook))
#CTM2 = gam(CTF2,data=totdat3, family = Tweedie(p=1.25,link=log), method = "REML")

CTF3 = formula(CODWEIGHT~ (Year) +  s(plon, plat, bs='ts',k=30)+offset(lHook))
CTM3 = gam(CTF3,data=totdat3, family = Tweedie(p=1.25,link=log), method = "REML")

memory.limit(size=16000)

set.seed(1000)
n_sims =1000	


#logbook trap hauls by grid by year
  require(bio.lobster)
  la()
  logs = lobster.db('process.logs')
  logs = subset(logs, LFA>32)
  logs$Year = year(logs$DATE_FISHED)

  lobster.db('logs41')
  log4 = logs41
  log4$Date = strptime(log4$FV_FISHED_DATETIME,format = '%Y-%m-%d %H:%M:%S')
   
  log4$year = year(log4$Date)  
  l41s = aggregate(NUM_OF_TRAPS~year+OFFAREA, data=log4, FUN=sum)
  lrest3 = aggregate(NUM_OF_TRAPS~GRID_NUM+Year,data=logs,FUN=sum)
  
    LFAgrid<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","GridPolys.csv"))
    LFA41<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","LFA41Offareas.csv"))
    grL = subset(LFAgrid, PID>32)    
    grL$area=grL$PID
    LFA41$area = LFA41$OFFAREA
    LFA41$OFFAREA <- NULL
    LFA41$SID = 1
    grs = rbind(grL,LFA41, all=T)
    if(any(grs$area==T)) grs = grs[-which(grs$area==T),]

latlon <- grs %>% dplyr::select(X,Y)
coordinates(latlon) <- c("X", "Y")
proj4string(latlon) <- CRS("+proj=longlat +datum=WGS84")
res <- spTransform(latlon, CRS("+proj=utm +zone=20 ellps=WGS84"))
planar <-as.data.frame(res)
planar <- planar %>% rename("plon" = "X") %>% rename("plat" = "Y")

#Further analysis needs km instead of meters#
planar <- planar %>% mutate(plon = plon/1000) %>% mutate(plat = plat/1000)
grs$X <- planar$plon
grs$Y <- planar$plat

Ps$X = Ps$plon
Ps$Y = Ps$plat
ht = findPolys(Ps, grs,maxRows=nrow(Ps))
Ps = merge(Ps,ht)


Pss = list()
Years = unique(totdat3$Year)

require(bio.utilities)
for(i in 1:length(Years)){
  Pst = Ps
  Pst$Year = Years[i]
  Pst$NUM_OF_TRAPS = NA
  Pst$GRID_NUM = Pst$SID
  lw = subset(lrest3, Year==Years[i])
  Pst1 = subset(Pst,PID>10)
  Pst1$GRID_NUM = Pst1$SID
  Pst1 = fillNaDf2(Pst1,lw,'GRID_NUM','NUM_OF_TRAPS' )
  Pst2 = subset(Pst,PID<10)
  l41s1 = subset(l41s, year==Years[i])
  l41s1$PID = ifelse(l41s1$OFFAREA=='GBANK',2,ifelse(l41s1$OFFAREA=='GBASIN',3,ifelse(l41s1$OFFAREA=='SEBROWNS',4,ifelse(l41s1$OFFAREA=='SWBROWNS',5,1))))

  Pst2 = fillNaDf2(Pst2,l41s1,'PID','NUM_OF_TRAPS' )

Pst = rbind(Pst1, Pst2)
Pst = subset(Pst, !is.na(NUM_OF_TRAPS))
Pst$lHook = log(Pst$NUM_OF_TRAPS)
   Pss[[i]] = Pst
} 

Ps = do.call(rbind,Pss)


Ps$lHook = log(10)
a_lp_matrix = predict(object = CTM3, Ps,
                      type = "lpmatrix")

a_coef_mean = coef(CTM3)
a_vcov = vcov(CTM3)
a_par_coef_posterior = rmvn(n = n_sims, 
                            mu = a_coef_mean,
                            V = a_vcov)
ilink = family(CTM3)$linkinv

preds = ilink(a_lp_matrix %*% t(a_par_coef_posterior))
apreds = as.data.frame(preds)

Press = cbind(Ps,apreds)
save(Press,file='~/tmp/outss.rdata')
load('~/tmp/outss.rdata')
#apreds$YEAR = Ps$Year

apply(Press[,14:1013],1,median)

asa = as.data.frame(aggregate(.~YEAR,data=apreds,FUN=sum))
ag = apply(asa[,:1001],1,quantile,0.5)/1000

png(file=file.path(fpf1,'ILTSrecruitabund.png'),units='in',width=15,height=12,pointsize=18, res=300,type='cairo')
plot(asa$YEAR,apply(asa[,2:1001],1,quantile,0.5)/1000,xlab='Year',ylab='Recruit abundance',pch=16,ylim=c(0,55000))
arrows(asa$YEAR, y0 = apply(asa[,2:1001],1,quantile,0.025)/1000, y1 =apply(asa[,2:1001],1,quantile,0.975)/1000,length=0 )
lines(asa$YEAR,runmed(ag,k=3),col='salmon',lwd=2)
dev.off()