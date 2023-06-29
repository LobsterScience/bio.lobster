###########################
#start with an overview of all fishing year by year
require(bio.lobster)
require(bio.utilities)
require(dplyr)
require(devtools)
require(sf)
require(ggplot2)
require(mgcv)
require(ggeffects)
require(ggforce)
fd = file.path(project.datadirectory('bio.lobster'),'analysis','CPUE')
setwd(fd)

gr = readRDS('C:/Users/Cooka/Documents/git/bio.lobster.data/mapping_data/GridPolyLand.rds')
st_crs(gr) <- 4326
gr = st_transform(gr,32620) 
st_geometry(gr) <- st_geometry(st_as_sf(gr$geometry/1000)) 
st_crs(gr) <- 32620

#need the temperature - catch relationship using IP/catchability/TempCatch.r output
preT = readRDS(file=file.path(project.datadirectory('bio.lobster'),'analysis','ClimateModelling','tempCatchability.rds'))
preT$temp = round(preT$temp,2)
preT = aggregate(pred~temp,data=preT,FUN=mean)

aT = readRDS('CPUETempDepth.rds')
aT = subset(aT,SYEAR>2005 & SYEAR<2023)

#start with an overall CPUE trend

aa = split(aT,f=list(aT$LFA,aT$SYEAR))
cpue.lst<-list()
cpue.ann<- list()
for(i in 1:length(aa)){
  tmp<-aa[[i]]
  if(nrow(tmp)==0) next
  tmp = tmp[,c('DATE_FISHED','WEIGHT_KG','NUM_OF_TRAPS','temp')]
  names(tmp)<-c('time','catch','effort','temp')
  tmp$date<-as.Date(tmp$time)
  first.day<-min(tmp$date)
  tmp$time<-julian(tmp$date,origin=first.day-1)
  g<-as.data.frame(biasCorrCPUE(tmp,by.time=T))
  g$lfa=unique(aa[[i]]$LFA)
  g$yr = unique(aa[[i]]$SYEAR)
  te = aggregate(temp~time, data=tmp,FUN=mean)
  g = merge(g,te,by.x='t',by.y='time')
  gl = aggregate(effort~time, data=tmp, FUN=sum)
  g = merge(g,gl,by.x='t',by.y='time')
  cpue.lst[[i]] <- g
  
  g<-as.data.frame(biasCorrCPUE(tmp,by.time=F))[2,]
  g=c(g,as.numeric(unique(aa[[i]]$LFA)),as.numeric(unique(aa[[i]]$SYEAR)),as.numeric(aggregate(temp~1, data=tmp,FUN=mean)))
  cpue.ann[[i]]=g
}

cc =as.data.frame(do.call(rbind,cpue.lst))
cc$dyear = cc$yr+cc$t/365
cc$temp = round(cc$temp,1)
cc = merge(cc,preT)
cc = cc[order(cc$fYear,cc$t),]
ggplot(subset(cc,lfa==36),aes(x=t,y=unBCPUE))+geom_point() + facet_wrap(~fYear)
ggplot(subset(cc,lfa==36),aes(x=t,y=temp))+geom_point() + facet_wrap(~fYear)

dd = as.data.frame(do.call(rbind,cpue.ann))
names(dd) = c('unBCPUE','lfa','yr','temp')
dd$temp = round(dd$temp,1)
dd = merge(dd,preT)
dd$unBCPUE_tcorr = dd$unBCPUE / dd$pred
dd = dd[order(dd$yr),]
#overall
#lfa 36
with(subset(dd,lfa==36),plot(yr,unBCPUE,pch=16,cex=.5,type='b',col='red',lwd=3,yaxt='n',ylab='CPUE'))
par(new=T)
with(subset(dd,lfa==36),plot(yr,unBCPUE_tcorr,pch=16,cex=.5,type='b',col='blue',lwd=3,yaxt='n',ylab= ''))
legend('topleft',col=c('red','blue'), lty=c(1,1),c('Bias Corrected', 'Bias and Temperature Corrected'),bty='n')

cc$fYear = as.factor(cc$yr)
l36 = gam(unBCPUE~s(t),data=subset(cc,lfa==36),family = 'nb')
l36a = gam(unBCPUE~s(t)+s(temp),data=subset(cc,lfa==36),family = 'nb')
l36b = gam(unBCPUE~s(t)+s(temp)+s(t,by=fYear),data=subset(cc,lfa==36),family = 'nb')
l36c = gam(unBCPUE~fYear+s(t)+s(temp)+s(t,by=fYear),data=subset(cc,lfa==36),family = 'nb') 
l36d = gam(unBCPUE~fYear+s(t)+s(temp),data=subset(cc,lfa==36),family = 'nb')#best by AIC

cc$land = cc$unBCPUE * cc$effort
cc$leffort = log(cc$effort)
l36cl = gam(land~fYear+s(t)+s(temp)+s(t,by=fYear)+offset(leffort),data=subset(cc,lfa==36),family = 'nb') #best by AIC
#l36dl = gam(land~fYear+s(t)+s(temp)+offset(leffort),data=subset(cc,lfa==36),family = 'nb') #best by AIC

##Part 1, raw CPUE
xx = subset(cc,lfa==36)
l36P1 = gam(land~fYear+offset(leffort),data=xx,family = 'nb') #best by AIC
pr1 = ggpredict(l36P1,terms='fYear',condition = c(leffort=0))
summary(l36P1)
AIC(l36P1)

plot(pr1)

##Part 2, add in Day of Season
xx = subset(cc,lfa==36)
l36P2 = gam(land~fYear+s(t)+offset(leffort),data=xx,family = 'nb') #best by AIC
pr2 = ggpredict(l36P2,terms=c('t','fYear'),condition = c(leffort=0))
summary(l36P2)
AIC(l36P2)
ggplot(pr2, aes(x = x, y = predicted)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)+
  facet_wrap(~group)+
  xlab('Day of Season')+
  ylab('CPUE')

##Part 3, add in Day of Season and Temp
xx = subset(cc,lfa==36)
l36P3 = gam(land~fYear+s(t)+s(temp)+offset(leffort),data=xx,family = 'nb') #best by AIC
pr3 = ggpredict(l36P3,terms=c('t','fYear'),condition = c(leffort=0,temp=8))
summary(l36P3)
AIC(l36P3)
ggplot(pr3, aes(x = x, y = predicted)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)+
  facet_wrap(~group)+
  xlab('Day of Season')+
  ylab('CPUE')


##Part 4, add in Day of Season and Temp and interaction
xx = subset(cc,lfa==36)
l36P4 = gam(land~fYear+s(t)+s(temp)+s(t,by=fYear)+offset(leffort),data=xx,family = 'nb') #best by AIC
pr4 = ggpredict(l36P4,terms=c('t','fYear'),condition = c(leffort=0,temp=8))
summary(l36P4)
AIC(l36P4)
ggplot(pr4, aes(x = x, y = predicted)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)+
  facet_wrap(~group)+
  xlab('Day of Season')+
  ylab('CPUE')

l36cl = gam(land~fYear+s(t)+s(temp)+s(t,by=fYear)+offset(leffort),data=subset(cc,lfa==36),family = 'nb') #best by AIC


##
l36Basel = gam(land~s(t)+s(temp)+offset(leffort),data=subset(cc,lfa==36),family = 'nb')


require(ggeffects)
mydft <- ggpredict(l36cl, terms = c('t'),condition=c(leffort=0,fYear=2016)) #
ggplot(mydft, aes(x = x, y = predicted)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)+
  xlab('Day of Season')+
  ylab('CPUE')

mydf <- ggpredict(l36cl, terms = c('t','fYear'),condition=c(leffort=0,temp=8)) #
plot(mydf,color='bw', ci=F)
ggplot(mydf, aes(x = x, y = predicted)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)+
  facet_wrap(~group)+
  xlab('Day of Season')+
  ylab('CPUE')

#residuals analysis
bprd = ggpredict(l36Basel,terms=c('t'),condition=c(leffort=0,temp=8))
bprdP = data.frame(baset = bprd$x,basepred = bprd$predicted)

mypred = data.frame(t=mydf$x,pred=mydf$predicted,yr=mydf$group)

combData = merge(mypred,bprdP,by.x='t',by.y='baset')
combData$res = combData$pred - combData$basepred

ggplot(combData, aes(x = t, y = res)) +
  geom_link2(aes(colour=after_stat(ifelse(y>0,'positive','negative'))),size=2,show.legend = F)+
  facet_wrap(~yr)+
  xlab('Day of Season')+
  ylab('Residuals')+
  labs(fill='Residual')+
  geom_hline(yintercept=0,color='grey')

xf = lobster.db('seasonal.landings')
xf$yr = as.numeric(substr(xf$SYEAR,6,9))

combDataL = merge(combData,xf[,c('yr','LFA36')])
combDataL = combDataL[order(combDataL$LFA36,combDataL$t),]
ggplot(combDataL, aes(x = t, y = res)) +
  geom_link2(aes(colour=after_stat(ifelse(y>0,'positive','negative'))),size=2,show.legend = F)+
  facet_wrap(~LFA36)+
  xlab('Day of Season')+
  ylab('Residuals')+
  labs(fill='Residual')+
  geom_hline(yintercept=0,color='grey')


#what about temperatures for those years
xxS = subset(xx,select=c(yr,temp,t))

combDataLT = merge(combDataL,xxS)
combDataLT = combDataLT[order(combDataLT$LFA36,combDataLT$t),]
avgT = aggregate(temp~t,data=combDataLT,FUN=mean)
names(avgT)[2] = 'avgT'
combDataLTa = merge(combDataLT,avgT)
combDataLTa$tempDiff = combDataLTa$temp-combDataLTa$avgT

ggplot(combDataLTa, aes(x = t, y = tempDiff)) +
  geom_link2(aes(colour=after_stat(ifelse(y>0,'positive','negative'))),size=2,show.legend = F)+
  facet_wrap(~LFA36)+
  xlab('Day of Season')+
  ylab('Temp Diffs')+
  geom_hline(yintercept=0,color='grey')

##index 
require(gratia)
o = data_slice(l36P4,var1='fYear',offset=0)
f  = fitted_values(l36P4,data=o,terms=c('(Intercept)','fYear'))
ggplot(f,aes(x=fYear,y=fitted,group=1))+ geom_point()+ geom_errorbar(aes(ymin=lower,ymax=upper),width=.2)+stat_summary(fun.y=sum,geom='line')


################################################################


#now full model including all lic holders
x = subset(aT,LFA==36 & SYEAR<2023 & CPUE<40)
x$fYear = as.factor(x$SYEAR)
x$leffort = log(x$NUM_OF_TRAPS)
l36L = gam(TOTAL_WEIGHT_KG~fYear+s(DOS)+s(temp)+s(DOS,by=fYear)+offset(leffort),data=x,family = 'nb') #best by AIC
pr4 = ggpredict(l36L,terms=c('DOS','fYear'),condition = c(leffort=0,temp=8))
summary(l36P4)
AIC(l36P4)
ggplot(pr4, aes(x = x, y = predicted)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)+
  facet_wrap(~group)+
  xlab('Day of Season')+
  ylab('CPUE')

pr4 = ggpredict(l36L,terms=c('temp'))
plot(pr4)


require(gratia)
o1 = data_slice(l36L,var1='fYear',offset=0)
f1  = fitted_values(l36L,data=o1,terms=c('(Intercept)','fYear'))
ggplot(f1,aes(x=fYear,y=fitted,group=1))+ geom_point()+ geom_errorbar(aes(ymin=lower,ymax=upper),width=.2)+stat_summary(fun.y=sum,geom='line')


#random licence effect
x$rLID = as.factor(x$LICENCE_ID)
l36rL = gam(TOTAL_WEIGHT_KG~fYear+s(DOS)+s(temp)+s(DOS,fYear,bs='fs',k=6)+s(rLID,bs='re')+offset(leffort),data=x,family = 'nb') #best by AIC
o1r = data_slice(l36rL,var1='fYear',offset=0)
f1r  = fitted_values(l36rL,data=o1r,terms=c('(Intercept)','fYear'))
ggplot(f1r,aes(x=fYear,y=fitted,group=1))+ geom_point()+ geom_errorbar(aes(ymin=lower,ymax=upper),width=.2)+stat_summary(fun.y=sum,geom='line')

pr4 = ggpredict(l36rL,terms=c('temp'))
plot(pr4)

pr4 = ggpredict(l36rL,terms=c('DOS','fYear'),condition = c(leffort=0,temp=8))
summary(l36P4)
AIC(l36P4)
ggplot(pr4, aes(x = x, y = predicted)) +
  geom_point() +
  #geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)+
  facet_wrap(~group)+
  xlab('Day of Season')+
  ylab('CPUE')
saveRDS(l36rL,file='LFA36RLicJune2023.rds')
l36rL=readRDS(file='LFA36RLicJune2023.rds')

###
##using this with ASFMC 65%% criteria
f = ASMFC_triggers(x=f1r$fitted,yrs=f1r$fYear)
 plot(f$yrs,f$index,xlab='Year',ylab='Scaled CPUE', type='b')
 abline(h=.65,lty=2)
##


aa = subset(aT,SYEAR>2005 & SYEAR<2023)
xx = split(aa,f=aa$SYEAR)
m=0
ou = list()
for(i in 1:length(xx)){
     w=   xx[[i]]
     y = unique(w$SYEAR)
     ww = unique(w$GRID_NUM)
    for(j in 1:length(ww)){
      m=m+1
      www = subset(w,GRID_NUM==ww[j])
      g = unique(www$GRID_NUM)
      uL = length(unique(www$LICENCE_ID))
      uLFA = length(unique(www$LFA))
      #grArea = as.numeric(sum(st_area(subset(gr,grid==ww[j]))))#these are wrong do no deal with holes right
      wa = aggregate(cbind(WEIGHT_KG,NUM_OF_TRAPS)~GRID_NUM+DATE_FISHED+temp,data=www,FUN=sum)
      wa = wa[order(wa$DATE_FISHED),]
      wa$CPUE = wa$WEIGHT_KG / wa$NUM_OF_TRAPS
      wa$CUME = cumsum(wa$NUM_OF_TRAPS)
      wa$CUML = cumsum(wa$WEIGHT_KG)
      wa$rT = round(wa$temp,2)
      wa = merge(wa,preT,by.x='rT',by.y='temp')
      wa = wa[order(wa$DATE_FISHED),]
      wa$SCPUE = wa$pred*wa$CPUE
      wa$cpue=wa$SCPUE
      wa$effort = wa$NUM_OF_TRAPS
      wa$catch = wa$WEIGHT_KG
      wa$weight = wa$NUM_OF_TRAPS
      le = deluryLeslie(y=wa,estimate='leslie',method='robust',plot=F,weight = wa$weight)
      ou[[m]] = c(y,g,uL,uLFA,le$q,le$N0,max(wa$CUML),max(wa$CUME),nrow(www))
    }
  
}
out = as.data.frame(do.call(rbind,ou))
names(out) = c('Year','Grid','NLics','NLFAs','q','No','Land','Effort','Trips')
out$CPUE = out$Land/out$Effort

################ grouping grids
ou = list()
m=0
LFAgrid <- read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","GridPolys.csv"))
grL = (LFAgrid)
grL$area=grL$PID
attr(grL,'projection') <- 'LL'

#grs is the grids
#making the list of connections to grids
require(sp)
require(spdep)


  aa = subset(aT,SYEAR>2005 & SYEAR<2023)
  xx = split(aa,f=aa$SYEAR)
  m=0
  ou = list()
  for(i in 1:length(xx)){
    w=   xx[[i]]
    y = unique(w$SYEAR)
    ww = unique(w$GRID_NUM)
    grL1 = subset(grL,SID %in% ww)
    g1 = split(grL1,grL1$SID)
    nm = c()
    gp = list()
    for(k in 1:length(g1)){
      gp[[k]] = Polygons(list(Polygon(g1[[k]][,c('X','Y')])),unique(g1[[k]]$SID))
    }
    gpp = SpatialPolygons(gp,proj4string=CRS("+proj=longlat +datum=WGS84"))
    gpnb = poly2nb(gpp,row.names=names(gpp))
    names(gpnb)=names(gpp)
    
    for(j in 1:length(gpnb)){
      gr = names(gpnb)[gpnb[[j]]]
      ss = ifelse(length(gr)>=3,3,length(gr))
      gr = sample(gr,ss)
      m=m+1
      www = subset(w,GRID_NUM==gr)
      g = length(unique(www$GRID_NUM))
      gn = paste(gr,collapse="_")
      uL = length(unique(www$LICENCE_ID))
      uLFA = length(unique(www$LFA))
      bgf = subset(grL,SID %in%gr)
      attr(bgf,'projection') <-"LL"
      grArea = sum(calcArea(convUL(bgf))$area)
      wa = aggregate(cbind(WEIGHT_KG,NUM_OF_TRAPS)~GRID_NUM+DATE_FISHED+temp,data=www,FUN=sum)
      wa = wa[order(wa$DATE_FISHED),]
      wa$CPUE = wa$WEIGHT_KG / wa$NUM_OF_TRAPS
      wa$CUME = cumsum(wa$NUM_OF_TRAPS)
      wa$CUML = cumsum(wa$WEIGHT_KG)
      wa$rT = round(wa$temp,2)
      wa = merge(wa,preT,by.x='rT',by.y='temp')
      wa = wa[order(wa$DATE_FISHED),]
      wa$SCPUE = wa$pred*wa$CPUE
      wa$cpue=wa$SCPUE
      wa$effort = wa$NUM_OF_TRAPS
      wa$catch = wa$WEIGHT_KG
      wa$weight = wa$NUM_OF_TRAPS
      le = deluryLeslie(y=wa,estimate='leslie',method='robust',plot=F,weight = wa$weight)
      ou[[m]] = c(y,gn,g,uL,uLFA,le$q,le$N0,max(wa$CUML),max(wa$CUME),nrow(www))
    }
    
  }
  outa = as.data.frame(do.call(rbind,ou))
  names(outa) = c('Year','Grid','NGrids','NLics','NLFAs','q','No','Land','Effort','Trips')
  outa$CPUE = outa$Land/outa$Effort
  outa = toNums(outa,c(1,3:10))
  outas = subset(outa,No>0)
  outas$Rexp = outas$Land/outas$No
  ggplot(outas,aes(x=Land,y=No))+geom_point()+facet_wrap(~Year,scales='free')
  ggplot(outas,aes(x=Year,y=Rexp))
  
  
head(outa)  
