require(bio.lobster)
require(bio.utilities)
require(devtools)
require(dplyr)
require(sf)
require(tidyr)
require(MASS)
la()

#cpue data
gg = read.csv(file.path(project.datadirectory('bio.lobster'),'data','maps','GridGroups.csv'))
gg = subset(gg,LFA %in% c(33,34))
lg = lobster.db('process.logs')
lg = subset(lg,LFA %in% c(33,34) & SYEAR>2007 & SYEAR<2024)
g1 = merge(lg,gg)
ga = aggregate(cbind(NUM_OF_TRAPS,WEIGHT_KG)~DATE_FISHED+LFA+GridGrouping+SYEAR,data=g1,FUN=sum)
ga$CPUE = ga$WEIGHT_KG / ga$NUM_OF_TRAPS

write.csv(ga,file='LFA33_34CPUE_GRIDGROUP_DATE.csv')


#observed BTs
#lobster.db('fsrs')
##fsrs = merge(fsrs,gg,by.x=c('LFA_GRID','LFA'),by.y=c('GRID_NUM','LFA'))
#fsrs=subset(fsrs,!is.na(TEMP))

#fs = fsrs %>% dplyr::select(c(HAUL_DATE,GridGrouping,LFA,LAT_DD,LONG_DD,TEMP)) %>% distinct()
#fs = fs[order(fs$HAUL_DATE,fs$GridGrouping,fs$LFA),]
#write.csv(fs,file='LFA33_34_Observed_FSRS_Temperature_GRIDGROUP_DATE.csv')

#polygon maps
#sf_use_s2(FALSE)
#gg = st_read(file.path(project.datadirectory('bio.lobster'),'data','maps','GIS','LFA 34 Grid Grouping polygons_NAD83_region.shp'))
#st_crs(gg) <- 4326
#gg$DESCRIPTIO[9]='2A'
#gg1 = st_centroid(gg)
#gg1$X[9] = st_coordinates(gg1)[9,1]
#gg1$Y[9] = st_coordinates(gg1)[9,2]
#gg1$X[8] = st_coordinates(gg1)[8,1]
#gg1$Y[8] = st_coordinates(gg1)[8,2]

#ggplot(gg)+geom_sf()+geom_text(data=gg1,aes(x=X,y=Y,label=DESCRIPTIO))

#lfa 33
g2 = st_read(file.path(project.datadirectory('bio.lobster'),'data','maps','GIS','LFA 33 Grid Grouping polygons_NAD83_region.shp'))
st_crs(g2) <- 4326
gg2 = st_centroid(g2)
gg2$X = st_coordinates(gg2)[,1]
gg2$Y = st_coordinates(gg2)[,2]
ggplot(g2)+geom_sf()+geom_text(data=gg2,aes(x=X,y=Y,label=ID))

g2$area = st_area(g2)/1000000


#glorys12v1 from Xianmin mean BT within polygon
x = read.csv(file.path(project.datadirectory('bio.lobster'),'Temperature Data','CPUEModelling','GLORYS12v1_daily_BottomTemp_LFA33.csv'))
names(x)[2:ncol(x)] = c('bot01','bot05','bot02','bot04','bot08','bot07','bot06','bot09','bot10','bot03')
x$date = as.Date(x$date,format='%d-%b-%Y')
xl = pivot_longer(x,cols=starts_with('bot'),names_to = 'GridGrouping',values_to='BT')
xl = xl %>% mutate(GridGrouping = case_when(
      GridGrouping=='bot01'~1,
      GridGrouping=='bot02'~2,
      GridGrouping=='bot03'~3,
      GridGrouping=='bot04'~4,
      GridGrouping=='bot05'~5,
      GridGrouping=='bot06'~6,
      GridGrouping=='bot07'~7,
      GridGrouping=='bot08'~8,
      GridGrouping=='bot09'~9,
      GridGrouping=='bot10'~10
))

##############comparing glorys to observed temps 
# g = lobster.db('temperature.data')
# g = g %>% st_as_sf(coords=c('LON_DD','LAT_DD'),crs=4326)
# g = subset(g,year(T_DATE)>2003)
# 
# bb = st_as_sfc(st_bbox(gg2))
# cg = st_intersection(g,bb,join=st_within)
# g1 = st_join(g,g2,join=st_within)
# 
# ga2 = subset(g1,!is.na(GRIDNO))
# ga2$date = as.Date(ga2$T_DATE)
# 
# 
# gaa = aggregate(TEMP~GRIDNO+date,data=ga2,FUN='median')
# gx = merge(gaa,xl,by.x=c('date','GRIDNO'),by.y=c('date','GridGrouping'))
# gx$yr = lubridate::year(gx$date)
# gx$mn = lubridate::month(gx$date)
# gx$diff = gx$TEMP-gx$BT
# ix = 10
# pdf('Glorys2Obs.pdf')
# ##gridgroup 1
# #overall
# for(i in 1:ix){
# par(mfrow = c(2, 2), mar = c(4, 4, 2, 2))
# with(subset(xl,GridGrouping==i & lubridate::year(date) %in% 2010:2020),plot(date,BT,type='l',ylab='Temperature'))
# with(subset(gaa,GRIDNO==i & lubridate::year(date)),points(date,TEMP,type='p',col='red',pch=16,cex=.7))
# 
# #fishing season
# with(subset(xl,GridGrouping==i & lubridate::year(date) %in% 2010:2020),plot(date,BT,type='l',xlab='Points for Fishing Months',ylab='Temperature'))
# with(subset(gaa,GRIDNO==i & lubridate::year(date) & lubridate::month(date) %in% c(12,1:5)),points(date,TEMP,type='p',,col='red',pch=16,cex=.7))
# 
# with(subset(gx,GRIDNO==i ),boxplot(diff~mn,ylab='Temperature Difference (Obs - Glorys)',xlab='Month of Year'))
# abline(h=0,col='red',lty=2)
# with(subset(gx,GRIDNO==i & mn %in% c(12,1:5)),hist(diff,'fd',xlab='Temperature Difference (Obs - Glorys): Fishing Months',main=""))
# abline(v=0,col='red',lty=2)
# 
# mtext(paste('Grid Grouping', i),cex=1.5,side=3,line=-2,outer=T)
# }
# dev.off()
# 

#combining based on previous day's temperature since that is when 'fishing occurred'
xl$date = xl$date+1

gx = merge(subset(ga,LFA==33),xl,by.x=c('DATE_FISHED','GridGrouping'),by.y=c('date','GridGrouping'))

#simple correlations, obv positive, full year
gx$logCPUE = log(gx$CPUE)
cor.test(gx$logCPUE,gx$BT,data=gx)

gx$mn = lubridate::month(gx$DATE_FISHED)

with(subset(gx,mn %in% c(12,1)),cor.test(BT,CPUE))
with(subset(gx,mn %in% c(12,1)),plot(BT,CPUE))

### nlics and ndays per lic

g1$mn = lubridate::month(g1$DATE_FISHED)
gL = aggregate(LICENCE_ID~SYEAR+GridGrouping, data=subset(g1,mn %in% c(12,1)), FUN=function(x) length(unique(x)))
gD = aggregate(DATE_FISHED~SYEAR+GridGrouping+LICENCE_ID, data=subset(g1,mn %in% c(12,1)), FUN=function(x) length(unique(x)))
gD = aggregate(DATE_FISHED~SYEAR+GridGrouping, data=gD, FUN=median)
names(gD)[3] = 'medianDaysPerLic'
gdl = merge(gD,gL)
#robust linear regression applied by year and grid grouping .... ie is there a relationship
gxx = subset(gx,mn %in% c(12,1))
gxs = split(gxx,f=list(gxx$GridGrouping,gxx$SYEAR))
gxs = rm.from.list(gxs)
out = data.frame(LFA=33,GridGrouping=NA,SYEAR=NA,
                 slopeTC=NA,confintTCL=NA,confintTCU=NA,
                 slopeDiffs=NA,confintDiffsL=NA,confintDiffsU=NA,
                 SlopeTemp=NA,confintTempL=NA,confintTempU=NA,
                 SlopeCPUE=NA,confintCPUEL=NA,confintCPUEU=NA, lquantTemp=NA, uquantTemp=NA,medTemp=NA,meanTemp=NA,sdTemp=NA,
                 lquantCPUE=NA, uquantCPUE=NA,medCPUE=NA,meanCPUE=NA,sdCPUE=NA,ndays=NA,NTraps=NA,startTemp=NA)
i=0
for(m in 1:length(gxs)){
  b = gxs[[m]]
  if(nrow(b)>5){
    i=i+1
  out[i,'GridGrouping'] = unique(b$GridGrouping)
  out[i,'SYEAR'] = unique(b$SYEAR)
  out[i,'LFA'] = 33
  out[i,'Ntraps'] = sum(b$NUM_OF_TRAPS)
  out[i,'lquantTemp'] = quantile(b$BT, na.rm=T,.05)
  out[i,'uquantTemp'] = quantile(b$BT, na.rm=T,.95)
  out[i,'medTemp'] = quantile(b$BT, na.rm=T,.5)
  out[i,'meanTemp'] = mean(b$BT, na.rm=T)
  out[i,'sdTemp'] = sd(b$BT, na.rm=T)
  out[i,'startTemp'] = mean(b$BT[1:5], na.rm=T)
  out[i,'lquantCPUE'] = quantile(b$CPUE, na.rm=T,.05)
  out[i,'uquantCPUE'] = quantile(b$CPUE, na.rm=T,.95)
  out[i,'medCPUE'] = quantile(b$CPUE, na.rm=T,.5)
  out[i,'meanCPUE'] = mean(b$CPUE, na.rm=T)
  out[i,'sdCPUE'] = sd(b$CPUE, na.rm=T)
  out[i,'ndays'] = nrow(b)
  
  
  
  first.day<-min(b$DATE_FISHED)
  b$time<-julian(b$DATE_FISHED,origin=first.day-1)
  b = b[order(b$time),]
  dt = diff(b$BT)
  ##nonparametric smoother
  # local averaging (cv span selection)
  locavg <- with(b, supsmu(time,logCPUE))
  dc = diff(locavg$y)
 
  g = glm(dc~dt) #day previous as catch process is not at time i its time i-1
  out[i,'slopeDiffs'] = coef(g)[2]
  out[i,'confintDiffsL'] = confint(g,level=.8)[2,1]
  out[i,'confintDiffsU'] = confint(g,level=.8)[2,2]
  
  ##robust model including CIs  
  v = rlm(logCPUE~BT, data=b,weights=b$NUM_OF_TRAPS,wt.method='case')
  out[i,'slopeTC']   = coef(v)[2]
  bootstrap_func <- function(data, i) {
    fit <- rlm(logCPUE ~ BT, data = data[i, ],weights=data[i,'NUM_OF_TRAPS'],wt.method='case',method='M')
    coef(fit)
  }
  boot_results <- boot::boot(b, bootstrap_func, R = 1000)
  conf_intervals <- t(sapply(1:ncol(boot_results$t), function(i) {
    quantile(boot_results$t[, i], c(0.1, 0.9),na.rm=T)
  }))
  out[i,'confintTCL'] = conf_intervals[2,1]
  out[i,'confintTCU'] = conf_intervals[2,2]
  
  tt = glm(BT~time,data=b)
  out[i,'SlopeTemp'] = coef(tt)[2]
  out[i,'confintTempL'] = confint(tt,level=.8)[2,1]
  out[i,'confintTempU'] = confint(tt,level=.8)[2,2]
  
  
  tt = MASS::rlm(logCPUE~time,data=b,weights=b$NUM_OF_TRAPS,wt.method='case')
  out[i,'SlopeCPUE'] = coef(tt)[2]
  bootstrap_func <- function(data, i) {
    fit <- rlm(logCPUE ~ time, data = data[i, ],weights=data[i,'NUM_OF_TRAPS'],wt.method='case')
    coef(fit)
  }
  boot_results <- boot::boot(b, bootstrap_func, R = 1000)
  conf_intervals <- t(sapply(1:ncol(boot_results$t), function(i) {
    quantile(boot_results$t[, i], c(0.1, 0.9),na.rm=T)
  }))
  out[i,'confintCPUEL'] = conf_intervals[2,1]
  out[i,'confintCPUEU'] = conf_intervals[2,2]
 }
}

oo = merge(out,gdl)
oo = merge(oo,g2[,c('ID','area')],by.x='GridGrouping',by.y='ID')

oo$denLic = oo$LICENCE_ID/oo$area
oo$denTraps = oo$Ntraps/oo$area
boxplot(denLic~GridGrouping,data=oo)
boxplot(denTraps~GridGrouping,data=oo)

with(subset(oo,GridGrouping==1),cor.test(diff(meanCPUE),diff(meanTemp),method='kendall'))
with(subset(oo,GridGrouping==3),cor.test(diff(meanCPUE),diff(meanTemp),method='kendall'))
with(subset(oo,GridGrouping==2),cor.test(diff(meanCPUE),diff(meanTemp),method='kendall'))


summary(glm( SlopeCPUE~startTemp+SlopeTemp,data=subset(oo,GridGrouping==5)))
plot( SlopeCPUE~SlopeTemp,data=subset(oo,GridGrouping==1))



require(lme4)
require(merTools)
require(lattice)

re = lmer(SlopeCPUE ~ startTemp + SlopeTemp + (1+SlopeTemp|GridGrouping),data=oo)

dotplot(ranef(re))

preds = merTools::predictInterval(re2,level=.5)
oo = cbind(oo,preds)

equal_breaks <- function(n = 3, s = 0.03,...){
  function(x){
    d <- s * diff(range(x)) / (1+2*s)
    seq = seq(min(x)+d, max(x)-d, length=n)
    round(seq, -floor(log10(abs(seq[2]-seq[1]))))
  }
}

effsize_starttemp = summary(re)$coef[2]/sqrt(summary(re)$varcor$GridGrouping[1]+summary(re)$varcor$GridGrouping[4]+1.875e-4)
effsize_slopetemp = summary(re)$coef[3]/sqrt(summary(re)$varcor$GridGrouping[1]+summary(re)$varcor$GridGrouping[4]+1.875e-4)

ggplot(oo,aes(x=SlopeTemp,y=SlopeCPUE))+
  geom_ribbon(aes(ymin=lwr,ymax=upr),fill='lightgrey')+
  geom_line(aes(y=fit),col='red',size=1.2)+
  geom_point()+
  facet_wrap(~factor(GridGrouping,levels=c(1,2,3,4,5,6,7,8,9,10)),scales = 'free')+
  scale_x_continuous(breaks=equal_breaks(n=3,x=.02)) +
 # geom_vline(xintercept = 0)+
#  geom_hline(yintercept = 0)+
    theme_test()

require(sjPlot)
require(performance)

oo = merge(out,gdl)
oo = merge(oo,g2[,c('ID','area')],by.x='GridGrouping',by.y='ID')

ggplot(oo,aes(x=startTemp,y=SlopeCPUE))+geom_point()+facet_wrap(~GridGrouping,scales='free_y')

re = lmer(SlopeCPUE ~ startTemp + SlopeTemp + (1+SlopeTemp|GridGrouping),data=oo)
re1 = lmer(SlopeCPUE ~ SlopeTemp + (1+SlopeTemp|GridGrouping),data=oo)
re2 = lmer(SlopeCPUE ~ startTemp + SlopeTemp + (1|GridGrouping),data=oo) #random intercept
re3 = lmer(SlopeCPUE ~ meanTemp + SlopeTemp + (1+SlopeTemp|GridGrouping),data=oo)
re4 = lmer(SlopeCPUE ~ SlopeTemp + (1|GridGrouping),data=oo)
re5 = lmer(SlopeCPUE ~ medTemp + (1|GridGrouping),data=oo)

compare_performance(re,re1,re2,re3,re4,re5)

plot_model(re4,type='est')
check_model(re4)


preds = merTools::predictInterval(re4,level=.5)
oo = cbind(oo,preds)

ggplot(oo,aes(x=SlopeTemp,y=SlopeCPUE))+
  geom_ribbon(aes(ymin=lwr,ymax=upr),fill='lightgrey')+
  geom_line(aes(y=fit),col='red',size=1.2)+
  geom_point()+
  facet_wrap(~factor(GridGrouping,levels=c(1,2,3,4,5,6,7,8,9,10)))+
  scale_x_continuous(breaks=equal_breaks(n=4,x=.02)) +
  theme_bw()


####################################################################
#### comps of observations and GLORYsv12 on a date and location basis

require(bio.lobster)
require(bio.utilities)
require(devtools)
require(sf)
require(ggplot2)

setwd(file.path(project.datadirectory('bio.lobster'),'Temperature Data','GLORYS','LFA33'))
ind = c('01','02','03','04','05','06','07','08','09','10')
sf_use_s2(FALSE)
if(redo.obs){
  g = lobster.db('temperature.data')
  g = g %>% st_as_sf(coords=c('LON_DD','LAT_DD'),crs=4326)
  g = subset(g,year(T_DATE)>2003)
  g2 = st_read(file.path(project.datadirectory('bio.lobster'),'data','maps','GIS','LFA 33 Grid Grouping polygons_NAD83_region.shp'))
  st_crs(g2) <- 4326
  gg2 = st_centroid(g2)
  gg2$X = st_coordinates(gg2)[,1]
  gg2$Y = st_coordinates(gg2)[,2]
  
  bb = st_as_sfc(st_bbox(gg2))
  cg = st_intersection(g,bb,join=st_within)
  g1 = st_join(g,g2,join=st_within)
  
  ga2 = subset(g1,!is.na(GRIDNO))
  ga2$date = as.Date(ga2$T_DATE)
  saveRDS(ga2,'obstemps.rds')
}
ga2 = readRDS('obstemps.rds')
ga2$lon = st_coordinates(ga2)[,1]
ga2$lat = st_coordinates(ga2)[,2]
ga2 = aggregate(TEMP~date+lon+lat+GRIDNO,data=ga2,FUN=median)
out = list()

for(i in 1:length(ind)){
  k = read.csv(paste('GLORYS12v1_daily_bottomT_LFA33_AdamPoly',ind[i],'.csv',sep=""),header=F)
  names(k)=c('date',rep(paste('X',1:(ncol(k)-1),sep="")))
  k$date = as.Date(k$date,format = '%d-%b-%y')
  j = read.csv(paste('GLORYS12v1_daily_bottomT_LFA33_AdamPoly',ind[i],'_lonlat.csv',sep=""),sep=" ",header=T)
  v = list()
  for(z in 1:(nrow(j))){
    kk = k[,c(1,(z+1))]
    kk$lat = j[z,2]
    kk$lon = j[z,1]
    names(kk)[2]='BT'
    kk$EID= z
    kk$GridGroup = ind[i]
    v[[z]] = kk
  }      
  out[[i]] = do.call('rbind',v)
}

oo = do.call(rbind,out)
oos = st_as_sf(oo,coords = c('lon','lat'),crs=4326)
oos$GRIDNO = as.numeric(oos$GridGroup)
gas = st_as_sf(ga2,coords = c('lon','lat'),crs=4326)
gas$dist = gas$GL = NA
o = list()


for(i in 1:nrow(ga2)){
  p = gas[i,]
  l = subset(oos,date==p$date & GRIDNO==p$GRIDNO)
  distances <- st_distance(p,l)
  st_geometry(l) <-NULL
  gas[i,'GL']   <- l[which.min(distances),'BT']
  gas[i,'dist'] <- min(distances)
}


saveRDS(gas,'obstemps2GL.rds')
gas = readRDS('obstemps2GL.rds')
gas$mn = lubridate::month(gas$date)
gas$fm = ifelse(gas$mn %in% c(1:5,12),1,0)
gas$diff = gas$TEMP - gas$GL

gas = subset(gas,dist<5000)

saveRDS(gas,'obstemps2GL_filtered.rds')

ggplot(subset(gas,fm==1),aes(x=diff))+geom_histogram()+facet_wrap(~GRIDNO)
ggplot(subset(gas,fm==1),aes(x=diff,after_stat(density)))+geom_histogram()+geom_vline(aes(xintercept=0),col='red') +facet_wrap(~GRIDNO,scales = 'free_y')
gas$Gr=as.factor(as.character(paste('Gr',gas$GRIDNO,sep="")))

ggplot(subset(gas,fm==1),aes(x=factor(GRIDNO,levels=c('1','2','3','4','5','6','7','8','9','10')),y=diff))+
  geom_violin(width=1)+geom_boxplot(width=.1, color="grey", alpha=0.5)+geom_hline(aes(yintercept=0),col='red')+
  theme_bw()+xlab('Grid')+ylab('(Observed Temperature - GLORYSv12)')


gas = st_as_sf(gas)
g2 = st_read(file.path(project.datadirectory('bio.lobster'),'data','maps','GIS','LFA 33 Grid Grouping polygons_NAD83_region.shp'))
st_crs(g2) <- 4326
gg2 = st_centroid(g2)
gg2$X = st_coordinates(gg2)[,1]
gg2$Y = st_coordinates(gg2)[,2]

g2$area = st_area(g2)/1000000

v = list()
for(i in 1:length(ind)){
  j = read.csv(paste('GLORYS12v1_daily_bottomT_LFA33_AdamPoly',ind[i],'_lonlat.csv',sep=""),sep=" ",header=T)
  v[[i]] = j
}
vv = do.call(rbind, v)
vg = st_as_sf(vv,coords=c('longitude','latitude'),crs=4326)
require(ggtext)
ggplot(g2)+geom_sf()+geom_sf(data=subset(gas,fm==1),col='red')+geom_sf(data=g2,fill=NA,linewidth=1.3)+
  geom_sf(data=vg,col='blue')+geom_richtext(data=gg2,aes(x=X,y=Y,label=ID), label.padding = grid::unit(rep(0, 6), "pt"))+
  theme_bw()

