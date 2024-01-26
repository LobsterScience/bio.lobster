require(lubridate)
require(bio.lobster)
require(gamlss)
require(devtools)
require(sf)
require(tidyr)
require(dplyr)

setwd('C:/Users/Cooka/OneDrive - DFO-MPO/Reports  Reviews/ComparativeFishingYinBenoitMartin2024/stations')
#Ryan's list
oo = dir()
li = list()
for(i in 1:length(oo)){
      li[[i]] = read.csv(oo[i])
}
ll = li
li1 = merge(li[[1]][,c('MISSION','SETNO','STATION')],li[[3]][,c('MISSION','SETNO','STATION')],by='STATION')
li2 = merge(li[[2]][,c('MISSION','SETNO','STATION')],li[[4]][,c('MISSION','SETNO','STATION')],by='STATION')

li = bind_rows(list(li1,li2))
li$CID = paste(li$MISSION.x,li$SETNO.x,li$MISSION.y,li$SETNO.y,sep="_")

ll=bind_rows(ll)
ll$id = paste(ll$MISSION,ll$SETNO,sep="_")
##back to the raw data

set = groundfish.db(DS='gsinf.odbc')
set$DIST = set$dist * 1.852 #nm to km

de = groundfish.db(DS='gsdet.odbc')

de$id = paste(de$mission,de$setno,sep="_")

de = subset(de, id %in% unique(ll$id) & spec==2550)
de = merge(de,set[,c('mission','setno','DIST','gear')])
de$gr = ifelse(grepl('C',de$mission),'NEST','WIIA')
sc1=seq(13,253,by=5)
de$SZ = sc1[cut(de$flen,sc1,labels=F)]
de$year = as.numeric(substr(de$mission,4,7))
de$clenc = de$clen/de$DIST
da = aggregate(cbind(clen,clenc)~SZ+gr+year,data=de,FUN=sum)

with(subset(da,year==2022 & gr=='NEST'),plot(SZ,clenc,type='h',lwd=3))
with(subset(da,year==2022 & gr=='WIIA'),lines(SZ+1,clenc,type='h',lwd=3,col='red'))


g = RV_sets()

b = subset(g,YEAR %in% 2022:2023 & month(DATE) %in% c(6,7,8))
b= st_as_sf(b,coords = c('LONGITUDE','LATITUDE'),crs=4326)

b$bottom_temperature = b$DYEAR = b$WEIGHT_KG = b$Legal = b$OFFSET_METRIC = b$EMPTY = b$SOURCE = b$Lobster = b$Berried = NULL
#bLo = pivot_longer(b,cols=-c( DATE,mission,setno,YEAR,OFFSET,Gear,geometry),names_to='rFL',values_to = 'N')
b1 = subset(b,Gear=='NEST' & YEAR ==2022)
b2 = subset(b,Gear=='Western IIA' & YEAR ==2022)


ss = st_nearest_feature(b1,b2)
ds = st_distance(b1,b2[ss,],by_element=T)
st_geometry(b2) = NULL

ou = list()
for(i in 1:nrow(b1)){
    
  x = b1[i,]
  x2 = b2[ss[i],]
  
  x = pivot_longer(x,cols=-c( DATE,mission,setno,YEAR,OFFSET,Gear,geometry),names_to='rFL',values_to = 'N')
  x2 = pivot_longer(x2,cols=-c( DATE,mission,setno,YEAR,OFFSET,Gear),names_to='rFL',values_to = 'N')
  x$dist = ds[i]
  x = rename.df(x,'N','WIIA_N')
  x2 = rename.df(x2,'N','NEST_N')
  
  ou[[i]] = merge(x,x2,by='rFL')
  
}

v22 = bind_rows(ou)


b3 = subset(b,Gear=='NEST' & YEAR ==2023)
b4 = subset(b,Gear=='Western IIA' & YEAR ==2023)

ss = st_nearest_feature(b3,b4)
ds = st_distance(b3,b4[ss,],by_element=T)
st_geometry(b4) = NULL

ou = list()
for(i in 1:nrow(b3)){
  
  x = b3[i,]
  x2 = b4[ss[i],]
  
  x = pivot_longer(x,cols=-c( DATE,mission,setno,YEAR,OFFSET,Gear,geometry),names_to='rFL',values_to = 'N')
  x2 = pivot_longer(x2,cols=-c( DATE,mission,setno,YEAR,OFFSET,Gear),names_to='rFL',values_to = 'N')
  x$dist = ds[i]
  x = rename.df(x,'N','WIIA_N')
  x2 = rename.df(x2,'N','NEST_N')
  
  ou[[i]] = merge(x,x2,by='rFL')
  
}

v23 = bind_rows(ou)

xx = bind_rows(list(v22,v23))
xx$CID=paste(xx$mission.x,xx$setno.x,xx$mission.y,xx$setno.y,sep="_")

subset(li, CID %ni% unique(xx$CID))


xx$timedist = as.numeric((xx$DATE.x - xx$DATE.y)/60)
xR = subset(xx,timedist<100)

xR = subset(xR,as.numeric(dist)<4900)
xR$ID=paste(xR$mission.x,xR$setno.x,sep="_")
xR$CID=paste(xR$mission.x,xR$setno.x,xR$mission.y,xR$setno.y,sep="_")

xR$FL = as.numeric(substr(xR$rFL,3,10))

#examine on aggregate based on sample size
xRR = aggregate(cbind(WIIA_N,NEST_N)~mission.x+setno.x,data=xR,FUN=sum)
xRR$ID=paste(xRR$mission.x,xRR$setno.x,sep="_")
xLob = subset(xRR,WIIA_N+NEST_N>0)
xLob$ID = paste(xLob$mission.x,xLob$setno.x,sep="_")

xLob = subset(xR,ID %in% xLob$ID)


xi =aggregate(cbind(WIIA_N,NEST_N)~FL,data=xR,FUN=sum)
write.csv(xi,'RV_comparative_sample_size.csv')
xi$C = xi$NEST_N / (xi$NEST_N+xi$WIIA_N)
ggplot(xi,aes(x=FL,y=C))+geom_point()

plot(xi$FL,xi$WIIA_N,type='h',lwd=3,ylim=c(0,300))
lines(xi$FL+1,xi$NEST_N,type='h',lwd=3,col='red')


#examine on aggregate based on sample size corrected by swept area
xR$WIIA_N_corr = round(xR$WIIA_N / xR$OFFSET.x)
xR$NEST_N_corr = round(xR$NEST_N / xR$OFFSET.y)

xRR = aggregate(cbind(WIIA_N_corr,NEST_N_corr)~mission.x+setno.x,data=xR,FUN=sum)
xRR$ID=paste(xRR$mission.x,xRR$setno.x,sep="_")
xLob = subset(xRR,WIIA_N_corr+NEST_N_corr>0)
xLob$ID = paste(xLob$mission.x,xLob$setno.x,sep="_")

xLob = subset(xR,ID %in% xLob$ID)


xig =aggregate(cbind(WIIA_N_corr,NEST_N_corr)~FL,data=xR,FUN=sum)
xig$C = xig$NEST_N_corr / (xig$NEST_N_corr+xig$WIIA_N_corr)
ggplot(xig,aes(x=FL,y=C))+geom_point()+geom_point(data=xi,aes(x=FL,y=C),colour='red')

plot(xig$FL,xig$WIIA_N,type='h',lwd=3,ylim=c(0,7400))
lines(xig$FL+1,xig$NEST_N,type='h',lwd=3,col='red')


require(gamlss)

fit= out3 = gamlss(cbind(WIIA_N_corr,NEST_N_corr)~1,data=xLob,family=BB())
fit1 = out1 = gamlss(cbind(WIIA_N_corr,NEST_N_corr)~cs(FL,df=3),data=xLob,family=BB())
fit2 = out2 = gamlss(cbind(WIIA_N_corr,NEST_N_corr)~cs(FL,df=3),sigma.formula=~cs(FL,df=3),data=xLob,family=BB())

out=out2

d = cbind(xLob,out$residuals) #Dunn and Smyth 1996
st_geometry(d) <- NULL
names(d)[ncol(d)]='Randomized_Quantile_Residuals'
d$STATION= d$CID

ggplot(data=d,aes(x=STATION,y=Randomized_Quantile_Residuals))+
  geom_boxplot(width = 0.6, position = position_dodge(width = 0.75))+
  geom_hline(yintercept = 0,color='red')+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(data=d,aes(x=as.factor(FL),y=Randomized_Quantile_Residuals))+
  geom_boxplot(width = 0.6, position = position_dodge(width = 0.75))+
  geom_hline(yintercept = 0,color='red')+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(x='CarapaceLength')

#bootstrapping across stations- assuming that the sampling unit is the station and that each unit is a sample of the population
st_geometry(xLob) <- NULL
xLob$STATION = xLob$CID
st = unique(xLob$STATION)
niter=length(st)

newd = expand.grid(FL=seq(min(xLob$FL),max(xLob$FL),by=1))

ou = matrix(NA,nrow=length(newd[,1]),ncol=niter,byrow=F)
for(i in 1:niter){
  stt = sample(st,length(st),replace=T)
  d1 = list()
  for(j in 1:length(stt)) {
    d1[[j]] = subset(xLob,STATION== stt[j])
  }
  d1 = as.data.frame(do.call(rbind,d1))
  out = gamlss(cbind(NEST_N_corr,WIIA_N_corr)~cs(FL,df=3),sigma.formula=~cs(FL,df=3),data=d1,family=BB())
  mu = predict(out,what='mu',type='response',newdata = newd)
  rho = mu / (1-mu)
  ou[,i] = rho
}


sou = as.data.frame(cbind(newd[,1],t(apply(ou,1,quantile,c(0.5,.0275,.975))),(apply(ou,1,mean)),(apply(ou,1,sd))))
names(sou) = c('Length','Median','L95','U95','Mean','SD')
sou$CV = sou$SD / sou$Mean

souL = aggregate(cbind(Median,L95,U95)~Length,data=sou,FUN=median)

ggplot(data=souL, aes(x=Length, y=Median)) + geom_line()+
  geom_ribbon(aes(ymin=L95, ymax=U95), linetype=2, alpha=0.1)+
  #  geom_point(data=ov,aes(x=rFL,y=C))+
  labs(x='CL',y='Relative Catch Efficiency [WIIA/NEST]')+
  geom_hline(yintercept = 1,colour='red')+
  theme_bw()


##mapping sets from ILTS with high abundance of <60mm
p = ggLobsterMap('all',return.object = T, addLFAlines = F, addGrids = F)
p+geom_sf(data=xR)+geom_sf(data=xLob,aes(colour='red'))

v = ILTS_ITQ_All_Data(species=2550,redo_base_data = F)
sc1=seq(3,253,by=5)
v$SZ = sc1[cut(v$FISH_LENGTH,sc1,labels=F)]
v = aggregate(SA_CORRECTED_PRORATED_N~SET_LONG+SET_LAT+YEAR+SZ+SET_DATE,data=v,FUN=sum)
vv = st_as_sf(v,coords = c('SET_LONG','SET_LAT'),crs=4326)
vv$sm = ifelse(vv$SZ<50,vv$SA_CORRECTED_PRORATED_N,0)

v$sm = ifelse(v$FISH_LENGTH<50,v$SA_CORRECTED_PRORATED_N,0)

b = aggregate(cbind(sm,SA_CORRECTED_PRORATED_N)~YEAR+SET_LAT+SET_LONG,data=subset(v,YEAR>2021),FUN=sum)
b = st_as_sf(b,coords = c('SET_LONG','SET_LAT'),crs=4326)
b$propSmall = b$sm/b$SA_CORRECTED_PRORATED_N

p+geom_sf(data=xR)+geom_sf(data=xLob,aes(colour='red'))
p+geom_sf(data=xR)+geom_sf(data=b,aes(colour='red'),size=.9)


xLob = st_as_sf(xLob)
b1 = subset(xLob,YEAR.y ==2022)
b2 = subset(vv, YEAR ==2022 & month(vv$SET_DATE) %in% c(6,7))

ss = st_nearest_feature(b1,b2)
ds = st_distance(b1,b2[ss,],by_element=T)
#ss = ss[which(as.numeric(ds)<5000)]
st_geometry(b2) = NULL

ou = list()
m=0
for(i in 1:nrow(b1)){
if(as.numeric(ds[i])<6000){  
  m=m+1
  x = b1[i,]
  x2 = b2[ss[i],]
  
  ou[[m]] = merge(x[,c('FL','WIIA_N_corr','NEST_N_corr','DATE.y')],x2[,c('SZ','SA_CORRECTED_PRORATED_N','SET_DATE')],by.x='FL',by.y='SZ',all.y=T)
  
  }
}
v22 = bind_rows(ou)
