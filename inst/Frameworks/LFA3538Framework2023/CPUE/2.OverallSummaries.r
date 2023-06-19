###########################
#start with an overview of all fishing year by year
require(bio.lobster)
require(bio.utilities)
require(dplyr)
require(devtools)
require(sf)
require(ggplot2)
fd = file.path(project.datadirectory('bio.lobster'),'analysis','CPUE')
setwd(fd)

gr = readRDS('C:/Users/Cooka/Documents/git/bio.lobster.data/mapping_data/GridPolyLand.rds')
st_crs(gr) <- 4326
gr = st_transform(gr,32620) 
st_geometry(gr) <- st_geometry(st_as_sf(gr$geometry/1000)) 
st_crs(gr) <- 32620

#need the temperature - catch relationship using IP/catchability/TempCatch.r output

preT = readRDS(file=file.path(project.datadirectory('bio.lobster'),'analysis','ClimateModelling','tempCatchability.rds'))
preT$temp = round(preT$Temperature,2)
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

dd = as.data.frame(do.call(rbind,cpue.ann))
names(dd) = c('unBCPUE','lfa','yr','temp')
dd$temp = round(dd$temp,1)
dd = merge(dd,preT)
dd$unBCPUE_tcorr = dd$unBCPUE / dd$pred
dd = dd[order(dd$yr),]
##overall
#lfa 38
with(subset(dd,lfa==38),plot(yr+.5,unBCPUE,pch=16,cex=.5,type='b',col='red',lwd=3))
par(new=T)
with(subset(dd,lfa==38),plot(yr+.5,unBCPUE_tcorr,pch=16,cex=.5,type='b',col='blue',lwd=3))

##daily
cc$unBCPUE_tcorr = cc$unBCPUE / cc$pred
cc = cc[order(cc$dyear),]

with(subset(cc,lfa==38),plot(dyear,unBCPUE,pch=16,cex=.5,type='p',col='red',lwd=3))
par(new=T)
with(subset(cc,lfa==38),plot(dyear,unBCPUE_tcorr,pch=16,cex=.5,type='p',col='blue'))

with(subset(cc,lfa==38),plot(t,unBCPUE_tcorr))

cc$fYear = as.factor(cc$yr)
l38 = gam(unBCPUE~s(t),data=subset(cc,lfa==38),family = 'nb')
l38a = gam(unBCPUE~s(t)+s(temp),data=subset(cc,lfa==38),family = 'nb')
l38b = gam(unBCPUE~s(t)+s(temp)+s(t,by=fYear),data=subset(cc,lfa==38),family = 'nb')
l38c = gam(unBCPUE~fYear+s(t)+s(temp)+s(t,by=fYear),data=subset(cc,lfa==38),family = 'nb')
l38d = gam(unBCPUE~fYear+s(t)+s(temp),data=subset(cc,lfa==38),family = 'nb')


require(ggeffects)

mydf <- ggpredict(l38a, terms = c('t'))
plot(mydf)

mydf <- ggpredict(l38a, terms = c('temp'))
plot(mydf)



mydf <- ggpredict(l38c, terms = c('t','fYear'))
plot(mydf,color='bw', ci=F)
ggplot(mydf, aes(x = x, y = predicted)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)+
  facet_wrap(~group)+
  xlab('Day of Season')+
  ylab('CPUE')


################################################################


###

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
