require(bio.lobster)
require(bio.utilities)

wd = ('C:\\Users\\Cooka\\OneDrive - DFO-MPO\\BycatchLobster')

setwd(wd)

##by week
a = lobster.db('process.logs')
a = subset(a,SYEAR %in% 2005:2021 & LFA %ni% 28)
aa = split(a,f=list(a$LFA,a$SYEAR))
cpue.lst<-list()
m=0
for(i in 1:length(aa)){
  tmp<-aa[[i]]
  if(nrow(tmp)>5){
    m=m+1
  tmp = tmp[,c('DATE_FISHED','WEIGHT_KG','NUM_OF_TRAPS')]
  names(tmp)<-c('time','catch','effort')
  tmp$date<-as.Date(tmp$time)
  first.day<-min(tmp$date)
  tmp$time<-julian(tmp$date,origin=first.day-1)
  tmp$time = floor(tmp$time/7) *7
  g<-as.data.frame(biasCorrCPUE(tmp,by.time=T))
  g$lfa=unique(aa[[i]]$LFA)
  g$yr = unique(aa[[i]]$SYEAR)
  cpue.lst[[m]] <- g
  }
}

cc =as.data.frame(do.call(rbind,cpue.lst))
f = unique(cc$lfa)
cols = c('black','red','blue')
for(i in 1:length(f)){
  png(file.path('Figures',paste('LFAs',f[i],'CPUEDOS.png',sep="")))
  
    u = subset(cc,lfa==f[i])
    y = 2019:2021
    yl = max(u$unBCPUE)
    xl=max(u$t)
    with(subset(u,yr==2019),plot(t,unBCPUE,type='b',col=cols[1],lwd=2,xlab='Day of Season',ylab='CPUE',main=paste('LFA',f[i]),ylim=c(0,yl),xlim=c(0,xl)))
    with(subset(u,yr==2020),lines(t,unBCPUE,type='b',col=cols[2],lwd=2,lty=j))
    with(subset(u,yr==2021),lines(t,unBCPUE,type='b',col=cols[3],lwd=2,lty=j))

    dev.off()
}
graphics.off()



##cumulative effort
couts = list()
coutsMed = list()
m=0
for(i in 1:length(aa)){
    tmp<-aa[[i]]
    if(nrow(tmp)>0){
      m=m+1
    
    tmp = tmp[,c('DATE_FISHED','WEIGHT_KG','NUM_OF_TRAPS')]
    names(tmp)<-c('time','catch','effort')
    tmp$date<-as.Date(tmp$time)
    first.day<-min(tmp$date)
    tmp$time<-julian(tmp$date,origin=first.day-1)
    tmp = aggregate(cbind(catch,effort)~time,data=tmp,FUN=sum)
    tmp = tmp[order(tmp$time),]
    tmp$cE = cumsum(tmp$effort) / sum(tmp$effort)
    tmp$cL = cumsum(tmp$catch) / sum(tmp$catch)
    tmp$lfa =unique(aa[[i]]$LFA)
    tmp$yr = unique(aa[[i]]$SYEAR)
    couts[[m]] = tmp[,c('yr','lfa','time','cE','cL')]
    eL=findValue(tmp$time, tmp$cE, ref=.4)
    eM=findValue(tmp$time, tmp$cE, ref=.5)
    eU=findValue(tmp$time, tmp$cE, ref=.6)
    cL=findValue(tmp$time, tmp$cL, ref=.4)
    cM=findValue(tmp$time, tmp$cL, ref=.5)
    cU=findValue(tmp$time, tmp$cL, ref=.6)
    coutsMed[[m]] = c(unique(tmp$yr),unique(tmp$lfa),eL,eM,eU,cL,cM,cU)
    }
  }

cl = as.data.frame(do.call(rbind,couts))
clM = as.data.frame(do.call(rbind,coutsMed))
names(clM) = c('yr','lfa','effort.4','effort.5','effort.6','catch.4','catch.5','catch.6')

f = unique(cl$lfa)
cols = c('black','red','blue')
for(i in 1:length(f)){
  png(file.path('Figures',paste('LFAs',f[i],'CumEffDOS.png',sep="")))
  u = subset(cl,lfa==f[i])
  y = 2019:2021
  with(subset(u,yr==2019),plot(time,cE,type='l',col=cols[1],lwd=2,xlab='Day of Season',ylab='Cumulative Effort',main=paste('LFA',f[i])))
  with(subset(u,yr==2020),lines(time,cE,type='l',col=cols[2],lwd=2,lty=j))
  with(subset(u,yr==2021),lines(time,cE,type='l',col=cols[3],lwd=2,lty=j))
  
  dev.off()
  
  png(file.path('Figures',paste('LFAs',f[i],'CumLandDOS.png',sep="")))
  with(subset(u,yr==2019),plot(time,cL,type='l',col=cols[1],lwd=2,xlab='Day of Season',ylab='Cumulative Landings',main=paste('LFA',f[i])))
  with(subset(u,yr==2020),lines(time,cL,type='l',col=cols[2],lwd=2,lty=j))
  with(subset(u,yr==2021),lines(time,cL,type='l',col=cols[3],lwd=2,lty=j))
  
  dev.off()
  
}

###median of catches

a =  lobster.db('process.logs')
a = subset(a,SYEAR>2004 & SYEAR<2022)
b = lobster.db('seasonal.landings')
b = b[30:47,]
b$LFA38B <- NULL
b$SYEAR = 2005:2022
b = subset(b,SYEAR<2022)
b = reshape(b,idvar='SYEAR', varying=list(2:6),direction='long')
b$LFA=rep(c(33,34,35,36,38),each=17)
b$time <- NULL
names(b)[1:2]=c('YR','SlipLand')


d = lobster.db('annual.landings')
d = subset(d,YR>2004, select=c(YR,LFA27,LFA28,LFA29,LFA30,LFA31A,LFA31B,LFA32))
d = reshape(d,idvar='YR', varying=list(2:8),direction='long')
d$LFA=rep(c(27,28,29,30,'31A','31B',32),each=17)
d$time <- NULL
names(d)[1:2]=c('YR','SlipLand')
bd = rbind(d,b)
clM = merge(clM, bd,by.x=c('yr','lfa'),by.y=c('YR','LFA'))
f = unique(clM$lfa)
clM = toNums(clM,cols=c(1,3:8))
i=1
for(i in 1:length(f)){
  png(file.path('Figures',paste("EffortbyDayofSeason",f[i],".png")))
  m = subset(clM,lfa==f[i])
  yl = range(c(m$effort.4,m$effort.6))
  plot(m$yr,m$effort.5,ylim=c(floor(yl[1]),ceiling(yl[2])),xlab='Year',ylab='Day of Season')
  arrows(x0=m$yr,y0=m$effort.4,y1=m$effort.6,length=0)  
  dev.off()
  
  png(file.path('Figures',paste("LandingsbyDayofSeason",f[i],".png")))
  yl = range(c(m$catch.4,m$catch.6))
  plot(m$yr,m$catch.5,ylim=c(floor(yl[1]),ceiling(yl[2])),xlab='Year',ylab='Day of Season')
  arrows(x0=m$yr,y0=m$catch.4,y1=m$catch.6,length=0)  
  dev.off()
}

