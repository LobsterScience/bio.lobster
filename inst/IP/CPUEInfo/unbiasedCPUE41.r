require(bio.lobster)
require(bio.utilities)
require(devtools)
la()

lobster.db('process.logs41')
a = logs41p

aa = split(a,f=a$yr)
aa = rm.from.list(aa)
cpue.lst<-list()
#by time
for(i in 1:length(aa)){
  tmp<-aa[[i]]
  tmp = tmp[,c('FV_FISHED_DATETIME','ADJCATCH_KG','NUM_OF_TRAPS')]
  names(tmp)<-c('time','catch','effort')
  tmp$date<-as.Date(tmp$time)
  first.day<-min(tmp$date)
  tmp$time<-julian(tmp$date,origin=first.day-1)
  tmp = tmp[order(tmp$time),]
  tmp$time = ceiling(tmp$time/7+1) #convert to week of season
  g<-as.data.frame(biasCorrCPUE(tmp,by.time=T))
 # g=as.data.frame(t(g))
  g$yr = unique(aa[[i]]$yr)
  cpue.lst[[i]] <- g
}

cc =as.data.frame(do.call(rbind,cpue.lst))
#cc$CPUE = as.numeric(cc$`biasCorrCPUE(tmp, by.time = F)`)
plot(aggregate(CPUE~yr,data=cc,FUN=function(x) round(mean(x),2)),type='b')

ggplot(subset(cc),aes(x=t,y=unBCPUE))+geom_point()+geom_smooth(se=F)+facet_wrap(~yr)


    u=cc
    y = 2019:2023
    for(j in 1:length(y)){
      if(j==1) {
          yl = max(u$u95)
          with(subset(u,yr==y[j]),plot(t,unBCPUE,type='b',pch=16,xlab='Week of Season',ylab='CPUE',ylim=c(0,yl)))
          with(subset(u,yr==y[j]),arrows(x0=t,y0=l95,y1=u95,length=0))
      } 
      if(j==2) {
        with(subset(u,yr==y[j]),lines(t,unBCPUE,type='b',pch=16,lty=2,col=2))
        with(subset(u,yr==y[j]),arrows(x0=t,y0=l95,y1=u95,length=0,col=2))
        
      }
      if(j==3) {
        with(subset(u,yr==y[j]),lines(t,unBCPUE,type='b',pch=16,lty=j,col=j))
        with(subset(u,yr==y[j]),arrows(x0=t,y0=l95,y1=u95,length=0,lty=j,col=j))
        
      }
      if(j==4) {
        with(subset(u,yr==y[j]),lines(t,unBCPUE,type='b',pch=16,lty=j,col=j))
        with(subset(u,yr==y[j]),arrows(x0=t,y0=l95,y1=u95,length=0,lty=j,col=j))
        
      }
      if(j==5) {
        with(subset(u,yr==y[j]),lines(t,unBCPUE,type='b',pch=16,lty=j,col=j))
        with(subset(u,yr==y[j]),arrows(x0=t,y0=l95,y1=u95,length=0,lty=j,col=j))
        
      }
      
    }
    legend('topright',legend=c(2019:2023),col=c(1:5),pch=rep(16,4),bty='n')

b = subset(cc,select=c(t,unBCPUE,yr))
names(b) = c('Week of Season','CPUE','Year')

#number of licences reporting by week
ac = aggregate(LICENCE_ID~WOS+SYEAR, data=a,FUN=function(x) length(unique(x)))
acx = aggregate(LICENCE_ID~WOS, data=subset(ac,SYEAR<2023),FUN=max)
names(acx)[2] = 'MeanLic'

xg = merge(subset(ac,SYEAR==2023),acx,all=T)
xg$perc = xg$LICENCE_ID/xg$MeanLic
xg = na.zero(xg)
barplot(perc~WOS,data=subset(xg,perc<1),ylab='Proportion of Lic Reporting')

###no time
a = lobster.db('process.logs')
a = subset(a,SYEAR %in% 2007:2023& LFA %in% '34') 

aa = split(a,f=list(a$LFA,a$SYEAR))
aa = rm.from.list(aa)
cpue.lst<-list()

for(i in 1:length(aa)){
  tmp<-aa[[i]]
  tmp = tmp[,c('DATE_FISHED','WEIGHT_KG','NUM_OF_TRAPS')]
  names(tmp)<-c('time','catch','effort')
  tmp$date<-as.Date(tmp$time)
  first.day<-min(tmp$date)
  tmp$time<-julian(tmp$date,origin=first.day-1)
  tmp = tmp[order(tmp$time),]
  tmp$time = ceiling(tmp$time/7) #convert to week of season
  g<-as.data.frame(biasCorrCPUE(tmp,by.time=F))
  g$lfa=unique(aa[[i]]$LFA)
  g$yr = unique(aa[[i]]$SYEAR)
   g = t(g)[,1]
  cpue.lst[[i]] <- g
}

cc =as.data.frame(do.call(rbind,cpue.lst))
cc$CPUE = as.numeric(cc$`biasCorrCPUE(tmp, by.time = F)`)
plot(CPUE~yr,data=cc,xlab='Year' ,ylab='CPUE')
cc$yr = as.numeric(cc$yr)
ggplot(cc,aes(x=yr,y=CPUE))+geom_point()+geom_line()+theme_test()

