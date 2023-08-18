require(bio.lobster)
require(bio.utilities)
require(devtools)
la()

a = lobster.db('process.logs')
a = subset(a,SYEAR %in% 2004:2023 & LFA =='35') 

aa = split(a,f=list(a$LFA,a$SYEAR))
cpue.lst<-list()
#by time
for(i in 1:length(aa)){
  tmp<-aa[[i]]
  tmp = tmp[,c('DATE_FISHED','WEIGHT_KG','NUM_OF_TRAPS')]
  names(tmp)<-c('time','catch','effort')
  tmp$date<-as.Date(tmp$time)
  first.day<-min(tmp$date)
  tmp$time<-julian(tmp$date,origin=first.day-1)
  tmp$time = ceiling(tmp$time/7) #convert to week of season
  g<-as.data.frame(biasCorrCPUE(tmp,by.time=T))
  g$lfa=unique(aa[[i]]$LFA)
  g$yr = unique(aa[[i]]$SYEAR)
  #g = t(g)[,1]
  cpue.lst[[i]] <- g
}

cc =as.data.frame(do.call(rbind,cpue.lst))
#cc$CPUE = as.numeric(cc$`biasCorrCPUE(tmp, by.time = F)`)
plot(aggregate(CPUE~t,data=subset(cc,lfa==35),FUN=function(x) round(mean(x),2)))

f = unique(cc$lfa)

for(i in 1:length(f)){
    u = subset(cc,lfa==f[i])
    y = 2019:2023
    for(j in i:length(y)){
      if(j==1) {
          yl = max(u$u95)
          with(subset(u,yr==y[j]),plot(t,unBCPUE,type='b',pch=16,xlab='Day of Season',ylab='CPUE',main=f[i],ylim=c(0,yl)))
          with(subset(u,yr==y[j]),arrows(x0=t,y0=l95,y1=u95,length=0))
      } else {
        with(subset(u,yr==y[j]),lines(t,unBCPUE,type='b',pch=16,lty=j,col=j))
        with(subset(u,yr==y[j]),arrows(x0=t,y0=l95,y1=u95,length=0,lty=j,col=j))
        
        }
      
    }
    legend('topright',legend=c(2019:2023),col=c(1:5),pch=rep(16,4),bty='n')
}
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
