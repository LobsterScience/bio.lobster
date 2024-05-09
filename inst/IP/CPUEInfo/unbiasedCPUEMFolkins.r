require(bio.lobster)
require(bio.utilities)

a = lobster.db('process.logs')
a = subset(a,SYEAR %in% 2019:2022 )

aa = split(a,f=list(a$LFA,a$SYEAR))
cpue.lst<-list()

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
aggregate(CPUE~lfa+t,data=subset(cc,lfa==35),FUN=function(x) round(mean(x),2))

f = unique(cc$lfa)

f="35"

for(i in 1:length(f)){
    u = subset(cc,lfa==f[i])
    y = 2019:2022
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
}
b = subset(cc,select=c(t,unBCPUE,yr))
names(b) = c('Week of Season','CPUE','Year')


