require(bio.lobster)
require(bio.utilities)

a = lobster.db('process.logs')
a = subset(a,SYEAR %in% 2019:2021 & LFA %in% 33:35)
a$WOS =
aa = split(a,f=list(a$LFA,a$SYEAR))
cpue.lst<-list()

for(i in 1:length(aa)){
  tmp<-aa[[i]]
  tmp = tmp[,c('DATE_FISHED','WEIGHT_KG','NUM_OF_TRAPS')]
  names(tmp)<-c('time','catch','effort')
  tmp$date<-as.Date(tmp$time)
  first.day<-min(tmp$date)
  tmp$time<-julian(tmp$date,origin=first.day-1)
  g<-as.data.frame(biasCorrCPUE(tmp,by.time=T))
  g$lfa=unique(aa[[i]]$LFA)
  g$yr = unique(aa[[i]]$SYEAR)
  cpue.lst[[i]] <- g
}

cc =as.data.frame(do.call(rbind,cpue.lst))

f = unique(cc$lfa)

for(i in 1:length(f)){
    u = subset(cc,lfa==f[i])
    y = 2019:2021
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