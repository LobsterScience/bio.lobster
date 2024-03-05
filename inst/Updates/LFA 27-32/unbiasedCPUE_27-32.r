
require(devtools)
require(bio.lobster)
require(bio.utilities)
require(sf)
require(ggplot2)
la()

a = lobster.db('process.logs')
a = subset(a,SYEAR %in% 2004:2023 & LFA %in% 27:32) 

aa = split(a,f=list(a$LFA,a$SYEAR))
cpue.lst<-list()
m=0
#annual
for(i in 1:length(aa)){
  tmp<-aa[[i]]
  tmp = tmp[,c('DATE_FISHED','WEIGHT_KG','NUM_OF_TRAPS')]
  names(tmp)<-c('time','catch','effort')
  tmp$date<-as.Date(tmp$time)
  first.day<-min(tmp$date)
  tmp$time<-julian(tmp$date,origin=first.day-1)
  tmp$time = ceiling(tmp$time/7) #convert to week of season
  if(nrow(tmp)>5){
    m=m+1
    g<-as.data.frame(biasCorrCPUE(tmp,by.time=F))
    g$lfa=unique(aa[[i]]$LFA)
    g$yr = unique(aa[[i]]$SYEAR)
    g = t(g)[,2]
    cpue.lst[[m]] <- g
  }
}
cc =as.data.frame(do.call(rbind,cpue.lst))
cc$CPUE = as.numeric(cc$`biasCorrCPUE(tmp, by.time = F)`)
cc = cc[order(cc$lfa,cc$yr),]
cc$yr = as.numeric(cc$yr)
cc$fyr = as.factor(cc$yr)

cc1 = split(cc,f=cc$lfa)

for(i in 1:length(cc1)){
    cc1[[i]]$mCPUE = as.numeric(with(cc1[[i]],rmed(yr,CPUE))$x)
}

cc2 = do.call(rbind,cc1)

ggplot(cc2,aes(x=yr,y=CPUE))+geom_point()+
  geom_line(aes(x=yr,y=mCPUE),colour='red',size=1.1)+facet_wrap(~lfa,scales='free_y')+geom_point(data=subset(cc2,yr==2023),aes(x=yr,y=CPUE),colour='orange',shape=16,size=2)

##by week
a = subset(a,SYEAR>2007)
aa = split(a,f=list(a$LFA,a$SYEAR))
cpue.lst<-list()
m=0

aa = split(a,f=list(a$LFA,a$SYEAR))
aa = rm.from.list(aa)
cpue.lst<-list()

#by time
for(i in 1:length(aa)){
  tmp<-aa[[i]]
 # if(unique(tmp$LFA)=='27' & unique(tmp$SYEAR==2021)) browser()
  
  tmp = tmp[,c('DATE_FISHED','WEIGHT_KG','NUM_OF_TRAPS')]
  names(tmp)<-c('time','catch','effort')
  tmp$date<-as.Date(tmp$time)
  first.day<-min(tmp$date)
  tmp$time<-julian(tmp$date,origin=first.day-1)
  tmp = tmp[order(tmp$time),]
  #tmp$time = ceiling(tmp$time/7) #convert to week of season
  
  if(nrow(subset(tmp,time==1))<5){
    l = which(tmp$time==1)
    tmp$time[l]=2
    tmp$time = tmp$time-1
  }
  g<-as.data.frame(biasCorrCPUE(tmp,by.time=T,min.sample.size = 1))
  g$lfa=unique(aa[[i]]$LFA)
  g$yr = unique(aa[[i]]$SYEAR)
  # g = t(g)[,1]
  cpue.lst[[i]] <- g
}

cc =as.data.frame(do.call(rbind,cpue.lst))
ggplot(subset(cc,lfa==27),aes(x=t,y=CPUE))+geom_point()+
  geom_smooth(se=F)+facet_wrap(~yr)

