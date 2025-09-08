require(bio.lobster)
require(bio.utilities)
require(devtools)
require(ggplot2)
a = lobster.db('process.logs')
a = subset(a,SYEAR %in% 2020:2024 )

aa = split(a,f=list(a$LFA,a$SYEAR))
cpue.lst<-list()
aa = rm.from.list(aa)

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
ggplot(cc,aes(x=yr,y=CPUE))+geom_point()+geom_line()+facet_wrap(~LFA)+theme_test()
cc$CPUE = round(cc$CPUE,2)

cc = subset(cc,select=c(yr,lfa,CPUE))
write.csv(cc,'FishingSeason_CommercialCPUE_2020-24.csv')
