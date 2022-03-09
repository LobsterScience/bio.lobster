a = lobster.db('process.logs')
a = subset(a,LFA==29 & SYEAR>2004)
aa = split(a,f=a$SYEAR)
cpue.lst<-list()

for(i in 1:length(aa)){
  tmp<-aa[[i]]
  tmp = tmp[,c('DATE_FISHED','WEIGHT_KG','NUM_OF_TRAPS')]
  names(tmp)<-c('time','catch','effort')
  tmp$date<-as.Date(tmp$time)
  first.day<-min(tmp$date)
  tmp$time<-julian(tmp$date,origin=first.day-1)
  g<-biasCorrCPUE(tmp)
  cpue.lst[[i]] <- c(lfa=unique(aa[[i]]$LFA),yr = unique(aa[[i]]$SYEAR),g)
}

cc =as.data.frame(do.call(rbind,cpue.lst))
}
