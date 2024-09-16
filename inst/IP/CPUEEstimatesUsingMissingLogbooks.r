# generate index of CPUE based on percent of missing logs by month
lfa = '35'
syear = 2024


#raw cpue
b = lobster.db('process.logs')
li = lobster.db('licence_characteristics') #this year
lic = subset(li, LFA==35,select=c(LICENCE_ID,LIC_TYPE,LIC_SUBTYPE,VR_NUMBER))
nLic = aggregate(LICENCE_ID~LIC_TYPE+LIC_SUBTYPE,data=lic,FUN=function(x) length(unique(x)))
i = grep('PART',nLic$LIC_SUBTYPE)
nLic$LICENCE_ID[i] = nLic$LICENCE_ID[i]/2 #partnerships only submit one log
nLic = sum(nLic$LICENCE_ID)


 b = subset(b, LFA ==lfa)
b$mn = month(b$DATE_FISHED)
fy = subset(b,SYEAR==syear)
fyProp = aggregate(LICENCE_ID~mn,data=fy,FUN=function (x) length(unique(x)))
fyProp$Prop = fyProp$LICENCE_ID/nLic

py = subset(b,SYEAR<syear)

aa = split(py,f=list(py$LFA,py$SYEAR))
cpue.lst<-list()
niter=100
out=matrix(nrow=length(aa),ncol=niter)
for(p in 1:niter){
for(i in 1:length(aa)){
  tmp<-aa[[i]]
  tmp2 = split(tmp,f=list(tmp$mn))
  jnk = list()
 #sample to match proportions
   for(j in 1:length(tmp2)){
    ju = tmp2[[j]]  
    mn = unique(ju$mn)
    prop = fyProp$Prop[which(fyProp$mn==mn)]
    if(length(prop)<1) prop = 0
    if(prop>1) prop=1
    id= unique(ju$LICENCE_ID)
    ii = length(id)
    idS = sample(id,round(ii*prop))
    jnk[[j]] = subset(ju,LICENCE_ID %in% idS)
  }
  tmp = as.data.frame(do.call(rbind,jnk))
  
  tmp = tmp[,c('DATE_FISHED','WEIGHT_KG','NUM_OF_TRAPS')]
  names(tmp)<-c('time','catch','effort')
  tmp$date<-as.Date(tmp$time)
  first.day<-min(tmp$date)
  tmp$time<-julian(tmp$date,origin=first.day-1)
  tmp$time = ceiling(tmp$time/7) #convert to week of season
  g<-as.data.frame(biasCorrCPUE(tmp,by.time=F))
  g$lfa=unique(aa[[i]]$LFA)
  g$yr = unique(aa[[i]]$SYEAR)
  g = t(g)[,1]
  cpue.lst[[i]] <- g
}

cc =as.data.frame(do.call(rbind,cpue.lst))
w = as.numeric(cc$`biasCorrCPUE(tmp, by.time = F)`)
out[,p] = w
}


fy = fy[,c('DATE_FISHED','WEIGHT_KG','NUM_OF_TRAPS')]
names(fy)<-c('time','catch','effort')
fy$date<-as.Date(fy$time)
first.day<-min(fy$date)
fy$time<-julian(fy$date,origin=first.day-1)
fy$time = ceiling(fy$time/7) #convert to week of season
g<-as.data.frame(biasCorrCPUE(fy,by.time=F))
g = t(g)[,1]

out<-rbind(out,rep(g,ncol(out)))

matplot(out,type='l',col='grey',ylab='Logbook reporting Scaled CPUE',xaxt='n')
points(nrow(out),g,pch=16)
axis(side=1,at=c(1,6,11,16),labels=c(2005,2010,2015,2020))