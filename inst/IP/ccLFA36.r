#comparing CC StM with rest of 36

a = lobster.db('process.logs')
a = subset(a,LFA==36)

aC = subset(a, LICENCE_ID %in% c(109841,110449,111770,111996,112070,112478,112545))

aCp = subset(aC,SYEAR>2016)
aR = subset(a,LFA==36& LICENCE_ID %ni% c(109841,110449,111770,111996,112070,112478,112545) & SYEAR>2016)

#Days Fished
sDR = aggregate(DATE_FISHED~SYEAR+LICENCE_ID,data=aR,FUN=function(x) length(unique(x))) #most possible is ~160 days (with extension)
summary(sDR) 
sDRa = aggregate(DATE_FISHED~LICENCE_ID,data=sDR,FUN=mean) #most possible is ~160 days (with extension)

sDC = aggregate(DATE_FISHED~SYEAR+LICENCE_ID,data=aC,FUN=function(x) length(unique(x)))
sDCa = aggregate(DATE_FISHED~LICENCE_ID,data=sDC,FUN=mean) #most possible is ~160 days (with extension)

summary(sDC)

brk = seq(2,122,by=3)


hist(sDRa$DATE_FISHED,breaks=brk,xlab='Number of Days Fished',ylab='Frequency',main="")
abline(v=sDCa[,2],col='blue',lwd=2)

#Landings per licence
aR$WKGB = aR$WEIGHT_KG*aR$BUMPUP
sDR = aggregate(WKGB~SYEAR+LICENCE_ID,data=aR,FUN=sum) #most possible is ~160 days (with extension)
summary(sDR) 
sDRa = aggregate(WKGB~LICENCE_ID,data=sDR,FUN=mean) #most possible is ~160 days (with extension)
sDRa$LT = sDRa$WKGB/1000


aC$WKGB = aC$WEIGHT_KG*aC$BUMPUP

sDC = aggregate(WKGB~SYEAR+LICENCE_ID,data=aC,FUN=sum) #most possible is ~160 days (with extension)
summary(sDC) 
sDCa = aggregate(WKGB~LICENCE_ID,data=sDC,FUN=mean) #most possible is ~160 days (with extension)
sDCa$LT = sDCa$WKGB/1000

brk = seq(1,55,by=3)


hist(sDRa$LT,breaks=brk,xlab='Average landings by licence',ylab='Frequency',main="")
abline(v=sDCa[,'LT'],col='blue',lwd=1)



#last number of days

rr = aggregate(WEIGHT_KG~DOS+DATE_FISHED+SYEAR,data=subset(aC,SYEAR>2018),FUN=sum)

rA = aggregate(WEIGHT_KG~DOS+DATE_FISHED+SYEAR,data=subset(aR,SYEAR>2018),FUN=sum)

y = 2019:2021

for(i in 1 : y){
  par(mfrow=c(1,2))
  w = subset(rr,SYEAR==i) 
  w$l = cumsum(w$WEIGHT_KG)/sum(w$WEIGHT_KG)
  plot(w$DOS,w$l,xlab='Day of Season',ylab='Cumulative proportion of landings',type='l',main='Commercial Fishery')
  ww = subset(w,DATE_FISHED>as.Date(paste(i,"06",'29',sep="-")))
  u = min(ww$l)
  with(ww,lines(DOS,l,col='red',lwd=2))
  legend('bottomleft',bty='n',paste('Extension represents',round(1-u,3)*100,"% of landings", sep=" "))
  v = subset(rA,SYEAR==i) 
  v$l = cumsum(v$WEIGHT_KG)/sum(v$WEIGHT_KG)
  plot(v$DOS,v$l,xlab='Day of Season',ylab='Cumulative proportion of landings',type='l',lty=1,main='St Marys Commercial Communal')
  vv = subset(v,DATE_FISHED>as.Date(paste(i,"06",'29',sep="-")))
  with(vv,lines(DOS,l,col='red',lwd=2,lty=1))
  uu = min(vv$l)
  legend('bottomleft',bty='n',paste('Extension represents',round(1-uu,3)*100,"% of landings", sep=" "))
  
  
}
}

