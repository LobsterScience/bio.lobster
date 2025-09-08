# landings to port
require(bio.lobster)
require(devtools)
la()


b = lobster.db('process.logs')
ba = aggregate(WEIGHT_KG~COMMUNITY_CODE+LICENCE_ID+SYEAR+LFA,data=b,FUN=sum)
bb = aggregate(WEIGHT_KG~LICENCE_ID+SYEAR,data=b,FUN=sum)
bc = aggregate(LICENCE_ID~COMMUNITY_CODE+SYEAR+LFA,data=b,FUN=function(x) length(unique(x)))

bb$WEIGHT_KG_T = bb$WEIGHT_KG
bb$WEIGHT_KG = NULL

ba = merge(ba,bb)


s = lobster.db('process_slips')
s = aggregate(cbind(adj_wt_kg,value)~LFA+SYEAR+LICENCE_NO,data=s,FUN=sum)
s$price = s$value/s$adj_wt_kg
s$LICENCE_ID = s$LICENCE_NO
 s$LICENCE_NO = NULL
bs = merge(ba,s)

cc1 = lobster.db('community_code')
cc1 = subset(cc1, select=c(COMMUNITY_CODE, COMMUNITY_NAME ))


bs$tWeight = bs$WEIGHT_KG/bs$WEIGHT_KG_T * bs$adj_wt_kg
bs$tValue = bs$tWeight * bs$price
bsa = aggregate(cbind(tWeight,tValue)~SYEAR+LFA+COMMUNITY_CODE,data=bs,FUN=sum)

bbb = aggregate(tWeight~SYEAR+LFA,data=bsa,FUN=sum)

bca = merge(bsa,bc)

bscr = subset(bca,LICENCE_ID>=5 & SYEAR>=2014)


###no time
a = lobster.db('process.logs')
a = subset(a,SYEAR %in% 2014:2024) 

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
cc$SYEAR = as.numeric(cc$yr)
ggplot(cc,aes(x=SYEAR,y=CPUE))+geom_point()+geom_line()+theme_test()+facet_wrap(~lfa)


cc$LFA = cc$lfa
cc =subset(cc,select=c(LFA, SYEAR, CPUE))

bbb = merge(bscr,cc)
bbb$TH = round(bbb$tWeight/bbb$CPUE)

bc1 = merge(bbb,cc1)
bc1$Landings_kg = round(bc1$tWeight)
bc1$Trap_hauls = bc1$TH
bc1$N_licences_reporting = bc1$LICENCE_ID
bc1$Dollar_Value = bc1$tValue
bbbb =subset(bc1, COMMUNITY_CODE>0, select=c(SYEAR, LFA, COMMUNITY_CODE, COMMUNITY_NAME, Trap_hauls, Landings_kg,N_licences_reporting,Dollar_Value))
write.csv(bbbb , 'Landing_TH_Value_by_Port_maritimes.csv')


