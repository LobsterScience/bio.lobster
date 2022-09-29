## licences by port

a = lobster.db('process.logs.unfiltered')
b = lobster.db('community_code')
s = aggregate(SD_LOG_ID~LICENCE_ID+LFA+COMMUNITY_CODE+SYEAR,data=a,FUN=function(x) length(unique(x)))

s = subset(s,SYEAR>2008)

s$N_Log_Entries = 1
aa = seq(1,120,by=5)
s$N_TRIPS = aa[cut(s$SD_LOG_ID,aa,labels=F)]

w = aggregate(N_Log_Entries~LFA+N_TRIPS+SYEAR,data=s,FUN=sum)

ggplot(subset(w,LFA==32)
       ,aes(x=N_TRIPS,y=N_Log_Entries)) + geom_bar(stat='identity') + facet_wrap(~SYEAR)



w = aggregate(N_Log_Entries~LFA+COMMUNITY_CODE+SYEAR,data=s,FUN=sum)

ggplot(subset(w,LFA==38)
       ,aes(x=COMMUNITY_CODE,y=N_Log_Entries)) + geom_bar(stat='identity') + facet_wrap(~SYEAR)


###seasonal fishing
###community
###lincence holder length of time
###vessel sizes




s = aggregate(SD_LOG_ID~LICENCE_ID+LFA+SYEAR,data=a,FUN=function(x) length(unique(x)))
ss = aggregate(SD_LOG_ID~LFA+SYEAR,data=s,FUN=quantile, c(0.025,0.5,.975))
ss = subset(ss,SYEAR>2008 & SYEAR<2022)

with(subset(ss,LFA==34),plot(SYEAR,SD_LOG_ID[,2],xlab='Year',ylab='DaysFished',type='b'))

with(subset(ss,LFA=='36'),plot(SYEAR,SD_LOG_ID[,2],xlab='Year',ylab='DaysFished',type='b'))

