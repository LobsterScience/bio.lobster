## licences by port
require(ggplot2)
require(bio.lobster)
require(bio.utilities)
require(devtools)

load_all('~/git/bio.utilities')

a = lobster.db('process.logs.unfiltered')
b = lobster.db('community_code')
d = lobster.db('vessels.by.port')
d = na.zero(d)
d1 = aggregate(cbind(GROSS_TONNAGE, BHP, LOA, BREADTH, DEPTH,YEAR_BUILT)~VR_NUMBER+LFA+YR_FISHED,data=d,FUN=min)
d = na.zero(d1,rev=T)
w = lobster.db('port')
v = lobster.db('port_location')

#Demographics on Lic
o = read.csv(file.path(project.datadirectory('bio.lobster'),'data','LicenceHolder','LicenceHolderInfo2022a.csv'))
i = grep('X',names(o))
o
o = subset(o,Years_Licence_Held<100)
o$LFA = do.call(rbind, strsplit(o$Desc_Eng," - "))[,2]
o$LicStartDate = as.Date(o$Licence_Participant_Start_Date,'%b %d, %Y')
o$BDate = as.Date(o$Birthdate,'%b %d, %Y')
o = subset(o,select=c(Licence_Id, Name_Last_First, BDate, LicStartDate, LFA, Years_Licence_Held, Age))

ggplot(o,aes(x=Age)) + geom_histogram(bins=10) + facet_wrap(~LFA,scales='free_y') 
aggregate(Age~LFA,data=o,FUN=function(x) quantile(x,probs=c(0.1,.5,.9)))



#Logbook Processing
a$DYR = lubridate::decimal_date(a$DATE_FISHED) - lubridate::year(a$DATE_FISHED)
a$WYR = ceiling(a$DYR*52)
a$DWYR = lubridate::year(a$DATE_FISHED) + a$WYR/52
a$P=1

xa = aggregate(cbind(WEIGHT_KG, NUM_OF_TRAPS)~SYEAR+VR_NUMBER+LICENCE_ID+LFA+SD_LOG_ID,data=a,FUN=sum)
xa$P = 1
x = aggregate(cbind(P,WEIGHT_KG, NUM_OF_TRAPS)~SYEAR+VR_NUMBER+LICENCE_ID+LFA,data=xa,FUN=sum)
x$CPUE = x$WEIGHT_KG/x$NUM_OF_TRAPS


#CPUE and vessel merge

xv = merge(x,d, by.x=c('VR_NUMBER','SYEAR','LFA'),by.y=c('VR_NUMBER','YR_FISHED','LFA'),all.x=T)

##CPUE and vessel and operator merge

xvo = merge(xv, o, by.x=c('LICENCE_ID','LFA'),by.y=c('Licence_Id','LFA'),all.x=T)

#how many trips

xx = aggregate(P~SYEAR+LFA,data=x,FUN=mean)
with(subset(xx,LFA==34),plot(SYEAR,P))

x2 = subset(x, SYEAR==2019)
x2$NTrips =x2$P
ggplot(x2, aes(x=NTrips))+geom_histogram()+facet_wrap(~LFA,scales='free_y')

#how many grids per year are they fishing
xg = aggregate(cbind(P,WEIGHT_KG, NUM_OF_TRAPS)~SYEAR+VR_NUMBER+LICENCE_ID+LFA+GRID_NUM,data=a,FUN=sum)
xgg = aggregate(GRID_NUM~SYEAR+VR_NUMBER+LICENCE_ID+LFA,data=subset(xg,GRID_NUM>0),FUN=length)


xvog = merge(xvo, xgg, by.x=c('SYEAR','LICENCE_ID','LFA', 'VR_NUMBER'),by.y=c('SYEAR','LICENCE_ID','LFA', 'VR_NUMBER'),all.x=T)
xvog$ageBoat = xvog$SYEAR - xvog$YEAR_BUILT

ggplot(subset(xvog, SYEAR==2019 & CPUE<8),aes(x=CPUE)) + geom_histogram() + facet_wrap(~LFA, scales='free_y')

ggplot(subset(xvog, SYEAR==2019),aes(x=BHP)) + geom_histogram() + facet_wrap(~LFA, scales='free_y')

ggplot(subset(xvog, SYEAR==2019),aes(x=WEIGHT_KG)) + geom_histogram() + facet_wrap(~LFA, scales='free_y')
ggplot(subset(xvog, SYEAR==2019),aes(x=LOA)) + geom_histogram() + facet_wrap(~LFA, scales='free_y')

ggplot(subset(xvog, SYEAR==2019),aes(x=ageBoat)) + geom_histogram() + facet_wrap(~LFA, scales='free_y')


ggplot(subset(xvog, SYEAR==2019& GRID_NUM<10),aes(x=GRID_NUM)) + geom_histogram() + facet_wrap(~LFA, scales='free_y')

##Statistics

(aggregate(GRID_NUM~LFA, data=subset(xvog,SYEAR==2019),FUN=function(x) c(mean(x),sd(x),length(x))))
(aggregate(Age~LFA, data=subset(xvog,SYEAR==2019),FUN=function(x) c(mean(x),sd(x),length(x))))
(aggregate(log10(WEIGHT_KG)~LFA, data=subset(xvog,SYEAR==2019),FUN=function(x) c(mean(x),sd(x),length(x))))
(aggregate(P~LFA, data=subset(xvog,SYEAR==2019),FUN=function(x) c(mean(x),sd(x),length(x))))
(aggregate(ageBoat~LFA, data=subset(xvog,SYEAR==2019),FUN=function(x) c(mean(x),sd(x),length(x))))
(aggregate(GROSS_TONNAGE~LFA, data=subset(xvog,SYEAR==2019),FUN=function(x) c(mean(x),sd(x),length(x))))

##gini index for evenness of catch using effort as well

t = subset(xvog, LFA==27 & SYEAR==2019)
ii = unique(x$LFA)
m=0
out=list()
for(i in 1:length(ii)){
  k = subset(x, LFA==ii[i])
  ll = unique(k$SYEAR)
  for(l in 1:length(ll)) {
    m=m+1
      n = subset(k,SYEAR==ll[l])
      out[[m]] = c(ii[i],ll[l],bio.survey::gini(x=n$CPUE,y=n$NUM_OF_TRAPS))
    
    }
  
}
out = as.data.frame(do.call(rbind,out))
out = toNums(out, cols=2:3)
names(out) = c('LFA','SYEAR','GINI')
ggplot(out,aes(x=SYEAR,y=GINI))+geom_line() + facet_wrap(~LFA)



