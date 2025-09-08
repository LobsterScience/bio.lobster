##LFA 34 log data and analyses BB

require(bio.lobster)
require(devtools)
require(sf)
require(ggplot2)

a = lobster.db('process.logs.unfiltered')
a = subset(a, LFA %in% c(34) & SYEAR <2025 )
#nliceneces

aggregate(LICENCE_ID~SYEAR,data=a,FUN=function(x) length(unique(x)))

x1 = aggregate(WEIGHT_KG~SYEAR+LICENCE_ID+LFA+DOS+DATE_FISHED,data=subset(a,WEIGHT_KG>0 & NUM_OF_TRAPS>0),FUN=sum)
x1$P=x1$WEIGHT_KG

x1a = aggregate(P~SYEAR+LFA+DOS+DATE_FISHED,data=x1,FUN=sum)

xxx = split(x1a,f=list(x1a$LFA,x1a$SYEAR))
junk = list()
ju=list()

for(i in 1:length(xxx)){
  o = xxx[[i]]
  o = o[order(o$DOS),]
  o$prp = cumsum(o$P) / sum(o$P)
  o$doy = lubridate::yday(o$DATE_FISHED)
  junk[[i]] = o  
  ju[[i]] = data.frame(yr = unique(o$SYEAR),LFA=unique(o$LFA),L50 = o$DOS[which.min(abs(o$prp-.5))],L60 = o$DOS[which.min(abs(o$prp-.6))],L75 = o$DOS[which.min(abs(o$prp-.75))], prop7days = o$prp[which.max(o$DOS)-7],propPreJan1 = o$prp[which(o$doy==1)-1])
}
x1a = do.call(rbind,junk)
x1a$Fishing_Season =x1a$SYEAR
ggplot(subset(x1a),aes(x=DOS,y=prp,group=Fishing_Season,colour=Fishing_Season))+scale_colour_viridis_c(option='inferno')+geom_line()+facet_wrap(~LFA)+xlab('Day of Season')+ylab('Proportion of Total Landings')+theme_test(base_size = 14)

###day of season to 50% landings
x1aa = do.call(rbind,ju)
x1aa$Fishing_Season =x1aa$SYEAR
ggplot(x1aa,aes(x=yr,y=L60))+scale_colour_viridis_c(option='inferno')+geom_line()+facet_wrap(~LFA)+xlab('Fishing Season')+ylab('Day of Season to 60%')+theme_test(base_size = 14)

ggplot(x1aa,aes(x=yr,y=L75))+scale_colour_viridis_c(option='inferno')+geom_point()+geom_smooth()+facet_wrap(~LFA)+xlab('Fishing Season')+ylab('Day of Season to 75%')+theme_test(base_size = 14)

ggplot(x1aa,aes(x=yr,y=propPreJan1))+scale_colour_viridis_c(option='inferno')+geom_point()+geom_smooth()+facet_wrap(~LFA)+xlab('Fishing Season')+ylab('Proportion of Landings Before Jan 1')+theme_test(base_size = 14)
mean(x1aa$propPreJan1[23:26])

b = lobster.db('seasonal.landings')
b$SYEAR = as.numeric(substr(b$SYEAR,6,9))
b = subset(b,select=c(SYEAR,LFA34))

x = merge(x1aa,b,by.x='yr',by.y='SYEAR')
x$LandLast7 = (1-x$prop7days) * x$LFA34
x$propMLS = x$LFA34 * (1-x$propPreJan1)*0.17
co = readRDS(file.path(project.datadirectory('Framework_LFA33_34_41'),'CPUE','unBIASED_CPUE.rds'))
cpu = co[[2]]
cc = subset(cpu,lfa==34)
ggplot(cc,aes(x=t,y=unBCPUE))+geom_point()+facet_wrap(~yr)+xlab('Day of Season')+ylab('CPUE (kg/TH)')

###catch composition

lobster.db('atSea')

as = subset(atSea,LFA==34)
as$YR = lubridate::year(as$STARTDATE)
as$mn = lubridate::month(as$STARTDATE)
as$SYEAR = ifelse(as$mn %in% 9:12, as$YR+1,as$YR)

sda = lobster.db('season.dates')
sda = subset(sda,LFA==34)

asd  = merge(as,sda[,c('SYEAR','START_DATE','END_DATE')])
asd$DOS = as.numeric(as.Date(asd$STARTDATE) - asd$START_DATE)
asd$EOS = abs(as.numeric(as.Date(asd$STARTDATE) - asd$END_DATE))

ass = subset(asd,SYEAR>2015 & SPECIESCODE==2550 & TRAPTYPE %in% 1:3)

ass$ID = paste(ass$TRIPNO,ass$TRAPNO,ass$STRINGNO,ass$STARTDATE,ass$LICENCE_ID,sep="_")
ass$WFE = ceiling(ass$EOS/7)
ass$WOS = ceiling(ass$DOS/7)

ass$Berr = ifelse(ass$SEX==3 ,1,0)
ass$Fem = ifelse(ass$SEX==2 ,1,0)
ass$Rec = ifelse(ass$CARLENGTH %in% 70:82 ,1,0)
ass$Leg = ifelse(ass$CARLENGTH >82 & ass$SEX %in% 1:2 ,1,0)

ass$TotL= 1

ag = aggregate(cbind(Berr,Fem,Rec,TotL,Leg)~SYEAR+LATITUDE+LONGITUDE+ID+WFE+DOS+EOS+WOS,data=ass,FUN=sum)

aga = aggregate(cbind(Berr,Fem,Rec,TotL,Leg)~WFE+SYEAR+WOS,data=ag,FUN=sum)

aga$PropB = aga$Berr / aga$TotL
aga$PropF = aga$Fem / aga$TotL
aga$PropR = aga$Rec / aga$TotL
aga$PropL = aga$Leg / aga$TotL

ggplot(aga,aes(x=WOS,y=PropB,colour=SYEAR))+geom_point()

ggplot(aga,aes(x=WOS,y=PropL,group=SYEAR,colour=SYEAR))+geom_point()+geom_smooth(aes(group=1),se=T)+xlab('Week of Season')+ylab('Proportion Legal Lobster')

bb = subset(aga,WFE %in% c(0,1))
bb = aggregate(cbind(Berr,Fem,Rec,TotL,Leg)~SYEAR,data=bb,FUN=sum)
bb = merge(bb,x,by.x='SYEAR',by.y='yr')
bb$PropL = bb$Leg / bb$TotL

bb$Discard_Last_7 = bb$LandLast7/(1-bb$PropL)


######value per trip


a = lobster.db('process.logs')
a = subset(a, LFA %in% c( 34) & SYEAR <2025 & SYEAR>2005)

xg = aggregate(cbind(WEIGHT_KG, NUM_OF_TRAPS)~SYEAR+LICENCE_ID+LFA+GRID_NUM,data=a,FUN=sum)
sp = lobster.db('process_slips')
sp$woy = lubridate::week(sp$Date)

sp = aggregate(PRICE~woy+LFA+SYEAR,data=sp,FUN=median)
ggplot(subset(sp,SYEAR>2006 & LFA %in% c(34)),aes(woy,PRICE, group=LFA,colour=LFA))+geom_path()+facet_wrap(~SYEAR)+theme_test()
a$woy = lubridate::week(a$DATE_FISHED)

asp = merge(a,sp)
asp$valuePerTrip = asp$WEIGHT_KG*2.20462*(asp$PRICE)
asp$valuePerTrap = asp$WEIGHT_KG*2.20462*(asp$PRICE)/asp$NUM_OF_TRAPS
asp$wtprice = asp$PRICE * asp$WEIGHT_KG

oo = aggregate(valuePerTrip~LFA+SYEAR+WOS,data=asp,FUN=function(x) quantile(x,probs=c(0.25,.5,.75)))
oa = aggregate(valuePerTrap~LFA+SYEAR+WOS,data=asp,FUN=function(x) quantile(x,probs=c(0.25,.5,.75)))
op = aggregate(cbind(wtprice,WEIGHT_KG)~LFA+SYEAR+WOS,data=asp,FUN=sum)
op$wtprice = op$wtprice/op$WEIGHT_KG

ggplot(op,aes(x=WOS,y=wtprice,group=SYEAR,colour=SYEAR))+geom_point()+facet_wrap(~LFA)+xlab('Week of Season')+ylab('Price per lb')+theme_test(base_size = 14) +geom_smooth(aes(group=1))+labs(colour='Fishing Season')


ggplot(oo,aes(x=WOS,y=valuePerTrip[,2],group=SYEAR,colour=SYEAR))+geom_point()+facet_wrap(~LFA)+xlab('Week of Season')+ylab('Value Per Trip ($)')+theme_test(base_size = 14)


ggplot(oo,aes(x=SYEAR,y=valuePerTrip[,2],ymin=valuePerTrip[,1],ymax=valuePerTrip[,3]))+geom_point()+geom_errorbar(width=0)+facet_wrap(~LFA)+xlab('Fishing Season')+ylab('Value Per Trip')+theme_test(base_size = 14)


ggplot(oa,aes(x=SYEAR,y=valuePerTrap[,2],ymin=valuePerTrap[,1],ymax=valuePerTrap[,3]))+geom_point()+geom_errorbar(width=0)+facet_wrap(~LFA)+xlab('Fishing Season')+ylab('Value Per Trap')+theme_test(base_size = 14)


oq = aggregate(WEIGHT_KG~LFA+SYEAR,data=asp,FUN=function(x) quantile(x,probs=c(0.25,.5,.75)))

ggplot(oq,aes(x=SYEAR,y=WEIGHT_KG[,2],ymin=WEIGHT_KG[,1],ymax=WEIGHT_KG[,3]))+geom_point()+geom_errorbar(width=0)+facet_wrap(~LFA)+xlab('Fishing Season')+ylab('Landings Per Trip')+theme_test(base_size = 14)



x1 = aggregate(valuePerTrip~SYEAR+LFA+DOS,data=subset(asp,WEIGHT_KG>0 & NUM_OF_TRAPS>0),FUN=sum)
x1$P=x1$valuePerTrip


xxx = split(x1,f=list(x1$LFA,x1$SYEAR))
junk = list()
ju = list()
for(i in 1:length(xxx)){
  o = xxx[[i]]
  o$prp = cumsum(o$P) / sum(o$P)
  junk[[i]] = o  
  ju[[i]] = data.frame(yr = unique(o$SYEAR),LFA=unique(o$LFA),L50 = o$DOS[which.min(abs(o$prp-.5))],L60 = o$DOS[which.min(abs(o$prp-.6))],L75 = o$DOS[which.min(abs(o$prp-.75))], prop7days = o$prp[which.max(o$DOS)-7])
  
}
x1a = do.call(rbind,junk)
x1a$Fishing_Season =x1a$SYEAR
ggplot(subset(x1a),aes(x=DOS,y=prp,group=Fishing_Season,colour=Fishing_Season))+scale_colour_viridis_c(option='inferno')+geom_line()+facet_wrap(~LFA)+xlab('Day of Season')+ylab('Proportion of Total Landings')+theme_test(base_size = 14)


y1aa = do.call(rbind,ju)
y1aa$Fishing_Season =y1aa$SYEAR
ggplot(x1aa,aes(x=yr,y=L60))+scale_colour_viridis_c(option='inferno')+geom_line()+facet_wrap(~LFA)+xlab('Fishing Season')+ylab('Day of Season to 60%')+theme_test(base_size = 14)



###maximum size?
lobster.db('survey')

bb = subset(surveyMeasurements,LFA=='L34' & lubridate::year(SET_DATE)>2020 & FISH_LENGTH>82)
hist(bb$FISH_LENGTH)

bb$I = 1
bc = aggregate(I~FISH_LENGTH,data=bb,FUN=sum)
bc$sI = cumsum(bc$I) / sum(bc$I)

lobster.db('atSea')
b = subset(atSea,LFA==34 & SPECIESCODE==2550 & lubridate::year(STARTDATE)>2020 & CARLENGTH>82) 
b$I = 1
ba = aggregate(I~CARLENGTH,data=b,FUN=sum)
ba$sI = cumsum(ba$I) / sum(ba$I)

ggplot()+geom_line(data=bc,aes(x=FISH_LENGTH,y=sI))+geom_line(data=ba,aes(x=CARLENGTH,y=sI),colour='red')+labs(x='Carapace Length of Legal Size lobster',y='Cumulative proportion of catch')
