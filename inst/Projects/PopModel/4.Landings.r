#landings

require(bio.lobster)
db.setup()
require(lubridate)

da = connect.command(con,'select * from FRAILC.lobslip_quarter ')
#da$QUARTER = ifelse(da$QUARTER=='Q4','Q3',da$QUARTER)
da = aggregate(SLIP_WEIGHT_LBS~YR+QUARTER,data=da,FUN=sum)
da$T = da$SLIP_WEIGHT_LBS/2.2/1000

da = subset(da,YR<2021,select=c(YR,QUARTER,T))
#daA = data.frame(YR=1975:2020,QUARTER='Q4',T=0)

#da = data.frame(rbind(da,daA))
da = da[order(da$YR,da$QUARTER),]
da$CV = .1
da$Fleet=1

#CPUE


a = lobster.db('process.logs,redo')
p$lfas = c('33',"34", "35", "36", "38") # specify lfas for data summary
p$subareas = c("34", "35", "36", "38") # specify lfas for data summary

lS<-lobster.db('process.logs')
lS = subset(lS,SYEAR<2021 & LFA %in% c(33,34,35,36,38))
lS$MONTH = month(lS$DATE_FISHED)
lS$Quarter = ifelse(lS$MONTH %in% c(10,11,12),'Q1',ifelse(lS$MONTH %in% c(1,2,3),'Q2',ifelse(lS$MONTH %in% c(4,5,6),'Q3','Q4')))

lSa = aggregate(cbind(WEIGHT_KG,NUM_OF_TRAPS)~SYEAR+Quarter,data=lS,FUN=sum)
lSa$CPUE = lSa$WEIGHT_KG / lSa$NUM_OF_TRAPS
lSa$WEIGHT_KG = lSa$NUM_OF_TRAPS = NULL

lobster.db('process.vlog')
V = vlog
V$SYEAR = as.numeric(year(V$FDATE))
V$SYEAR = year(V$FDATE)
V$MONTH = month(V$FDATE)
ii = which(V$MONTH>9)
V$SYEAR[ii] = V$SYEAR[ii]+1 

V$Q = ifelse(V$MONTH %in% c(10,11,12),1,ifelse(V$MONTH %in% c(1,2,3),2,ifelse(V$MONTH %in% c(4,5,6),3,4)))

Va = aggregate(cbind(W_KG,N_TRP)~SYEAR+Q, data=subset(V,LFA %in% p$lfas), FUN=sum)
Va$CPUE = Va$W_KG / Va$N_TRP
Va$W_KG = Va$N_TRP = NULL
names(Va) = c('SYEAR','Quarter','CPUE')

CpE = as.data.frame(rbind(Va,lSa))

da = merge(da,CpE,by.x=c('YR','QUARTER'),by.y=c('SYEAR','Quarter'),all.x=T)

#se = connect.command(con, 'select * from lobster.lobster_atsea_vw')
sc1=seq(53,223,by=5)

see = subset(se,SPECIESCODE==2550 & LFA %in% c(33,34,35,36,38,41))
see$YR = year(see$STARTDATE)
see$Mon = month(see$STARTDATE)
see$QUARTER = ifelse(see$Mon %in% c(10,11,12),'Q1', ifelse(see$Mon %in% c(1,2,3), 'Q2', ifelse(see$Mon %in% c(4,5,6), 'Q3','Q4')))

see$CL = sc1[cut(see$CARLENGTH,sc1,labels=F)]
see$P = 1
see = subset(see,CARLENGTH>=53)
ssA = aggregate(P~CL+YR+QUARTER,data=see,FUN=sum)
ssAA = aggregate(P~YR+QUARTER,data=see,FUN=sum)
names(ssAA)[3] = 'TP'

ssA = merge(ssA,ssAA)
ssA$Pr = ssA$P/ssA$TP
ssA = ssA[order(ssA$CL,ssA$YR,ssA$QUARTER),]
ssAR = reshape(ssA[,c('YR','QUARTER','CL','Pr')],idvar=c('YR','QUARTER'),timevar = 'CL',direction = 'wide')
ssU= aggregate(TRIPNO~YR+QUARTER,data=see, FUN= function(x) length(unique(x)) )
ssARU = merge(ssAR,ssU)
ssARU = na.zero(ssARU)
#ii = which(ssARU$QUARTER=='Q4')
#ssARU[ii,3:37] <- -1
daS = merge(da,ssARU,all.x=T)
daS[is.na(daS)] <- -1

write.csv(daS,file=file.path(wd,paste('EGOM','Catch_propsv2.csv',sep="-")))


##if by sex

see$SID = ifelse(see$SEX %in% c(2,3),paste("F",see$CL,sep="-"),paste('M',see$CL,sep="-"))
see$SX = ifelse(see$SEX %in% c(2,3),"F",'M')

ssA = aggregate(P~SID+SX+CL+YR+QUARTER,data=see,FUN=sum)
ssAA = aggregate(P~YR+QUARTER,data=ssA,FUN=sum)
names(ssAA)[3] = 'TP'

ssA = merge(ssA,ssAA)
ssA$Pr = ssA$P/ssA$TP
ssA = ssA[order(ssA$SX,ssA$CL,ssA$YR,ssA$QUARTER),]
ssAR = reshape(ssA[,c('YR','QUARTER','SID','Pr')],idvar=c('YR','QUARTER'),timevar = 'SID',direction = 'wide')
ssU= aggregate(TRIPNO~YR+QUARTER,data=see, FUN= function(x) length(unique(x)) )
ssARU = merge(ssAR,ssU)
ssARU = na.zero(ssARU)
daS = merge(da,ssARU,all.x=T)
daS[is.na(daS)] <- -1


write.csv(daS,file=file.path(wd,paste('EGOM','Catch_props_sexv4.csv',sep="-")))


