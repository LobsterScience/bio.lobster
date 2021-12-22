#landings

require(bio.lobster)
db.setup()
require(lubridate)

da = connect.command(con,'select * from FRAILC.lobslip_quarter ')
da$QUARTER = ifelse(da$QUARTER=='Q4','Q3',da$QUARTER)
da = aggregate(SLIP_WEIGHT_LBS~YR+QUARTER,data=da,FUN=sum)
da$T = da$SLIP_WEIGHT_LBS/2.2/1000

da = subset(da,YR<2021,select=c(YR,QUARTER,T))
daA = data.frame(YR=1975:2020,QUARTER='Q4',T=0)

da = data.frame(rbind(da,daA))
da = da[order(da$YR,da$QUARTER),]
da$CV = .1
da$Fleet=1
da$CVCPUE = da$CPUE2=da$CPUE=-1

#se = connect.command(con, 'select * from lobster.lobster_atsea_vw')
sc1=seq(53,223,by=5)

see = subset(se,SPECIESCODE==2550 & LFA %in% c(33,34,35,36,38,41))
see$YR = year(see$STARTDATE)
see$Mon = month(see$STARTDATE)
see$QUARTER = ifelse(see$Mon %in% c(10,11,12),'Q1', ifelse(see$Mon %in% c(1,2,3), 'Q2', ifelse(see$Mon %in% c(4,5,6), 'Q3','Q4')))

see$CL = sc1[cut(see$CARLENGTH,sc1,labels=F)]
see$P = 1
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

daS = merge(da,ssARU,all.x=T)
daS[is.na(daS)] <- -1

write.csv(daS,file=file.path(wd,paste('EGOM','Catch_props.csv',sep="-")))