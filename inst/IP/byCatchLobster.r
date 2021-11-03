require(bio.lobster)
require(bio.utilities)
require(RODBC)
require(lubridate)
options(stringAsFactors=F)
#SWLSS
if(data.redo){
			channel=odbcConnect(dsn='ptran',uid='cooka',pwd='')
			xAll = sqlQuery(channel,'select * from cooka.lobster_bycatch_assoc')
				xAll$X = convert.dd.dddd(xAll$LONGDDMM)*-1
			xAll$Y = convert.dd.dddd(xAll$LATDDMM)
			saveRDS(xAll,'C:/Users/cooka/Desktop/sharedfolder/Bycatch in the Lobster Fishery/data/CompiledAtSeaSept2021.rds')

			#LOGS
			x = lobster.db('process.logs.unfiltered')
			x = subset(x,LFA %in% c(33,34,35) & SYEAR %in% 2019:2021)
			saveRDS(x,'C:/Users/cooka/Desktop/sharedfolder/Bycatch in the Lobster Fishery/data/LogbooksProUnf.rds')
		
		con = odbcConnect(oracle.lobster.server , uid=oracle.lobster.user, pwd=oracle.lobster.password, believeNRows=F) # believeNRows=F required for oracle db's

			tr = sqlQuery(con,'select * from lobster.istraps')
			se = sqlQuery(con,'select * from lobster.issets_mv')
			de = sqlQuery(con,'select * from lobster.isdetails_mv')
saveRDS(list(tr,se,de),file='C:/Users/cooka/Desktop/sharedfolder/Bycatch in the Lobster Fishery/data/ObserverInfo.rds')



		}


setwd('~/dellshared/Bycatch in the Lobster Fishery')
#a = readRDS('data/CompiledSWLSS.rds')
a = readRDS('data/CompiledAtSeaSept2021.rds')
a = subset(a, OWNER_GROUP=='SWLSS')
x = lobster.db('process.logs.unfiltered')
b = subset(x,LFA %in% c(33,34,35) & SYEAR %in% 2019:2021)
a$COMAREA_ID = toupper(a$COMAREA_ID)
a$COMAREA_ID[which(a$TRIP== '107869-081219')] <- "L33"
o = readRDS('data/ObserverInfo.rds')
ms = read.csv('data/SWLSSTripmatch.csv')
 ms = subset(ms,select=c(TRIP,SD_LOG_ID_1,QUALITY))
 gt = read.csv('data/Grids2Targets.csv')

#logbook handling
b$mn = month(b$DATE_FISHED)
b$GridGroup = b$target = b$Period = NA
for(i in 1:nrow(b)){
	pit = gt$GridGrouping[which(b$GRID_NUM[i]==gt$GRID_NUM & gt$LFA==b$LFA[i])]
	if(length(pit)>0){
	b$GridGroup[i] = pit
	m = b$mn[i]
	k = subset(gt,GRID_NUM==b$GRID_NUM[i]& gt$LFA==b$LFA[i])
	ll = ifelse(m >=k$Period1.Start & m<=k$Period1.End,'Period1',ifelse(m >=k$Period2.Start & m<=k$Period2.End,'Period2',ifelse(m >=k$Period3.Start & m<=k$Period3.End,'Period3','Period4')))
	lll = as.numeric(strsplit(ll,'Period')[[1]][2])
	b$target[i] <- k[,ll]
	b$Period[i] = lll
	}
}
saveRDS(b,'data/logbookReady.rds')
#SWLSS sea sampling handings

a$Legal = ifelse(a$SPECCD_ID == 2550 & a$FISH_LENGTH > 82 & a$SEXCD_ID %in% 1:2,1,0)
a$Berried = ifelse(a$SPECCD_ID == 2550 & a$SEXCD_ID %in% 3,1,0)
a$Lobster = ifelse(a$SPECCD_ID == 2550,1,0)
a$Cod = ifelse(a$SPECCD_ID == 10,1,0)
a$Cusk = ifelse(a$SPECCD_ID == 15,1,0)
a$Jonah = ifelse(a$SPECCD_ID == 2511,1,0)
a$SPECCD_ID  = ifelse(is.na(a$SPECCD_ID),9999,a$SPECCD_ID)
a$Empty = ifelse(a$SPECCD_ID == 9999,1,0)
a$LegalWt = a$Legal * a$CALWT_G/1000
a$LegalWt[which(is.na(a$LegalWt))] <- 0
a$CodWt = a$Cod * a$CALWT_G/1000
a$CuskWt = a$Cusk * a$CALWT_G/1000
a$JonahWt = a$Jonah * a$CALWT_G/1000
a$CodWt[which(is.na(a$CodWt))] <- 0
a$CuskWt[which(is.na(a$CuskWt))] <- 0 
a$JonahWt[which(is.na(a$JonahWt))] <- 0
a$CALWT_G[which(is.na(a$CALWT_G))] <- 0
a$LobsterWt = a$Lobster * a$CALWT_G/1000
a$LobsterWt[which(is.na(a$LobsterWt))] <- 0

a$Sz = round(a$FISH_LENGTH/5)*5
a$SP_SZ = paste(a$SPECCD_ID, a$Sz, sep="-")

a$UID = paste(a$TRIP, a$FISHSET_ID, a$TRAP_ID,sep="-")
a$P = 1
bb = reshape(a[,c('UID','SP_SZ','P')],idvar='UID',timevar='SP_SZ', direction='wide')
bb = na.zero(bb)

#per trap
ac = aggregate(cbind(Lobster, Cod, Cusk, Jonah, Legal, Berried,Empty,LegalWt,CALWT_G,CodWt,CuskWt,JonahWt,LobsterWt)~UID
	+TRIP+X+Y+TRAP_ID+FISHSET_ID+COMAREA_ID+STRATUM_ID+NUM_HOOK_HAUL+BOARD_DATE, data=a,FUN=sum,na.rm=F)

CDa = merge(ac,bb,by='UID')
CDa$'P.9999-NA' = NULL
CDa$P=1

#Empties by month
CDa$mn = month(CDa$BOARD_DATE)
CDa$SYEAR = year(CDa$BOARD_DATE)
CDa$SYEAR = ifelse(CDa$mn>=10,CDa$SYEAR+1,CDa$SYEAR)
Emp = aggregate(cbind(Empty,P)~COMAREA_ID+mn,data=CDa,FUN=sum)
Emp$PropEmt = Emp$Empty / Emp$P

#Traps Sampled By Area Need to merge in grid2targets

CDa$GridGroup = CDa$target = CDa$Period = NA
for(i in 1:nrow(CDa)){
	pit = gt$GridGrouping[which(CDa$STRATUM_ID[i]==gt$GRID_NUM & gt$LFA==strsplit(CDa$COMAREA_ID[i],'L')[[1]][2])]
	if(length(pit)>0){
	CDa$GridGroup[i] = (pit)
	m = CDa$mn[i]
	k = subset(gt,GRID_NUM==CDa$STRATUM_ID[i] & LFA==strsplit(CDa$COMAREA_ID[i],'L')[[1]][2])
	ll = ifelse(m >=k$Period1.Start & m<=k$Period1.End,'Period1',ifelse(m >=k$Period2.Start & m<=k$Period2.End,'Period2',ifelse(m >=k$Period3.Start & m<=k$Period3.End,'Period3','Period4')))
	lll = as.numeric(strsplit(ll,'Period')[[1]][2])
	CDa$target[i] <- as.numeric(k[,ll])
	CDa$Period[i] = lll
#	rm(k,m,ll,lll)
	}
}	

#Total number of traps per Year, Period and GridGrouping

STraps = aggregate(P~SYEAR+GridGroup+COMAREA_ID+Period+target,data=CDa,FUN=sum)
STraps$LFA = as.numeric(unlist(lapply(strsplit(STraps$COMAREA_ID,'L'),"[[",2)))

TTraps = aggregate(NUM_OF_TRAPS~SYEAR+LFA+GridGroup+Period+target,data=b,FUN=sum)

STTraps = merge(TTraps,STraps, all.x=T)

STTraps$Prp = STTraps$P / STTraps$NUM_OF_TRAPS *100
boxplot(Prp~SYEAR+LFA,data=STTraps,las=2,xlab='',ylab='Total Traps Hauled (%)')
savePlot('Figures/ProportionOfTrapsSampled.png')

hist(CDa$Lobster,breaks=0:50)
savePlot('Figures/DistributionOfLobsterCatchesSampledTraps.png')




#Start with averages across traps

aC = aggregate(cbind(Lobster,Cod,Cusk,Jonah,Legal,Berried, Empty,LegalWt)~TRIP+FISHSET_ID+COMAREA_ID+STRATUM_ID+NUM_HOOK_HAUL,data=CDa,FUN=median)

aC$TotLegal = aC$LegalWt * aC$NUM_HOOK_HAUL

aCC = aggregate(cbind(TotLegal,NUM_HOOK_HAUL)~TRIP,data=aC,FUN=sum)

bL = merge(b,ms,by.x='SD_LOG_ID', by.y='SD_LOG_ID_1')
bLL = aggregate(cbind(WEIGHT_KG,NUM_OF_TRAPS)~SD_LOG_ID+TRIP,data=bL,FUN=sum)
SBUMP = merge(aCC,bLL)

#any bias in reported landings?
with(SBUMP,plot(TotLegal,WEIGHT_KG))
abline(b=1,a=0)

with(SBUMP,lm(log(WEIGHT_KG)~log(TotLegal+.01)-1)) #treating the SWLSS data as 'truth'

#using NUM_OF_TRAPS

SBUMP$WEIGHT_ASS_LOGS = SBUMP$TotLegal/SBUMP$NUM_HOOK_HAUL * SBUMP$NUM_OF_TRAPS
with(SBUMP,plot(WEIGHT_ASS_LOGS,WEIGHT_KG))
abline(b=1,a=0)



#any bias in reported trap hauls?
we expect NUM_HOOK_HAUL is under represtned, not entire trip being sampled.dd



#OBSERVERS

tr = o[[1]]
se = o[[2]]
de = o[[3]]

tr = subset(tr, COMAREA_ID %ni% 'L41')

se = subset(se, COMAREA_ID %ni% 'L41')
de = subset(de, COMAREA_ID %ni% 'L41')

LobsterMap('33-35')
se$X = se$HAUL_LONG*-1
se$Y = se$HAUL_LAT
se$EID = 1:nrow(se)

addPoints(se,col='blue',pch=16)
addPoints(na.omit(xAll[,c('X','Y','EID')]),col='red',pch=16)

#do species in se match de?

seSpec = aggregate(NUM_CAUGHT~TRIP+TRIP_ID+FISHSET_ID+SET_NO+SPECCD_ID,data=se,FUN=sum)

deSpec = aggregate(FISH_LENGTH~TRIP+TRIP_ID+FISHSET_ID+SET_NO+SPECCD_ID,data=de,FUN=length)

ouSpec = merge(seSpec,deSpec,all=T)

#issue with FISHSET_ID  100319337  did not measure lobster, dropping this set from further analyses

se = subset(se, FISHSET_ID %ni%100319337 )

seR = aggregate(SPECCD_ID~TRIP+TRIP_ID+FISHSET_ID+CFV+HAUL_DATE+SET_NO+X+Y+COMAREA_ID,data=se,FUN=function(x) length(unique(x)))

merge(seR, subset(tr,select=c(TRIP, TRIP_ID, FISHSET_ID, SET_NO, STRING_NO, NUM_OF_TRAPS, TRAP_NO, GEAR_COND, GEARCOMPONENT_ID, BAIT_1, BAIT_2)))


################\
#relying on atSea.clean for observer data


g = lobster.db('atSea.clean')
gg = subset(g,DESCRIPTION=='ISDB' & SYEAR > 2018 & LFA %in% 33:35)

#add in details where traps are empty
gg$UID = paste(gg$TRIPNO,gg$STRINGNO,sep='_')
iu = unique(gg$UID)
out=list()
for(i in 1:length(iu)){
	kk = which(gg$UID == iu[i])
	tw = gg[kk,]
	if(any(is.na(tw$X))){ 
			oi = tw$TRAPNO[which(is.na(tw$X))]
			ttw = subset(tw,!is.na(X))
			if(any(ttw$TRAPNO %in% oi)) {
				m = m+1
				out[[m]] = subset(tw,TRAPNO==oi)
	}

}
}

out = do.call(rbind,out)