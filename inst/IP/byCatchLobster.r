require(bio.lobster)
require(bio.utilities)
require(RODBC)
require(lubridate)
options(stringAsFactors=F)
#SWLSS
if(data.redo){
			channel=odbcConnect(dsn='ptran',uid='cooka',pwd='bzz7plf')
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
b = readRDS('data/LogbooksProUnf.rds')
o = readRDS('data/ObserverInfo.rds')
 ms = read.csv('data/SWLSSTripmatch.csv')
 ms = subset(ms,select=c(TRIP,SD_LOG_ID_1,QUALITY))

a$Legal = ifelse(a$SPECCD_ID == 2550 & a$FISH_LENGTH > 82 & a$SEXCD_ID %in% 1:2,1,0)
a$Berried = ifelse(a$SPECCD_ID == 2550 & a$SEXCD_ID %in% 3,1,0)
a$Lobster = ifelse(a$SPECCD_ID == 2550,1,0)
a$Cod = ifelse(a$SPECCD_ID == 10,1,0)
a$Cusk = ifelse(a$SPECCD_ID == 15,1,0)
a$Jonah = ifelse(a$SPECCD_ID == 2511,1,0)
a$Empty = ifelse(a$SPECCD_ID == 9999,1,0)
a$LegalWt = a$Legal * a$CALWT_G/1000
a$LegalWt = na.zero(a$LegalWt) 
a$LegalWt[which(is.na(a$LegalWt))] <- 0


a$Sz = round(a$FISH_LENGTH/5)*5
a$SP_SZ = paste(a$SPECCD_ID, a$Sz, sep="-")

a$UID = paste(a$TRIP, a$FISHSET_ID, a$TRAP_ID,sep="-")
a$P = 1
bb = reshape(a[,c('UID','SP_SZ','P')],idvar='UID',timevar='SP_SZ', direction='wide')
bb = na.zero(bb)
ac = aggregate(cbind(Lobster, Cod, Cusk, Jonah, Legal, Berried,Empty,LegalWt)~UID+TRIP+X+Y+TRAP_ID+FISHSET_ID+COMAREA_ID+STRATUM_ID+NUM_HOOK_HAUL, data=a,FUN=sum)

CDa = merge(ac,bb,by='UID')
CDa$'P.9999-NA' = NULL

#Start with averages

aC = aggregate(cbind(Lobster,Cod,Cusk,Jonah,Legal,Berried, Empty,LegalWt)~TRIP+FISHSET_ID+COMAREA_ID+STRATUM_ID+NUM_HOOK_HAUL,data=CDa,FUN=median)

aC$TotLegal = aC$LegalWt * aC$NUM_HOOK_HAUL

aCC = aggregate(cbind(TotLegal,NUM_HOOK_HAUL)~TRIP,data=aC,FUN=sum)

bL = merge(b,ms,by.x='SD_LOG_ID', by.y='SD_LOG_ID_1')
bLL = aggregate(cbind(WEIGHT_KG,NUM_OF_TRAPS)~SD_LOG_ID+TRIP,data=bL,FUN=sum)
SBUMP = merge(aCC,bLL)

#any bias in reported landings?
with(SBUMP,plot(TotLegal,WEIGHT_KG))
abline(b=1,a=0)

with(SBUMP,lm(log(WEIGHT_KG)~log(TotLegal)-1)) #treating the SWLSS data as 'truth'

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


