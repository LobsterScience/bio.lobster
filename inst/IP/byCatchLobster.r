require(bio.lobster)
require(bio.utilities)
require(RODBC)
require(lubridate)
options(stringAsFactors=F)
#SWLSS
if(data.redo){
			x20 = read.csv(	'C:/Users/cooka/Desktop/BYCATCH/Bycatch Validation 20-21/SummaryOfTrips/CompiledData.SWLSS.2021-07-09.csv')
			x19 = read.csv(	'C:/Users/cooka/Desktop/BYCATCH/Bycatch Validation 19-20/SummaryOfTrips/CompiledData.SWLSS.2021-07-09.csv')
			x18 = read.csv(	'C:/Users/cooka/Desktop/BYCATCH/Bycatch Validation Oct 2019/SummaryOfTrips/CompiledData.SWLSS.2020-02-21.csv')

			xAll = rbind(x18,x19,x20)
			xAll$X = convert.dd.dddd(xAll$LONGDDMM)*-1
			xAll$Y = convert.dd.dddd(xAll$LATDDMM)
			xAll$EID = 1:nrow(xAll)
			saveRDS(xAll,'C:/Users/cooka/Desktop/sharedfolder/Bycatch in the Lobster Fishery/data/CompiledSWLSS.rds')

			#LOGS
			x = lobster.db('process.logs.unfiltered')
			x = subset(x,LFA %in% c(33,34,35) & SYEAR %in% 2019:2021)
			saveRDS(x,'C:/Users/cooka/Desktop/sharedfolder/Bycatch in the Lobster Fishery/data/Compiledlogs.rds')
		
		con = odbcConnect(oracle.lobster.server , uid=oracle.lobster.user, pwd=oracle.lobster.password, believeNRows=F) # believeNRows=F required for oracle db's

			tr = sqlQuery(con,'select * from lobster.istraps')
			se = sqlQuery(con,'select * from lobster.issets_mv')
			de = sqlQuery(con,'select * from lobster.isdetails_mv')
saveRDS(list(tr,se,de),file='C:/Users/cooka/Desktop/sharedfolder/Bycatch in the Lobster Fishery/data/ObserverInfo.rds')



		}


setwd('~/dellshared/Bycatch in the Lobster Fishery')
a = readRDS('data/CompiledSWLSS.rds')
b = readRDS('data/Compiledlogs.rds')
o = readRDS('data/ObserverInfo.rds')
 ms = read.csv('data/SWLSSTripmatch.csv')


#SWLSS Data
a$BOARD_DATE = as.Date(a$BOARD_DATE)
a$LANDING_DATE = as.Date(a$LANDING_DATE)

outputs = list()
m=0
for(i in 1:nrow(ms)) {
	g = ms[i,]
	print(i)
	p = subset(a,TRIP==g$TRIP)
	p$COMMENTS=NA
    w = grep('SD_LOG',names(g))
	k = which(!is.na(g[,w]))
	if(length(k)==1){
			l = subset(b,SD_LOG_ID==g[,w[k]], select=c(SYEAR,WOS,DOS,GRID_NUM,WEIGHT_KG,NUM_OF_TRAPS))
			if(nrow(l)==1){
				m=m+1
				print(paste(m,'One Grid'))
				ll = l[rep(seq_len(nrow(l)),each=nrow(p)),]
				 ll$GRID_NUM = NULL
				 p$COMMENTS ='Only one grid and matches'
				 outputs[[m]] = cbind(p,ll)
			}
			if(nrow(l)>1){
				m=m+1
				v = unique(l$GRID_NUM)
				vv = unique(p$STRATUM_ID)
				if(all(v %in% vv)){
					print(paste(m,'all match'))
					p = merge(p,l,by.x='STRATUM_ID',by.y='GRID_NUM')
					p$COMMENTS ='Multiple grids match'
				   p$GRID_NUM = NULL
				 outputs[[m]] = p
				 	
				} else if(any(vv %in% v)) {
					print(paste(m,'some match'))
					l = aggregate(cbind(NUM_OF_TRAPS,WEIGHT_KG)~SYEAR+WOS+DOS,data=l,FUN=sum)
				 ll = l[rep(seq_len(nrow(l)),each=nrow(p)),]
				 p$COMMENTS ='Some grids dont match'
				 ll$GRID_NUM = NULL
				 outputs[[m]] = cbind(p,ll)
				} else {
					print(paste(m,'no match'))
				l = aggregate(cbind(NUM_OF_TRAPS,WEIGHT_KG)~SYEAR+WOS+DOS,data=l,FUN=sum)
				 ll = l[rep(seq_len(nrow(l)),each=nrow(p)),]
				 ll$GRID_NUM = NULL
				p$COMMENTS = 'Grids Dont Match'
				outputs[[m]] = cbind(p,ll)
				}
			}
		}
	}


## need to finish for the rows with multiple SD_LOGS
s=do.call(rbind,outputs)

t = aggregate(NUM_HOOK_HAUL~TRIP+NUM_OF_TRAPS+WEIGHT_KG+STRATUM_ID,data=s,FUN=sum)



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


