#' @export

ILTS_ITQ_sets <-function(x){
  lobster.db("survey")
  lat1 = surveyCatch$SET_LAT
	lat2 = surveyCatch$HAUL_LAT
	lon1 = surveyCatch$SET_LONG
	lon2 = surveyCatch$HAUL_LONG
	surveyCatch$TowL = 6371.3*acos(cos(CircStats::rad(90-lat1))*cos(CircStats::rad(90-lat2))+sin(CircStats::rad(90-lat1))*sin(CircStats::rad(90-lat2))*cos(CircStats::rad(lon1-lon2))) #dist in km
  ii = which(is.na(surveyCatch$NUM_CAUGHT) & !is.na(surveyCatch$EST_KEPT_WT) | !is.na(surveyCatch$EST_DISCARD_WT))
	surveyCatch$NUM_CAUGHT[ii] = sum(c(surveyCatch$EST_KEPT_WT[ii],surveyCatch$EST_DISCARD_WT[ii]),na.rm=T)
	surveyCatch$WEIGHT_KG = ifelse(is.na(surveyCatch$EST_DISCARD_WT),0,surveyCatch$EST_DISCARD_WT) + ifelse(is.na(surveyCatch$EST_KEPT_WT),0,surveyCatch$EST_KEPT_WT)
	sC = surveyCatch[, c('TRIP_ID','LFA','HAULCCD_ID','SET_NO','GEAR','FISHSET_ID','SPECCD_ID','NUM_CAUGHT','SET_LAT','SET_LONG','SET_DATE','HAUL_LAT','HAUL_LONG','HAUL_DATE','SET_ID','YEAR','TowL','WEIGHT_KG')]
	sC$TowW = ifelse(sC$GEAR=='280 BALLOON', 20,13)/1000
	sC = subset(sC, HAULCCD_ID %in% c(1))
	sCa = aggregate(NUM_CAUGHT~TRIP_ID+FISHSET_ID+GEAR+SET_LAT+SET_LONG+SET_DATE+YEAR+TowL+TowW,data=sC,FUN=sum)
  sCa$NUM_CAUGHT = NULL
  
  sCL = aggregate(cbind(NUM_CAUGHT,WEIGHT_KG)~TRIP_ID+FISHSET_ID+GEAR+SET_LAT+SET_LONG+SET_DATE+YEAR+TowL+TowW,data=subset(sC,SPECCD_ID==2550),FUN=sum)
  
  sCT = merge(sCa,sCL,all.x=T)
  sCT = na.zero(sCT)
  
  sM = subset(surveyMeasurements,SPECCD_ID==2550)
  sc1=seq(13,253,by=5)
  sM$SZ = sc1[cut(sM$FISH_LENGTH,sc1,labels=F)]
  sM$P=1
  sM$UID = paste(sM$TRIP_ID,sM$FISHSET_ID,sep="_")
  aa = aggregate(P~UID+SZ,data=sM,FUN=sum)
  bb = reshape(aa[,c('UID','SZ','P')],idvar='UID',timevar='SZ', direction='wide')
  bb = na.zero(bb)
  w=do.call(rbind,strsplit(bb$UID,"_"))
  bb$TRIP_ID=as.numeric(w[,1])
  bb$FISHSET_ID=as.numeric(w[,2])
  sM$Berried = ifelse(sM$SEX==3,1,0)
  sM$Legal = ifelse(sM$FISH_LENGTH>82,1,0)
  sB = aggregate(cbind(Berried,Legal)~UID,data=sM,FUN=sum)
  sMB = merge(bb,sB)
  
  iltstot = merge(sCT,sMB,all.x=T)
  iltstot$UID = NULL
  iltstot = bio.utilities::rename.df(iltstot,c('SET_LAT','SET_LONG','SET_DATE','NUM_CAUGHT','GEAR'),c('LATITUDE','LONGITUDE','DATE','Lobster','Gear'))
	iltstot$OFFSET = iltstot$TowL * iltstot$TowW
  iltstot$OFFSET_METRIC = 'TowedDist x wing spread km2'
  iltstot$SOURCE = 'ILTS_ITQ'
  iltstot$EMPTY = ifelse(iltstot$Lobster==0,1,0)
  
  iltstot$timestamp = as.POSIXct(iltstot$DATE,tz='America/Halifax',origin=lubridate::origin)
  iltstot$timestamp = with_tz(iltstot$timestamp,"UTC")
  iltstot$DYEAR = lubridate::decimal_date(iltstot$timestamp)- lubridate::year(iltstot$timestamp)
  
  iltstot$TowL = iltstot$TowW = iltstot$timestamp = NULL
  i=which(iltstot$YEAR>2012)
  iltstot[i,] = na.zero(iltstot[i,])
  
  return(iltstot)
	}