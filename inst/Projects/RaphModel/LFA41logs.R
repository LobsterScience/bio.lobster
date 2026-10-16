lobster.db('logs41') #make sure to do a database recapture through logs41.redo before moving on

logs41 = bio.utilities::rename.df(logs41,c('FV_FISHED_DATETIME'),c('DATE_FISHED'))

logs41$yr = year(logs41$DATE_FISHED) #2002 to present
ziff41$yr = year(ziff41$DATE_FISHED) #1995 to 2001

off41$yr  = year(off41$DATE_FISHED) #1981 to 1994
off41$WEIGHT_KG = off41$ADJ_LOB_LBS*0.453592
ziff41$WEIGHT_KG = ziff41$EST_WEIGHT_LOG_LBS*0.453592
logs41$WEIGHT_KG = logs41$ADJCATCH*0.453592
logs41$DEPTH = ziff41$DEPTH = NA
ziff41$LICENCE_ID = ziff41$ADJCATCH = off41$ADJ_LOB_LBS = ziff41$EST_WEIGHT_LOG_LBS= off41$OFFAREA = off41$LOB_EST_LBS = ziff41$CAPTAIN = logs41$OFFAREA = NULL 
logs41$STRINGID = logs41$SOAK_DAYS = logs41$TRIP_ID = logs41$LAT = logs41$LON =  logs41$LOG_EFRT_STD_INFO_ID = logs41$CAPTAIN = logs41$LICENCE_ID = logs41$EST_WEIGHT_LOG_LBS = logs41$ADJCATCH = NULL

a41 = rbind(off41,ziff41,logs41)
a41 = subset(a41,!is.na(DATE_FISHED) & !is.na(WEIGHT_KG) & !is.na(DDLAT) & DDLON> 60 & DDLAT<50) 
a41$DDLON = a41$DDLON * -1

a41s = st_as_sf(a41,coords=c('DDLON','DDLAT'),crs=4326)
lo = readRDS(file.path(git.repo,'bio.lobster.data','mapping_data','LFA41Offareas.rds'))

a41p = st_join(a41s,lo)
a41p = subset(a41p, !is.na(OFFAREA))
