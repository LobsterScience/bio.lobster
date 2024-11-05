#' @export

NEFSC_sets <- function(){
  
  set =  nefsc.db(DS='usinf.clean')
  cas =  nefsc.db(DS='uscat.clean')
  de =   nefsc.db(DS='usdet.clean')
  
  sc1=seq(13,253,by=1)
  de$SZ = sc1[cut(de$FLEN,sc1,labels=F)]
  de$Berried = ifelse(de$FSEX==3,de$CLEN,0)
  de$Legal = ifelse(de$FSEX<3 & de$FLEN>82,de$CLEN,0)
  de$Rec = ifelse( de$FLEN<=82 & de$FLEN>=70,de$CLEN,0)
  
  de$WEIGHT_KG = de$CLEN * de$FWT/1000
  de$Legal_wt = de$Legal * de$FWT/1000
  
  dA = aggregate(CLEN~SZ+FSEX+MISSION+SETNO+ID,data=de,FUN=sum)
  dS = aggregate(cbind(Berried,Legal,CLEN,WEIGHT_KG,Legal_wt,Rec)~MISSION+SETNO+ID,data=de,FUN=sum)
  dS$Lobster = dS$CLEN
  dS$CLEN = NULL
  dA$P = dA$CLEN
  aa = aggregate(P~ID+SZ,data=dA,FUN=sum)
  bb = reshape(aa[,c('ID','SZ','P')],idvar='ID',timevar='SZ', direction='wide')
  bb = na.zero(bb)
  
  ca = merge(dS,bb)
  sc = merge(set[,c('MISSION','SETNO','BEGIN_GMT_TOWDATE','DIST','X','Y','BOTTEMP')],ca,all.x=T)
  sc = na.zero(sc)
  sc = rename.df(sc,c('BEGIN_GMT_TOWDATE',"X","Y",'BOTTEMP'),c('DATE','LONGITUDE','LATITUDE','TEMP'))
  sc$ID = NULL
  
  sc$DIST = sc$DIST * 1.852
  sc$OFFSET = sc$DIST * 1.852 * 0.013
  sc$OFFSET_METRIC = 'TowedDist x wing spread km2'
  sc$SOURCE = 'NEFSC_RV'
  sc$EMPTY = ifelse(sc$Lobster==0,1,0)
  
  sc$timestamp = as.POSIXct(sc$DATE,tz='America/Halifax',origin=lubridate::origin)
  sc$timestamp = with_tz(sc$timestamp,"UTC")
  sc$DYEAR = lubridate::decimal_date(sc$timestamp)- lubridate::year(sc$timestamp)
 sc$DIST = sc$timestamp = NULL
 sc$YEAR = year(sc$DATE)
 sc$Gear = 'NEST'
  return(sc)
  
}