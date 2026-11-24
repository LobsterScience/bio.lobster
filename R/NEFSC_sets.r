#' @export

NEFSC_sets <- function(length.group=5){
  
  set =  nefsc.db(DS='usinf.clean')
  cas =  nefsc.db(DS='uscat.clean')
  de =   nefsc.db(DS='usdet.clean')
  
  ss = st_as_sf(set,coords=c('X','Y'),crs=4326)
  gr = readRDS(file.path(git.repo,'bio.lobster.data','mapping_data','LFAPolysSF.rds'))
  gr <- gr %>%
    mutate(area = abs(st_area(geometry))) %>%
    group_by( LFA) %>%
    slice_max(order_by = area, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(-area)
  ss = st_join(ss,gr,largest = T)
  ss = subset(as_tibble(ss),select=c(ID,LFA))
  set = merge(set,ss,by='ID',all.x=T)
  set$YEAR = set$GMT_YEAR
  ses = subset(set,select=c(ID,YEAR,LFA))
  
  sc1=seq(13,253,by=length.group)
  de = merge(de,ses,all.x=T)
  de$SZ = sc1[cut(de$FLEN,sc1,labels=F)]
  de$Berried = ifelse(de$FSEX==3,de$CLEN,0)
  de$Legal = ifelse(de$FSEX<3 & de$FLEN>82,de$CLEN,0)
  de$Legal = ifelse(de$FSEX<3 & de$FLEN==82,de$CLEN/2,de$Legal)
  de$Recruit = ifelse( de$FLEN %in% 70:82,de$CLEN,0)
  de$Recruit = ifelse( de$FLEN ==82,de$CLEN/2,de$Recruit)
  
  de$Juv = ifelse( de$FLEN <= 60,de$CLEN,0)
  
  de$WEIGHT_KG = de$CLEN * de$FWT/1000
  de$Legal_wt = de$Legal * de$FWT/1000
  de$Recruit_wt = de$Recruit * de$FWT/1000
  i = which(de$FSEX %in% c(2,3))
  j = which(!is.na(de$LFA))
  i = intersect(i,j)
  de$EggProd = 0
  de$EggProd[i] = eggProduction(cl=de$FLEN[i],lfa=de$LFA[i], year =de$YEAR ) * de$CLEN[i]
  j = which(is.na(de$LFA))
  de$EggProd[j] = NA
  
  dA = aggregate(CLEN~SZ+FSEX+MISSION+SETNO+ID,data=de,FUN=sum)
  
  dS = aggregate(cbind(Berried,Legal,CLEN,WEIGHT_KG,Legal_wt,Recruit,Juv,Recruit_wt,EggProd)~MISSION+SETNO+ID,data=de,FUN=sum)
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
  sc$timestamp = lubridate::with_tz(sc$timestamp,"UTC")
  sc$DYEAR = lubridate::decimal_date(sc$timestamp)- lubridate::year(sc$timestamp)
 sc$DIST = sc$timestamp = NULL
 sc$YEAR = lubridate::year(sc$DATE)
 sc$Gear = 'NEST'
 print('this is n or wt, accounting for trawl corrections but is not the numbers per km2')
  return(sc)
  
}