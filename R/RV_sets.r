#' @export
#' 
RV_sets <- function(){
  
  o = groundfish.db('gs_trawl_conversions')
  set = o$gsinf
  cas = o$gscat
  de = o$gsdet
  
  
  set$X = convert.dd.dddd(set$slong) *-1
  set$Y = convert.dd.dddd(set$slat)
  caL = subset(cas,spec==2550)
  deL = subset(de,spec==2550)
  deL$fsex = ifelse(is.na(deL$fsex),0,deL$fsex)
  
  ii = which(is.na(deL$fwt))
  lobLW1 <- function(row) {
    lobLW(CL=row[1],sex=row[2])
  }
  deL$fwt[ii] =  apply(deL[ii,c('flen','fsex')],1,lobLW1)
  
  deL$wts = deL$fwt * deL$clen
  deL$UID = paste(deL$mission,deL$setno, deL$size_class,sep="-")
  deL$Berried = ifelse(deL$fsex==3,deL$clen,0)
  deL$Legal = ifelse(deL$fsex<3 & deL$flen>82,deL$clen,0)
  deL$Legal = ifelse(deL$fsex<3 & deL$flen==82,deL$clen/2,deL$Legal)
  deL$Legal_wt = deL$Legal * deL$fwt
  deL$Recruit = ifelse(deL$flen<82 & deL$flen>=70 ,deL$clen,0)
  deL$Recruit = ifelse(deL$flen==82 ,deL$clen/2,deL$Recruit)
  deL$Juv = ifelse(deL$flen<=60,deL$clen,0)
  
  sc1=seq(13,253,by=5)
  deL$SZ = sc1[cut(deL$flen,sc1,right=FALSE,labels=F)]
  
  deL1 = aggregate(cbind(wts,clen,Berried,Legal,Legal_wt,Recruit,Juv)~UID+SZ,data=deL,FUN=sum)
  caL$UID = paste(caL$mission, caL$setno, caL$size_class,sep="-")
  deL1 = merge(deL1, caL[,c('UID','sampwgt','totwgt','totno')],all.x=T)
  
  #correcting for subsampling
  deL1$clen = round(ifelse(deL1$sampwgt == deL1$totwgt, deL1$clen, deL1$clen * (deL1$totwgt/deL1$sampwgt)))
  deL1$wts = ifelse(deL1$sampwgt == deL1$totwgt, deL1$wts, deL1$wts * (deL1$totwgt/deL1$sampwgt))
  
  deL1$Berried = round(ifelse(deL1$sampwgt == deL1$totwgt, deL1$Berried, deL1$Berried * (deL1$totwgt/deL1$sampwgt)))
  deL1$Legal = ifelse(deL1$sampwgt == deL1$totwgt, deL1$Legal, deL1$Legal * (deL1$totwgt/deL1$sampwgt))
  deL1$Legal_wt = ifelse(deL1$sampwgt == deL1$totwgt, deL1$Legal_wt, deL1$Legal_wt * (deL1$totwgt/deL1$sampwgt))
  deL1$Recruit = ifelse(deL1$sampwgt == deL1$totwgt, deL1$Recruit, deL1$Recruit * (deL1$totwgt/deL1$sampwgt))
  
  d1 = as.data.frame(do.call(rbind,strsplit(deL1$UID,"-")))
  deL1 = cbind(deL1, d1)
  deL1 = rename.df(deL1, c('V1','V2','V3'),c('mission','setno','scla'))
  deL2 = aggregate(cbind(clen,wts,Legal,Legal_wt,Berried,Recruit,Juv)~mission+setno+SZ,data=deL1,FUN=sum)
  deL2$UID = paste(deL2$mission, deL2$setno, sep="_")
  deL3 = aggregate(cbind(clen,wts,Legal,Legal_wt,Berried,Recruit,Juv)~mission+setno+UID,data=deL2,FUN=sum)
  
  deL2$P=deL2$clen
  aa = aggregate(P~UID+SZ,data=deL2,FUN=sum)
  bb = reshape(aa[,c('UID','SZ','P')],idvar='UID',timevar='SZ', direction='wide')
  bb = na.zero(bb)
  ddd = merge(deL3,bb)
  ddd = rename.df(ddd,c('clen','wts'),c('Lobster','WEIGHT_KG'))
  ddd$WEIGHT_KG = ddd$WEIGHT_KG/1000
  ddd$Legal_wt = ddd$Legal_wt/1000
  
  #
  #combine files
  d4 = aggregate(cbind(totwgt,totno)~mission+setno,data=caL,FUN=sum)
  names(d4)[3:4] = c('WEIGHT_KG','Lobster')
  d4$UID = paste(d4$mission, d4$setno,sep='_')
  d4 = subset(d4, UID %ni% unique(ddd$UID))
  sw = plyr::rbind.fill(ddd,d4)
  com = merge(sw,set[,c('mission','setno','X','Y','dist','WingSpread','gear','sdate','bottom_temperature')],all.y=T)
  io = which(is.na(com$UID))
  com$Lobster[io] = com$WEIGHT_KG[io] = 0
  com$YEAR = lubridate::year(com$sdate)
  io = which(com$YEAR>1998)
  com[io,] = na.zero(com[io,])
  com$UID = NULL
  
  ca = st_as_sf(com, coords = c('X','Y'),crs=4326)

  ouR = st_as_sf(readRDS(file.path( bio.directory,'bio.lobster.data', "survey_corrections",'modelled_recruit_Proportions_34-38.rds')))
  ouC = st_as_sf(readRDS(file.path( bio.directory,'bio.lobster.data', "survey_corrections",'modelled_Commercial_Proportions_34-38.rds')))
  ouCW = st_as_sf(readRDS(file.path( bio.directory,'bio.lobster.data', "survey_corrections" ,'modelled_Commercial_Proportions_wt_34-38.rds')))
  
  ss = st_nearest_feature(ca,ouR)
  ca$Recprop = ouR$Modelled_Proportion[ss]
  ss = st_nearest_feature(ca,ouC)
  ca$Comprop = ouC$Modelled_Proportion[ss]
  ss = st_nearest_feature(ca,ouCW)
  ca$CompropW = ouCW$Modelled_Proportion[ss]
  
  iw = which(ca$YEAR<1999)
  ca$Legal[iw] = round(ca$Lobster[iw] * ca$Comprop[iw])
  ca$Legal_wt[iw] = ca$WEIGHT_KG[iw] * ca$CompropW[iw]
  ca$Recruit[iw] = round(ca$Lobster[iw] * ca$Recprop[iw])
  ca$Juv[iw] = round(ca$Lobster[iw] * ca$Recprop[iw])
  
  ca$X = st_coordinates(ca)[,1]
  ca$Y = st_coordinates(ca)[,2]
  st_geometry(ca) <- NULL
  com = as_tibble(ca)
  
  com$OFFSET = com$dist * com$WingSpread
  com$OFFSET_METRIC = 'TowedDist x wing spread km2'
  com$SOURCE = 'DFO_RV'
  com$EMPTY = ifelse(com$Lobster==0,1,0)
  
  com$timestamp = as.POSIXct(com$sdate,tz='America/Halifax',origin=lubridate::origin)
  com$timestamp = lubridate::with_tz(com$timestamp,"UTC")
  com$DYEAR = lubridate::decimal_date(com$timestamp)- lubridate::year(com$timestamp)
  com$Gear = ifelse(com$gear==3,'Yankee 36',ifelse(com$gear==9,'Western IIA','NEST'))
  com = rename.df(com,c('X','Y','sdate'),c('LONGITUDE','LATITUDE','DATE'))
  com$UID = com$DIST = com$WingSpread = com$gear= com$timestamp = NULL
  if(any(com$LONGITUDE>0)) {
    o = which(com$LONGITUDE>0)
    com$LONGITUDE[o] = com$LONGITUDE[o]*-1
  }
  com <- com %>%
   rowwise() %>%
    mutate(across(c(Lobster, WEIGHT_KG,Legal,Legal_wt,Berried,Recruit,Juv,P.13:P.223, P.13:P.223), ~ .x * OFFSET))
  
  print('this is n or wt, accounting for trawl corrections but is not the numbers per km2')
  return(com)
  
}