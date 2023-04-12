#' @export
#' 
RV_sets <- function(){
  set = groundfish.db(DS='gsinf.odbc')
  cas = groundfish.db(DS='gscat.odbc')
  de = groundfish.db(DS='gsdet.odbc')
  
  set$X = convert.dd.dddd(set$slong) *-1
  set$Y = convert.dd.dddd(set$slat)
  set$DIST = set$dist * 1.852 #nm to km
  #3 is yankee 36, 9 is western iia and 15 is NEST
  set = subset(set, gear %in% c(3,9,15))
  set$WingSpread = ifelse(set$gear==3,10.97/1000,ifelse(set$gear==9,12.49/1000,ifelse(set$gear==15,13/100,NA))) #yankee 36
  
  ii = which(set$type %in% c(1,5))
  print('Both set types 1 and 5 are saved in data frame but only 1 is used for stratified')
  set = set[ii,]
  
  io = which(is.na(cas$totwgt) | cas$totwgt==0 & cas$totno>0)
  cas[io,'totwgt'] <- 1
  
  io = which(is.na(cas$totno) & !is.na(cas$totwgt))
  cas[io,'totno'] = cas[io,'totwgt']/0.806 #mean weight of individual per tow taken from 1999 to 2015
  
  io = which(is.na(cas$sampwgt) & !is.na(cas$totwgt))
  cas[io,'sampwgt'] <- cas[io,'totwgt']

  io = which(cas$sampwgt==0 & cas$totwgt!=0)
  cas[io,'sampwgt'] <- cas[io,'totwgt']
  
    caL = subset(cas,spec==2550)
  deL = subset(de,spec==2550)
  
  
    io = which(!is.finite(deL$fwt))
    fit = nls(fwt~a*flen^b,subset(deL,is.finite(fwt)),start=list(a=0.001,b=3.3))
    ab = coef(fit)
    deL$fwt[io] = ab[1]*deL$flen[io]^ab[2]
  
    deL$wts = deL$fwt * deL$clen
    deL$UID = paste(deL$mission,deL$setno, deL$size_class,sep="-")
    dd = aggregate(cbind(wts,clen)~UID,data=deL,FUN=sum)
    names(dd)[2:3] = c('sumwts','sumclen')
    deL = merge(deL,dd)
    
    sc1=seq(13,253,by=5)
    deL$SZ = sc1[cut(deL$flen,sc1,labels=F)]
    deL$Berried = ifelse(deL$fsex==3,deL$clen,0)
    deL$Legal = ifelse(deL$flen>82 & deL$fsex <3,deL$clen,0)
    
    deL1 = aggregate(cbind(wts,clen,Legal, Berried)~UID+SZ+sumwts+sumclen,data=deL,FUN=sum)
    caL$UID = paste(caL$mission, caL$setno, caL$size_class,sep="-")
    deL1 = merge(deL1, caL[,c('UID','sampwgt','totwgt','totno')],all.x=T)
    
    #correcting for subsampling
    deL1$clenCorr = ceiling(ifelse(deL1$sampwgt == deL1$totwgt, deL1$clen, deL1$totno * (deL1$clen/deL1$sumclen)))
    deL1$wtsCorr = ifelse(deL1$sampwgt == deL1$totwgt, deL1$wts/1000, deL1$totwgt * (deL1$wts/deL1$sumwts))
    d1 = as.data.frame(do.call(rbind,strsplit(deL1$UID,"-")))
    deL1 = cbind(deL1, d1)
    deL1 = rename.df(deL1, c('V1','V2','V3'),c('mission','setno','scla'))
    deL2 = aggregate(cbind(clenCorr,wtsCorr,Legal,Berried)~mission+setno+SZ,data=deL1,FUN=sum)
    deL2$UID = paste(deL2$mission, deL2$setno, sep="_")
    deL3 = aggregate(cbind(clenCorr,wtsCorr,Legal,Berried)~mission+setno+UID,data=deL2,FUN=sum)
    
  deL2$P=deL2$clenCorr
  aa = aggregate(P~UID+SZ,data=deL2,FUN=sum)
  bb = reshape(aa[,c('UID','SZ','P')],idvar='UID',timevar='SZ', direction='wide')
  bb = na.zero(bb)
  ddd = merge(deL3,bb)
  ddd = rename.df(ddd,c('clenCorr','wtsCorr'),c('Lobster','WEIGHT_KG'))
  #
  #combine files
  d4 = aggregate(cbind(totwgt,totno)~mission+setno,data=caL,FUN=sum)
  names(d4)[3:4] = c('WEIGHT_KG','Lobster')
  d4$UID = paste(d4$mission, d4$setno,sep='_')
  d4 = subset(d4, UID %ni% unique(ddd$UID))
  sw = plyr::rbind.fill(ddd,d4)
  com = merge(sw,set[,c('mission','setno','X','Y','DIST','WingSpread','gear','sdate','bottom_temperature')],all.y=T)
  io = which(is.na(com$UID))
  com$Lobster[io] = com$WEIGHT_KG[io] = 0
  com$YEAR = year(com$sdate)
  io = which(com$YEAR>1998)
  com[io,] = na.zero(com[io,])
  com$UID = NULL
  
  com$OFFSET = com$DIST * com$WingSpread
  com$OFFSET_METRIC = 'TowedDist x wing spread km2'
  com$SOURCE = 'DFO_RV'
  com$EMPTY = ifelse(com$Lobster==0,1,0)
  
  com$timestamp = as.POSIXct(com$sdate,tz='America/Halifax',origin=lubridate::origin)
  com$timestamp = with_tz(com$timestamp,"UTC")
  com$DYEAR = lubridate::decimal_date(com$timestamp)- lubridate::year(com$timestamp)
  com$Gear = ifelse(com$gear==3,'Yankee 36',ifelse(com$gear==9,'Western IIA','NEST'))
  com = rename.df(com,c('X','Y','sdate'),c('LONGITUDE','LATITUDE','DATE'))
  com$UID = com$DIST = com$WingSpread = com$gear= com$timestamp = NULL
  
  return(com)
 
}