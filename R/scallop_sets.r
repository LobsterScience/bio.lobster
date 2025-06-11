#' @export

scallop_sets <- function(){
  a = lobster.db('scallop')
  scallop.tows=a[[1]]
  scallopSurv = a[[2]]
  scallop.tows$Y = convert.dd.dddd(scallop.tows$START_LAT)
  scallop.tows$X = convert.dd.dddd(scallop.tows$START_LONG)
  scallop.tows$OFFSET = ifelse(scallop.tows$DRAG_WIDTH==2.5, scallop.tows$TOW_LEN*3.048, scallop.tows$TOW_LEN*5.8464 ) #sameoto conversion email July 28 2024; 
  scT = subset(scallop.tows,select=c('TOW_SEQ','TOW_DATE','STRATA_ID','X','Y', 'OFFSET'))
  
  scC = subset(scallopSurv,select=c("TOW_SEQ",  "ABUNDANCE_RAW","MEAS_VAL", "SEX_ID") )
  ms = sum(scC$MEAS_VAL*scC$ABUNDANCE_RAW,na.rm=T) / sum(scC$ABUNDANCE_RAW,na.rm=T)
  scC$MEAS_VAL = ifelse(scC$ABUNDANCE_RAW>0 & is.na(scC$MEAS_VAL),ms,scC$MEAS_VAL)
  sc1=seq(13,253,by=5)
  scC$SZ = sc1[cut(scC$MEAS_VAL,sc1,right=FALSE,labels=F)]
  scC$Berried = ifelse(scC$SEX_ID==3,scC$ABUNDANCE_RAW,0)
  scC$Legal = ifelse(scC$MEAS_VAL>82,scC$ABUNDANCE_RAW,0)
  scC$Legal = ifelse(scC$MEAS_VAL==82,scC$ABUNDANCE_RAW/2,scC$Legal)
  scC$Recruit = ifelse(scC$MEAS_VAL %in% 70:81,scC$ABUNDANCE_RAW,0)
  scC$Recruit = ifelse(scC$MEAS_VAL==82,scC$ABUNDANCE_RAW/2,scC$Recruit)
  scC$Legal_wt = lobLW(CL=scC$MEAS_VAL,sex=scC$SEX_ID)* scC$Legal
  deL1 = aggregate(cbind(ABUNDANCE_RAW,Berried,Legal,Legal_wt,Recruit)~TOW_SEQ+SZ,data=scC,FUN=sum)
  deL = subset(deL1,select=c(TOW_SEQ,SZ,ABUNDANCE_RAW))
  names(deL)[3]= 'P'
  aa = aggregate(P~TOW_SEQ+SZ,data=deL,FUN=sum)
  bb = reshape(aa[,c('TOW_SEQ','SZ','P')],idvar='TOW_SEQ',timevar='SZ', direction='wide')
  bb = na.zero(bb)
  
  deL1 = aggregate(cbind(ABUNDANCE_RAW,Berried,Legal,Legal_wt,Recruit)~TOW_SEQ,data=scC,FUN=sum)
  
  scC = merge(deL1,bb)
  scC = bio.utilities::rename.df(scC, 'ABUNDANCE_RAW','Lobster')
  sc = merge(scT,scC,all=T) 
  sc = bio.utilities::na.zero(sc)
  sc$YEAR = lubridate::year(sc$TOW_DATE)
  sc = subset(sc,YEAR>=1990)
  sc$OFFSET_METRIC = 'Tow m2'
  return(sc)
    }