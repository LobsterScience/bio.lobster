RV_survey_from_ESE <- function(){
  
  db.setup(un=oracle.username,pw=oracle.password)
  ca = connect.command(con,"select * from groundfish.TEL_ESE_CATCHES")
  de = connect.command(con,"select * from groundfish.TEL_ESE_lv1_observations")
  inf = connect.command(con,"select * from groundfish.TEL_ESE_sets")
  ba = connect.command(con,"select * from groundfish.TEL_ESE_baskets")
  
  ba$id = paste(ba$MISSION,ba$SETNO,sep="-")
  inf$id = paste(inf$MISSION,inf$SETNO,sep="-")
  de$id = paste(de$MISSION,de$SETNO,sep="-")
  ca$id = paste(ca$MISSION,ca$SETNO,sep="-")
  
  if(all(c(all(ba$id %in% ca$id),all(de$id %in% ca$id),all(ca$id %in% inf$id)))){
  
  
  names(inf) =tolower(names(inf))
  inf$sdate = inf$start_date
  inf$time = inf$start_time
  inf$type = inf$experiment_type_code
  inf = subset(inf,select=c(mission,setno,sdate,time,strat,slat,slong,elat,elong,area,dist,dmin,dmax,type,gear,surface_temperature,bottom_temperature,bottom_salinity))
  inf$sdate = as.Date(inf$sdate,format="%d%m%Y")
    
  ba = aggregate(BASKET_WEIGHT~MISSION+SETNO+SPEC+SIZE_CLASS+SAMPLED,data=ba,FUN=sum )
  bac  = merge(ba,ca,by=c('MISSION','SETNO','SPEC','SIZE_CLASS'),all=T)
  
  dea = aggregate(SPECIMEN_ID~MISSION+SETNO+SPEC+SIZE_CLASS,data=de,FUN=function(x) length(unique(x)))
  names(dea)[5] = 'NUMBER_SAMPLED'
  dea$SAMPLED = 'Y'
  bd = merge(bac,dea,by=c('MISSION','SETNO','SPEC','SIZE_CLASS','SAMPLED'),all.x=T)
  bd$id = paste(bd$MISSION, bd$SETNO, bd$SPEC, bd$SIZE_CLASS,sep="_")
  ui = unique(bd$id)
  
  out = list()
  for(i in 1:length(ui)){
      v = subset(bd,id==ui[i])
      if(nrow(v)==1){
        if(!is.na(v$NUMBER_SAMPLED)) v$NUMBER_CAUGHT = v$NUMBER_SAMPLED
        vv = c(unique(v$MISSION),unique(v$SETNO),unique(v$SPEC),unique(v$SIZE_CLASS),sum(v$BASKET_WEIGHT,na.rm=T),sum(v$BASKET_WEIGHT,na.rm=T),round(sum(c(v$NUMBER_CAUGHT,v$NUMBER_SAMPLED),na.rm=T)))
        }
      if(nrow(v)>1){
          j = which(v$SAMPLED=='Y')
          mw = sum(v$BASKET_WEIGHT[j],na.rm=T)/sum(v$NUMBER_SAMPLED[j],na.rm=T) 
          k = which(v$SAMPLED=='N')
          v$NUMBER_CAUGHT[k] = v$BASKET_WEIGHT[k] / mw
          if(any(!is.na(v$UNWEIGHED_BASKETS))) browser()
          vv = c(unique(v$MISSION),unique(v$SETNO),unique(v$SPEC),unique(v$SIZE_CLASS),sum(v$BASKET_WEIGHT[j],na.rm=T),sum(v$BASKET_WEIGHT,na.rm=T),round(sum(c(v$NUMBER_CAUGHT,v$NUMBER_SAMPLED),na.rm=T)))
        }
    out[[i]]=vv
  }
  
  
  ou = as.data.frame(do.call(rbind, out))
  names(ou) = c('mission',"setno",'spec','size_class','sampwgt','totwgt','totno')
  ou$year = as.numeric(substr(ou$mission,4,7))
  
  de$id = NULL
  de1 = subset(de, LV1_OBSERVATION=='Length',select=c(-DATA_DESC)) %>% spread(key=LV1_OBSERVATION,value=DATA_VALUE)
  de2 = subset(de, LV1_OBSERVATION=='Sex',select=c(-DATA_DESC))  %>% spread(key=LV1_OBSERVATION,value=DATA_VALUE)
  de3 = subset(de, LV1_OBSERVATION=='Weight',select=c(-DATA_DESC))  %>% spread(key=LV1_OBSERVATION,value=DATA_VALUE)
  
   de3$LV1_OBSERVATION_ID= de2$LV1_OBSERVATION_ID =de1$LV1_OBSERVATION_ID<- NULL
  de4 = merge(de3,merge(de2,de1,all=T),all=T)
  
  names(de4) = c('mission','setno','spec','size_class','fshno','fwt','fsex','flen')
  de4$clen=1
  
  # ODBC data dump of bio.groundfish tables
  gscat = groundfish.db( DS="gscat.odbc" )
  gsdet = groundfish.db( DS="gsdet.odbc")
  gsinf = groundfish.db( DS="gsinf.odbc")
  
  #reduce columns and combine
      gsdetr = subset(gsdet,select=names(de4))
      gsdetN = as.data.frame(rbind(gsdetr,de4))
  
      gscatr = subset(gscat,select=names(ou))
      gscatN = as.data.frame(rbind(gscatr,ou))
      
      
      gsinfr = subset(gsinf,select=names(inf))
      gsinfN = as.data.frame(rbind(gsinfr,inf))
 
      gsinfN$id = paste(gsinfN$mission,gsinfN$setno,sep="-")
      gsdetN$id = paste(gsdetN$mission,gsdetN$setno,sep="-")
      gscatN$id = paste(gscatN$mission,gscatN$setno,sep="-")
      
      if(!all(c(all(gsdetN$id %in% gscatN$id),all(gscatN$id %in% gsinfN$id)))) browser()
      
      return(list(gsinfN, gscatN, gsdetN))
    } else {
    stop('missing sets in ese tables')
  }
      
}