#' @export
scallopSurveyIndex<-function(redo=F,	size_range=c(70,82),sex=0:3, lfa,layerDir=file.path(project.datadirectory("bio.lobster"), "data","maps"),outDir=file.path(project.datadirectory('bio.lobster'),'data'),return_sets_object=F){
if(redo){	
  require(sf)
	# import bycatch data from scallop survey
	lobster.db('scallop')
	scallopStratDefs$X = scallopStratDefs$LONGITUDE
	scallopStratDefs$Y = scallopStratDefs$LATITUDE
	scallopStratDefs$label=as.character(scallopStratDefs$STRATA_ID)
	scallopStratDefs$PID=scallopStratDefs$STRATA_ID
  scallopStratDefs$POS=scallopStratDefs$ORDINAL

sdef=Mar.utils::df_to_sf(scallopStratDefs) ## Make Dataframe into SF object

#   sdef = bio.lobster::pbs.2.gis(scallopStratDefs,make.sf = T,env.object = T,type='polygon',spdf = F)
# 	sdef = bio.utilities::list.names.to.columns(sdef)
# browser()
names(sdef)[1]='STRATA_ID'
	sdef$total_area = st_area(sdef) #total area of strata
	
	#polygons LFA
	rL = read.csv(file.path(layerDir,"Polygons_LFA.csv"))
	rL$label=rL$LFA
	#rdef=Mar.utils::df_to_sf(rL)

	
rdef=Mar.utils::df_to_sf(rL,lat.field = "Y",lon.field = "X") 
rdef<-merge(rdef,unique(rL[,c("PID", "LFA")]), all.x=T, by="PID")

	# rdef = bio.lobster::pbs.2.gis(rL,make.sf = T,env.object = T,type='polygon',spdf = F)
	# rdef = bio.utilities::list.names.to.columns(rdef)
	# 
	# names(rdef)[1]='LFA'
	
	#intersect LFA and strata
	
	lf = subset(rdef,LFA==lfa)
	v = st_intersection(sdef,lf)
	v$total_area_r = st_area(v) #area within LFA
	j = which(v$STRATA_ID ==49)
	if(length(j)>0) v$total_area_r[j] = v$total_area[j] = 239000000
	#
	scallop.tows$Y = convert.dd.dddd(scallop.tows$START_LAT)
	scallop.tows$X = convert.dd.dddd(scallop.tows$START_LONG)
	scT = subset(scallop.tows,select=c('TOW_SEQ','TOW_DATE','STRATA_ID','X','Y','BOTTOM_TEMP'))
	
	scC = subset(scallopSurv,select=c("TOW_SEQ",  "ABUNDANCE_STD","MEAS_VAL", "SEX_ID"))
  
	scC = merge(scT,scC,all.x=T)
	scC$YEAR = lubridate::year(scC$TOW_DATE)
	scC$BOTTOM_TEMP = ifelse(is.na(scC$BOTTOM_TEMP),-99,scC$BOTTOM_TEMP)
	#year subset
  s = subset(scC,YEAR>=1999)
  i = which(is.na(s$ABUNDANCE_STD))
  s$ABUNDANCE_STD[i]=0
  
  #add a dummy column for lobsters meeting a criteria for adjusting catch
  s$dumm= s$dumm2 = 0
  
  #size subset 
  j = which(s$ABUNDANCE_STD==0) #give me the zeros
  k = which(s$MEAS_VAL >= size_range[1]& s$MEAS_VAL<=size_range[2])
  s$dumm[c(j,k)] = 1
  
  #sex subset
  k = which(s$SEX_ID %in% sex) 
  s$dumm2[c(j,k)] = 1
  
  
  s$ABUNDANCE_STD_PRU = s$ABUNDANCE_STD * s$dumm * s$dumm2
  
  sa = aggregate(cbind(ABUNDANCE_STD_PRU)~TOW_SEQ+STRATA_ID+TOW_DATE+X+Y+BOTTOM_TEMP+YEAR,data=s,FUN=sum)
  
	totS = st_as_sf(sa,coords = c('X','Y'),crs=st_crs(4326))
	if(return_sets_object) {return(totS); stop()}
	ss = st_join(totS,v, join=st_within)
	xx = subset(ss,LFA==lfa)
	xx$STRATA_ID = xx$STRATA_ID.y 
	xx$STRATA_ID.y = xx$STRATA_ID.x = xx$total_area = NULL
	
	x = aggregate(ABUNDANCE_STD_PRU~STRATA_ID+YEAR+total_area_r,data=xx,FUN=mean)
	xa = aggregate(total_area_r~YEAR,data=x,FUN=sum)
	names(xa)[2]='total_area'
	xxa =  merge(x,xa)
	sca = aggregate(ABUNDANCE_STD_PRU*total_area_r/total_area~YEAR,data=xxa,FUN=sum)
	names(sca)=c('YEAR','Index')
	recxx = as.data.frame(do.call(cbind,rmed(sca$YEAR,sca$Index)))
	names(recxx)=c('YEAR','Rmed')
	out = merge(sca,recxx)
	saveRDS(list(out,xx),file.path(outDir,paste('scallopSurveyIndex',size_range[1],"_",size_range[2],"-LFA",lfa,'.rds',sep="")))
  }
  readRDS(file.path(outDir,paste('scallopSurveyIndex',size_range[1],"_",size_range[2],"-LFA",lfa,'.rds',sep="")))
  
	
}