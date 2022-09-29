#' @export
CPUEModelData = function(p,redo=T,TempModelling, TempSkip=F){

	if(redo){

		logs = lobster.db("process.logs")
		vlog = lobster.db("process.vlog.redo")

		tmp1 = subset(logs,select=c("DATE_FISHED","SYEAR","WEIGHT_KG","LFA","NUM_OF_TRAPS","subarea","GRID_NUM"))
		tmp1$type = 'mandatory'
		tmp2 = subset(vlog,select=c("FDATE","SYEAR","W_KG","N_TRP","LFA","X","Y"))
		names(tmp2) = c("DATE_FISHED","SYEAR","WEIGHT_KG","NUM_OF_TRAPS","subarea","X","Y")
		tmp2$LFA = tmp2$subarea
		tmp2 = assignArea(tmp2,coords=c("X","Y"))
		tmp2 = subset(tmp2,select=c("DATE_FISHED","SYEAR","WEIGHT_KG","LFA","NUM_OF_TRAPS","subarea","LFA_GRID"))
		tmp2$type = 'voluntary'
	    names(tmp2) = names(tmp1)

	    tmp = rbind(tmp2,tmp1)

	    if('subareas'%in%names(p)){
	       tmp = subset(tmp,subarea %in% p$subareas)
	    } else { tmp = subset(tmp,LFA %in% p$lfas)}

	    tmp$LFA = tmp$subarea
	    tmp$LFA[tmp$subarea%in%c("27N","27S")] = "27"
	    tmp$LFA[tmp$subarea%in%c("33W","33E")] = "33"

	    depths = getGridVar(grids=sort(unique(tmp$GRID_NUM)))
	    names(depths)[1] = "GRID_NUM"

	    cpue.data = merge(tmp,depths,all=T)
		cpue.data$y =	decimal_date(cpue.data$DATE_FISHED)
		cpue.data$DATE_FISHED = as.Date(cpue.data$DATE_FISHED)


		# Create column for week and day of season (WOS, DOS)
		subareas<-unique(cpue.data$subarea[!is.na(cpue.data$subarea)])
		cpue.data$WOS<-NA
		cpue.data$DOS<-NA
		for(a in 1:length(subareas)){
			season<-sort(unique(cpue.data$SYEAR[cpue.data$subarea==subareas[a]]))
			for(i in 1:length(season)){
				cpue.data$WOS[cpue.data$SYEAR==season[i]&cpue.data$subarea==subareas[a]]<-floor((cpue.data$DATE_FISHED[cpue.data$SYEAR==season[i]&cpue.data$subarea==subareas[a]]-min(cpue.data$DATE_FISHED[cpue.data$SYEAR==season[i]&cpue.data$subarea==subareas[a]]))/7)+1
				cpue.data$DOS[cpue.data$SYEAR==season[i]&cpue.data$subarea==subareas[a]]<-cpue.data$DATE_FISHED[cpue.data$SYEAR==season[i]&cpue.data$subarea==subareas[a]]-min(cpue.data$DATE_FISHED[cpue.data$SYEAR==season[i]&cpue.data$subarea==subareas[a]])+1
			}
		}
	    

		
		if (TempSkip){
		  write.csv(cpue.data,file.path( project.datadirectory("bio.lobster"), "data","products","CPUEModelData.csv"),row.names=F)
		}
	 else
		{	if(missing(TempModelling)) TempModelling = TempModel(annual.by.area=F)
		#	save(TempModelling,file=file.path( project.datadirectory('bio.lobster'), "data","products","TempModelling.rdata"))
		#	}else load(file.path( project.datadirectory('bio.lobster'), "data","products","TempModelling.rdata"))


	    newdata = with(cpue.data,data.frame(y=y, cos.y=cos(2*pi*y), sin.y=sin(2*pi*y), DEPTH=DEPTH, area=subarea))

	  cpue.data$TEMP = predict(TempModelling$Model, newdata, type='response')

	  w = max(TempModelling$Data$y)
	  cw = max(cpue.data$y)
	  cpue.data$yy = cpue.data$y
	  #dealing with predictions for years with no data -- taking the mean of the previous 3 years model predictions AMC Sept 2022
	  if(cw-w>1){
	    print(paste('Missing raw temperature years, predictions using the last year of raw data==',floor(w)))
	    if((cw-w>=3)){stop('Too many missing years')}
	    cpue.data$index=1:nrow(cpue.data)
	    k = (floor(cpue.data$y)-floor(w))
	    l = cpue.data$index[which(k>0)]
	    m = list(cpue.data$index[which(k==1)],cpue.data$index[which(k==2)])
	    cpue.data.red = subset(cpue.data, index %ni% l)
	    cpue.data.f = subset(cpue.data, index %in% l)
	    cpue.data.f$TEMP = NULL
	    for(i in 1:length(m)){
	          if(length(m[[i]])>0) {
	              if(i==1){
	                  sc2 = sc3= sc1 = cpue.data[which(cpue.data$index %in% m[[i]]),] 
	                  
	                  sc1$y = sc1$y-1
	                  sc2$y = sc2$y-2
	                  sc3$y = sc3$y-3
	               sc = plyr::rbind.fill(sc1, sc2, sc3)   
	               nd = with(sc,data.frame(y=y, cos.y=cos(2*pi*y), sin.y=sin(2*pi*y), DEPTH=DEPTH, area=subarea))
	               sc$TEMP = predict(TempModelling$Model, nd, type='response')
	               ts = aggregate(TEMP~index,data=sc,FUN=mean)
	              }
	            if(i==2){
	              sc2 = sc3= sc1 = cpue.data[which(cpue.data$index %in% m[[i]]),] 
	              
	              sc1$y = sc1$y-2
	              sc2$y = sc2$y-3
	              sc3$y = sc3$y-4
	              sc = plyr::rbind.fill(sc1, sc2, sc3)   
	              nd = with(sc,data.frame(y=y, cos.y=cos(2*pi*y), sin.y=sin(2*pi*y), DEPTH=DEPTH, area=subarea))
	              sc$TEMP = predict(TempModelling$Model, nd, type='response')
	              ts2 = aggregate(TEMP~index,data=sc,FUN=mean)
	            }
	          }
	      
	    }
	    ts = rbind(ts,ts2)
	    cpue.data.f = merge(cpue.data.f,ts,all.x=T,by='index')
	   cpue.data = rbind(cpue.data.red,cpue.data.f) 
	   }
	  
	  

		write.csv(cpue.data,file.path( project.datadirectory("bio.lobster"), "data","products","CPUEModelData.csv"),row.names=F)}
		
	}
	cpue.data = read.csv(file.path( project.datadirectory("bio.lobster"), "data","products","CPUEModelData.csv"))


	return(cpue.data)


}