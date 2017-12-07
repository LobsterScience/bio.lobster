#' @export
CPUEModelData = function(p,redo.temp=F){

	logs = lobster.db("process.logs")
	vlogs = lobster.db("process.vlog")

	tmp1 = subset(logs,select=c("DATE_FISHED","SYEAR","TOTAL_WEIGHT_KG","NUM_OF_TRAPS","subarea","GRID_NUM"))
	tmp1$type = 'mandatory'
	tmp2 = subset(vlogs,select=c("FDATE","SYEAR","W_KG","N_TRP","LFA","X","Y"))
	names(tmp2) = c("DATE_FISHED","SYEAR","TOTAL_WEIGHT_KG","NUM_OF_TRAPS","subarea","X","Y")
	tmp2 = assignArea(tmp2,coords=c("X","Y"))
	tmp2 = subset(tmp2,select=c("DATE_FISHED","SYEAR","TOTAL_WEIGHT_KG","NUM_OF_TRAPS","subarea","LFA_GRID"))
	tmp2$type = 'voluntary'
    names(tmp2) = names(tmp1)

    tmp = rbind(tmp2,tmp1)

    tmp = subset(tmp,subarea %in% p$subareas)

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
    

 
	if(redo.temp){
		TempModelling = TempModel()
		save(TempModelling,file=file.path( project.datadirectory('bio.lobster'), "data","products","TempModelling.rdata"))
		}else load(file.path( project.datadirectory('bio.lobster'), "data","products","TempModelling.rdata"))


    newdata = with(cpue.data,data.frame(y=y, cos.y=cos(2*pi*y), sin.y=sin(2*pi*y), DEPTH=DEPTH, area=subarea))
	cpue.data$TEMP = predict(TempModelling$Model, newdata, type='response')


	write.csv(cpue.data,file.path( project.datadirectory("bio.lobster"), "data","products","CPUEModelData.csv"),row.names=F)

	return(cpue.data)


}