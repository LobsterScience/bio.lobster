#' @export
addSYEAR<-function(data,date.field="STARTDATE"){
	Fish.Date<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","inputs","FishingSeasonDates.csv"))
	Fish.Date$START_DATE<-as.Date(Fish.Date$START_DATE,"%d/%m/%Y")
	Fish.Date$END_DATE<-as.Date(Fish.Date$END_DATE,"%d/%m/%Y")

 	names(data)[which(names(data)==date.field)]<-'SDATE'
	lfa <- unique(data$LFA)

	# select for records within season
	data$SYEAR<-NA
	data$SDATE<-as.Date(data$SDATE)
	for(i in 1:length(lfa)) {
		h <- Fish.Date[Fish.Date$LFA==lfa[i],]	
		for(j in 1:nrow(h)) {
			data$SYEAR[data$LFA==lfa[i]&data$SDATE>=h[j,'START_DATE']&data$SDATE<=h[j,'END_DATE']]<-h[j,'SYEAR']
		}
	}
	data


}