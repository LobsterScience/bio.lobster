#' @export
calcIndexStations = function(trend.dat,n=16, map=F, include.current.year=F){
		#Stns<-read.csv(file.path(project.datadirectory('bio.lobster'),'data',"survey32Stations.csv"))
		#trend.dat<-subset(trend.dat,SID%in%Stns$SID)
		
		h = as.data.frame(unique(cbind(trend.dat$SID,trend.dat$YEAR)))
		names(h) = c('SID','YEAR')
		g = aggregate(YEAR~SID,data=h,FUN=length)
		g = subset(g,YEAR>=n)$SID
		h = subset(h,SID %in% c(g))
		h = aggregate(YEAR~SID,data=h,FUN=max)
		h = subset(h,YEAR==max(trend.dat$YEAR))$SID
		if(include.current.year)trend.dat = subset(trend.dat,SID %in% h)
		else trend.dat = subset(trend.dat,SID %in% g)
		print(h)
		if(map){
			x11()
			bioMap('lfa34')
			points(SET_LAT~SET_LONG,trend.dat)
		}

		print(paste(length(h),"index stations sampled in at least", n, "years"))

		return(trend.dat)

	}
