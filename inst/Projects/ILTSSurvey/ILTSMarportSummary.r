require(bio.lobster)
require(bio.utilities)
options(stringsAsFactors=F)
la()
fpath = file.path(project.datadirectory('bio.lobster'),'data','survey','2016','ILTS_2016_Marport')

#marpath = file.path(fpath,'marport')
#tpath = file.path(fpath,'temperature')

fm = list.files(file.path(fpath),recursive=T, all.files=T,full.names=T)

#LobsterMarport(file=fm[3])

out = list()
m = 0
for(i in fm) {
	print(i)
	m = m+1
	file = i
	out = LobsterMarport(file=file)
 	gps = out[[2]]
 	sensors =  out[[1]]
	
	sensors$DT = as.POSIXct(paste(sensors$Date, sensors$Time))
	gps$DT = as.POSIXct(paste(gps$Date, gps$Time))
	
	gpsX = aggregate(X~DT,data=gps,FUN=mean)
	gpsY = aggregate(Y~DT,data=gps,FUN=mean)
	
	gps = merge(gpsX,gpsY)
	all = merge(sensors, gps, all=T, by='DT')
	
	su = strsplit(unlist(strsplit(all$Station,'Tow')),'Stn')
	su = do.call(rbind,su)
	all2=as.data.frame(cbind(all,su))
	names(all2) = c('DateTime','Date','Time','Info','Info2','Measure','X1','X2','Info3','Info4','Station','X','Y',"Tow",'Station')
 	out[[i]] = all2
}



gps = do.call(rbind,gps)
sensors = do.call(rbind,sensors)
save(sensors, file=file.path(project.datadirectory('lobster'),'data','survey','marport.rdata'))
save(gps, file=file.path(project.datadirectory('lobster'),'data','survey','marport.gps.rdata'))