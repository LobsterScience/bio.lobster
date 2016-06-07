loadfunctions(c('lobster','utility','netmensuration'))
options(stringsAsFactors=F)
fpath = file.path(project.datadirectory('lobster'),'data','survey','2015')

#marpath = file.path(fpath,'marport')
#tpath = file.path(fpath,'temperature')

fm = dir(fpath,full.names=T)

gps = list()
sensors = list()
m = 0
for(i in fm) {
	print(i)
	m = m+1
	file = i
	out = LobsterMarport(file=file)
 	gps[[m]] = out[[2]]
 	sensors[[m]] =  out[[1]]
 	
}

gps = do.call(rbind,gps)
sensors = do.call(rbind,sensors)
save(sensors, file=file.path(project.datadirectory('lobster'),'data','survey','marport.rdata'))
save(gps, file=file.path(project.datadirectory('lobster'),'data','survey','marport.gps.rdata'))