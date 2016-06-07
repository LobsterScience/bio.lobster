collectorTemperatureData <- function(base.dir = file.path(ecomod.datadirectory,'lobster')) {
			#//combine CTS temperature data

			a = file.path(base.dir,'data','temperature','LobsterCollector')
			g = dir(a)
			g = g[grep('csv',g)]

			k=NULL
			for(i in g) {
			h = read.csv(file.path(a,i),header=T)
			names(h)[3] = 'Site'
			k = rbind(k,h)
			}
			return(k)
		#save(k,file=file.path(a,'combinedTemperatureData.Rdata'))
		print('saved')
}