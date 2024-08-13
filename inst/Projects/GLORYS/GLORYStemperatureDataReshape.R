
require(bio.lobster)
require(satin)
require(tidyr)
require(PBSmapping)
setwd('~/tmp/GLORYS')

y1 = read.cmems('GLORYS2020-01-01.nc')
a = y1$bottomT
image(a@lon, a@lat, t(a@data[,,1,]))

#LFA 36
fil = dir()
fil = fil[grep('GLO',fil)]
	LFAs<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolys.csv"))
	L = subset(LFAs, PID %in% 34:38)


for(i in 1:length(fil)){
		g = glorysSubset(glorysfile=fil[i], polygon=L)
		saveRDS(g,file = file.path('SummaryFiles',paste(fil[i],'LFA3438.rds',sep="_")))
	}


#glorys reshape to r Object

for(i in 1:length(fil)){
		g = glorysReshape(glorysfile=fil[i])
		saveRDS(g,file = file.path('SummaryFiles',paste(fil[i],'ShelfBoF.rds',sep="_")))
		print(fil[i])
	}

