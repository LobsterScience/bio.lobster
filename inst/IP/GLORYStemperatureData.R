
require(bio.lobster)
require(satin)
require(tidyr)
require(PBSmapping)
setwd(file.path(project.datadirectory('bio.lobster'),'data','GLORYS'))

y1 = read.cmems('GLORYS1993')
a = y1$bottomT
image(a@lon, a@lat, t(a@data[,,1,]))

load(file='/SpinDr/backup/bio_data/bio.lobster/data/maps/LFA27-33100mIsobath.rdata') #Isob100 the 100 m isobath for 27-33


fil = dir()
fil = fil[grep('GLO',fil)]

for(i in 1:length(fil)){
		g = glorysSubset(glorysfile=fil[i], polygon=Isob100)
		saveRDS(g,file = file.path('SummaryFiles',paste(fil[i],'Isobath100.rds',sep="_")))
	}



#LFA 36
fil = dir()
fil = fil[grep('GLO',fil)]

	LFAs<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolys.csv"))
	L = subset(LFAs, PID==36)


for(i in 1:length(fil)){
		g = glorysSubset(glorysfile=fil[i], polygon=L)
		saveRDS(g,file = file.path('SummaryFiles',paste(fil[i],'LFA36.rds',sep="_")))
	}
