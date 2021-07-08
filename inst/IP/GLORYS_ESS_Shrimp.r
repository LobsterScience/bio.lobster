#Pruning GLORYS to ESS Shrimp

require(bio.lobster)
require(satin)
require(tidyr)
require(PBSmapping)
setwd(file.path(project.datadirectory('bio.lobster'),'data','GLORYS'))

y1 = read.cmems('GLORYS1993')
a = y1$bottomT
image(a@lon, a@lat, t(a@data[,,1,]))

po = data.frame(X=c(-62,-62,-57,-57),Y=c(44,46,46,44),PID=1,POS=c(1,2,3,4))


fil = dir()
fil = fil[grep('GLO',fil)]

for(i in 1:length(fil)){
		g = glorysSubset(glorysfile=fil[i], polygon=po)
		saveRDS(g,file = file.path('SummaryFiles',paste(fil[i],'ESSShrimp.rds',sep="_")))
	}




