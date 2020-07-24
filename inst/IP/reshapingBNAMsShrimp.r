require(bio.lobster)
la()
require(lubridate)
require(bio.utilities)
require(PBSmapping)


anc = bnamR(redo=F)
sh = importShapefile(file.path(project.datadirectory('bio.lobster'),'data','maps','SFA Polygons','SFAsPolygon'))
attr(sh,'projection') <- 'LL'
plotPolys(sh)
ssh = joinPolys(sh,operation='UNION')
##maps of sfa areas
	
hg = findPolys(anc$locsP,ssh)
loc = subset(anc$locsP,EID %in% hg$EID)
aTemp = anc$bTs[which(anc$bTs[,1] %in% loc$EID),]
aTemp=aTemp[,-1]
plot(anc$timeS ,apply(aTemp,2,mean),type='l')
require(forecast)