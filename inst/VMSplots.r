#VMS_plots


logs41   = lobster.db('logs41')
vms.data = lobster.db('lfa41.vms')
logs41$YEAR = year(logs41$FV_FISHED_DATETIME)

  vms.data = makePBS(vms.data,polygon=F)
  logs41 = makePBS(logs41,polygon=F)

  logs41 = logs41[-which(is.na(logs41[,c('X','Y','EID')])),]

yr = unique(logs41$YEAR)
yrv = unique(vms.data$YEAR)
yr = intersect(yr,yrv)

pdf(file=file.path(project.figuredirectory('lobster'),'lfa41LogsVMS.pdf'))
for(y in yr) {

LobsterMap('41')
title(y)
addPoints(subset(vms.data,YEAR==y),pch='.',col='red')
addPoints(subset(logs41,YEAR==y),pch='.',col='green')
}
dev.off()