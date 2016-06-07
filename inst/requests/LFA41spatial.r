#AMC May 2016
RLibrary("PBSmapping")

loadfunctions(c('lobster','groundfish','BIOsurvey','utility'))


LFA41areasEXT<-read.csv(file.path( project.datadirectory("lobster"), "data","maps","LFA41Offareas_ext.csv"))
b = calcCentroid(LFA41areasEXT)
b$label = b$PID


a = lobster.db('lfa41.vms')
a = makePBS(a,polygon=F)

d = lobster.db('logs41')
d = makePBS(d,polygon=F)
d$FV_FISHED_DATETIME = as.Date(d$FV_FISHED_DATETIME)
d$YEAR = year(d$FV_FISHED_DATETIME)
d$MONTH = month(d$FV_FISHED_DATETIME)
d = completeFun(d,c('X','Y','EID'))


LobsterMap('41')
addPolys(LFA41areasEXT,border='blue')	
addLabels(b)

addPoints(subset(a,YEAR==2014),pch='.', col='green')
addPoints(subset(d,YEAR==2014),pch='.', col='red')

ud = unique(d$MONTH)

pdf('seasonal41.pdf')
for(i in ud) {
	g = subset(d,MONTH==i)
	uy = unique(g$YEAR)
	for(j in uy) {
		h = subset(g[!is.na(g[,c('X','Y','EID')]),],YEAR==j)
		if(nrow(h)>1) {
		LobsterMap('41')
		title(paste(i,j,sep="-"))
		addPolys(LFA41areasEXT,border='blue')
		addPoints(h,col='red',pch=16,cex=0.2)
		}
	}
}
dev.off()
L = gridData(d[,c('EID','X','Y','ADJCATCH')],FUN=sum, lvls=1:9)
E = gridData(d[,c('EID','X','Y','NUM_OF_TRAPS')],FUN=sum, lvls=1:9)

L$polyData$Z = L$polyData$Z / E$polyData$Z
L$polyData$Z = quantileBreak2NA(L$polyData$Z)
rm(E)

#need to make levels and finish the footprint		