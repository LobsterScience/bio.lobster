#find area of us strata in canadian waters
require(bio.lobster)
require(bio.polygons)
require(bio.utilities)
require(PBSmapping)

a = importShapefile(find.bio.gis('BTS_Strata'),readDBF=T) 
l = attributes(a)$PolyData[,c('PID','STRATA')]
a = merge(a,l,by='PID',all.x=T)
a = a[which(a$STRATA<2000),]
attr(a,'projection') <- "LL"


b = read.table(find.bio.gis('limit200'))
b = b[-nrow(b),]
names(b) = c('X','Y')
b = makePBS(b)

LobsterMap('41')
addPolys(b,border='blue')

f = joinPolys(a,b,'INT') #canada only
attr(f,'projection') <- 'LL'
g = calcArea(f)
g = aggregate(area~PID,data=g,FUN=sum)
h = calcArea(a)
h = aggregate(area~PID,data=h,FUN=sum)
j = merge(g,h,by=c('PID'))
j = merge(j,l,by='PID')
j$prop = j$area.x/j$area.y #Proportion Canadian Strata


#LFA 41 only

	LFA41 = read.csv(file.path( project.datadirectory("lobster"), "data","maps","LFA41Offareas.csv"))
	LFA41 = joinPolys(as.PolySet(LFA41),operation='UNION')
	attr(LFA41,'projection') <- 'LL'

	k = joinPolys(a,LFA41,'INT')
	attr(k,'projection') <- 'LL'

	g = calcArea(k)
	g = aggregate(area~PID,data=g,FUN=sum)
	h = calcArea(a)
	h = aggregate(area~PID,data=h,FUN=sum)
	j = merge(g,h,by=c('PID'))
	j = merge(j,l,by='PID')
	j$prop = j$area.x/j$area.y #Proportion LFA41

	


	h = 