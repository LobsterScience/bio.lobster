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

	LFA41 = read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","LFA41Offareas.csv"))
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

	
#New offshore area defns

l41 = read.csv(file.path(project.datadirectory('bio.lobster'),'data','maps','LFA41Offareas.csv'))
h = calcArea(a)
h = aggregate(area~PID,data=h,FUN=sum)

#CrowellBasin
lo = joinPolys(a,subset(l41,PID==1),'INT')
la = calcArea(lo)
la = merge(la,l,by='PID')
le = merge(la,h,by='PID',all.x=T)
le$prop = le$area.x/le$area.y

#GeorgesBank
lo = joinPolys(a,subset(l41,PID==2),'INT')
la = calcArea(lo)
la = aggregate(area~PID,data=la,FUN=sum)
la = merge(la,l,by='PID')
le = merge(la,h,by='PID',all.x=T)
le$prop = le$area.x/le$area.y


#GeorgesBasin
lo = joinPolys(a,subset(l41,PID==3),'INT')
la = calcArea(lo)
la = aggregate(area~PID,data=la,FUN=sum)
la = merge(la,l,by='PID')
le = merge(la,h,by='PID',all.x=T)
le$prop = le$area.x/le$area.y


#SEBROWNS
lo = joinPolys(a,subset(l41,PID==4),'INT')
la = calcArea(lo)
la = aggregate(area~PID,data=la,FUN=sum)
la = merge(la,l,by='PID')
le = merge(la,h,by='PID',all.x=T)
le$prop = le$area.x/le$area.y


#SWBROWNS
lo = joinPolys(a,subset(l41,PID==5),'INT')
la = calcArea(lo)
la = aggregate(area~PID,data=la,FUN=sum)
la = merge(la,l,by='PID')
le = merge(la,h,by='PID',all.x=T)
le$prop = le$area.x/le$area.y


#DFO surveys


l41 = read.csv(file.path(project.datadirectory('bio.lobster'),'data','maps','LFA41Offareas.csv'))


			  a = find.bio.gis('strat.gf',return.one.match=F)
			  a = read.table(a)
			  names(a) <- c('X','Y','PID')
			  a = within(a,{POS <- ave(PID,list(PID),FUN=seq_along)})
			  attr(a,'projection') <- "LL"
				h = calcArea(a)
			h = aggregate(area~PID,data=h,FUN=sum)
			h$area = abs(h$area)
			h$area[which(h$PID==481)] = h$area[which(h$PID==481)] - h$area[which(h$PID==480)]


#CrowellBasin
lo = joinPolys(a,subset(l41,PID==1),'INT')
la = calcArea(lo)
la = aggregate(area~PID,data=la,FUN=sum)

le = merge(la,h,by='PID',all.x=T)
le$prop = le$area.x/le$area.y

#GeorgesBank
lo = joinPolys(a,subset(l41,PID==2),'INT')
la = calcArea(lo)
la = aggregate(area~PID,data=la,FUN=sum)
le = merge(la,h,by='PID',all.x=T)
le$prop = le$area.x/le$area.y




#SEBROWNS
lo = joinPolys(a,subset(l41,PID==4),'INT')
la = calcArea(lo)
la = aggregate(area~PID,data=la,FUN=sum)
le = merge(la,h,by='PID',all.x=T)
le$prop = le$area.x/le$area.y


#SWBROWNS
lo = joinPolys(a,subset(l41,PID==5),'INT')
la = calcArea(lo)
la = aggregate(area~PID,data=la,FUN=sum)
le = merge(la,h,by='PID',all.x=T)
le$prop = le$area.x/le$area.y


#DFO geoges survey
x = dir('/mnt/R_PED/Shared/Spatial/Science/Strata/ped_groundfish_archive/georges',full.names=T)
out = NULL
for(i in 1:length(x)) {
	w = read.table(x[i],sep=",")
	names(w) = c('Y','X')
	w$POS = 1:nrow(w)
	w$PID=i
	w$STRATA = strsplit(strsplit(x[i],"/")[[1]][10],"\\.")[[1]][1]
	out = rbind(out,w)
}
 
save(out, file= file.path(project.datadirectory('bio.lobster'),'data','maps','GeorgesBankStrata.rdata'))

			a = out
		  attr(a,'projection') <- "LL"
				h = calcArea(a)

 l = as.data.frame(unique(cbind(a$PID,a$STRATA))	)
 names(l) = c('PID','STRATA')

#GeorgesBank
lo = joinPolys(a,subset(l41,PID==2),'INT')
la = calcArea(lo)
la = aggregate(area~PID,data=la,FUN=sum)
la = merge(la,l,by='PID')
le = merge(la,h,by='PID',all.x=T)
le$prop = le$area.x/le$area.y


#GeorgesBasin
lo = joinPolys(a,subset(l41,PID==3),'INT')
la = calcArea(lo)
la = aggregate(area~PID,data=la,FUN=sum)
la = merge(la,l,by='PID')
le = merge(la,h,by='PID',all.x=T)
le$prop = le$area.x/le$area.y


