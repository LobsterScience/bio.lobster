#maps
require(bio.lobster)
require(bio.utilities)
require(bio.polygons)

#Survey Maps
fp = file.path(project.datadirectory('bio.lobster'),"analysis","LFA34-38")

	LFAs<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolys.csv"))
	LFA34 = subset(LFAs,PID %in% c(34,35,36,38))
	attr(LFA34,'projection') <- 'LL'

LobsterMap('34-38',boundaries='LFAs',addSummerStrata=F,output='bio.lobster',fname = 'summerstratamap.pdf',save=F,labcex =0.8,labels=F)



	a = importShapefile(find.bio.gis('BTS_Strata'),readDBF=T) 
                          l = attributes(a)$PolyData[,c('PID','STRATA')]
                          a = merge(a,l,by='PID',all.x=T)
   # addPolys(a,border='red')    
    addPolys(subset(a,STRATA %in% c(3900,3920,1352,1351,1360,1330,1340,1310)),lty=1,border='green',col=adjustcolor('white',alpha.f=1))
#3900 and 3920 are in LFA 38



#areas within each LFA

#LFA34 total
	LFAs<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolys.csv"))
	LFA34 = subset(LFAs,PID %in% c(34))
	LFA34 = joinPolys(LFA34,operation='UNION')
	LFA34 = subset(LFA34,SID==1)
	attr(LFA34,'projection') <- 'LL'

a34 = calcArea(LFA34)$area
#20364 km2

#Summer strata
				a = importShapefile(find.bio.gis('MaritimesRegionEcosystem'),readDBF=T) 
                          l = attributes(a)$PolyData[,c('PID','StrataID')]
                          a = merge(a,l,by='PID',all.x=T)
  			  d = joinPolys(LFA34,a,'INT')
			  attr(d,'projection') <- "LL"
			  d = joinPolys(d,operation='UNION')
			  d = calcArea(d)
			  RVa = sum(d$area)
# 13316 / 20364 = 0.655
 	##strata by strata areas
 	 			a = importShapefile(find.bio.gis('MaritimesRegionEcosystem'),readDBF=T) 
                          l = attributes(a)$PolyData[,c('PID','StrataID')]
                          a = merge(a,l,by='PID',all.x=T)
  			  attr(a,'projection') <- "LL"
			  bb = calcArea(a)
			  bb = aggregate(area~PID,data=bb,FUN=sum)
			  bb = rename.df(bb,'area','totalarea')
			  d = joinPolys(LFA34,a,'INT')
			  attr(d,'projection') <- "LL"
			  dd = calcArea(d)
			  dd = aggregate(area~PID,data=dd, FUN=sum)
			  dd = merge(dd,bb,all.x=T)
			 dd = merge(dd,l,by='PID',all.x=T)
			  dd$prop = dd$area / dd$totalarea



#LFA35 total
	LFAs<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolys.csv"))
	LFA35 = subset(LFAs,PID %in% c(35))
	LFA35 = joinPolys(LFA35,operation='UNION')
	LFA35 = subset(LFA35,SID==1)
	attr(LFA35,'projection') <- 'LL'
	a35 = sum(calcArea(LFA35)$area)
	#5396 km2

#Summer strata
				a = importShapefile(find.bio.gis('MaritimesRegionEcosystem'),readDBF=T) 
                          l = attributes(a)$PolyData[,c('PID','StrataID')]
                          a = merge(a,l,by='PID',all.x=T)
  			  d = joinPolys(LFA35,a,'INT')
			  attr(d,'projection') <- "LL"
			  d = joinPolys(d,operation='UNION')
			  d = calcArea(d)
			  RVa = sum(d$area)
# 2304.03 / 5396 = 0.427
 	##strata by strata areas
 	 			a = importShapefile(find.bio.gis('MaritimesRegionEcosystem'),readDBF=T) 
                          l = attributes(a)$PolyData[,c('PID','StrataID')]
                          a = merge(a,l,by='PID',all.x=T)
  			  attr(a,'projection') <- "LL"
			  bb = calcArea(a)
			  bb = aggregate(area~PID,data=bb,FUN=sum)
			  bb = rename.df(bb,'area','totalarea')
			  d = joinPolys(LFA35,a,'INT')
			  attr(d,'projection') <- "LL"
			  dd = calcArea(d)
			  dd = aggregate(area~PID,data=dd, FUN=sum)
			  dd = merge(dd,bb,all.x=T)
			 dd = merge(dd,l,by='PID',all.x=T)
			  dd$prop = dd$area / dd$totalarea


#LFA36 total
	LFAs<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolys.csv"))
	LFA36 = subset(LFAs,PID %in% c(36))
	LFA36 = subset(LFA36,SID==1)
	attr(LFA36,'projection') <- 'LL'
	a36 = sum(calcArea(LFA36)$area)
	#4408/ km2

#Summer strata
				a = importShapefile(find.bio.gis('MaritimesRegionEcosystem'),readDBF=T) 
                          l = attributes(a)$PolyData[,c('PID','StrataID')]
                          a = merge(a,l,by='PID',all.x=T)
  			  d = joinPolys(LFA36,a,'INT')
			  attr(d,'projection') <- "LL"
			  d = joinPolys(d,operation='UNION')
			  d = calcArea(d)
			  RVa = sum(d$area)
# 3081.5 / 4408 = 0.699
 	##strata by strata areas
 	 			a = importShapefile(find.bio.gis('MaritimesRegionEcosystem'),readDBF=T) 
                          l = attributes(a)$PolyData[,c('PID','StrataID')]
                          a = merge(a,l,by='PID',all.x=T)
  			  attr(a,'projection') <- "LL"
			  bb = calcArea(a)
			  bb = aggregate(area~PID,data=bb,FUN=sum)
			  bb = rename.df(bb,'area','totalarea')
			  d = joinPolys(LFA36,a,'INT')
			  attr(d,'projection') <- "LL"
			  dd = calcArea(d)
			  dd = aggregate(area~PID,data=dd, FUN=sum)
			  dd = merge(dd,bb,all.x=T)
			 dd = merge(dd,l,by='PID',all.x=T)
			  dd$prop = dd$area / dd$totalarea


#LFA38 total
	LFAs<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolys.csv"))
	LFA38 = subset(LFAs,PID %in% c(38))
	LFA38 = subset(LFA38,SID==1)
	attr(LFA38,'projection') <- 'LL'
	a38 = sum(calcArea(LFA38)$area)
	#4277/ km2

#Summer strata
				a = importShapefile(find.bio.gis('MaritimesRegionEcosystem'),readDBF=T) 
                          l = attributes(a)$PolyData[,c('PID','StrataID')]
                          a = merge(a,l,by='PID',all.x=T)
  			  d = joinPolys(LFA38,a,'INT')
			  attr(d,'projection') <- "LL"
			  d = joinPolys(d,operation='UNION')
			  d = calcArea(d)
			  RVa = sum(d$area)
# 2962.77 / 4277 = 0.693
 	##strata by strata areas
 	 			a = importShapefile(find.bio.gis('MaritimesRegionEcosystem'),readDBF=T) 
                          l = attributes(a)$PolyData[,c('PID','StrataID')]
                          a = merge(a,l,by='PID',all.x=T)
  			  attr(a,'projection') <- "LL"
			  bb = calcArea(a)
			  bb = aggregate(area~PID,data=bb,FUN=sum)
			  bb = rename.df(bb,'area','totalarea')
			  d = joinPolys(LFA38,a,'INT')
			  attr(d,'projection') <- "LL"
			  dd = calcArea(d)
			  dd = aggregate(area~PID,data=dd, FUN=sum)
			  dd = merge(dd,bb,all.x=T)
			 dd = merge(dd,l,by='PID',all.x=T)
			  dd$prop = dd$area / dd$totalarea


#LFA36-38 total
	LFAs<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolys.csv"))
	LFA38 = subset(LFAs,PID %in% c(35,36,38))
	LFA38 = subset(LFA38,SID==1)
	LFA38 = joinPolys(LFA38,operation = "UNION")
	attr(LFA38,'projection') <- 'LL'
	a38 = sum(calcArea(LFA38)$area)
	#14089/ km2

#Summer strata
				a = importShapefile(find.bio.gis('MaritimesRegionEcosystem'),readDBF=T) 
                          l = attributes(a)$PolyData[,c('PID','StrataID')]
                          a = merge(a,l,by='PID',all.x=T)
  			  d = joinPolys(LFA38,a,'INT')
			  attr(d,'projection') <- "LL"
			  d = joinPolys(d,operation='UNION')
			  d = calcArea(d)
			  RVa = sum(d$area)
# 8350.751 / 4277 = 0.693
 	##strata by strata areas
 	 			a = importShapefile(find.bio.gis('MaritimesRegionEcosystem'),readDBF=T) 
                          l = attributes(a)$PolyData[,c('PID','StrataID')]
                          a = merge(a,l,by='PID',all.x=T)
  			  attr(a,'projection') <- "LL"
			  bb = calcArea(a)
			  bb = aggregate(area~PID,data=bb,FUN=sum)
			  bb = rename.df(bb,'area','totalarea')
			  d = joinPolys(LFA38,a,'INT')
			  attr(d,'projection') <- "LL"
			  dd = calcArea(d)
			  dd = aggregate(area~PID,data=dd, FUN=sum)
			  dd = merge(dd,bb,all.x=T)
			 dd = merge(dd,l,by='PID',all.x=T)
			  dd$prop = dd$area / dd$totalarea



#######################################################################################
#American surveys
		LFAs<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolys.csv"))
	LFA34 = subset(LFAs,PID %in% c(34))
	LFA34 = joinPolys(LFA34,operation='UNION')
	LFA34 = subset(LFA34,SID==1)
	attr(LFA34,'projection') <- 'LL'

	a34 = calcArea(LFA34)$area
#LFA34 total
#20364 km2

				a = importShapefile(find.bio.gis('BTS_Strata'),readDBF=T) 
                          l = attributes(a)$PolyData[,c('PID','STRATA')]
                          a = merge(a,l,by='PID',all.x=T)

			  d = joinPolys(LFA34,a,'INT')
			  attr(d,'projection') <- "LL"
			  d = joinPolys(d,operation='UNION')
			  d = calcArea(d)
			USa = sum(d$area)

# 13273 / 20364 = 0.652
 	##strata by strata areas
  			  attr(a,'projection') <- "LL"
			  bb = calcArea(a)
			  bb = aggregate(area~PID,data=bb,FUN=sum)
			  bb = rename.df(bb,'area','totalarea')
			  d = joinPolys(LFA34,a,'INT')
			  attr(d,'projection') <- "LL"
			  dd = calcArea(d)
			  dd = aggregate(area~PID,data=dd, FUN=sum)
			  dd = merge(dd,bb,all.x=T)
			 dd = merge(dd,l,by='PID',all.x=T)
			  dd$prop = dd$area / dd$totalarea



#LFA35 total -- no overlap


#LFA36 total - 56km2 of overlap, not worth including

#LFA38 total
	LFAs<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolys.csv"))
	LFA38 = subset(LFAs,PID %in% c(38))
	LFA38 = subset(LFA38,SID==1)
	attr(LFA38,'projection') <- 'LL'
	a38 = sum(calcArea(LFA38)$area)
	#4277/ km2

#Summer strata
			  d = joinPolys(LFA38,a,'INT')
			  attr(d,'projection') <- "LL"
			  d = joinPolys(d,operation='UNION')
			  d = calcArea(d)
			  RVa = sum(d$area)
# 4070 / 4277 = 0.9517
 	##strata by strata areas
 	 		  attr(a,'projection') <- "LL"
			  bb = calcArea(a)
			  bb = aggregate(area~PID,data=bb,FUN=sum)
			  bb = rename.df(bb,'area','totalarea')
			  d = joinPolys(LFA38,a,'INT')
			  attr(d,'projection') <- "LL"
			  dd = calcArea(d)
			  dd = aggregate(area~PID,data=dd, FUN=sum)
			  dd = merge(dd,bb,all.x=T)
			 dd = merge(dd,l,by='PID',all.x=T)
			  dd$prop = dd$area / dd$totalarea


#LFA36-38 total
	
#194119 / 32686 = 0.651 
		#since 2009
  # 
#######################################################################################
  ###Overlap map
require(PBSmapping)
require(bio.utilities)
require(bio.lobster)
require(bio.polygons)

LobsterMap(xlim=c(-69.5,-63.5),ylim=c(40.5,45),boundaries='LFAs',addGeorgesStrata=F,output='bio.lobster',fname='georgesmap41.pdf',save=F,labcex =0.8,labels=T)

  		LFA41 = read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","LFA41Offareas.csv"))
				LFA41 = joinPolys(as.PolySet(LFA41),operation='UNION')
				LFA41 = subset(LFA41,SID==1)
				attr(LFA41,'projection') <- 'LL'

#NEFSC
a = importShapefile(find.bio.gis('BTS_Strata'),readDBF=T) 
                          l = attributes(a)$PolyData[,c('PID','STRATA')]
                          a = merge(a,l,by='PID',all.x=T)
   # addPolys(a,border='red')                      
   	Ne = subset(a,STRATA %in% c(1160, 1170, 1180, 1190, 1200, 1210, 1220, 1290, 1300, 1340, 1360))
   	Ne = joinPolys(Ne,operation='UNION')
   	Ne = joinPolys(Ne,LFA41,operation='INT')
    addPolys(Ne,col= rgb(1,0,0,.15))


##DFO Summer
  			  a = find.bio.gis('summer_strata_labels',return.one.match=F)
			  a = read.csv(a,header=T)
			  names(a)[4] <- 'label'
			  b = find.bio.gis('strat.gf',return.one.match=F)
			  b = read.table(b)
			  names(b) <- c('X','Y','PID')
			  b = within(b,{POS <- ave(PID,list(PID),FUN=seq_along)})
			  d = joinPolys(LFA41,b,'INT')
			  d = joinPolys(d,operation='UNION')
			
			  d = subset(d,SID %in% c(1,4))
  			  d = d[c(-251,-252),]
			  d = within(d,{POS <- ave(SID,list(SID),FUN=seq_along)})
			  attr(d,'projection') <- "LL"
			addPolys(d,col= rgb(1,0,0,.15))			  
		#Georges
		  b = file.path(project.datadirectory('bio.polygons'),'data','Science','PED','GeorgesBankStrata.rdata')
			  load(b)
			  d = joinPolys(LFA41,out,'INT')
			  attr(d,'projection') <- "LL"
			  dd = joinPolys(d,operation='UNION')
		addPolys(dd,col= rgb(1,0,0,.15))

		addPolys(LFA41,border='blue')

		savePlot(file=file.path(project.figuredirectory('bio.lobster'),'AllSurveyCoverage.png'))



				