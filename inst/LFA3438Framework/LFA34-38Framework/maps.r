#maps
require(bio.lobster)
require(bio.utilities)
require(bio.polygons)
la()
#Survey Maps
fp = file.path(project.datadirectory('bio.lobster'),"analysis","LFA34-38")
fpf = file.path(project.figuredirectory('bio.lobster'),'LFA3438Framework2019')

	LFAs<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolys.csv"))
	LFA34 = subset(LFAs,PID %in% c(34,35,36,38))
	attr(LFA34,'projection') <- 'LL'

LobsterMap('34-38',boundaries='LFAs',addSummerStrata=F,output='bio.lobster',fname = 'summerstratamap.pdf',save=F,labcex =0.8,labels=T)
savePlot(file.path(fpf,'LFAMap34-38.png'),)

LobsterMap('34-38',boundaries='LFAs',addSummerStrata=T,output='bio.lobster',fname = 'summerstratamap.pdf',save=F,labcex =0.8,labels=T,addGrids=F)
savePlot(file.path(fpf,'LFAMap34-38DFOSummerSurvey.png'),)

LobsterMap('34-38',boundaries='LFAs',addSummerStrata=F,output='bio.lobster',fname = 'summerstratamap.pdf',save=F,labcex =0.8,labels=T,addAmericanStrata=T,addGrids=F)
savePlot(file.path(fpf,'LFAMap34-38AmericanSurvey.png'),)

##
##Areas by maps
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
  lobster.db( DS="fsrs", p=p)		# FSRS recruitment traps
      
  LobsterMap('34-38')

  fsrs$EID = 1:nrow(fsrs)

  fsrs = subset(fsrs,HAUL_YEAR>2014 & LFA >33)

  fsrs$X  = fsrs$LONG_DD
  fsrs$Y = fsrs$LAT_DD

  addPoints(fsrs,pch=16,col='red',cex=0.5)

  savePlot(file.path(fpf,'LFAMap34-38FSRSgr2014.png'),)



##ff mapos

require(bio.lobster)
require(bio.utilities)
require(PBSmapping)

p = bio.lobster::load.environment()


    p$syr = 2005
    p$yrs = p$syr:2018

    figdir = fpf

     p$lfas = c("34", "35", "36", "38") # specify lfas for data summary
  
    logsInSeason<-lobster.db('process.logs')
catchLevels = c(0,100000,200000,300000,400000,500000,600000,700000,800000)
	yrs = c(2003:2018)
	outs = list()
	for(i in 1:length(yrs)){
		catchgrids = lobGridPlot(subset(logsInSeason,LFA%in%p$lfas&SYEAR==yrs[i],c("LFA","GRID_NUM","TOTAL_WEIGHT_KG")),FUN=sum,lvls=catchLevels)
	a = catchgrids$grid
	b = catchgrids$pdata
	a = calcCentroid(a)
	d = merge(a,b)
	outs[[i]] = with(subset(d,PID==34),{
		Xc = sum(X*Z)/sum(Z)
		Yc = sum(Y*Z)/sum(Z)
		return(c(Xc,Yc))
	})
	#	saveRDS(catchgrids,file=file.path(fd,paste('Figure3',yrs[i],'.rds')))
		pdf(file.path(figdir,paste0("FisheryFootprintLandings",yrs[i],".pdf")))
		LobsterMap('34-38',poly.lst=catchgrids)
	  	title(yrs[i],line=-3,cex.main=2,adj=0.3)
	    SpatialHub::contLegend('bottomright',lvls=catchgrids$lvls/1000,Cont.data=catchgrids,title="Catch (tons)",inset=0.02,cex=0.8,bg='white')
	    dev.off()
	}

##fishery moving ? weighted centroids of landings
xx = as.data.frame(do.call(rbind,outs))
names(xx) = c('X','Y')
xx$POS=1:nrow(xx)
xx$PID = 1

LobsterMap('34')
addLines(xx,col='red')