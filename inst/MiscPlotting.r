

### season timing

pdf(file.path( project.datadirectory("lobster"), "R","SPA6SurveyTiming.pdf"),8,11)
par(mfrow=c(5,2),mar=c(0,0,0,0))
for(i in 2005:2014){
	fishing.season(subset(lobdat,YEAR==i,c('TOW_DATE','NLobsStd')),smooth=0.05,title='')
}
mtext("Relative catch of lobsters in SPA 6 scallop survey",3,-2,cex=1.2,outer=T)	

dev.off()

### poster

# Logs

loadfunctions('lobster')
logsInSeason<-read.csv(file.path( project.datadirectory("lobster"), "data","logsInSeason.csv"))
logsInSeason$WEIGHT_BUMP<-logsInSeason$WEIGHT_KG*logsInSeason$BUMPUP

catchgrids<-lobGridPlot(subset(logsInSeason,SYEAR==2014,c("LFA","GRID_NUM","WEIGHT_BUMP")),lvls=c(100,50000,100000,200000,400000,600000,800000,1000000),FUN=sum,border=NA)
	
pdf(file.path( project.datadirectory("lobster"), "R","SpatialLandings2014.pdf"),11,9)

LobsterMap(poly.lst=catchgrids[1:2],title="2014 Lobster Catch LFA 27-38")
ContLegend("bottomright",lvls=catchgrids$lvls/1000,Cont.data=catchgrids,title="Catch (t)",inset=0.02,cex=0.8,bg='white')

dev.off()
lfas<-c("27", "28", "29", "30", "31A", "31B", "32", "33", "34", "35", "36", "38")
#daily.dat<-CPUEplot(logsInSeason,lfa=lfas,yrs=2001:2014)

pdf(file.path( project.datadirectory("lobster"), "R","CPUE.pdf"),9,11)
daily.dat<-CPUEplot(logsInSeason,lfa=lfas[1:6],yrs=2001:2014)


daily.dat<-CPUEplot(logsInSeason,lfa=lfas[7:12],yrs=2001:2014)
dev.off()

# Survey

	bins=seq(0,200,5)
	Yrs=2005:2015

	surveyLobsters34<-LobsterSurveyProcess(lfa="34",yrs=1996:2015,mths=c("Jul","Jun"),bin.size=diff(bins)[1])

	## Plot Survey Index
	plotSurveyIndex(surveyLobsters34,moving.avg=F,ref.points=F,fn="Abundance")

		
	## Plot Length Frequency
	LobsterSurveyCLF<-t(sapply(Yrs,function(y){colMeans(subset(surveyLobsters34,YEAR==y,paste0("CL",bins[-length(bins)])),na.rm=T)}))
	BarPlotCLF(list(LobsterSurveyCLF),yrs=2014,CLFyrs=Yrs,bins=bins,col='grey',filen="SizeStructure",rel=F,ymax=19,wd=8,ht=6)

	## Plot Distribution

		# interpolate abundance
		interp.data<-na.omit(subset(surveyLobsters34,YEAR==2015,c('SET_ID','SET_LONG','SET_LAT','NUM_STANDARDIZED')))
		lob.contours<-interpolation(interp.data,ticks='define',place=3,nstrata=5,str.min=0,interp.method='gstat',blank=T,res=0.005,smooth=F,idp=3.5,blank.dist=0.2)

		# define contour lines
		print(lob.contours$str.def)
		lvls=c(1, 5, 10, 20, 50, 100, 200, 500)

		# generate contour lines
		LFAs<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolys.csv"))

		cont.lst<-contourGen(lob.contours$image.dat,lvls,subset(LFAs,PID==34),col="YlGn",colorAdj=1)

		# plot Map
		#pdf(file.path( project.datadirectory("lobster"), "R","Distribution.pdf"),8,11)
		png(file.path( project.datadirectory("lobster"), "R","Distribution.png"),800,1100)
		LobsterMap(ylim=c(42.8,44.6), xlim=c(-67.15,-65.2),mapRes="UR",contours=cont.lst,title="LFA 34 Lobster Density",isobath=seq(50,500,50),bathcol=rgb(0,0,1,0.2),bathy.source='bathy')
		points(SET_LAT~SET_LONG,surveyLobsters34,subset=YEAR==2015,pch=21,cex=0.5,bg='red')#,col=rgb(0,0,0,0.5))
		ContLegend("topright",lvls=lvls,Cont.data=cont.lst$Cont.data,title="#/standard tow",inset=0.02,cex=0.8,bg='white')
		dev.off()

# 
	
	surveyLobsters34<-LobsterSurveyProcess(lfa="34",yrs=1996:2016,mths=c("Jul","Jun"),LFS=120)


		interp.data<-na.omit(subset(surveyLobsters34,select=c('SET_ID','SET_LONG','SET_LAT','N_LARGE_FEMALES')))
		lob.contours<-interpolation(interp.data,ticks='define',place=3,nstrata=5,str.min=0,interp.method='gstat',blank=T,res=0.005,smooth=T,idp=3.5,blank.dist=0.2,no.data='NA')

		lvls=c(0.1, 0.2, 0.5, 1, 1.5, 2, 2.5, 3)

		# generate contour lines
		LFAs<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolys.csv"))

		cont.lst<-contourGen(lob.contours$image.dat,lvls,subset(LFAs,PID==34),col="YlGn",colorAdj=1)

		pdf(file.path( project.datadirectory("bio.lobster"), "figures","LargeFemalesDist.pdf"))
		LobsterMap(ylim=c(42.8,44.6), xlim=c(-67.15,-65.2),mapRes="UR",contours=cont.lst,title="LFA 34 Females >120mm",isobath=seq(50,500,50),bathcol=rgb(0,0,1,0.2),bathy.source='bathy')
		points(SET_LAT~SET_LONG,interp.data,pch=21,cex=0.5,bg='red')#,col=rgb(0,0,0,0.5))
		ContLegend("topright",lvls=lvls,Cont.data=cont.lst$Cont.data,title="#/standard tow",inset=0.02,cex=0.8,bg='white')
		dev.off()


subset(surveyLobsters34,SET_ID%in% subset(interp.data,N_LARGE_FEMALES>0)$SET_ID)


 SET_LAT  SET_LONG SET_DEPTH            SET_DATE 
42.77417 -66.56333     140.0 2006-07-04 01:00:00 
43.31550 -66.56150      97.0 2009-07-10 01:00:00 
42.90250 -66.56233     101.0 2011-07-07 01:00:00 
43.73600 -66.20100      23.0 2011-07-10 01:00:00 
44.63700 -66.30750      72.0 2012-07-09 01:00:00 
44.33983 -66.18533      20.0 2012-07-09 01:00:00 
43.32233 -66.67033      58.0 2012-07-06 01:00:00 
43.42450 -66.07433      20.0 2012-07-09 01:00:00 
42.90367 -66.56117     101.0 2012-07-11 01:00:00 
44.25833 -66.22617      30.0 2013-07-03 01:00:00 
44.19000 -66.32850      56.0 2014-07-09 01:00:00 
43.79433 -66.14567       7.0 2014-07-10 01:00:00 
43.37433 -66.34817      60.0 2014-07-11 01:00:00 
43.16450 -66.14250      96.6 2016-07-05 01:00:00 
43.12667 -66.38950     102.0 2016-07-05 01:00:00 
