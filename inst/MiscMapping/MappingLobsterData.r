
require("bio.lobster")

### LOGS ###

#loadfunctions('bio.lobster')
#cpuegrids<-lobGridPlot(subset(logsInSeason,LFA=='34'&SYEAR==2014,c("LFA","GRID_NUM","CPUE")),FUN=mean)
#LobsterMap('34',poly.lst=cpuegrids)


####### 2014 CATCH with survey location LFA 34
lobster.db('process.logs.redo')
logsInSeason=lobster.db('process.logs')


logsInSeason$CPUE = logsInSeason$WEIGHT_KG / logsInSeason$NUM_OF_TRAPS
logsInSeason$ADJ_WEIGHT = logsInSeason$WEIGHT_KG * logsInSeason$BUMPUP

#catchgrids<-lobGridPlot(subset(logsInSeason,LFA=='34'&SYEAR==2014,c("LFA","GRID_NUM","WEIGHT_KG")),lvls=c(1000,50000,100000,200000,400000,600000,800000,1000000),FUN=sum,border=NA)

#catchgrids<-lobGridPlot(subset(logsInSeason,LFA %in% c('34')&SYEAR %in% c(2005:2015) & CPUE<10 & WOS %in% c(19:23),c("LFA","GRID_NUM","CPUE")),lvls = c(0.1,0.25,0.5,0.75,1,1.5,2.5,3.8),FUN=mean,border=NA)

#catch rate by year within LFA 33 and LFA 34

logsInSeason$mns = months(logsInSeason$DATE_FISHED)


catchgrids<-lobGridPlot(a[,c("LFA","GRID_NUM","CPUE")],FUN=length)

catchgrids<-lobGridPlot(subset(logsInSeason,LFA %in% c('34')&SYEAR %in% c(2005:2015) & CPUE<10,c("LFA","GRID_NUM","CPUE")),lvls = c(0.1,0.25,0.5,0.75,1,1.5,2.5,3.8),FUN=mean,border=NA)
catchgrids<-lobGridPlot(subset(logsInSeason,LFA %in% c('34')&SYEAR %in% 2016 & CPUE<10,c("LFA","GRID_NUM","CPUE")),lvls = c(0.1,0.25,0.5,0.75,1,1.5,2.5,3.8),FUN=mean,border=NA)

pdf(file.path( project.datadirectory("lobster"), "R","LFA34.pdf"),8,11)

#LobsterMap('SWN',poly.lst=catchgrids[1:2],title="2013 April Lobster Catch")
LobsterMap('34',poly.lst=catchgrids[1:2],title="2005 - 2015 Lobster CPUE")

ContLegend("bottomleft",lvls=catchgrids$lvls,Cont.data=catchgrids,title="CPUE (kg/TH)",inset=0.02,cex=0.8,bg='white')
ss2015<-read.csv(file.path( project.datadirectory("lobster"), "data","LFA34TrawlStations2015.csv"))
with(subset(ss2015,TYPE%in%c('index','2014_index')),points(DDLON,DDLAT,pch=16,col='red'))
with(subset(ss2015,TYPE%in%c('2014','2014_index')),points(DDLON,DDLAT))
legend('topleft',c('index','2014'),col=c('red','black'),pch=c(16,1),inset=0.02,cex=0.8,bg='white')

dev.off()

############# Grid Catch 

catchgrids <-lobGridPlot(subset(logsInSeason,SYEAR==2014,c("LFA","GRID_NUM","WEIGHT_KG")),lvls=c(0,65720,198753,328007,564229),FUN=sum,border=NA,bcol="RdYlGn",rev=T)
	
pdf(file.path( project.datadirectory("bio.lobster"), "figures","2014GridLandings.pdf"),10,8)

LobsterMap('all',poly.lst=catchgrids[1:2],title="2014 Lobster Catch")
ContLegend("bottomright",lvls=catchgrids$lvls,Cont.data=catchgrids,title="Catch (kg)",inset=0.02,cex=0.8,bg='white')

dev.off()

############# Grid Catch Adjusted

catchadjgrids <-lobGridPlot(subset(logsInSeason,SYEAR==2014,c("LFA","GRID_NUM","ADJ_WEIGHT")),lvls=c(0,65720,198753,328007,564229),FUN=sum,border=NA,bcol="RdYlGn",rev=T)
	
pdf(file.path( project.datadirectory("bio.lobster"), "figures","2014GridLandingsAdj.pdf"),10,8)

LobsterMap('all',poly.lst=catchgrids[1:2],title="2014 Lobster Catch")
ContLegend("bottomright",lvls=catchgrids$lvls,Cont.data=catchadjgrids,title="Catch (kg)",inset=0.02,cex=0.8,bg='white')

dev.off()


############# Grid Catch lbs

catchlbsgrids <-lobGridPlot(subset(logsInSeason,SYEAR==2014,c("LFA","GRID_NUM","WEIGHT_LBS")),lvls=c(0,65720,198753,328007,564229),FUN=sum,border=NA,bcol="RdYlGn",rev=T)
	
pdf(file.path( project.datadirectory("bio.lobster"), "figures","2014GridLandingsLbs.pdf"),10,8)

LobsterMap('all',poly.lst=catchgrids[1:2],title="2014 Lobster Catch")
ContLegend("bottomright",lvls=catchgrids$lvls,Cont.data=catchlbsgrids,title="Catch (lbs)",inset=0.02,cex=0.8,bg='white')

dev.off()


############# Grid Effort

effortgrids <-lobGridPlot(subset(logsInSeason,SYEAR==2014,c("LFA","GRID_NUM","NUM_OF_TRAPS")),lvls=c(0,15400,49461,129458,247331),FUN=sum,border=NA,bcol="RdYlGn",rev=T)
	
pdf(file.path( project.datadirectory("bio.lobster"), "figures","2014GridEffort.pdf"),10,8)

LobsterMap('all',poly.lst=effortgrids[1:2],title="2014 Lobster Effort")
ContLegend("bottomright",lvls=effortgrids$lvls,Cont.data=effortgrids,title="Trap Hauls",inset=0.02,cex=0.8,bg='white')

dev.off()


############# Grid CPUE

cpuegrids <-lobGridPlot(subset(logsInSeason,SYEAR==2014,c("LFA","GRID_NUM","CPUE")),lvls=c(0.2,1.6,2.5,3.8,4.8),FUN=sum,border=NA,bcol="RdYlGn",rev=T)
	
pdf(file.path( project.datadirectory("bio.lobster"), "figures","2007GridCPUE.pdf"),10,8)

LobsterMap('all',poly.lst=catchgrids[1:2],title="2014 Lobster CPUE")
ContLegend("bottomright",lvls=catchgrids$lvls,Cont.data=catchgrids,title="kg/TH",inset=0.02,cex=0.8,bg='white')

dev.off()



griddata2014=data.frame(LFA=catchgrids[[2]]$PID,GRID=catchgrids[[2]]$SID,CATCH=catchgrids[[2]]$Z,ADJCATCH=catchadjgrids[[2]]$Z,EFFORT=effortgrids[[2]]$Z)
save( griddata2014, file=file.path( project.datadirectory("bio.lobster"), "gridsummary2014.rdata"), compress=T)
############## LFA grid effort


loadfunctions('bio.lobster')
logsInSeason<-read.csv(file.path( project.datadirectory("lobster"), "data","logsInSeason.csv"))
yy = unique(logsInSeason$SYEAR)


fp = file.path( project.datadirectory("bio.lobster"), "figures")
dir.create(fp, recursive =T, showWarnings =F)
for(y in yy) {
effortgrids <-lobGridPlot(subset(logsInSeason,SYEAR==y,c("LFA","GRID_NUM","NUM_OF_TRAPS")),lvls=c(100,50000,100000,200000,400000,600000,800000,1000000),FUN=sum,border=NA)
pdf(file=file.path(fp,paste("GridLandings",y,"pdf",sep="."),11,8)
LobsterMap(poly.lst=effortgrids[1:2],title=paste(y,"Lobster Catch"))
ContLegend("bottomright",lvls=effortgrids$lvls/1000,Cont.data=effortgrids,title="Catch (t)",inset=0.02,cex=0.8,bg='white')

dev.off()
}

############# LFA 41




grids<-read.csv(file.path( project.datadirectory("lobster"), "data","maps","GridPolys.csv"))


##### From Scallop Survey

# get lobster data from scallop survey
lobdat<-LAFSS(SPA=c("6A","6B","6C"))

#
ScallopAreas<-read.csv(file.path( project.datadirectory("lobster"), "data","maps","NewAreaDefsforISAREADEFS2013.csv"))


pdf(file.path( project.datadirectory("lobster"), "R","SPA6LobsterDensity.pdf"),8,11)

for(i in 2005:2014){
	
	# interpolate abundance
	lob.contours<-interpolation(subset(lobdat,YEAR==i,c('TOW_SEQ','lon','lat','NLobsStd')),ticks='define',place=3,nstrata=5,str.min=0,interp.method='gstat',blank=T,res=0.005,smooth=F,idp=3.5,blank.dist=0.03)

	# define contour lines
	print(lob.contours$str.def)
	lvls=c(1, 2, 5, 10, 20, 50)

	# generate contour lines
	cont.lst<-contour.gen(lob.contours$image.dat,lvls,col="YlGn",colorAdj=1)

	# plot Map
	LobsterMap(ylim=c(44.4,45.2),xlim=c(-67.2,-66.3),mapRes="UR",contours=cont.lst,title=paste("SPA 6 Lobster Density",i),isobath=seq(10,500,10),bathcol=rgb(0,0,1,0.2),bathy.source='bathy',boundaries='scallop',poly.lst=list(ScallopAreas,data.frame(PID=c(16,18))))
	points(lat~lon,lobdat,subset=YEAR==i,pch=16,cex=0.5)#,col=rgb(0,0,0,0.5))
	ContLegend("bottomright",lvls=lvls,Cont.data=cont.lst$Cont.data,title="#/standard tow",inset=0.02,cex=0.8,bty='n')
}
dev.off()


	LobsterMap(ylim=c(44.4,45.2),xlim=c(-67.2,-66.3),mapRes="UR",isobath=seq(10,500,10),bathcol=rgb(0,0,1,0.2),bathy.source='bathy',boundaries='scallop',poly.lst=list(ScallopAreas,data.frame(PID=c(9,16,18))))




		LFAgrid<-read.csv(file.path( project.datadirectory("lobster"), "data","maps","LFAgridPolys.csv"))

		joined<-joinPolys(LFAgrid,operation="UNION")

ScallopAreas<-read.csv(file.path( project.datadirectory("lobster"), "data","maps","NewAreaDefsforISAREADEFS2013.csv"))
pdf(file.path( project.datadirectory("lobster"), "R","LobsterSurveyLFA36.pdf"),11,8)
	LobsterMap(ylim=c(44.8,45.7), xlim=c(-66.8,-64.5),mapRes="UR",isobaths=c(25,40,70,100),bathcol=rev(brewer.pal(5,'Blues')[-1]),bathy.source='bathy',boundaries='scallop',poly.lst=list(ScallopAreas,data.frame(PID=c(16,18))),plot.rivers=F)

dev.off()

		
		LobsterMap()
		LFAlines<-read.csv(file.path(project.datadirectory('lobster'),'data','maps','LFA_Lines.csv'))
		addLines(LFAlines,col='red')



############### FSRS


FSRSvesday.dat$lat<-convert.dd.dddd(FSRSvesday.dat$LATITUDE)
FSRSvesday.dat$lon<-convert.dd.dddd(FSRSvesday.dat$LONGITUDE)

LobsterMap()
points(lat~lon,FSRSvesday.dat,pch=16,col=rgb(0,0,0,0.1))


################ Licences by port


	x<-read.csv(file.path(project.datadirectory('lobster'),'data','LFA33LicencesByPortDATA.csv'))

	yrs=2002:2015
	
	pdf(file.path( project.datadirectory("lobster"), "R","LFA33LicencesByPort.pdf"),11,8)

	for(i in 1:length(yrs)){

	LobsterMap(ylim=c(42.5,45),xlim=c(-66.5,-62.2),title=yrs[i])
	addPoints(data.frame(EID=1:nrow(x),X=x$DDLON,Y=x$DDLAT),pch=21,bg=rgb(0,1,0,0.3),cex=sqrt(x[,i+7]))
	legend('bottomright',legend=c(50,40,30,20,10,1),pch=21,pt.bg=rgb(0,1,0,0.3),pt.cex=sqrt(c(50,40,30,20,10,1)),bty='n',title="No. of Licences",inset=0.05,x.intersp=2,y.intersp=2)
	}

	dev.off()
	