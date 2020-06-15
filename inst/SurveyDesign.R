require(PBSmapping)
require(RColorBrewer)
loadfunctions('lobster')

extent<-data.frame(PID=1,POS=1:4,X=c(-66.3,-65.4,-65.4,-66.3),Y=c(43.15,43.15,43.8,43.8))
attr(extent,"projection")<-"LL"


load(file.path(project.datadirectory("substrate"),"data","InshoreSubstrate.rdata"))

subpoly.data<-read.csv(file.path(project.datadirectory("substrate"),"data","SubPolyData.csv"))
subpoly.data$col<-brewer.pal(10,"Set3")
subpoly.data$border<-NA

attr(substrate,"projection")<-"UTM"
attr(substrate,"zone")<-20
substrate<-convUL(substrate,km=F)

LBsubstrate<-joinPolys(substrate,extent,operation="INT")

pdf(file.path(project.datadirectory("lobster"),"R","LobsterBaySubstrate.pdf"),9,9)
LobsterMap(xlim=c(-66.35,-65.35),ylim=c(43.1,43.85),poly.lst=list(LBsubstrate,subpoly.data),isobath=40,bathcol='blue',bathy.source='bathy',mapRes="UR")
with(subset(subpoly.data,PID%in%unique(LBsubstrate$PID)),legend('topright',PName,fill=col,inset=0.02,cex=0.8,bg='white'))
dev.off()
	 			
load(file.path( project.datadirectory("lobster"), "data","maps", "bathy", "bathyPoly1.rdata"))
LBsubstrate<-joinPolys(LBsubstrate,subset(bathy.poly,Z==40),operation="INT")
LFAs<-read.csv(file.path(project.datadirectory('lobster'),'data','maps','Polygons_LFA.csv'))
LBsubstrate<-joinPolys(LBsubstrate,subset(LFAs,LFA=='34'),operation="INT")
	

# repeated index stations
LFA34TrawlStations2015<-read.csv(file.path(project.datadirectory("lobster"),"data","LFA34TrawlStations2015.csv"))
LFA34TrawlStations2015$EID<-1:nrow(LFA34TrawlStations2015)
names(LFA34TrawlStations2015)[2:3]<-c("Y","X")
key<-findPolys(LFA34TrawlStations2015,LBsubstrate)
repeatStations<-merge(LFA34TrawlStations2015,key)

LB.towlst<-alloc.poly(poly.lst=list(LBsubstrate, subpoly.data),ntows=20,mindist=2)

pdf(file.path(project.datadirectory("lobster"),"R","LobsterBaySurvey2015.pdf"),9,9)
LobsterMap(xlim=c(-66.35,-65.35),ylim=c(43.1,43.85),poly.lst=list(LBsubstrate,subpoly.data),isobath=c(20,40,70,100),bathy.source='bathy',mapRes="UR")
with(subset(subpoly.data,PID%in%unique(LBsubstrate$PID)),legend('topright',paste(PID,PName),fill=col,inset=0.02,cex=0.8,bg='white'))
addPoints(LB.towlst$Tows)
points(Y~X,repeatStations,pch=2)
dev.off()
write.csv(LB.towlst$Tows,file.path(project.datadirectory('lobster'),'R','LFA34extraSurveyStation2015.csv'))

# Take 2

extent<-data.frame(PID=1,POS=1:4,X=c(-66.05,-65.7,-65.7,-66.05),Y=c(43.15,43.15,43.8,43.8))
attr(extent,"projection")<-"LL"


load(file.path(project.datadirectory("substrate"),"data","InshoreSubstrate.rdata"))

bathpoly.data<-data.frame
subpoly.data$col<-brewer.pal(10,"Set3")
subpoly.data$border<-NA

attr(substrate,"projection")<-"UTM"
attr(substrate,"zone")<-20
substrate<-convUL(substrate,km=F)

LBsubstrate<-joinPolys(substrate,extent,operation="INT")

pdf(file.path(project.datadirectory("lobster"),"R","LobsterBaySubstrate.pdf"),9,9)
LobsterMap(xlim=c(-66.35,-65.35),ylim=c(43.1,43.85),poly.lst=list(LBsubstrate,subpoly.data),isobath=40,bathcol='blue',bathy.source='bathy',mapRes="UR")
with(subset(subpoly.data,PID%in%unique(LBsubstrate$PID)),legend('topright',PName,fill=col,inset=0.02,cex=0.8,bg='white'))
dev.off()


	 			
load(file.path( project.datadirectory("lobster"), "data","maps", "bathy", "bathy1Poly1.rdata"))
		junk<-data.frame(PID=1,POS=1:4,X=c(162,161,161,162),Y=c(-41,-41,-40,-40))

LBbathy<-joinPolys(subset(bathy.poly,Z%in%c(10,25,40)),extent,operation="INT")
bathpoly.data<-data.frame(PID=c(10,25,40),PName=c("<10m","10-25m","25-40m"),col=brewer.pal(3,"Blues"),border=NA)

pdf(file.path(project.datadirectory("lobster"),"R","LobsterBayDepth.pdf"),9,9)
LobsterMap(xlim=c(-66.2,-65.5),ylim=c(43.1,43.85),poly.lst=list(LBbathy,bathpoly.data),isobath=c(10,25,40,70,100),bathy.source='bathy',mapRes="UR")
with(bathpoly.data,legend('topright',legend=paste(PID[-length(PID)],'-',PID[-1],sep=''),fill=col[-1],inset=0.02,cex=0.8,bg='white'))
dev.off()

b40<-joinPolys(subset(LBbathy,PID==40),junk,operation="DIFF")
b25<-joinPolys(subset(LBbathy,PID==25),junk,operation="DIFF")
b10<-joinPolys(subset(LBbathy,PID==10),junk,operation="DIFF")

strata2<-joinPolys(b40,b25,operation="DIFF")
strata2<-joinPolys(strata2,junk,operation="DIFF")
strata2<-joinPolys(strata2,strata1,operation="DIFF")

strata1<-joinPolys(b25,b10,operation="DIFF")
strata1<-joinPolys(strata1,junk,operation="DIFF")
strata1<-joinPolys(strata1,strata2,operation="DIFF")


LBbathy<-rbind(strata1,strata2)
LBbathy<-joinPolys(LBbathy,junk,operation="DIFF")

LBsubstrate<-joinPolys(LBsubstrate,subset(bathy.poly,Z==40),operation="INT")
LFAs<-read.csv(file.path(project.datadirectory('lobster'),'data','maps','Polygons_LFA.csv'))
LBsubstrate<-joinPolys(LBsubstrate,subset(LFAs,LFA=='34'),operation="INT")
	

# repeated index stations
LFA34TrawlStations2015<-read.csv(file.path(project.datadirectory("lobster"),"data","LFA34TrawlStations2015.csv"))
LFA34TrawlStations2015$EID<-1:nrow(LFA34TrawlStations2015)
names(LFA34TrawlStations2015)[2:3]<-c("Y","X")
key<-findPolys(LFA34TrawlStations2015,LBsubstrate)
repeatStations<-merge(LFA34TrawlStations2015,key)

LB.towlst<-alloc.poly(poly.lst=list(LBbathy, bathpoly.data),ntows=40,mindist=2)

pdf(file.path(project.datadirectory("lobster"),"R","LobsterBaySurvey2015.pdf"),8,10)
LobsterMap(xlim=c(-66.2,-65.5),ylim=c(43.1,43.85),poly.lst=list(LBbathy,bathpoly.data),isobath=c(10,25,40,70,100),bathy.source='bathy',mapRes="UR")
with(bathpoly.data,legend('topright',legend=paste(PID[-length(PID)],'-',PID[-1],sep=''),fill=col[-1],inset=0.02,cex=0.8,bg='white'))
addPoints(LB.towlst$Tows)
points(Y~X,repeatStations,pch=2)
dev.off()
write.csv(LB.towlst$Tows,file.path(project.datadirectory('lobster'),'R','LFA34extraSurveyStation2015.csv'),row.names=F)


LB.towlst$Tows$EID<-LB.towlst$Tows$EID+100

LB.towlst$Tows$STATION<-paste0('extra',1:40)
allTows<-merge(LFA34TrawlStations2015,LB.towlst$Tows,all=T)
allTows<-allTows[order(allTows$EID),]

		Relief.plots(allTows,graphic="pdf",file=file.path(project.datadirectory('lobster'),'R','SurveyMB','Tow'),digits=4,gerfiles=1:77,key=file.key)

