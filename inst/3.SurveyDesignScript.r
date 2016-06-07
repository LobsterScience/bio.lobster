#------------------------- ecosystem survey data

  p = list()

  p$init.files = loadfunctions( "groundfish", functionname="load.groundfish.environment.r") 
  loadfunctions(c('utility','lobster','surveydesign'))

  p$libs = RLibrary( c( "chron", "lubridate", "parallel","sp" ,"PBSmapping")  )

  odbc.data.yrs=1970:2015

  groundfish.db( DS="gscat.redo" )
  groundfish.db( DS="gsdet.redo" )
  groundfish.db( DS="gsinf.redo" )

  gi = groundfish.db('gsinf')
  gc = groundfish.db('gscat')
  gc = gc[which(gc$spec==2550),]
  gic =  merge(gi,gc,by='id',all.x=T)

  i = which(is.na(gic$spec))
  gic$spec[i] = 2550
  gic$totno[i] = 0
  gic$totwgt[i] = 0
  gic = makePBS(gic,polygon=F)

  #abundance and weight in per km2
  gic$totwgt = gic$totwgt / gic$dist_km 
  gic$totno = gic$totno / gic$dist_km 



  # select for LFA 34

  LFAs<-read.csv(file.path( project.datadirectory("lobster"), "data","maps","LFAPolys.csv"))
  LFAgrid<-read.csv(file.path( project.datadirectory("lobster"), "data","maps","GridPolys.csv"))

  # Multibeam
  gerbk50 <-read.csv(file.path( project.datadirectory("bathymetry"),"data","GermanBathy","gerbk_50.txt"))
  key<-read.csv(file.path( project.datadirectory("bathymetry"),"data","GermanBathy","fileKey.csv"))

  # groundfish survey
  gfkey<-findPolys(na.omit(gic[c('X','Y','EID')]),LFAs)
  gic=merge(gic,gfkey[c('EID','PID')],all=T)
  gic34=subset(gic,PID==34)
  


  ### create Map1 
  
  pdf('SDlfa34map1.pdf')
  assignStation(subset(gic34,yr>1970,c('EID','X','Y')),maxdist=0.07,res=0.005,expwin=0.05,map='34')

  noRVgrids=c(69,81,92,103,114,125:127,137:141,153:159,170:177,188:193,207:210)
  addPolys(subset(LFA34grid,SID%in%noRVgrids),col=rgb(1,0,0,0.2))

  median(with(subset(gic34,yr>=1999),tapply(EID,yr,length)))

  stations=	read.csv(file.path(project.datadirectory('lobster'),"data","products","surveyStations.csv"))
  stations$EID=stations$SID
  addPoints(stations,pch=16)
  Stns32<-read.csv(file.path(project.datadirectory('lobster'),'data',"survey32Stations.csv"))
  indexStns=subset(stations,SID%in%Stns32$SID)
  addPoints(indexStns,pch=16,col='green')
  with(indexStns,text(X,Y,SID,pos=4,col='darkgreen',cex=0.5))

  dev.off()


  #typical number of RV tow in 34
  median(with(subset(gic34,yr>=1999),tapply(EID,yr,length)))

  LFA34poly=subset(LFAs,PID==34)
  LFA34grid=subset(LFAgrid,PID==34)

  ###-------------- Create polygons

  LobsterMap('34',labels='grid',labcex=0.5)

  innerGrids=c(69,81,92,101:103,112:114,123:127,136:141,152:159,170:177,188:193,207:210)
  addPolys(subset(LFA34grid,SID%in%innerGrids),col=rgb(1,0,0,0.2))

  junk<-data.frame(PID=1,POS=1:4,X=c(162,161,161,162),Y=c(-41,-41,-40,-40))
  innerGridPoly1=joinPolys(subset(LFA34grid,SID%in%innerGrids),junk,operation="DIFF")

  LFA34poly=subset(LFAs,PID==34)
  innerGridPoly<-joinPolys(innerGridPoly1,LFA34poly,operation="INT")
  outerGridPoly<-joinPolys(LFA34poly,innerGridPoly,operation="DIFF")


  MBpoly=outerBounds(gerbk50)
  
  # map of survey polygons
  pdf(file.path(project.datadirectory("lobster"),"figures","SurveyDesign","LFA34SurveyPolygons.pdf"),9,9)
  LobsterMap('34',labels='grid',labcex=0.5)
  addPolys(innerGridPoly,col=rgb(1,0,0,0.3))
  addPolys(outerGridPoly,col=rgb(0,0,1,0.3))
  addPolys(MBpoly,col=rgb(0,1,0,0.3))
  #with(gerbk50,points(X,Y,pch='.'))
  addPoints(indexStns,pch=21,bg='yellow')
  with(indexStns,text(X,Y,SID,pos=4,cex=0.5))
  dev.off()


  innerGridPoly<-joinPolys(innerGridPoly,MBpoly,operation="DIFF")
  outerGridPoly<-joinPolys(outerGridPoly,MBpoly,operation="DIFF")
  innerGridPoly$PID=1
  outerGridPoly$PID=3
  
  LFA34surveyPolys=rbind(innerGridPoly,MBpoly,outerGridPoly)



  ###-------------- Determine allocation

  attr(LFA34surveyPolys,"projection")<-"LL"
  polyAreas=calcArea(LFA34surveyPolys,1)

  areaAlloc=round(polyAreas$area/sum(polyAreas$area)*120)
  areaAlloc[3]=areaAlloc[3]-20
  
  iStnkey=findPolys(indexStns,LFA34surveyPolys)

  iStnAlloc=with(iStnkey,tapply(EID,PID,length))
  newAlloc=areaAlloc-iStnAlloc

  otherStns=subset(stations,!SID%in%Stns32$SID)
  oStnkey=findPolys(otherStns,LFA34surveyPolys)
  oStnAlloc=with(oStnkey,tapply(EID,PID,length))
  
  P3adds=otherStns[c(4,  5,  7,  8, 10, 12, 14, 18, 19, 23, 24, 30, 38, 42),]
  P1adds=otherStns[c(17, 20, 27, 28, 29, 33, 34, 41, 44, 45, 47, 48, 49, 55, 61),]


	  LobsterMap('34',labels='grid',labcex=0.5)
	  addPolys(innerGridPoly,col=rgb(1,0,0,0.3))
	  addPolys(outerGridPoly,col=rgb(0,0,1,0.3))
	  addPolys(MBpoly,col=rgb(0,1,0,0.3))
	  addPoints(indexStns,pch=21,bg='yellow')
	  addPoints(otherStns,pch=16,col='grey')
	  addPoints(P1adds,pch=21,bg='orange')
	  addPoints(P3adds,pch=21,bg='orange')

  repeatStns=rbind(indexStns,P1adds,P3adds)
  rStnkey=findPolys(repeatStns,LFA34surveyPolys)
  repeatStns=with(merge(repeatStns,rStnkey,by='EID',all=T),data.frame(EID,X,Y,PID))


  rStnAlloc=with(rStnkey,tapply(EID,PID,length))
  newAlloc=areaAlloc-rStnAlloc


  LFA34surveyPolyData=data.frame(PID=1:3,col=c(rgb(1,0,0,0.3),rgb(0,1,0,0.3),rgb(0,0,1,0.3)),PName=c("inner","multibeam","outer"),allocation=newAlloc,repeats=rStnAlloc)

  

  towlst<-alloc.poly(poly.lst=list(LFA34surveyPolys, LFA34surveyPolyData),ntows=100,pool.size=3,mindist=5,repeated.tows=repeatStns,map='34',show.pool=T,UTMzone=20)

 file.key<-read.csv(file.path( project.datadirectory("bathymetry"),"data","GermanBathy","fileKey.csv"))
 Relief.plots(subset(towlst$Tows$new.tows,STRATA=='multibeam'),graphic="pdf",file=file.path(project.datadirectory('lobster'),'figures','SurveyDesign','SurveyMB','2016','Tow'),digits=4,gerfiles=1:77,key=file.key)
 



save.image(file.path(project.datadirectory("lobster"),"R","LFA34SurveyDesign2016.Rdata"))
load(file.path(project.datadirectory("lobster"),"R","LFA34SurveyDesign2016.Rdata"))


pdf("Survey2016.pdf",8,11)
    LobsterMap('34',labels='grid',labcex=0.5)
    addPolys(innerGridPoly,col=rgb(1,0,0,0.3))
    addPolys(outerGridPoly,col=rgb(0,0,1,0.3))
    addPolys(MBpoly,col=rgb(0,1,0,0.3))

    addPoints(towlst$Tows$new.tows,pch=21,cex=0.5,bg='orange')
    addPoints(towlst$Tows$repeated.tows,pch=21,cex=0.5,bg='orange')
    MBtows=subset(towlst$Tows$new.tows,Poly.ID==2)
    MBtows$label=MBtows$EID
    MBtows$Y= MBtows$Y-0.03
    addLabels(as.EventData(MBtows),cex=0.7)


dev.off()

MBEIDs=c(5,6,9,10,13,15,18,23,24,25,27,28,29,30,31,34,35,36,38,40)

new.tows <- rbind(subset(towlst$Tows$new.tows,Poly.ID==2&EID%in%MBEIDs),subset(towlst$Tows$new.tows,Poly.ID!=2&!EID%in%c(2,51,56)),data.frame(EID=c(2,51,56),X=c(-66.312,-67.031,-66.854),Y=c(42.969,43.5,43.854),Poly.ID=c(1,3,3),STRATA=c("inner","outer","outer"),nndist=NA))




pdf(file.path(project.datadirectory('lobster'),'figures','SurveyDesign','SurveyMB','2016',"Survey2016.pdf"),8,11)
    LobsterMap('34',labels='grid',labcex=0.5)
    addPolys(innerGridPoly,col=rgb(1,0,0,0.3))
    addPolys(outerGridPoly,col=rgb(0,0,1,0.3))
    addPolys(MBpoly,col=rgb(0,1,0,0.3))

    addPoints(new.tows,pch=21,cex=0.6,bg='orange')
    addPoints(towlst$Tows$repeated.tows,pch=21,cex=0.6,bg='red')
    labtows=rbind(new.tows[,-6],towlst$Tows$repeated.tows)
    labtows$label=labtows$EID
    labtows$Y= labtows$Y-0.03
    addLabels(as.EventData(labtows),cex=0.7)


dev.off()


test.tow=data.frame(EID=1:2,X=c(-66.34800,),Y=c(43.27362,)

new.tows

 file.key<-read.csv(file.path( project.datadirectory("bathymetry"),"data","GermanBathy","fileKey.csv"))
 Relief.plots(subset(new.tows,STRATA=='multibeam'),graphic="pdf",file=file.path(project.datadirectory('lobster'),'figures','SurveyDesign','SurveyMB','2016','Tow'),digits=4,gerfiles=1:77,key=file.key)


Relief.plots(subset(towlst$Tows$repeated.tows,STRATA=='multibeam'),graphic="pdf",file=file.path(project.datadirectory('lobster'),'figures','SurveyDesign','SurveyMB','2016','Tow'),digits=4,gerfiles=1:77,key=file.key)



















################################################################################################################################################################################################################
#
#               extra stuff
#
#####################################################



# # key to files for german MB res 5m

# pdf(file.path(project.datadirectory("lobster"),"figures","SurveyDesign","LFA34MultibeamExtent.pdf"),9,9)
# key<-read.csv(file.path( project.datadirectory("bathymetry"),"data","GermanBathy","fileKey.csv"))
# LobsterMap('34',labels='grid',labcex=0.5,isobath=c(50,100,200),bathy.source='bathy')
# with(key,rect(X1,Y1,X2,Y2,col='black'))
# dev.off()
# 
# 

#
# calcArea(LFA34grid2)

# key<-findPolys(na.omit(gic[c('X','Y','EID')]),LFAs)

# gic=merge(gic,key[c('EID','PID')],all=T)

# gic34=subset(gic,PID==34)
# 
# assignStation(subset(gic34,yr>1970,c('EID','X','Y')),maxdist=0.07,res=0.005,expwin=0.05,map='34')

# median(with(subset(gic34,yr>=1999),tapply(EID,yr,length)))

# gfpoly<-gic34[chull(gic34$X,gic34$Y),c("X","Y")]
# gfpoly$PID=1
# gfpoly$POS=1:nrow(gfpoly)
# addPolys(gfpoly)




# innerGrids=c(69,81,92,101:103,112:114,123:127,136:141,152:159,170:177,188:193,207:210)
# addPolys(subset(LFA34grid,SID%in%innerGrids),col=rgb(1,0,0,0.2))

# junk<-data.frame(PID=1,POS=1:4,X=c(162,161,161,162),Y=c(-41,-41,-40,-40))
# innerGridPoly=joinPolys(subset(LFA34grid,SID%in%innerGrids),junk,operation="DIFF")



# coastalGrids=c(69,81,92,103,114,125:127,137:141,153:159,170:177,188:193,207:210)


# LobsterMap('34',labels='grid',labcex=0.5)
# addPoints(na.omit(gic[which(gic$yr==2015),c('X','Y','EID')]),pch=16,col='red',cex=0.75)
# addPoints(na.omit(gic[which(gic$yr>=1999),c('X','Y','EID')]),pch=16,col='red',cex=0.75)
# addPoints(na.omit(gic[which(gic$yr>=1999 & gic$totno>0),c('X','Y','EID')]),pch=16,col='green',cex=0.75) #lobster only

#
#require(PBSmapping)
#require(RColorBrewer)
#loadfunctions('lobster')
#
#extent<-data.frame(PID=1,POS=1:4,X=c(-66.3,-65.4,-65.4,-66.3),Y=c(43.15,43.15,43.8,43.8))
#attr(extent,"projection")<-"LL"
#
#
#load(file.path(project.datadirectory("substrate"),"data","InshoreSubstrate.rdata"))
#
#subpoly.data<-read.csv(file.path(project.datadirectory("substrate"),"data","SubPolyData.csv"))
#subpoly.data$col<-brewer.pal(10,"Set3")
#subpoly.data$border<-NA
#
#attr(substrate,"projection")<-"UTM"
#attr(substrate,"zone")<-20
#substrate<-convUL(substrate,km=F)
#
#LBsubstrate<-joinPolys(substrate,extent,operation="INT")
#
#pdf(file.path(project.datadirectory("lobster"),"R","LobsterBaySubstrate.pdf"),9,9)
#LobsterMap(xlim=c(-66.35,-65.35),ylim=c(43.1,43.85),poly.lst=list(LBsubstrate,subpoly.data),isobath=40,bathcol='blue',bathy.source='bathy',mapRes="UR")
#with(subset(subpoly.data,PID%in%unique(LBsubstrate$PID)),legend('topright',PName,fill=col,inset=0.02,cex=0.8,bg='white'))
#dev.off()
#        
#load(file.path( project.datadirectory("lobster"), "data","maps", "bathy", "bathyPoly1.rdata"))
#LBsubstrate<-joinPolys(LBsubstrate,subset(bathy.poly,Z==40),operation="INT")
#LFAs<-read.csv(file.path(project.datadirectory('lobster'),'data','maps','Polygons_LFA.csv'))
#LBsubstrate<-joinPolys(LBsubstrate,subset(LFAs,LFA=='34'),operation="INT")
#  
#
## repeated index stations
#LFA34TrawlStations2015<-read.csv(file.path(project.datadirectory("lobster"),"data","LFA34TrawlStations2015.csv"))
#LFA34TrawlStations2015$EID<-1:nrow(LFA34TrawlStations2015)
#names(LFA34TrawlStations2015)[2:3]<-c("Y","X")
#key<-findPolys(LFA34TrawlStations2015,LBsubstrate)
#repeatStations<-merge(LFA34TrawlStations2015,key)
#
#LB.towlst<-alloc.poly(poly.lst=list(LBsubstrate, subpoly.data),ntows=20,mindist=2)
#
#pdf(file.path(project.datadirectory("lobster"),"R","LobsterBaySurvey2015.pdf"),9,9)
#LobsterMap(xlim=c(-66.35,-65.35),ylim=c(43.1,43.85),poly.lst=list(LBsubstrate,subpoly.data),isobath=c(20,40,70,100),bathy.source='bathy',mapRes="UR")
#with(subset(subpoly.data,PID%in%unique(LBsubstrate$PID)),legend('topright',paste(PID,PName),fill=col,inset=0.02,cex=0.8,bg='white'))
#addPoints(LB.towlst$Tows)
#points(Y~X,repeatStations,pch=2)
#dev.off()
#write.csv(LB.towlst$Tows,file.path(project.datadirectory('lobster'),'R','LFA34extraSurveyStation2015.csv'))
#
## Take 2
#
#extent<-data.frame(PID=1,POS=1:4,X=c(-66.05,-65.7,-65.7,-66.05),Y=c(43.15,43.15,43.8,43.8))
#attr(extent,"projection")<-"LL"
#
#
#load(file.path(project.datadirectory("substrate"),"data","InshoreSubstrate.rdata"))
#
#bathpoly.data<-data.frame
#subpoly.data$col<-brewer.pal(10,"Set3")
#subpoly.data$border<-NA
#
#attr(substrate,"projection")<-"UTM"
#attr(substrate,"zone")<-20
#substrate<-convUL(substrate,km=F)
#
#LBsubstrate<-joinPolys(substrate,extent,operation="INT")
#
#pdf(file.path(project.datadirectory("lobster"),"R","LobsterBaySubstrate.pdf"),9,9)
#LobsterMap(xlim=c(-66.35,-65.35),ylim=c(43.1,43.85),poly.lst=list(LBsubstrate,subpoly.data),isobath=40,bathcol='blue',bathy.source='bathy',mapRes="UR")
#with(subset(subpoly.data,PID%in%unique(LBsubstrate$PID)),legend('topright',PName,fill=col,inset=0.02,cex=0.8,bg='white'))
#dev.off()
#
#
#        
#load(file.path( project.datadirectory("lobster"), "data","maps", "bathy", "bathy1Poly1.rdata"))
#    junk<-data.frame(PID=1,POS=1:4,X=c(162,161,161,162),Y=c(-41,-41,-40,-40))
#
#LBbathy<-joinPolys(subset(bathy.poly,Z%in%c(10,25,40)),extent,operation="INT")
#bathpoly.data<-data.frame(PID=c(10,25,40),PName=c("<10m","10-25m","25-40m"),col=brewer.pal(3,"Blues"),border=NA)
#
#pdf(file.path(project.datadirectory("lobster"),"R","LobsterBayDepth.pdf"),9,9)
#LobsterMap(xlim=c(-66.2,-65.5),ylim=c(43.1,43.85),poly.lst=list(LBbathy,bathpoly.data),isobath=c(10,25,40,70,100),bathy.source='bathy',mapRes="UR")
#with(bathpoly.data,legend('topright',legend=paste(PID[-length(PID)],'-',PID[-1],sep=''),fill=col[-1],inset=0.02,cex=0.8,bg='white'))
#dev.off()
#
#b40<-joinPolys(subset(LBbathy,PID==40),junk,operation="DIFF")
#b25<-joinPolys(subset(LBbathy,PID==25),junk,operation="DIFF")
#b10<-joinPolys(subset(LBbathy,PID==10),junk,operation="DIFF")
#
#strata2<-joinPolys(b40,b25,operation="DIFF")
#strata2<-joinPolys(strata2,junk,operation="DIFF")
#strata2<-joinPolys(strata2,strata1,operation="DIFF")
#
#strata1<-joinPolys(b25,b10,operation="DIFF")
#strata1<-joinPolys(strata1,junk,operation="DIFF")
#strata1<-joinPolys(strata1,strata2,operation="DIFF")
#
#
#LBbathy<-rbind(strata1,strata2)
#LBbathy<-joinPolys(LBbathy,junk,operation="DIFF")
#
#LBsubstrate<-joinPolys(LBsubstrate,subset(bathy.poly,Z==40),operation="INT")
#LFAs<-read.csv(file.path(project.datadirectory('lobster'),'data','maps','Polygons_LFA.csv'))
#LBsubstrate<-joinPolys(LBsubstrate,subset(LFAs,LFA=='34'),operation="INT")
#  
#
## repeated index stations
#LFA34TrawlStations2015<-read.csv(file.path(project.datadirectory("lobster"),"data","LFA34TrawlStations2015.csv"))
#LFA34TrawlStations2015$EID<-1:nrow(LFA34TrawlStations2015)
#names(LFA34TrawlStations2015)[2:3]<-c("Y","X")
#key<-findPolys(LFA34TrawlStations2015,LBsubstrate)
#repeatStations<-merge(LFA34TrawlStations2015,key)
#
#LB.towlst<-alloc.poly(poly.lst=list(LBbathy, bathpoly.data),ntows=40,mindist=2)
#
#pdf(file.path(project.datadirectory("lobster"),"R","LobsterBaySurvey2015.pdf"),8,10)
#LobsterMap(xlim=c(-66.2,-65.5),ylim=c(43.1,43.85),poly.lst=list(LBbathy,bathpoly.data),isobath=c(10,25,40,70,100),bathy.source='bathy',mapRes="UR")
#with(bathpoly.data,legend('topright',legend=paste(PID[-length(PID)],'-',PID[-1],sep=''),fill=col[-1],inset=0.02,cex=0.8,bg='white'))
#addPoints(LB.towlst$Tows)
#points(Y~X,repeatStations,pch=2)
#dev.off()
#write.csv(LB.towlst$Tows,file.path(project.datadirectory('lobster'),'R','LFA34extraSurveyStation2015.csv'),row.names=F)
#
#
#LB.towlst$Tows$EID<-LB.towlst$Tows$EID+100
#
#LB.towlst$Tows$STATION<-paste0('extra',1:40)
#allTows<-merge(LFA34TrawlStations2015,LB.towlst$Tows,all=T)
#allTows<-allTows[order(allTows$EID),]
#
#    Relief.plots(allTows,graphic="pdf",file=file.path(project.datadirectory('lobster'),'R','SurveyMB','Tow'),digits=4,gerfiles=1:77,key=file.key)
#
