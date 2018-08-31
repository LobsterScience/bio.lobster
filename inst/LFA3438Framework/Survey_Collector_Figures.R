p = bio.lobster::load.environment()
lobster.db('survey')
head(surveyCatch)
require(SpatialHub)
datSurvey<-LobsterSurveyProcess()
require(gstat)


# interpolate abundance
interp.data<-na.omit(subset(datSurvey,YEAR==2016,c('SET_ID','SET_LONG','SET_LAT','NUM_STANDARDIZED')))
lob.contours<-interpolation(interp.data,ticks='define',place=3,nstrata=5,str.min=0,interp.method='gstat',blank=T,res=0.005,smooth=F,idp=3.5,blank.dist=0.2)

# define contour lines
print(lob.contours$str.def)
lvls=c(1, 5, 10, 20, 50, 100, 200, 500)

# generate contour lines
LFAs<-read.csv(file.path(project.datadirectory("bio.lobster"), "data","maps","LFAPolys.csv"))
cont.lst<-contourGen(lob.contours$image.dat,lvls,subset(LFAs,PID==34),col="YlGn",colorAdj=1)
# plot Map
pdf(file.path( project.datadirectory("bio.lobster"), "figures","LFA34_38Framework","SurveyDensity.pdf"),8,11)
#png(file.path(project.datadirectory("bio.lobster"), "R","SurveyDensity.png"),1200,1500)
LobsterMap(ylim=c(42.8,44.6), xlim=c(-67.15,-65.2),mapRes="UR",contours=cont.lst,title="LFA 34 Lobster Density",isobath=seq(50,500,50),bathcol=rgb(0,0,1,0.2),bathy.source='bathy')
points(SET_LAT~SET_LONG,datSurvey,subset=YEAR==2016,pch=21,cex=0.5,bg='red')#,col=rgb(0,0,0,0.5))
contLegend("topright",lvls=lvls,Cont.data=cont.lst$Cont.data,title="#/standard tow",inset=0.02,cex=0.8,bg='white')
dev.off()


#Collector Sampling footprint
collector<-read.csv(file.path(project.datadirectory("bio.lobster"), "data","CollectorData","collectors_07_17.csv"))
 

pdf(file.path(project.datadirectory("bio.lobster"), "figures","LFA34_38Framework","CollectorLoc.pdf"),8,11)
LobsterMap(ylim=c(42.5,48),xlim=c(-67.4,-57.8), addGrids= F, title= "Collector Site Locations")
points(latitude~longitude, collector, pch = 21, cex=0.5, bg='red')

pdf(file.path(project.datadirectory("bio.lobster"), "figures","LFA34_38Framework","Collector31.pdf"),8,11)
LobsterMap(ylim=c(44.1,45.7),xlim=c(-62.2,-60), addGrids= F, title= "Collector Site Locations")
points(latitude~longitude, collector, pch = 21, cex=0.5, bg='red')



