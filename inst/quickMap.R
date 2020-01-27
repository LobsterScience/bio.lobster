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

pdf(file.path( project.datadirectory("bio.lobster"), "figures","maptest.pdf"),8,11)

LobsterMap(ylim=c(42,45.5), xlim=c(-68.15,-65.2),mapRes="UR",contours=cont.lst,isobath=seq(50,500,50),bathcol=rgb(0,0,1,0.2),bathy.source='bathy')
dev.off()