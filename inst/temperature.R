	
p = bio.lobster::load.environment()

la()

##################### Temperature Data

lobster.db('fsrs')
fsrs$SYEAR<-fsrs$HAUL_YEAR
fsrs$HAUL_DATE<-as.Date(fsrs$HAUL_DATE)
fsrs$SYEAR[fsrs$LFA%in%c("33","34")]<-as.numeric(substr(fsrs$S_LABEL[fsrs$LFA%in%c("33","34")],6,9))

fsrsT = subset(fsrs,TEMP>-90)
fsrsT$Dloc = paste(fsrsT$HAUL_DATE,fsrsT$LATITUDE,fsrsT$LONGITUDE)

fsrsT = subset(fsrsT,!duplicated(Dloc))


lfas = c("27", "28", "29", "30", "31A", "31B", "32", "33")
nLFAs = c(27, 28, 29, 30, 31.1, 31.2, 32, 33)

fsrsT = subset(fsrsT,LFA%in%nLFAs&DEPTH<20&SYEAR>1999)

pdf('FSRStempsMap.pdf')
LobsterMap('all')

with(fsrsT,points(LONG_DD,LAT_DD,pch=16,cex=0.3,col=rgb(1,0,0,0.1)))
with(ScallopTemp,points(LONGITUDE,LATITUDE,pch=16,cex=0.3,col=rgb(0,0,1,0.1)))
dev.off()


with(subset(fsrsT,!duplicated(paste(LATITUDE,LONGITUDE))),tapply(TEMP,LFA,length))

fsrsT$y = decimal_date(fsrsT$HAUL_DATE)
fsrsT$cos.y = cos(2*pi*fsrsT$y)
fsrsT$sin.y = sin(2*pi*fsrsT$y)

#mf = formula(TEMP ~ s(SYEAR, k=5, bs="ts") + s(cos.d, k=3, bs="ts") + s(sin.d, k=3, bs="ts") )
 require(mgcv)  


TempModel <- gam(TEMP ~ s(sin.y,k=3,bs="ts")+s(cos.y,k=3,bs="ts")+s(y)+as.factor(subarea),data=fsrsT)
 summary(TempModel)
 #plot(TempModel)

plot(TEMP~y,data=subset(fsrsT,subarea='27N'),pch=16,col=rgb(0,0,0,0.1),cex=0.4,ylim=c(-2,25),xlim=c(1998,2027))
y=	seq(1999,2027,0.01)
t=predict(TempModel,data.frame(y=y,cos.y=cos(2*pi*y),sin.y=sin(2*pi*y),subarea='27N'),type='response')
 lines(y,t,col=2)
y=	seq(1998.4,2027.4,1)
t=predict(TempModel,data.frame(y=y,cos.y=cos(2*pi*y),sin.y=sin(2*pi*y),subarea='27N'),type='response')

 lines(y,t,col=2,lty=2,lwd=2)

pdf('FSRStemps.pdf',height=11,width=8)
par(mfrow=c(5,1),mar=c(3,3,3,3),las=1)
for(i in 1:length(lfas)){
	plot(TEMP~HAUL_DATE,subset(fsrsT,LFA==nLFAs[i]),pch='.',col=rgb(0,0,0,0.2),main=paste("LFA",lfas[i]),ylim=c(-1,18))
	#lines(rDailyTemps

}
dev.off()
	
####### Compare to Jae's temp predictions

	RLibrary("bio.bathymetry","bio.indicators","bio.temperature","bio.spacetime")
	p = spatial_parameters( type = "canada.east" ) 
	p$yrs = 1978:2016
 p$nw = 10 # number of intervals in time within a year
 p$dyears = (c(1:p$nw)-1) / p$nw # intervals of decimal years... fractional year breaks

	TempData = read.csv("LobsterTempData.csv")
	TempData = lonlat2planar(TempData, input_names=c("LONG_DD", "LAT_DD"),proj.type = p$internal.projection)

	baseLine = bathymetry.db(p=p, DS="baseline")
 
 # identify locations of data relative to baseline for envionmental data
 locsmap = match( 
  lbm::array_map( "xy->1", TempData[,c("plon","plat")], gridparams=p$gridparams ), 
  lbm::array_map( "xy->1", baseLine, gridparams=p$gridparams ) )
 
 # assign environmental data based on space-time coordinates
 timestamp=date_decimal(round(decimal_date(as.Date(TempData$HAUL_DATE)),1))
 btemp = indicators.lookup( p=p, DS="spatial.annual.seasonal", locsmap=locsmap, timestamp=timestamp, varnames="tmean" )

x=with(TempData,tapply(TEMP,HAUL_DATE,mean))
y=tapply(btemp,timestamp,mean,na.rm=T)
z=tapply(btemp,timestamp,mean,na.rm=T)

plot(TEMP~as.Date(HAUL_DATE),TempData,pch='.',col=rgb(0,0,0,0.2),ylim=c(-1,18))
points(as.Date(timestamp),btemp,pch='.',col=rgb(1,0,0,0.2))
points(as.Date(timestamp),btemp,pch='.',col=rgb(0,0,1,0.2))
 lines(as.Date(names(x)),x)
 lines(as.Date(names(y)),y,col='red')
 lines(as.Date(names(z)),z,col='blue')

RLibrary("bio.bathymetry")
LobsterTemp = read.csv("LobsterTempData.csv")
InshoreScallop = read.csv(file.path(project.datadirectory('bio.lobster'),'Temperature Data','InshoreScallopTemp.csv'))
InshoreScallop$DATE = as.Date(InshoreScallop$DATE,"%d/%m/%Y")
InshoreScallop$LONGITUDE = convert.dd.dddd(InshoreScallop$LONGITUDE)
InshoreScallop$LATITUDE = convert.dd.dddd(InshoreScallop$LATITUDE)
OffshoreScallop = read.csv(file.path(project.datadirectory('bio.lobster'),'Temperature Data','OffshoreScallopTemp.csv'))
OffshoreScallop$DATE = as.Date(OffshoreScallop$DATE,"%d/%m/%Y")
OffshoreScallop$LONGITUDE = convert.dd.dddd(OffshoreScallop$LONGITUDE)
OffshoreScallop$LATITUDE = convert.dd.dddd(OffshoreScallop$LATITUDE)
ScallopTemp = rbind(InshoreScallop,OffshoreScallop)

TempData = rbind(
	with(ScallopTemp,data.frame(DATE=DATE,LONGITUDE=LONGITUDE,LATITUDE=LATITUDE,TEMPERATURE=TEMPERATURE,DEPTH=NA)),
	with(LobsterTemp,data.frame(DATE=HAUL_DATE,LONGITUDE=LONG_DD,LATITUDE=LAT_DD,TEMPERATURE=TEMP,DEPTH=DEPTH)))

	p = spatial_parameters( type = "canada.east" ) 

	TempData = lonlat2planar(TempData, input_names=c("LONGITUDE", "LATITUDE"),proj.type = p$internal.projection)

	Complete = bathymetry.db(p=p, DS="complete")
 
 # identify locations of data relative to baseline for envionmental data
 locsmap = match( 
  lbm::array_map( "xy->1", TempData[,c("plon","plat")], gridparams=p$gridparams ), 
  lbm::array_map( "xy->1", Complete[,c("plon","plat")], gridparams=p$gridparams ) )
 

 TempData$z = Complete$z[locsmap]
 TempData$DEPTH = TempData$DEPTH * 1.8288
 TempData$DEPTH[is.na(TempData$DEPTH)] = TempData$z[is.na(TempData$DEPTH)]
 TempData$DEPTH[TempData$DEPTH<1] = 1
 TempData1 = TempData
 TempData = TempData1

		TempData = assignArea(TempData)
		TempData = assignSubArea2733(TempData)

 write.csv(TempData,file.path( project.datadirectory("bio.lobster"),"Temperature Data","TempData.csv"),row.names=F)
 TempData = read.csv(file.path( project.datadirectory("bio.lobster"),"Temperature Data","TempData.csv"))

 	B = hydro.db( p=p, DS="bottom.all" ) 

 	moreTempData = subset(B,date>'1977-01-01')
		moreTempData = assignArea(moreTempData)
		moreTempData = assignSubArea2733(moreTempData)
		moreTempData$DEPTH = 	moreTempData$z
	moreTempData <- rename.df(moreTempData,n0=c('t','date'),n1=c('TEMPERATURE',"DATE"))

	x=moreTempData[,c("LFA", "LFA_GRID", "EID", "DATE", "X", "Y", "TEMPERATURE", "DEPTH", "plon", "plat", "z",  "subarea")]

 TempData = rbind(TempData,moreTempData[,c("LFA", "LFA_GRID", "EID", "DATE", "X", "Y", "TEMPERATURE", "DEPTH", "plon", "plat", "z",  "subarea")])
write.csv(TempData,file.path( project.datadirectory("bio.lobster"),"Temperature Data","TempData.csv"),row.names=F)
 TempData = read.csv(file.path( project.datadirectory("bio.lobster"),"Temperature Data","TempData.csv"))
	


	#datascale = seq(10,1000,l=50)

	#planarMap( Complete[,c("plon","plat","z")], pts=TempData[TempData,c("plon","plat")], datascale=datascale , display=T)

plot(TEMP~DEPTH,TempData,pch=16,col=rgb(0,0,0,0.1))


TempData$y = decimal_date(as.Date(TempData$DATE))
TempData$cos.y = cos(2*pi*TempData$y)
TempData$sin.y = sin(2*pi*TempData$y)
TempData$subarea = as.factor(TempData$subarea)

#mf = formula(TEMP ~ s(SYEAR, k=5, bs="ts") + s(cos.d, k=3, bs="ts") + s(sin.d, k=3, bs="ts") )
 require(mgcv)  


#TempModel <- gam(TEMPERATURE ~ s(sin.y,k=3,bs="ts")+s(cos.y,k=3,bs="ts")
#	 + s(y) + subarea 
#		+ s(DEPTH, cos.y, sin.y), data=TempData)

TempModel <- gam(TEMPERATURE ~ sin.y +cos.y+ s(y) + subarea 
	+ s(DEPTH, cos.y, sin.y), data=TempData)
 #plot(TempModel)
 summary(TempModel)
y=	seq(min(TempData$y),max(TempData$y),0.01)

#points(TEMPERATURE~y,data=subset(TempData,subarea=='27N'),pch=16,col=rgb(1,0,0,0.2),cex=0.5)
t5=predict(TempModel,data.frame(y=y,cos.y=cos(2*pi*y),sin.y=sin(2*pi*y),DEPTH=5,subarea='33W'),type='response')
t25=predict(TempModel,data.frame(y=y,cos.y=cos(2*pi*y),sin.y=sin(2*pi*y),DEPTH=25,subarea='33W'),type='response')
t50=predict(TempModel,data.frame(y=y,cos.y=cos(2*pi*y),sin.y=sin(2*pi*y),DEPTH=50,subarea='33W'),type='response')
t100=predict(TempModel,data.frame(y=y,cos.y=cos(2*pi*y),sin.y=sin(2*pi*y),DEPTH=100,subarea='33W'),type='response')
t200=predict(TempModel,data.frame(y=y,cos.y=cos(2*pi*y),sin.y=sin(2*pi*y),DEPTH=200,subarea='33W'),type='response')
t500=predict(TempModel,data.frame(y=y,cos.y=cos(2*pi*y),sin.y=sin(2*pi*y),DEPTH=500,subarea='33W'),type='response')
 
 plot(TEMPERATURE~y,data=TempData,pch='.',col=rgb(0,0,0,0.1),ylim=c(-2,20),xlim=range(y))
 lines(y,t5,col=2)
 lines(y,t25,col=3)
 lines(y,t50,col=4)
 lines(y,t100,col=6)
 #lines(y,t200,col=7)
 #lines(y,t500,col=8)


	#	LFAs<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","Polygons_LFA.csv"))
	#	LFAs<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolys.csv"))
	#	LFAs<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolys.csv"))
x11()


install_github("BradHubley/SpatialHub")
library(SpatialHub)
bioMap("SS")


points(Y~X,TempData,pch='.',col=rgb(0,0,0,0.1))