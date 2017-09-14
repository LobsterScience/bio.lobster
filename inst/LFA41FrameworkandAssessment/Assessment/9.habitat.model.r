#habitat model


#need to rerun with new depth stuff dec 21 2016
    require(bio.lobster)
	require(bio.utilities)
	require(bio.temperature)
	#require(bio.habitat)
	require(bio.indicators)
	require(bio.spacetime)
	require()
require(dismo)
require(gbm)	
require(lubridate)
require(bio.groundfish)
	#Prediction surface
 p = bio.lobster::load.environment()
require(mgcv)
	la()

	p$years = c(1970:2016)
if(redo){
	#done Aug 28, 2017
	b = habitat.model.data('nefsc.surveys.redo',p=p)
	a = habitat.model.data('dfo.summer.redo',p=p)
	d = habitat.model.data('dfo.georges.redo',p=p)
	}

	b = habitat.model.data('nefsc.surveys',p=p)
	a = habitat.model.data('dfo.summer',p=p)
	d = habitat.model.data('dfo.georges',p=p)
	
dat = rbind(a,b,d)
	dat$yr = year(dat$timestamp)
	dat$z = log(dat$z)
	dat = dat[which(dat$t>=0),]
	dat = subset(dat,yr>1969)
save(dat,file=file.path(project.datadirectory('bio.lobster'),'analysis','habitatmodellingdata.rdata'))

load(file=file.path(project.datadirectory('bio.lobster'),'analysis','habitatmodellingdata.rdata'))
require(gbm)
require(dismo)
#probability of occurence

###using cotinuous time with decimal years
############playing with Dec 2016



dat$Y = ifelse(dat$B>0,1,0)
dat$year = year(dat$timestamp)
dat$Time = year(dat$timestamp) + dat$dyear

ii = which(dat$t>20)
dat = dat[-ii,]

ii = which(dat$dZ>20)
dat = dat[-ii,]

ii = which(dat$ddZ>15)
dat = dat[-ii,]

aa2a <- gbm.step(data=na.omit(dat), gbm.x = c('Time','t','z','dZ','ddZ'), gbm.y = 'Y', family = "bernoulli", tree.complexity = 5, learning.rate = 0.015, bag.fraction = 0.5) #bernoulli is same as binomial in this formulation
summary(aa2)
savePlot(file.path(project.figuredirectory('bio.lobster'),'brtinfluenceplots.png'))

save(aa2,file=file.path(project.datadirectory('bio.lobster'),'data','products','brtContinuousTime.rdata'))
load(file=file.path(project.datadirectory('bio.lobster'),'data','products','brtContinuousTime.rdata'))

dyear = seq(0.1,1,0.1) #autumn 
p$yrs = unique(dat$yr)
p$dyear=0.8
outs = matrix(NA,ncol=length(dyear),nrow=length(p$yrs))
p$annual.T.means=TRUE


	J = habitat.model.data('prediction.surface',p=p)
	
#Index of prediction surface within LFA41
	jj = J[,c('plon','plat')]
	jj$EID = 1:nrow(jj)
	names(jj)[1:2] <- c('X','Y')

#LFA41 polygon
	LFA41 = read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","LFA41Offareas.csv"))
	LFA41 = joinPolys(as.PolySet(LFA41),operation='UNION')
	LFA41 = subset(LFA41,SID==1)
	attr(LFA41,'projection') <- 'LL'
	aa = lonlat2planar(LFA41,input_names = c('X','Y'),proj.type = p$internal.projection)
	aa$plon = grid.internal(aa$plon,p$plons)
	aa$plat = grid.internal(aa$plat,p$plats)
	aa$X = aa$plon
	aa$Y = aa$plat

	s2 = findPolys(jj,aa,maxRows = 760000)
	s2 = subset(jj, EID %in% s2$EID)

outa = list()
outb = list()
for(i in 1:length(p$yrs)) {
		a = p$yrs[i]
		pI = J[,c('z','dZ','ddZ')]
		k = grep(a,names(J))
		pI = data.frame(pI,J[,k])
		names(pI)[ncol(pI)] <- 't'
		pI$dyear = p$dyear
		pI$z = log(pI$z)
		pI$Time= a+.0
		ab = predict.gbm(aa2,newdata=pI,n.trees = aa2$gbm.call$best.trees,type='response')
		dr = seq( 0,1, length.out=100)
		png(file=file.path(project.figuredirectory('bio.lobster'),paste('trial.continuousTimeboostedRegTree',a,'.png',sep="")),units='in',width=15,height=12,pointsize=18, res=300,type='cairo')
	o = levelplot(ab~J$plon+J$plat,aspect='iso',xlim=c(-600,100),ylim=c(-450,100),at=dr,col.regions=color.code('blue.yellow.red',dr),main=as.character(a))
	print(o)
	dev.off()
		pI = pI[s2$EID,]
		ab = predict.gbm(aa2,newdata=pI,n.trees = aa2$gbm.call$best.trees,type='response')
		print(a)
		outa[[i]] = ab
	
		}
	

# 25 % probability of occurrenece
u = c()
			for(i in 1:length(outa)) {
				u[i] <- sum(outa[[i]]>=0.35) / length(outa[[i]])
			}
plot(1970:2016,u,type='b',col='black',pch=16,xlab='Year',ylab = 'Proportion Suitable Lobster Habitat')
savePlot(file=file.path(project.figuredirectory('bio.lobster'),'ProportionSuitableLobsterHabitat35.png'))
 a = data.frame(yr=1970:2016,sdm.habitat=u)
 write.csv(a,file=file.path(project.datadirectory('bio.lobster'),'analysis','lfa41Assessment','indicators','sdmhabitat.csv')
)

#predictor variables
require(classInt)
		dr = seq( 1,7, length.out=1000)
	mypalette = colorRampPalette(c("darkblue","blue3", "green", "yellow", "orange","red3", "darkred"), space = "Lab")(100)
  mybreaks = classIntervals( (J$z), n=length(mypalette), style="quantile")$brks
 levelplot((J$z)~J$plon+J$plat,aspect='iso',xlim=c(-600,100),ylim=c(-450,100),at=mybreaks,col.regions=mypalette,xlab='Planar Longitude', ylab = 'Planar Latitude', main='Depth')
  savePlot(file.path(project.figuredirectory('bio.lobster'),'DepthSurface.png'))

mypalette = colorRampPalette(c("darkblue","blue3", "green", "yellow", "orange","red3", "darkred"), space = "Lab")(50)
  mybreaks = classIntervals( log(J$dZ), n=length(mypalette), style="quantile")$brks
 levelplot(log(J$dZ)~J$plon+J$plat,aspect='iso',xlim=c(-600,100),ylim=c(-450,100),at=mybreaks,col.regions=mypalette,xlab='Planar Longitude', ylab = 'Planar Latitude', main= 'Slope')
  savePlot(file.path(project.figuredirectory('bio.lobster'),'SlopeSurface.png'))

mypalette = colorRampPalette(c("darkblue","blue3", "green", "yellow", "orange","red3", "darkred"), space = "Lab")(50)
  mybreaks = classIntervals( log(J$ddZ), n=length(mypalette), style="quantile")$brks
 levelplot(log(J$ddZ)~J$plon+J$plat,aspect='iso',xlim=c(-600,100),ylim=c(-450,100),at=mybreaks,col.regions=mypalette,xlab='Planar Longitude', ylab = 'Planar Latitude', main='Curvature')
  savePlot(file.path(project.figuredirectory('bio.lobster'),'CurvatureSurface.png'))
mypalette = colorRampPalette(c("darkblue","blue3", "green", "yellow", "orange","red3", "darkred"), space = "Lab")(100)
mybreaks = seq(0,18,length.out = length(mypalette))

levelplot(J$x.1970~J$plon+J$plat,aspect='iso',xlim=c(-600,100),ylim=c(-450,100),at=mybreaks,col.regions=mypalette,xlab='Planar Longitude', ylab = 'Planar Latitude', main='1970')
savePlot(file.path(project.figuredirectory('bio.lobster'),'temperature1970.png'))


levelplot(J$x.1975~J$plon+J$plat,aspect='iso',xlim=c(-600,100),ylim=c(-450,100),at=mybreaks,col.regions=mypalette,xlab='Planar Longitude', ylab = 'Planar Latitude',main='1975')
savePlot(file.path(project.figuredirectory('bio.lobster'),'temperature1975.png'))

levelplot(J$x.1980~J$plon+J$plat,aspect='iso',xlim=c(-600,100),ylim=c(-450,100),at=mybreaks,col.regions=mypalette,xlab='Planar Longitude', ylab = 'Planar Latitude',main='1980')
savePlot(file.path(project.figuredirectory('bio.lobster'),'temperature1980.png'))

levelplot(J$x.1985~J$plon+J$plat,aspect='iso',xlim=c(-600,100),ylim=c(-450,100),at=mybreaks,col.regions=mypalette,xlab='Planar Longitude', ylab = 'Planar Latitude',main='1985')
savePlot(file.path(project.figuredirectory('bio.lobster'),'temperature1985.png'))

levelplot(J$x.1990~J$plon+J$plat,aspect='iso',xlim=c(-600,100),ylim=c(-450,100),at=mybreaks,col.regions=mypalette,xlab='Planar Longitude', ylab = 'Planar Latitude',main='1990')
savePlot(file.path(project.figuredirectory('bio.lobster'),'temperature1990.png'))

levelplot(J$x.1995~J$plon+J$plat,aspect='iso',xlim=c(-600,100),ylim=c(-450,100),at=mybreaks,col.regions=mypalette,xlab='Planar Longitude', ylab = 'Planar Latitude',main='1995')
savePlot(file.path(project.figuredirectory('bio.lobster'),'temperature1995.png'))

levelplot(J$x.2000~J$plon+J$plat,aspect='iso',xlim=c(-600,100),ylim=c(-450,100),at=mybreaks,col.regions=mypalette,xlab='Planar Longitude', ylab = 'Planar Latitude',main='2000')
savePlot(file.path(project.figuredirectory('bio.lobster'),'temperature2000.png'))

levelplot(J$x.2005~J$plon+J$plat,aspect='iso',xlim=c(-600,100),ylim=c(-450,100),at=mybreaks,col.regions=mypalette,xlab='Planar Longitude', ylab = 'Planar Latitude',main='2005')
savePlot(file.path(project.figuredirectory('bio.lobster'),'temperature2005.png'))

levelplot(J$x.2010~J$plon+J$plat,aspect='iso',xlim=c(-600,100),ylim=c(-450,100),at=mybreaks,col.regions=mypalette,xlab='Planar Longitude', ylab = 'Planar Latitude',main='2010')
savePlot(file.path(project.figuredirectory('bio.lobster'),'temperature2010.png'))

levelplot(J$x.2016~J$plon+J$plat,aspect='iso',xlim=c(-600,100),ylim=c(-450,100),at=mybreaks,col.regions=mypalette,xlab='Planar Longitude', ylab = 'Planar Latitude',main='2016')
savePlot(file.path(project.figuredirectory('bio.lobster'),'temperature2016.png'))


gbm.plot(aa2,variable.no=1,plot.layout=c(1,1),write.title=F)
title('Time (decimal year)')
savePlot(file.path(project.figuredirectory('bio.lobster'),'brtTime.png'))
gbm.plot(aa2,variable.no=2,plot.layout=c(1,1),write.title=F)
title('Temperature')
savePlot(file.path(project.figuredirectory('bio.lobster'),'brtTemperature.png'))
gbm.plot(aa2,variable.no=3,plot.layout=c(1,1),write.title=F)
title('Depth (log(m))')
savePlot(file.path(project.figuredirectory('bio.lobster'),'brtDepth.png'))
gbm.plot(aa2,variable.no=4,plot.layout=c(1,1),write.title=F)
title('Curvature')
savePlot(file.path(project.figuredirectory('bio.lobster'),'brtCurvature.png'))
gbm.plot(aa2,variable.no=5,plot.layout=c(1,1),write.title=F)
title('Slope')
savePlot(file.path(project.figuredirectory('bio.lobster'),'brtSlope.png'))

#self test deviance explained
(aa2$self.statistics$mean.null - aa2$self.statistics$mean.resid) / aa2$self.statistics$mean.null 