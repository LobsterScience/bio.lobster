







	yrs=2005:2018
	bins = seq(20,195,5)
	clf=matrix(NA,length(yrs),length(bins))

	for (l in 1:length(bins)){

		surveyLobsters<-LobsterSurveyProcess(lfa="34", yrs=yrs, mths=c("Aug","Jul","Jun"),bin.size=5, Net='NEST',size.range=c(bins[l],bins[l]+5))


		clf[,l]=SpatialGamLobsterSurvey(surveyLobsters,Years = yrs,lab=paste0("Bins",bins[l]))
	}



















#### below is mostly messing around on stuff
#############################################################################3
############ Do Spatial Model







setwd("bio/TMBcourse/Part2/Day4")

load("simQt.RData")

xyz=data.frame(x = rep(simQt$xy[,1],simQt$nt),y = rep(simQt$xy[,2],simQt$nt),z=log(simQt$Y+1),t=sort(rep(1:simQt$nt,nrow(simQt$xy))))

colfun=colorRampPalette(c("darkblue","cyan","green", "yellow", "orange","darkred", "black"))
pdf(file.path( figdir,"test.pdf"),8,8)
for(i in 1:simQt$nt)print(levelplot(z~x+y,subset(xyz,t==i),at=seq(min(xyz$z),max(xyz$z),l=200),col.regions=colfun(200)))
dev.off()


library(TMB)
compile("estQt.cpp")
dyn.load(dynlib("estQt"))

param<-list()
param$mu <- 0
param$logDelta <- 0
param$logSigma <- 0
param$logitRho <- 0
n<-dim(simQt$Q0)[1]
param$u <- array(0, dim=c(simQt$nt,n))

obj <- MakeADFun(simQt, param, random="u", DLL="estQt")

fit <- nlminb(obj$par, obj$fn, obj$gr)
sdr <- sdreport(obj)
pl <- as.list(sdr,"Est")

estQt<-list(fit=fit, sdr=sdr)


xyz$u=as.vector(pl$u)

colfun=colorRampPalette(c("darkblue","cyan","green", "yellow", "orange","darkred", "black"))
pdf(file.path( figdir,"testu.pdf"),8,8)
for(i in 1:simQt$nt)print(levelplot(u~x+y,subset(xyz,t==i),at=seq(min(xyz$u),max(xyz$u),l=200),col.regions=colfun(200)))
dev.off()


spaceData = subset(AllSurveyData,survey=="LobsterNest"&year==2017&LFA%in%34:38)
library(INLA)
library(TMB)
mesh <- inla.mesh.2d(loc=spaceData[,c("X","Y")], max.edge=1, offset=1)
plot(mesh, asp=1)
points(spaceData[,c("X","Y")], col="red", pch=16)

struct <- inla.spde2.matern(mesh)

compile("spde.cpp")
dyn.load(dynlib("spde"))
Data <- list(Y=spaceData$LobDen, v_i = mesh$idx$loc-1)
Data$spde <- struct$param.inla[c("M0", "M1", "M2")]
Params <- list(beta0 = 0, beta1 = 0, ln_kappa = 0,    ln_tau = 0, omega = rep(0,mesh$n))


obj <- MakeADFun(Data, Params, random = "omega", DLL = "spde")
fit <- nlminb(obj$par, obj$fn, obj$gr)
sdr<-sdreport(obj)
pl<-as.list(sdr,"Est")

image(xtabs(pl$omega~mesh$loc[,1]+mesh$loc[,2]))



      LobSurvNest<-LobsterSurveyProcess(lfa="34",yrs=Years,mths=c("Aug","Jul","Jun"),bin.size=2.5,gear.type='NEST',size.range=Size.range)
      LobSurvBalloon<-LobsterSurveyProcess(lfa="34",yrs=Years,mths=c("Aug","Jul","Jun"),bin.size=2.5,gear.type='280 BALLOON',size.range=Size.range)

      LobSurvBalloon$survey = "LobsterBalloon"
      LobSurvNest$survey = "LobsterNest"

 surveyStations	=	read.csv(file.path(project.datadirectory('bio.lobster'),"data","products","surveyStations.csv"))
    
LobsterSurvey = merge(rbind(LobSurvBalloon,LobSurvNest),surveyStations)

      col.names = c("year","setno","date","X","Y","Z","LobDen","survey","SID")
      LobsterSurvey = subset(LobsterSurvey,!is.na(LobDen),c("YEAR","SET_NO","SET_DATE","X","Y","SET_DEPTH","LobDen","survey","SID"))
      names(LobsterSurvey) = col.names



locs = subset(LobsterSurvey,!duplicated(SID),c("SID","X","Y"))

Y= with(LobsterSurvey,tapply(LobDen,SID,mean))

mesh <- inla.mesh.2d(loc=locs[,c("X","Y")], max.edge=.1, offset=.1)
plot(mesh, asp=1)
points(locs[,c("X","Y")], col="red", pch=16)

struct <- inla.spde2.matern(mesh)

setwd(file.path(project.codedirectory("bio.lobster"),"inst","tmb"))


compile("spde.cpp")
dyn.load(dynlib("spde"))
Data <- list(Y=Y, v_i = mesh$idx$loc-1)
Data$spde <- struct$param.inla[c("M0", "M1", "M2")]
Params <- list(beta0 = 0, beta1 = 0, ln_kappa = 0,    ln_tau = 0, omega = rep(0,mesh$n))


obj <- MakeADFun(Data, Params, random = "omega", DLL = "spde")
fit <- nlminb(obj$par, obj$fn, obj$gr)
sdr<-sdreport(obj)
pl<-as.list(sdr,"Est")

image(xtabs(pl$omega~mesh$loc[,1]+mesh$loc[,2]))

pgrid0 <- inla.mesh.projector(mesh, dims=c(1000,1000))
prd0.m <- inla.mesh.project(pgrid0, pl$omega)

colfun=colorRampPalette(c("darkblue","cyan","green", "yellow", "orange","darkred", "black"))
levelplot(prd0.m,col.regions=colfun(200))






interp.data = data.frame(ID=1:nrow(mesh$loc),X=mesh$loc[,1],Y=mesh$loc[,2],Z=exp(pl$omega))
		lob.contours<-interpolation(interp.data,ticks='define',place=3,nstrata=5,str.min=0,interp.method='gstat',blank=F,res=0.005,smooth=F,idp=3.5)

		# define contour lines
		print(lob.contours$str.def)
		lvls=c(1, 50, 100, 500, 1000, 5000, 10000)
		lvls=lob.contours$str.def
		# generate contour lines
		LFAs<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolys.csv"))

		cont.lst<-contourGen(lob.contours$image.dat,lvls,subset(LFAs,PID==34),col="YlGn",colorAdj=1)

		# plot Map
		pdf(file.path( project.datadirectory("bio.lobster"), "figures","Distribution2017.pdf"),8,11)
		#png(file.path( project.datadirectory("bio.lobster"), "figures","Distribution2017.png"),8,11,units='in',pointsize=12, res=300,type='cairo')
		LobsterMap(ylim=c(42.8,44.6), xlim=c(-67.15,-65.2),mapRes="UR",contours=cont.lst,title="LFA 34 Lobster Density",isobath=seq(50,500,50),bathcol=rgb(0,0,1,0.2),bathy.source='bathy')
		points(SET_LAT~SET_LONG,surveyLobsters34,subset=YEAR==2017,pch=21,cex=0.5,bg='red')#,col=rgb(0,0,0,0.5))
		contLegend("topright",lvls=lvls,Cont.data=cont.lst$Cont.data,title="#/square km",inset=0.02,cex=0.8,bg='white')
		dev.off()





rbPal <- colorRampPalette(c('red','blue'))
mycol <- rbPal(10)[as.numeric(cut(pl$omega,breaks = 10))]
plot(mesh, asp=1)
points(mesh$loc[,1:2], col=mycol, pch=16, cex=3)
points(spde$xy,cex=3)





####################################### R1 Lobster Survey


	
	require(mgcv)

	# Survey Data
	surveyLobsters34index<-LobsterSurveyProcess(lfa="34", yrs=1996:2018, mths=c("Aug","Jul","Jun"), bin.size=2.5, Net='NEST',size.range=c(70,82.5))
	surveyLobsters34index = lonlat2planar(surveyLobsters34index,"utm20", input_names=c("SET_LONG", "SET_LAT"))
	surveyLobsters34index$dyear = decimal_date(as.Date(surveyLobsters34index$SET_DATE))
	surveyLobsters34index$PA = as.numeric(surveyLobsters34index$LobDen>0)
	surveyLobsters34index$LogDen = log(surveyLobsters34index$LobDen)

	# Spatial temporal parameters
	Years = 1996:2018
	LFAs<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolys.csv"))
	LFAs = lonlat2planar(LFAs,"utm20", input_names=c("X", "Y"))
	LFAs = LFAs[,-which(names(LFAs)%in%c('X','Y'))]
	LFAs = rename.df(LFAs,c('plon','plat'),c('X','Y'))

    # Presence-Absence model
    #paMf = formula( PA ~ s(dyear) + s(SET_DEPTH) + s(plon, plat)  )
    paMf = formula( PA ~ as.factor(YEAR) + s(SET_DEPTH) + s(plon, plat, k=100) )
    #paMd = subset(surveyLobsters34index,YEAR>=min(Years),c('PA','plon','plat','dyear','SET_DEPTH'))
    paMd = subset(surveyLobsters34index,YEAR>=min(Years),c('PA','plon','plat','dyear','YEAR','SET_DEPTH'))
	paMo = gam( paMf, data=paMd, family=binomial())
	summary(paMo)
	
    # Abundance model
    #abMf = formula( LogDen ~ s(dyear) + s(SET_DEPTH) + s(plon, plat)  )
    abMf = formula( LogDen ~  as.factor(YEAR) + s(SET_DEPTH) + s(plon, plat, k=100) )
   # abMd = subset(surveyLobsters34index,YEAR>=min(Years)&LobDen>0,c('LogDen','plon','plat','dyear','SET_DEPTH'))
    abMd = subset(surveyLobsters34index,YEAR>=min(Years)&LobDen>0,c('LogDen','plon','plat','dyear','YEAR','SET_DEPTH'))
	abMo = gam( abMf, data=abMd, family=gaussian(link='log'))
	summary(abMo)
	
	# Prediction Surface
	load("/home/hubleyb/bio/EA/data/predspace.rdata") # predSpace
	Ps = data.frame(EID=1:nrow(predSpace),predSpace[,c('plon','plat','z')])
	Ps = rename.df(Ps,c('plon','plat','z'),c('X','Y','SET_DEPTH'))
	key=findPolys(Ps,subset(LFAs,PID==34))
	Ps = subset(Ps,EID%in%key$EID)
	Ps = rename.df(Ps,c('X','Y'),c('plon','plat'))

	# annual predictions
	R1index=c()
	R1surface=list()

	for(i in 1:length(Years)){
	require(mgcv)
			
		#Ps$dyear =Years[i]+.5
		Ps$YEAR =Years[i]

		plo = predict(paMo,Ps,type='response') 
		xyz = data.frame(Ps[,c('plon','plat')],z=plo)
		corners = data.frame(lon=c(-67.8,-65),lat=c(42.5,45))
		planarMap( xyz, fn=paste("gamPAR1",Years[i],sep='.'), annot=Years[i],loc=figdir, corners=corners,save=T)
		#planarMap( xyz, fn=paste("lobster.gambi.pred",Years[i],sep='.'), annot=Years[i],loc="output",corners=corners)
		#planarMap( xyz, corners=corners)


		ldp = predict(abMo,Ps,type='response') 
		#xyz = data.frame(Ps[,c('plon','plat')],z=ldp)
		xyz = data.frame(Ps[,c('plon','plat')],z=exp(ldp)*plo)
		corners = data.frame(lon=c(-67.8,-65),lat=c(42.5,45))
		planarMap( xyz, fn=paste("gamSurveyR1",Years[i],sep='.'), annot=Years[i],loc=figdir, corners=corners, datascale=seq(9.9,21000,l=50),save=T,log.variable=T)
		R1surface[[i]]=xyz

		R1index[i]= sum(xyz$z)
	}
	save(R1surface,file.path(project.datadirectory("bio.lobster"),"output","R1surface34.rdata"))


	plot(Years,R1index/10^6,type='b',ylab="Lobsters (millions)",ylim=c(0,140))

	lines(Years,with(subset(surveyLobsters34index,YEAR>1995),tapply(LobDen,YEAR,mean))*20131/10^6,lty=2,col='red')
	lines(Years,R1index2/10^6,lty=2,col='red')

#####################################	R0

	require(mgcv)

	# Survey Data
	surveyLobsters34index<-LobsterSurveyProcess(lfa="34", yrs=1996:2018, mths=c("Aug","Jul","Jun"), bin.size=2.5, Net='NEST',size.range=c(82.5,95))
	surveyLobsters34index = lonlat2planar(surveyLobsters34index,"utm20", input_names=c("SET_LONG", "SET_LAT"))
	surveyLobsters34index$dyear = decimal_date(as.Date(surveyLobsters34index$SET_DATE))
	surveyLobsters34index$PA = as.numeric(surveyLobsters34index$LobDen>0)
	surveyLobsters34index$LogDen = log(surveyLobsters34index$LobDen)

	# Spatial temporal parameters
	Years = 1996:2018
	LFAs<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolys.csv"))
	LFAs = lonlat2planar(LFAs,"utm20", input_names=c("X", "Y"))
	LFAs = LFAs[,-which(names(LFAs)%in%c('X','Y'))]
	LFAs = rename.df(LFAs,c('plon','plat'),c('X','Y'))

    # Presence-Absence model
    #paMf = formula( PA ~ s(dyear) + s(SET_DEPTH) + s(plon, plat)  )
    paMf = formula( PA ~ as.factor(YEAR) + s(SET_DEPTH)  + s(plon, plat,k=100)  )
    paMd = subset(surveyLobsters34index,YEAR>=min(Years),c('PA','plon','plat','dyear','YEAR','SET_DEPTH'))
	paMo = gam( paMf, data=paMd, family=binomial())
	summary(paMo)
	
    # Abundance model
    #abMf = formula( LogDen ~ s(dyear) + s(SET_DEPTH) + s(plon, plat)  )
    abMf = formula( LogDen ~ as.factor(YEAR) + s(SET_DEPTH) + s(plon, plat,k=100)  )
    abMd = subset(surveyLobsters34index,YEAR>=min(Years)&LobDen>0,c('LogDen','plon','plat','dyear','YEAR','SET_DEPTH'))
	abMo = gam( abMf, data=abMd, family=gaussian(link='log'))
	summary(abMo)
	
	# Prediction Surface
	load("/home/hubleyb/bio/EA/data/predspace.rdata") # predSpace
	Ps = data.frame(EID=1:nrow(predSpace),predSpace[,c('plon','plat','z')])
	Ps = rename.df(Ps,c('plon','plat','z'),c('X','Y','SET_DEPTH'))
	key=findPolys(Ps,subset(LFAs,PID==34))
	Ps = subset(Ps,EID%in%key$EID)
	Ps = rename.df(Ps,c('X','Y'),c('plon','plat'))

	#Ps = Ps[,-which(names(Ps)=='SET_DEPTH')]

	# annual predictions
	R0index=c()

	for(i in 1:length(Years)){
	require(mgcv)
			
		#Ps$dyear =Years[i]+.5
		Ps$YEAR =Years[i]

		plo = predict(paMo,Ps,type='response') 
		xyz = data.frame(Ps[,c('plon','plat')],z=plo)
		corners = data.frame(lon=c(-67.8,-65),lat=c(42.5,45))
		#planarMap( xyz, fn=paste("lobster.gambi.pred",Years[i],sep='.'), annot=Years[i],loc="output",corners=corners)
		#planarMap( xyz, corners=corners)


		ldp = predict(abMo,Ps,type='response') 
		#xyz = data.frame(Ps[,c('plon','plat')],z=ldp)
		xyz = data.frame(Ps[,c('plon','plat')],z=exp(ldp)*plo)
		corners = data.frame(lon=c(-67.8,-65),lat=c(42.5,45))
		planarMap( xyz, fn=paste("gamSurveyR0",Years[i],sep='.'), annot=Years[i],loc=figdir, corners=corners, datascale=seq(9.9,6000,l=50),save=T,log.variable=T)

		R0index[i]= sum(xyz$z)
	}

x11()
	plot(Years,R0index/10^6,type='b',ylab="Lobsters (millions)",ylim=c(0,40))

	lines(Years,with(subset(surveyLobsters34index,YEAR>1995),tapply(LobDen,YEAR,mean))*20131/10^6,lty=2,col='red')





####################################### Other Surveys 


	# Spatial temporal parameters
	Years = 2000:2018
	LFAs<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolys.csv"))
	LFAs = lonlat2planar(LFAs,"utm20", input_names=c("X", "Y"))
	LFAs = LFAs[,-which(names(LFAs)%in%c('X','Y'))]
	LFAs = rename.df(LFAs,c('plon','plat'),c('X','Y'))
	


	# Survey Data


	surveyLobsters<-LobsterSurveyProcess(lfa="34", yrs=1996:2018, mths=c("Aug","Jul","Jun"), bin.size=5, Net='NEST',size.range=c(0,200))

	surveyLobsters<-subset(surveyLobsters,!is.na(CL82))

	surveyLobsters=reshape(surveyLobsters, idvar = "SET_ID", varying = list(which(substr(colnames(surveyLobsters),1,2)=='CL')), v.names = "LobDen2", direction = "long",timevar='CL')

	surveyLobsters<-subset(surveyLobsters,LFA==34&year%in%Years)

	# Survey Data
	surveyLobsters = lonlat2planar(surveyLobsters,"utm20", input_names=c("SET_LONG", "SET_LAT"))
	surveyLobsters$dyear = decimal_date(as.Date(surveyLobsters$SET_DATE))
	surveyLobsters$PA = as.numeric(surveyLobsters$LobDen2>0)
	surveyLobsters$LogDen = log(surveyLobsters$LobDen)


    # Presence-Absence model
    paMf = formula( PA ~ as.factor(YEAR) + s(SET_DEPTH)  + s(plon, plat,k=100) + s(CL,by=YEAR) +s(CL)  )
    paMd = subset(surveyLobsters,YEAR>=min(Years),c('PA','plon','plat','dyear','YEAR','SET_DEPTH','CL'))
	paMo = gam( paMf, data=paMd, family=binomial())
	print(summary(paMo))
	plot(paMo)
	
    # Abundance model
    abMf = formula( LogDen ~ as.factor(YEAR) + s(SET_DEPTH) + s(plon, plat,k=100)  )
    abMd = subset(surveyLobsters,YEAR>=min(Years)&LobDen>0,c('LogDen','plon','plat','dyear','YEAR','SET_DEPTH'))
	abMo = gam( abMf, data=abMd, family=gaussian(link='log'))
	print(summary(abMo))
	
	# Prediction Surface
	load(file.path(project.codedirectory("EA"),"data","predspace.rdata")) # predSpace
	Ps = data.frame(EID=1:nrow(predSpace),predSpace[,c('plon','plat','z')])
	Ps = rename.df(Ps,c('plon','plat','z'),c('X','Y','SET_DEPTH'))
	key=findPolys(Ps,subset(LFAs,PID==34))
	Ps = subset(Ps,EID%in%key$EID)
	Ps = rename.df(Ps,c('X','Y'),c('plon','plat'))


	# annual predictions
	index=c()
	surface=list()

	for(i in 1:length(Years)){
	require(mgcv)
			
		Ps$YEAR =Years[i]

		plo = predict(paMo,Ps,type='response') 
		xyz = data.frame(Ps[,c('plon','plat')],z=plo)
		corners = data.frame(lon=c(-67.8,-65),lat=c(42.5,45))
		if("PA"%in%surface.plots)planarMap( xyz, fn=paste0("gamSurveyPA",lab,Years[i]), annot=Years[i],loc=fd, corners=corners, save=T)


		ldp = predict(abMo,Ps,type='response') 
		xyz = data.frame(Ps[,c('plon','plat')],z=exp(ldp)*plo)
		corners = data.frame(lon=c(-67.8,-65),lat=c(42.5,45))
		if("AB"%in%surface.plots)planarMap( xyz, fn=paste0("gamSurveyAB",lab,Years[i]), annot=Years[i],loc=fd, corners=corners, datascale=seq(legend.scale[1],legend.scale[2],l=50),save=T,log.variable=T)

		surface[[i]]= xyz
		index[i]= sum(xyz$z)

	}
	save(surface,file=file.path(project.datadirectory("bio.lobster"),"outputs",paste0(lab,"surface.rdata")))
	save(index,file=file.path(project.datadirectory("bio.lobster"),"outputs",paste0(lab,"index.rdata")))

	x11()
	plot(Years,index/10^6,type='b',ylab=ylab,ylim=c(0,max(index)/10^6),main=lab)

	return(index)
}



    # Presence-Absence model
    paMf = formula( PA ~ as.factor(year) + s(Z) + s(yday) + s(plon, plat, k=100) )#+ as.factor(survey))
    paMd = subset(surveyLobsters34index,year>=min(Years),c('PA','plon','plat','year','yday','Z'))
	paMo = gam( paMf, data=paMd, family=binomial())
	summary(paMo)
	
    # Abundance model
    abMf = formula( LogDen ~ as.factor(year) + s(Z) + s(yday) + s(plon, plat, k=100) )#+ as.factor(survey))
    abMd = subset(surveyLobsters34index,year>=min(Years)&LobDen>0,c('LogDen','plon','plat','yday','year','Z'))
	abMo = gam( abMf, data=abMd, family=gaussian(link='log'))
	summary(abMo)
	
	# Prediction Surface
	load("/home/hubleyb/bio/EA/data/predspace.rdata") # predSpace
	Ps = data.frame(EID=1:nrow(predSpace),predSpace[,c('plon','plat','z')])
	Ps = rename.df(Ps,c('plon','plat','z'),c('X','Y','Z'))
	key=findPolys(Ps,subset(LFAs,PID==34))
	Ps = subset(Ps,EID%in%key$EID)
	Ps = rename.df(Ps,c('X','Y'),c('plon','plat'))

	# annual predictions
	R1index=c()
	R1surface=list()

	for(i in 1:length(Years)){
	require(mgcv)
			
		Ps$year =Years[i]
		Ps$yday =189

		plo = predict(paMo,Ps,type='response') 
		xyz = data.frame(Ps[,c('plon','plat')],z=plo)
		corners = data.frame(lon=c(-67.8,-65),lat=c(42.5,45))
		#planarMap( xyz, fn=paste("gamPAR1_2",Years[i],sep='.'), annot=Years[i],loc=figdir, corners=corners,save=T)
		#planarMap( xyz, fn=paste("lobster.gambi.pred",Years[i],sep='.'), annot=Years[i],loc="output",corners=corners)
		#planarMap( xyz, corners=corners)


		ldp = predict(abMo,Ps,type='response') 
		#xyz = data.frame(Ps[,c('plon','plat')],z=ldp)
		xyz = data.frame(Ps[,c('plon','plat')],z=exp(ldp)*plo)
		corners = data.frame(lon=c(-67.8,-65),lat=c(42.5,45))
		planarMap( xyz, fn=paste("gamSurveyR1_2",Years[i],sep='.'), annot=Years[i],loc=figdir, corners=corners, datascale=seq(9.9,21000,l=50),save=T,log.variable=T)
		R1surface[[i]]=xyz

		R1index[i]= sum(xyz$z)
	}
	save(R1surface,file.path(project.datadirectory("bio.lobster"),"output","R1surface34.rdata"))


	plot(Years,R1index/10^6,type='b',ylab="Lobsters (millions)",ylim=c(0,140))

	lines(Years,with(subset(surveyLobsters34index,year>=min(Years)),tapply(LobDen,year,mean))*20131/10^6,lty=2,col='red')





####################################### All Sizes




	# Spatial temporal parameters
	Years = 2005:2018
	LFAs<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolys.csv"))
	LFAs = lonlat2planar(LFAs,"utm20", input_names=c("X", "Y"))
	LFAs = LFAs[,-which(names(LFAs)%in%c('X','Y'))]
	LFAs = rename.df(LFAs,c('plon','plat'),c('X','Y'))


	
	require(mgcv)

	# Survey Data
	surveyLobsters34index<-LobsterSurveyProcess(lfa="34", yrs=Years, mths=c("Aug","Jul","Jun"), bin.size=2.5, Net='NEST',size.range=c(70,82.5))
	surveyLobsters34index = lonlat2planar(surveyLobsters34index,"utm20", input_names=c("SET_LONG", "SET_LAT"))
	surveyLobsters34index$dyear = decimal_date(as.Date(surveyLobsters34index$SET_DATE))
	R0s<-LobsterSurveyProcess(lfa="34", yrs=Years, mths=c("Aug","Jul","Jun"), bin.size=2.5, Net='NEST',size.range=c(82.5,95))
	R0s$LobDenR0 = R0s$LobDen
	surveyLobsters34index = merge(surveyLobsters34index,R0s[,c("SET_ID","LobDenR0")])
	surveyLobsters34index$PA = as.numeric((surveyLobsters34index$LobDen+surveyLobsters34index$LobDenR0)>0)
 	surveyLobsters34index$LogDen = log(surveyLobsters34index$LobDen)
 	surveyLobsters34index$LogDenR0 = log(surveyLobsters34index$LobDenR0)
   

    # Presence-Absence model
    #paMf = formula( PA ~ s(dyear) + s(SET_DEPTH) + s(plon, plat)  )
    paMf = formula( PA ~ as.factor(YEAR) + s(SET_DEPTH) + s(plon, plat, k=100) )
    #paMd = subset(surveyLobsters34index,YEAR>=min(Years),c('PA','plon','plat','dyear','SET_DEPTH'))
    paMd = subset(surveyLobsters34index,YEAR>=min(Years),c('PA','plon','plat','dyear','YEAR','SET_DEPTH'))
	paMo = gam( paMf, data=paMd, family=binomial())
	summary(paMo)
	
    # Abundance model
    #abMf = formula( LogDen ~ s(dyear) + s(SET_DEPTH) + s(plon, plat)  )
    abMf = list(formula( LogDen ~  as.factor(YEAR) + s(SET_DEPTH) + s(plon, plat, k=100) ),formula( LogDenR0 ~  as.factor(YEAR) + s(SET_DEPTH) + s(plon, plat, k=100) ))
   # abMd = subset(surveyLobsters34index,YEAR>=min(Years)&LobDen>0,c('LogDen','plon','plat','dyear','SET_DEPTH'))
    abMd = subset(surveyLobsters34index,YEAR>=min(Years)&LobDen>0&LobDenR0>0,c('LogDen','LogDenR0','plon','plat','dyear','YEAR','SET_DEPTH'))
	abMo = gam( abMf, data=abMd, family=mvn(d=2))
	summary(abMo)
	
	# Prediction Surface
	load("/home/hubleyb/bio/EA/data/predspace.rdata") # predSpace
	Ps = data.frame(EID=1:nrow(predSpace),predSpace[,c('plon','plat','z')])
	Ps = rename.df(Ps,c('plon','plat','z'),c('X','Y','SET_DEPTH'))
	key=findPolys(Ps,subset(LFAs,PID==34))
	Ps = subset(Ps,EID%in%key$EID)
	Ps = rename.df(Ps,c('X','Y'),c('plon','plat'))

	# annual predictions
	R1index=c()
	R1surface=list()

	for(i in 1:length(Years)){
	require(mgcv)
			
		#Ps$dyear =Years[i]+.5
		Ps$YEAR =Years[i]

		plo = predict(paMo,Ps,type='response') 
		xyz = data.frame(Ps[,c('plon','plat')],z=plo)
		corners = data.frame(lon=c(-67.8,-65),lat=c(42.5,45))
		planarMap( xyz, fn=paste("gamPAR1",Years[i],sep='.'), annot=Years[i],loc=figdir, corners=corners,save=T)
		#planarMap( xyz, fn=paste("lobster.gambi.pred",Years[i],sep='.'), annot=Years[i],loc="output",corners=corners)
		#planarMap( xyz, corners=corners)


		ldp = predict(abMo,Ps,type='response') 
		#xyz = data.frame(Ps[,c('plon','plat')],z=ldp)
		xyz = data.frame(Ps[,c('plon','plat')],z=exp(ldp)*plo)
		corners = data.frame(lon=c(-67.8,-65),lat=c(42.5,45))
		planarMap( xyz, fn=paste("gamSurveyR1",Years[i],sep='.'), annot=Years[i],loc=figdir, corners=corners, datascale=seq(9.9,21000,l=50),save=T,log.variable=T)
		R1surface[[i]]=xyz

		R1index[i]= sum(xyz$z)
	}
	save(R1surface,file.path(project.datadirectory("bio.lobster"),"output","R1surface34.rdata"))


	plot(Years,R1index/10^6,type='b',ylab="Lobsters (millions)",ylim=c(0,140))

	lines(Years,with(subset(surveyLobsters34index,YEAR>1995),tapply(LobDen,YEAR,mean))*20131/10^6,lty=2,col='red')
	lines(Years,R1index2/10^6,lty=2,col='red')

########

	surveyLobsters34index<-LobsterSurveyProcess(lfa="34", yrs=1996:2018, mths=c("Aug","Jul","Jun"), bin.size=5, Net='NEST',size.range=c(0,200))
	surveyLobsters34index = lonlat2planar(surveyLobsters34index,"utm20", input_names=c("SET_LONG", "SET_LAT"))

	CLF = reshape(CLF[order(CLF$BIN),],idvar='SET_ID',timevar='BIN',direction='wide',sep='')



	surveyLobsters34index$dyear = decimal_date(as.Date(surveyLobsters34index$SET_DATE))
	surveyLobsters34index$PA = as.numeric(surveyLobsters34index$LobDen>0)
	surveyLobsters34index$LogDen = log(surveyLobsters34index$LobDen)

	# Spatial temporal parameters
	Years = 1996:2018
	LFAs<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolys.csv"))
	LFAs = lonlat2planar(LFAs,"utm20", input_names=c("X", "Y"))
	LFAs = LFAs[,-which(names(LFAs)%in%c('X','Y'))]
	LFAs = rename.df(LFAs,c('plon','plat'),c('X','Y'))

    # Presence-Absence model
    paMf = formula( PA ~ s(dyear) + s(SET_DEPTH) + s(plon, plat)  )
    paMd = subset(surveyLobsters34index,YEAR>2000,c('PA','plon','plat','dyear','SET_DEPTH'))
	paMo = gam( paMf, data=paMd, family=binomial())
	summary(paMo)
	
    # Abundance model
    abMf = formula( LogDen ~ s(dyear) + s(SET_DEPTH) + s(plon, plat)  )
    abMd = subset(surveyLobsters34index,YEAR>2000&LobDen>0,c('LogDen','plon','plat','dyear','SET_DEPTH'))
	abMo = gam( abMf, data=abMd, family=gaussian(link='log'))
	summary(abMo)
	
	# Prediction Surface
	load("/home/hubleyb/bio/EA/data/predspace.rdata") # predSpace
	Ps = data.frame(EID=1:nrow(predSpace),predSpace[,c('plon','plat','z')])
	Ps = rename.df(Ps,c('plon','plat','z'),c('X','Y','SET_DEPTH'))
	key=findPolys(Ps,subset(LFAs,PID==34))
	Ps = subset(Ps,EID%in%key$EID)
	Ps = rename.df(Ps,c('X','Y'),c('plon','plat'))

	# annual predictions
	R1index=c()

	for(i in 1:length(Years)){
	require(mgcv)
			
		Ps$dyear =Years[i]+.5

		plo = predict(paMo,Ps,type='response') 
		xyz = data.frame(Ps[,c('plon','plat')],z=plo)
		corners = data.frame(lon=c(-67.8,-65),lat=c(42.5,45))
		#planarMap( xyz, fn=paste("lobster.gambi.pred",Years[i],sep='.'), annot=Years[i],loc="output",corners=corners)
		#planarMap( xyz, corners=corners)


		ldp = predict(abMo,Ps,type='response') 
		#xyz = data.frame(Ps[,c('plon','plat')],z=ldp)
		xyz = data.frame(Ps[,c('plon','plat')],z=ldp*plo)
		corners = data.frame(lon=c(-67.8,-65),lat=c(42.5,45))
		planarMap( xyz, fn=paste("gamSurveyR1",Years[i],sep='.'), annot=Years[i],loc=figdir, corners=corners, datascale=seq(9.9,6000,l=50),save=T,log.variable=T)

		R1index[i]= sum(exp(xyz$z))
	}


	plot(Years,R1index/10^6,type='b',ylab="Lobsters (millions)",ylim=c(0,140))

	lines(Years,with(subset(surveyLobsters34index,YEAR>2000),tapply(LobDen,YEAR,mean))*20131/10^6,lty=2,col='red')


###

	AllSurveyDataR1 = SurveyTowData(Size.range=c(70,82.5),Sex = c(1,2,3), Years=1996:2018,redo=T,lab="R1")
	AllSurveyDataR1 = lonlat2planar(AllSurveyDataR1,"utm20", input_names=c("SET_LONG", "SET_LAT"))


    # identify locations of data relative to baseline for envionmental data
	require(bio.bathymetry)
	baseLineDepth = bathymetry.db(p=p, DS="complete")
    locsmap = match( 
        lbm::array_map( "xy->1", AllSurveyDataR1[,c("plon","plat")], gridparams=p$gridparams ), 
        lbm::array_map( "xy->1", baseLineDepth, gridparams=p$gridparams ) )





#GWmodel

	library(GWmodel)

	surveyLobsters34index<-LobsterSurveyProcess(lfa="34", yrs=1996:2018, mths=c("Aug","Jul","Jun"), bin.size=2.5, Net='NEST',size.range=c(70,82.5))
	lobSurv.spdf <- SpatialPointsDataFrame(surveyLobsters34index[,c("plon","plat")], surveyLobsters34index)
	DM <- gw.dist(dp.locat = coordinates(lobSurv.spdf))

