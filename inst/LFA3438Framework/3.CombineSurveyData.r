  
	p = bio.lobster::load.environment()
	la()

    figdir = file.path(project.datadirectory("bio.lobster"),"figures","LFA3438Framework2019")


#####################################################
######## Plot Data

# R1 = sub-legal sized lobsters expected to recruit to fishery after on more molt
AllSurveyDataR1 = SurveyTowData(Size.range=c(70,82.5),Sex = c(1,2,3), Years=1970:2018,redo=F)
#AllSurveyDataR1 =assignArea(AllSurveyDataR1,coords=c("X","Y"))

	surveyplotdata = AllSurveyDataR1

	surveyplotdata$fill.cols=NA
	surveyplotdata$fill.cols[surveyplotdata$survey=="LobsterNest"] = rgb(1,0,0,0.7)
	surveyplotdata$fill.cols[surveyplotdata$survey=="LobsterBalloon"] = rgb(1,0,0,0.7)
	surveyplotdata$fill.cols[surveyplotdata$survey=="Scallop"] = rgb(0,0,1,0.7)
	surveyplotdata$fill.cols[surveyplotdata$survey=="DFOsummer"] = rgb(0,1,0,0.7)
	surveyplotdata$fill.cols[surveyplotdata$survey=="NEFSCfall"] = rgb(0,1,0,0.7)


	for(i in 1970:2018){

		data = with(subset(surveyplotdata,year==i),data.frame(den=LobDen,x=X,y=Y,line.cols='black',fill.cols=fill.cols))
		zeros = subset(data,den==0)
		data = subset(data,den>0)
		if(nrow(data)==0) next

		# map
		pdf(file.path( figdir,paste0("SurveyBubblesR1",i,".pdf")),8,8)
		LobsterMap("34-38",title=paste("Surveys R1",i),isobath=seq(50,500,50),bathcol=rgb(0,0,1,0.2),bathy.source='bathy')
		points(y~x,zeros,pch=4,col=zeros$fill.cols)
		surveyBubbles(data,scaler=0.5,pie=F)
		dev.off()
	}


# R0 = legal sized lobsters that have recruited to fishery after their last molt
AllSurveyDataR0 = SurveyTowData(Size.range=c(82.5,95),Sex = c(1,2,3), Years=1970:2018,redo=F)
AllSurveyDataR0 =assignArea(AllSurveyDataR0,coords=c("X","Y"))

	surveyplotdata = AllSurveyDataR0

	surveyplotdata$fill.cols=NA
	surveyplotdata$fill.cols[surveyplotdata$survey=="LobsterNest"] = rgb(1,0,0,0.7)
	surveyplotdata$fill.cols[surveyplotdata$survey=="LobsterBalloon"] = rgb(1,0,0,0.7)
	surveyplotdata$fill.cols[surveyplotdata$survey=="Scallop"] = rgb(0,0,1,0.7)
	surveyplotdata$fill.cols[surveyplotdata$survey=="DFOsummer"] = rgb(0,1,0,0.7)
	surveyplotdata$fill.cols[surveyplotdata$survey=="NEFSCfall"] = rgb(0,1,0,0.7)


	for(i in 1970:2018){

		data = with(subset(surveyplotdata,year==i),data.frame(den=LobDen,x=X,y=Y,line.cols='black',fill.cols=fill.cols))
		zeros = subset(data,den==0)
		data = subset(data,den>0)
		if(nrow(data)==0) next

		# map
		pdf(file.path( figdir,paste0("SurveyBubblesR0",i,".pdf")),8,8)
		LobsterMap("34-38",title=paste("Surveys R0",i),isobath=seq(50,500,50),bathcol=rgb(0,0,1,0.2),bathy.source='bathy')
		points(y~x,zeros,pch=4,col=zeros$fill.cols)
		surveyBubbles(data,scaler=0.5,pie=F)
		dev.off()
	}





#############################################################################3
############ Do Model







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
pl
