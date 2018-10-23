
  
p = bio.lobster::load.environment()
la()

require(gamlss)




	surveyLobsters2017 = LobsterSurveyProcess(lfa="34",yrs=2017,mths=c("Aug","Jul","Jun"),bin.size=1,Net='NEST')
	surveyLobsters2016 = LobsterSurveyProcess(lfa="34",yrs=2016,mths=c("Aug","Jul","Jun"),bin.size=1,Net='NEST')
	scalSurv<-ScallopSurveyProcess(size.range=c(0,200),bin.size=1)
	scalSurv2017 = subset(scalSurv,YEAR==2017&MGT_AREA_ID==3)
	scalSurv2016 = subset(scalSurv,YEAR==2016&MGT_AREA_ID==3)


	#2017
	events = scalSurv2017
	events$X = events$lon
	events$Y = events$lat
	tows = surveyLobsters2017
	tows$X = tows$SET_LONG
	tows$Y = tows$SET_LAT

	lt.list = linkTows(events,tows,mindist=10)

	LobsterMap('34')
	points(SET_LAT~SET_LONG,lt.list$tows,col=EID)
 	points(lat~lon,lt.list$events,pch=16,cex=.5,col=NNwhich)

 	
#lt.list$events
#SS=aggregate(. ~ NNwhich, data = lt.list$events[,c(which(names(lt.list$events)=="NNwhich"),grep('CL',names(lt.list$events)))], mean,na.rm=T)
#names(SS)[1] = "EID"
#SS$SURVEY = "scallop"
#LS=subset(lt.list$tows,EID%in%lt.list$events$NNwhich,c(which(names(lt.list$tows)=="EID"),grep('CL',names(lt.list$tows))))
#LS$SURVEY = "lobster"
#
#CS1 = rbind(SS,LS)
#CS1$YEAR = 2017
#CS1$EID = CS1$EID + 1000

CS1 = data.frame(Year=2017,Length=1:200,Scallop=colMeans(lt.list$events[,grep('CL',names(lt.list$events))]),Lobster=colMeans(subset(lt.list$tows,EID%in%lt.list$events$NNwhich,grep('CL',names(lt.list$tows)))))

	#2016
	events = scalSurv2016
	events$X = events$lon
	events$Y = events$lat
	tows = surveyLobsters2016
	tows$X = tows$SET_LONG
	tows$Y = tows$SET_LAT

	lt.list = linkTows(events,tows,mindist=10)

	LobsterMap('34')
	points(SET_LAT~SET_LONG,lt.list$tows,col=EID)
 	points(lat~lon,lt.list$events,pch=16,cex=.5,col=NNwhich)

 CS2 = data.frame(Year=2016,Length=1:200,Scallop=colMeans(lt.list$events[,grep('CL',names(lt.list$events))]),Lobster=colMeans(subset(lt.list$tows,EID%in%lt.list$events$NNwhich,grep('CL',names(lt.list$tows)))))
	
#lt.list$events
#SS=aggregate(. ~ NNwhich, data = lt.list$events[,c(which(names(lt.list$events)=="NNwhich"),grep('CL',names(lt.list$events)))], mean,na.rm=T)
#names(SS)[1] = "EID"
#SS$SURVEY = "scallop"
#LS=subset(lt.list$tows,EID%in%SS$EID,c(which(names(lt.list$tows)=="EID"),grep('CL',names(lt.list$tows))))
#LS$SURVEY = "lobster"
#
#
#
#
#CS2 = rbind(SS,LS)
#CS2$YEAR = 2016
#CS2$EID = CS2$EID + 2000
#
#CS = rbind(CS1,CS2)
#CS$SET_ID = 1:nrow(CS)
#d1 = CS[c("SET_ID","EID","SURVEY","YEAR")]
#d2 = CS[,c(which(names(CS)=="SET_ID"),grep('CL',names(CS)))]
#
#CS = reshape(d2,idvar='SET_ID',varying=list(2:ncol(d2)),timevar='Length',direction='long')
#CS = na.zero(CS)
#CSa = merge(d1,CS,by='SET_ID',all.y=T)
#
##af = aggregate(CL1~Length+SURVEY,data=CSa,FUN=sum)
##i = af$Length[which(af$CL1==0)]
##CSa = subset(CSa,!Length %in% i)
#
#
#CSl = subset(CSa,SURVEY=='lobster', select = c(EID,Length,CL1)); CSl = rename.df(CSl,'CL1','lobster')
#CSs = subset(CSa,SURVEY=='scallop',select = c(EID,Length,CL1)); CSs = rename.df(CSs,'CL1','scallop')
#dat = merge(CSl,CSs,by=c('EID','Length'))
#dat$lobster = round(dat$lobster)
#dat$scallop = round(dat$scallop) 

dat = rbind(CS1,CS2)

dat = subset(dat,Length<83)

#out = gamlss(cbind(scallop,lobster)~cs(Length,df=3),nu.formula=~cs(Length,df=3),data=dat,family=ZIBB())
out = gamlss(cbind(Lobster,Scallop)~cs(Length,df=3),data=dat,family=BB())

newd = data.frame(Length=1:82)
mu = predict(out,what='mu',type='response',newdata=newd)
#nu = predict(out,what='nu',type='response',newdata=newd)

#rho = mu / (1-mu+nu)
rho = mu / (1-mu)

#ov = aggregate(cbind(lobster,scallop)~Length,data=dat,FUN=sum)
#ov$C = ov$lobster / ov$scallop
#ov$C = ov$scallop / ov$lobster
x11()
#with(ov,plot(Length,C))
#lines(newd[,1],rho)
with(dat,plot(Length,Lobster/Scallop,type='n'))
 with(subset(dat,Year==2016),points(Length,Lobster/Scallop,col='red'))
with(subset(dat,Year==2017),points(Length,Lobster/Scallop,col='blue'))

lines(newd[,1],rho)
abline(h=1,lty=3)
savePlot(file=file.path(project.datadirectory('bio.lobster'),'data','survey','ConvLobScal.png'),type='png')
saveRDS(data.frame(length=newd[,1],rho=rho),file=file.path(project.datadirectory('bio.lobster'),'data','survey','RhoLobScal.rds'))
