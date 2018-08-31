require(bio.lobster)
require(bio.utilities)
require(gamlss)
options(stringsAsFactors=F)

	#surveyLobsters34<-LobsterSurveyProcess(lfa="34",yrs=1996:2017,mths=c("Jul","Jun"),bin.size=1)
 	#comparativeStations = sort(subset(surveyLobsters34,YEAR==2016&GEAR=="280 BALLOON")$STATION)
	#ComparativeSurvey = subset(surveyLobsters34,YEAR==2016&STATION%in%comparativeStations)
	#saveRDS(ComparativeSurvey,file=file.path(project.datadirectory('bio.lobster'),"outputs","comparativeSurvey2.rds"))
	

ComparativeSurvey = readRDS(file=file.path(project.datadirectory('bio.lobster'),"outputs","comparativeSurvey2.rds"))

i = ComparativeSurvey$STATION[which(ComparativeSurvey$AREA_SWEPT<0.005)]
ComparativeSurvey = subset(ComparativeSurvey,!STATION %in% i)

CS = ComparativeSurvey[,c(grep('SET_ID',names(ComparativeSurvey)),grep('CL',names(ComparativeSurvey)))]

CS$FISHSET_ID <- NULL

CS = reshape(CS,idvar='SET_ID',varying=list(2:ncol(CS)),timevar='Length',direction='long')
CS$Length = CS$Length-1
CS = na.zero(CS)
vars = c('SET_ID','STATION','GEAR','LobDen','HAUL_DEPTH')
CSa = merge(CS,ComparativeSurvey[,vars],by='SET_ID',all.x=T)


#Remove lengths that were not represented in any tow
af = aggregate(CL0~Length+GEAR,data=CSa,FUN=sum)
i = af$Length[which(af$CL0==0)]

CSa = subset(CSa,!Length %in% i)

CSn = subset(CSa,GEAR=='NEST', select = c(STATION,Length,CL0)); CSn = rename.df(CSn,'CL0','NEST')
CSb = subset(CSa,GEAR=='280 BALLOON',select = c(STATION,Length,CL0)); CSb = rename.df(CSb,'CL0','BALL')
dat = merge(CSn,CSb,by=c('STATION','Length'))
dat$NEST = round(dat$NEST)
dat$BALL = round(dat$BALL) 
dat = subset(dat,Length>45)
#dat = subset(dat,!STATION %in% '83H')
out = gamlss(cbind(NEST,BALL)~cs(Length,df=3),nu.formula=~cs(Length,df=3),data=dat,family=ZIBB())
newd = data.frame(Length=46:144)
mu = predict(out,what='mu',type='response',newdata=newd)
nu = predict(out,what='nu',type='response',newdata=newd)

rho = mu / (1-mu+nu)

ov = aggregate(cbind(NEST,BALL)~Length,data=dat,FUN=sum)
ov$C = ov$NEST / ov$BALL

with(ov,plot(Length,C))
lines(newd[,1],rho)

#bootstrapping across stations- assuming that the sampling unit is the station and that each unit is a sample of the population

st = unique(dat$STATION)
niter=2500
newd = data.frame(Length=46:144)
ou = matrix(NA,nrow=length(newd[,1]),ncol=niter,byrow=F)
for(i in 1:niter){
		stt = sample(st,length(st),replace=T)
		d1 = list()
		for(j in 1:length(stt)) {
				d1[[j]] = subset(dat,STATION== stt[j])
			}
			d1 = as.data.frame(do.call(rbind,d1))
			out = gamlss(cbind(NEST,BALL)~cs(Length,df=3),nu.formula=~cs(Length,df=3),data=d1,family=ZIBB())
			mu = predict(out,what='mu',type='response',newdata=newd)
			nu = predict(out,what='nu',type='response',newdata=newd)
			rho = mu / (1-mu+nu)
			ou[,i] = rho
			}

save(ou,file=file.path(project.datadirectory('bio.lobster'),'data','survey','bootRhoNestBall.rdata'))

sou = as.data.frame(cbind(newd[,1],t(apply(ou,1,quantile,c(0.5,.0275,.975))),(apply(ou,1,mean)),(apply(ou,1,sd))))
names(sou) = c('Length','Median','L95','U95','Mean','SD')
sou$CV = sou$SD / sou$Mean
saveRDS(sou,file=file.path(project.datadirectory('bio.lobster'),'data','survey','summarybootRhoNestBall.rds'))


with(sou,{
	plot(Length,Median,xlab='Carapace Length (mm)',ylab=expression(paste('Conversion Coefficient (',rho,')')),type='l',lwd=1.5,ylim=c(0,18))
	lines(Length,L95,type='l',lwd=1.5,lty=2)
	lines(Length,U95,type='l',lwd=1.5,lty=2)
	points(ov$Length,ov$C,pch=16,cex=.75)
})
savePlot(file=file.path(project.datadirectory('bio.lobster'),'data','survey','ConvNestBall.png'),type='png')

plot(sou$Length,sou$CV,type='l',xlab = 'Carapace Length',ylab = expression(paste('CV of ',rho)),lwd=1.5,ylim=c(0,.6))
savePlot(file=file.path(project.datadirectory('bio.lobster'),'data','survey','CVConvNestBall.png'),type='png')
