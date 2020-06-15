#lobster landings trajectory

require(bio.lobster)
require(bio.utilities)

a = lobster.db('annual.landings')
a = subset(a,YR<2018 & YR>1969)
a$LFA31A <- a$LFA31B <- a$LFA41 <- a$LFA28 <- NULL

a$a27=a$LFA27 
a$CBES = a$LFA29 + a$LFA30 + a$LFA31 + a$LFA32
a$SWN = a$LFA33 + a$LFA34
a$BOF = a$LFA35 + a$LFA36+a$LFA38
a[,2:ncol(a)] <- apply(a[,2:ncol(a)],2,scale)
b = a[-nrow(a),]

b[,2:ncol(b)] <- apply(a[,2:ncol(a)],2,diff)

matplot(a[,1],a[,2:ncol(a)],type='l',col=c(rep('black',5),rep('blue',2),rep('red',3),'orange','black','blue','red'),lwd=c(rep(1,10),3,3,3,3),lty=1)

matplot(a[,1],a[,(ncol(a)-3):ncol(a)],type='l',col=c('orange','black','blue','red'),lwd=2,lty=1,ylab='Landings',xlab='Year')
legend('topleft',c('LFA27', 'South Cape Breton and Eastern Shore','Southwest Nova Scotia','Bay of Fundy'),lty=c(1,1,1,1),lwd=c(2,2,2,2),col=c('orange','black','blue','red'),,bty='n',cex=0.8)


plot(1,1,ylim=c(-1.2,1.2),xlim=c(1970,2017),xlab='Year',ylab=expression(Delta* Z.Landings))
r=ncol(b)
r = (r-3):r
cols=c('orange','black','blue','red')
plus = c(0,.15,.25,.35)
for(i in 1:length(r)){
	lines(b$YR+plus[i],b[,r[i]],type='h',col=cols[i],lwd=2)
	}
	savePlot('~/tmp/DeltaLandings.png',type='png')

matplot(b$YR,b[,(ncol(b)-4):(ncol(b)-1)],type='h',,lwd=2,lty=1,ylab='Landings',xlab='Year')
legend('topleft',c('LFA27', 'South Cape Breton and Eastern Shore','Southwest Nova Scotia','Bay of Fundy'),lty=c(1,1,1,1),lwd=c(2,2,2,2),col=c('orange','black','blue','red'),,bty='n',cex=0.8)

#environmental data

	lobster.db('fsrs')
	fsrs$SYEAR<-fsrs$HAUL_YEAR
	fsrs$HAUL_DATE<-as.Date(fsrs$HAUL_DATE)
	
	fsrsT = subset(fsrs,TEMP>-90)
	fsrsT$Dloc = paste(fsrsT$HAUL_DATE,fsrsT$LATITUDE,fsrsT$LONGITUDE)

	fsrsT = subset(fsrsT,!duplicated(Dloc))

	LobsterTemp = fsrsT[,c('LFA','SYEAR',"HAUL_DATE","LONG_DD","LAT_DD","TEMP","DEPTH")]

	dat = aggregate(cbind(TEMP,DEPTH)~SYEAR+LFA,data=LobsterTemp,FUN=mean)

	with(subset(dat,LFA==27),plot(SYEAR,TEMP,type='l'))

		a = read.csv(file.path( project.datadirectory("bio.lobster"),"Temperature Data","TempData.csv"))
		a$YEAR = year(a$DATE)
		a$MM = month(a$DATE)

		gg = aggregate(cbind(TEMPERATURE,DEPTH)~YEAR+LFA,data=subset(a,DEPTH<50 & MM %in% c(4,5,6)),FUN=mean)

		with(subset(gg,LFA==27),plot(YEAR,TEMPERATURE,type='l'))