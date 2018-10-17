####### Update 35: Sept 2018
require(bio.lobster)
require(ggplot2)
require(bio.utilities)
	## Landings


AnnualSlip<-lobster.db( DS="annual.landings")
SeasonalSlip<-lobster.db( DS="seasonal.landings")
SeasonalSlip$YEAR<-as.numeric(substr(SeasonalSlip$SYEAR,6,9))
dat = subset(SeasonalSlip,YEAR<2018)
dat$SYEAR <- NULL

#Historic<-lobster.db( DS="historical.landings")



###stacked bar plot


library(reshape2)

dat2 <- melt(dat, id.vars = "YEAR")
dat2 = subset(dat2, variable %in% c('LFA35','LFA36','LFA38'))
Da = aggregate(value~YEAR,data=dat2,FUN=sum)
Da = as.data.frame(mavg(Da))
Da$V1 = Da$V1+1
Da = na.omit(Da)
names(Da) = c('YEAR','value')
Da$variable = 'Running Mean'

library(ggplot2)
ggplot(dat2, aes(x=YEAR, y=value, fill=variable)) + 
  geom_bar(stat="identity") +
  theme_bw()+
scale_fill_grey()+
scale_x_continuous(breaks=seq(1980,2015,5),labels=paste(seq(1979,2014,5),seq(1980,2015,5),sep="-"))+
scale_y_continuous(breaks=seq(0,15000,2500))+
geom_line(data=Da,color='red',size=1.5) +
geom_hline(yintercept=c(1575,788),linetype=c('dashed','dotted'),color=c('black','black'),size=1.5)+
	theme(legend.position='none',axis.text.x=element_text(angle=90, hjust=1, vjust=0.5,size=10, colour='black'),panel.background=element_rect(fill="white", colour='white'),panel.border=element_rect(,fill=NA,colour='black'),axis.text.y=element_text(size=10, colour='black'),
					axis.title.y= element_text(size=15),panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + xlab('') + ylab('Landings (t)') 
savePlot(file.path( project.figuredirectory("bio.lobster"), "LFA35-38Landings2018.png"),type='png')

	dev.off()
		


####each LFA alone

	#Seasonal35<-subset(SeasonalSlip,select=c("LFA35","SYEAR","YEAR"))
		Seasonal35<-subset(SeasonalSlip,select=c("LFA38","SYEAR","YEAR"))


	require(ggplot2)
cols = c(rep('navyblue',times=42),'green')
	# Plot Landings (1892-present) Figure 2
	pdf(file.path( project.figuredirectory("bio.lobster"), "LFA38Landings2018.pdf"),8,6)
	ggplot(subset(Seasonal35),aes(YEAR,LFA38)) + geom_bar(data=Seasonal35,fill=cols,stat='identity') +
		scale_fill_manual(values=cols) +
		geom_line(data=Seasonal35,colour='red',size=1) +
		scale_y_continuous(breaks=seq(0, 25000, 2000)) + scale_x_continuous(breaks=seq(1890, 2020, 5)) +
		geom_hline(yintercept=c(518,259),linetype=c('dashed','dotted'),color=c('black','black'),size=1.5) +
		theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5,size=10, colour='black'),panel.background=element_rect(fill="white", colour='black'),panel.border=element_rect(,fill=NA,colour='black'),axis.text.y=element_text(size=10, colour='black'),
					axis.title.y= element_text(size=15)) + xlab('') + ylab('Landings (t)') 

	dev.off()



#35-38 combined

	SeasonalSlip$YEAR<-as.numeric(substr(SeasonalSlip$SYEAR,6,9))
	SeasonalSlip$LFA35 = SeasonalSlip$LFA35 + SeasonalSlip$LFA36 + SeasonalSlip$LFA38
	Seasonal35<-subset(SeasonalSlip,select=c("LFA35","SYEAR","YEAR"))

	require(ggplot2)
cols = c(rep('navyblue',times=42),'green')
	# Plot Landings (1892-present) Figure 2
	pdf(file.path( project.figuredirectory("bio.lobster"), "LFA35-38Landings2018.pdf"),8,6)
	ggplot(subset(Seasonal35),aes(YEAR,LFA35)) + geom_bar(data=Seasonal35,fill=cols,stat='identity') +
		scale_fill_manual(values=cols) +
		geom_line(data=Seasonal35,colour='red',size=1) +
		scale_y_continuous(breaks=seq(0, 25000, 2000)) + scale_x_continuous(breaks=seq(1890, 2020, 5)) +
		geom_hline(yintercept=c(1636,746),linetype=c('dashed','dotted'),color=c('yellow','yellow'),size=1.5) +
		theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5,size=10, colour='black'),panel.background=element_rect(fill="white", colour='black'),panel.border=element_rect(,fill=NA,colour='black'),axis.text.y=element_text(size=10, colour='black'),
					axis.title.y= element_text(size=15)) + xlab('') + ylab('Landings (t)') 
	dev.off()


	## Commercial catch rate index

				logDat <- lobster.db('process.logs')
				logDat <- subset(logDat,SYEAR %in% 2003:2018 & LFA %in% c(38))
		
				catch.new  <- with(logDat,tapply(WEIGHT_KG,SYEAR,sum))
				effort.new <- with(logDat,tapply(NUM_OF_TRAPS,SYEAR,sum))
				n.new      <- with(logDat,tapply(NUM_OF_TRAPS,SYEAR,length))

				cpueLFA35.dat <- data.frame(year=2005:2018,n=c(n.new),catch=c(catch.new),effort=c(effort.new),cpue=c(catch.new/effort.new))

			write.csv(cpueLFA35.dat,file.path(project.datadirectory('bio.lobster'),'data',"LFA38CPUE2018.csv"),row.names=F)

	KgPTH<- cpueLFA35.dat$cpue
	yrs<-sort(as.numeric(cpueLFA35.dat$year))
	rmKgPTH<-mavg(KgPTH)

	# Plot Commercial CPUE Figure 3

	pdf(file.path( project.figuredirectory("bio.lobster"), "LFA38CommercialCPUE2018.pdf"),8,6)
	plot(yrs,KgPTH,pch=16,ylim=c(0,max(KgPTH)),xlab='',ylab='CPUE (Kg/TH)',las=1, main="LFA 38 - Commercial Log CPUE",xaxt='n',col=c(rep('black',times=(length(yrs)-1)),'green'))
	axis(1,yrs,lab=paste(yrs-1,substr(yrs,3,4),sep='-'),las=2)
	lines(yrs[-(1:2)],rmKgPTH[!is.na(rmKgPTH)],lty=2,col='orange',lwd=2)
	abline(h=median(KgPTH[1:4])*.5,col=rgb(0,0,1,0.5))
	text(max(yrs)+.5,median(KgPTH[2:5])*.85,"Upper Stock Reference",col=rgb(0,0,1,0.5),pos=2,cex=0.8)
	dev.off()


#lfa 35-38
#with current imcomplete year

				logDat <- lobster.db('process.logs')
				logDat <- subset(logDat,SYEAR %in% 2005:2018 & LFA %in% c(35,36,38))
		
				catch.new  <- with(logDat,tapply(WEIGHT_KG,SYEAR,sum))
				effort.new <- with(logDat,tapply(NUM_OF_TRAPS,SYEAR,sum))
				n.new      <- with(logDat,tapply(NUM_OF_TRAPS,SYEAR,length))

				cpueLFA35.dat <- data.frame(year=2005:2018,n=c(n.new),catch=c(catch.new),effort=c(effort.new),cpue=c(catch.new/effort.new))

			write.csv(cpueLFA35.dat,file.path(project.datadirectory('bio.lobster'),'data',"LFA35-38CPUE2018.csv"),row.names=F)

	KgPTH<- cpueLFA35.dat$cpue
	yrs<-sort(as.numeric(cpueLFA35.dat$year))
	rmKgPTH<-mavg(KgPTH)

	# Plot Commercial CPUE Figure 3

	pdf(file.path( project.figuredirectory("bio.lobster"), "LFA35-38CommercialCPUE2018.pdf"),8,6)
	plot(yrs,KgPTH,pch=16,ylim=c(0,max(KgPTH)),xlab='',ylab='CPUE (Kg/TH)',las=1, main="LFA 35 - 38 - Commercial Log CPUE",xaxt='n',col=c(rep('black',times=(length(yrs)-1)),'green'))
	axis(1,yrs,lab=paste(yrs-1,substr(yrs,3,4),sep='-'),las=2)
	lines(yrs[-(1:2)],rmKgPTH[!is.na(rmKgPTH)],lty=2,col='orange',lwd=2)
	abline(h=median(KgPTH[2:5])*.5,col=rgb(0,0,1,0.5))
	text(max(yrs)+.5,median(KgPTH[2:5])*.85,"Upper Stock Reference",col=rgb(0,0,1,0.5),pos=2,cex=0.8)
	dev.off()


####with only complete years


#lfa 35-38
#with current imcomplete year

				logDat <- lobster.db('process.logs')
				logDat <- subset(logDat,SYEAR %in% 2005:2017 & LFA %in% c(35,36,38))
		
				catch.new  <- with(logDat,tapply(WEIGHT_KG,SYEAR,sum))
				effort.new <- with(logDat,tapply(NUM_OF_TRAPS,SYEAR,sum))
				n.new      <- with(logDat,tapply(NUM_OF_TRAPS,SYEAR,length))

				cpueLFA35.dat <- data.frame(year=2005:2017,n=c(n.new),catch=c(catch.new),effort=c(effort.new),cpue=c(catch.new/effort.new))

			write.csv(cpueLFA35.dat,file.path(project.datadirectory('bio.lobster'),'data',"LFA35-38CPUEcomplete2018.csv"),row.names=F)

	KgPTH<- cpueLFA35.dat$cpue
	yrs<-sort(as.numeric(cpueLFA35.dat$year))
	rmKgPTH<-mavg(KgPTH)

	# Plot Commercial CPUE Figure 3
	plot(yrs,KgPTH,pch=16,ylim=c(0,max(KgPTH)),xlab='',ylab='CPUE (Kg/TH)',las=1, main="LFA 35 - 38 - Commercial Log CPUE",xaxt='n',col='black')
	axis(1,yrs,lab=paste(yrs-1,substr(yrs,3,4),sep='-'),las=2)
	lines(yrs[-(1:2)],rmKgPTH[!is.na(rmKgPTH)],lty=2,col='orange',lwd=2)
	abline(h=median(KgPTH[2:5])*.5,col=rgb(0,0,1,0.5))
	text(max(yrs)+.5,median(KgPTH[2:5])*.85,"Upper Stock Reference",col=rgb(0,0,1,0.5),pos=2,cex=0.8)
	savePlot(file.path( project.figuredirectory("bio.lobster"), "LFA35-38CPUE2018.png"),type='png')



###############RV survey


require(bio.survey)
require(bio.lobster)
require(bio.groundfish)
la()

p = bio.lobster::load.environment()
p$libs = NULL
fp = file.path(project.datadirectory('bio.lobster'),"analysis")
load_all('~/git/bio.survey/')

if(get.odbc.data){
        p = bio.groundfish::load.groundfish.environment(assessment.year = 2018)
        # these should be run on a windows machine: NULL values get mangled for some reason
        p$odbc.data.yrs=1970:2018 #  <<<<< ---- DATA YEAR can be a single year update too
        groundfish.db( DS="odbc.redo", datayrs=p$odbc.data.yrs )
        groundfish.db( DS="gscat.redo" )
        groundfish.db( DS="gsdet.redo" )
        groundfish.db( DS="gsinf.redo" )
      }

##############################################################
#DFO RV Setup

      p$series =c('summer')# p$series =c('georges');p$series =c('fall')
      p$define.by.polygons = F
      p$lobster.subunits=F
      p$area = 'custom'
      p$years.to.estimate = c(1970:2018)
      p$length.based = F
      p$by.sex = F
      p$bootstrapped.ci=T
      p$strata.files.return=F
      p$vessel.correction.fixed=1.2
      p$strat = 490:495
      p$clusters = c( rep( "localhost", 7) )
      p$strata.efficiencies = F
      p = make.list(list(yrs=p$years.to.estimate),Y=p)
      p$reweight.strata = F #this subsets 
      
      aout= dfo.rv.analysis(DS='stratified.estimates.redo',p=p)
      

         #Figure
                              p$add.reference.lines = T
                              p$time.series.start.year = p$years.to.estimate[1]
                              p$time.series.end.year = p$years.to.estimate[length(p$years.to.estimate)]
                              p$metric = 'numbers' #weights
                              p$measure = 'stratified.mean' #'stratified.total'
                              p$figure.title = ""
                              p$reference.measure = 'mean' # mean, geomean
                              p$file.name = 'lfa35-382018.png'
                              p$error.polygon=F


                              p$y.maximum = NULL # NULL # if ymax is too high for one year
                              p$show.truncated.numbers = F #if using ymax and want to show the numbers that are cut off as values on figure

                                p$legend = FALSE
                                p$running.mean = T
                                p$running.length = 3
                                p$running.median = F #can only have rmedian or rmean
                                p$error.bars=T
                                p$reference.start.year = 1985
                                p$reference.end.year = 2016
                                p$ref.level = 1.9
                                p$add.primary.line = T
                                p$ylim=c(0,150)
                                p$custom.legend = T
                                p$legend.details = list(legend.placement='topleft',legend=c('Annual Mean','Running mean','USR'), line.types=c(1,1,1), col.types=c('black','salmon','blue'))

                       ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p)
                      

a = groundfish.db('gsinf')
a = subset(a,grepl('TEL2018023',id) & strat %in% c(490:495))
a = makePBS(a,polygon=F)
LobsterMap('BoF')
addPoints(a,pch=16,col='red')   
title(2018)               
savePlot(file.path(project.figuredirectory('bio.lobster'),'GSSetLocations2018.png'),type='png')


a = groundfish.db('gsinf')
a = subset(a,grepl('2017',id) & strat %in% c(490:495))
a = makePBS(a,polygon=F)
LobsterMap('')
title(2017)
addPoints(a,pch=16,col='red')                  
savePlot(file.path(project.figuredirectory('bio.lobster'),'GSSetLocations2017.png'),type='png')



a = groundfish.db('gsinf')
a = subset(a,yr>2009)
a = makePBS(a,polygon=F)
LobsterMap('west')
addPoints(a,pch=16,col='red')                  
savePlot(file.path(project.figuredirectory('bio.lobster'),'GSSetLocations2010-2018.png'),type='png')