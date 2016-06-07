	loadfunctions(c('lobster','utility','growth'))
	options(stringsAsFactors=F)
	require(lattice)


	FigLoc = file.path(project.figuredirectory('lobster'),'collectors')
	dir.create(FigLoc, showWarnings=F)
	fp = file.path(project.datadirectory('lobster'),'data')

	x = read.csv(file.path(fp,'CollectorData2016.csv'))

	x$Study.Area = trimws(x$Study.Area,'right') #lobster bay has space in name sometimes
	x$Site = trimws(x$Site,'right') #lobster bay has space in name sometimes
	
	i = which(x$Site=='Whitehead Harbor')
	x$Site[i] = 'Whitehead Harbour'

	x = rename.df(x,"Size.CL..CW.or.TL",'Size')                   
	#set up counter for individuals
	x$n = 1

	 sites = c("Lobster Bay", "St Mary's Bay","Canso.Whitehead" ,"Cape Breton",     "Port LaTour")
	 x = x[which(x$Study.Area %in% sites),]
	 x = toNums(x,'Size')


	#distinct number of collectors sampled per year
			a = data.frame(unique(cbind(x$Year,x$Collector.Number,x$Study.Area)))
			names(a) = c('Year','Collector.Count','Study.Area')
			a = aggregate(Collector.Count~Year+Study.Area,data=a,FUN=length)

	xyplot(Collector.Count~as.numeric(Year)|factor(Study.Area),data=a, main="N Collectors Sampled by Year", xlab='Year',type='b',lwd=5,xlim=c(2006,2016))


#indices using the size classes determined below	 
		 x$SC=0
		 i = which(x$Study.Area %in% 'Lobster Bay')
		 x$SC[i] = ifelse(x$Size[i]<11.5,1,ifelse(x$Size[i] >=11.5 & x$Size[i]<32,2,3))
		 
		i = which(x$Study.Area %in% "St Mary's Bay")
		 x$SC[i] = ifelse(x$Size[i]<12.5,1,ifelse(x$Size[i] >=12.5 & x$Size[i]<33.1,2,3))
	
		i = which(x$Study.Area %in% "Port LaTour")
		 x$SC[i] = ifelse(x$Size[i]<11.5,1,ifelse(x$Size[i] >=11.5 & x$Size[i]<32,2,3))

		i = which(x$Study.Area %in% c("Canso.Whitehead",'Cape Breton'))
		 x$SC[i] = ifelse(x$Size[i]<14.2,1,ifelse(x$Size[i] >=14.2 & x$Size[i]<33.85,2,3))


			 

	 a1 = aggregate(n~Year+Study.Area,data=x[which(x$Common.Name=='American Lobster' & x$SC==1),],FUN=sum)
	 names(a1)[3] = 'YOY'
	 a2 = aggregate(n~Year+Study.Area,data=x[which(x$Common.Name=='American Lobster' & x$SC==2),],FUN=sum)
	 names(a2)[3] = 'One'
	 a3 = aggregate(n~Year+Study.Area,data=x[which(x$Common.Name=='American Lobster' & x$SC==3),],FUN=sum)
	 names(a3)[3] = 'Twop'
	 
	 a4 = merge(a1,a2,by=c('Year','Study.Area'),all=T)
	 a5 = merge(a4,a3,by=c('Year','Study.Area'),all=T)
	 a6 = merge(a5,a, by=c('Year','Study.Area'),all=T)

	 a6 = na.zero(a6)

#number per m2
	 a6$YOY = (a6$YOY) /0.56 / a6$Collector.Count
	 a6$One = (a6$One)/0.56 / a6$Collector.Count
	 a6$Twop =(a6$Twop)/0.56/ a6$Collector.Count


	xyplot(YOY~as.numeric(Year)|factor(Study.Area),data=a6, main="Density of Newly Settled", xlab='Year',type='b',ylab='Count per m2',lwd=3,xlim=c(2006,2016))
	savePlot(file.path(getwd(),'YOY.png'),'png')

	xyplot(One~as.numeric(Year)|factor(Study.Area),data=a6, main="Density of One Year Old", xlab='Year',type='b',ylab='Count per m2',lwd=3,xlim=c(2006,2016))
	savePlot(file.path(getwd(),'One.png'),'png')


#load the comparison data of yoy to one plus from the following year by site
xc = read.csv(file.path(project.datadirectory('lobster'),'data','collectorYOYOnes.csv'))
xc$col = c(rep('red',14),rep('black',19))
plot(xc$YOY,xc$One,xlab='Settlers',ylab='Ones',pch=16,col='black',ylim=c(0.5,1.4))
	savePlot(file.path(getwd(),'YoyvOne.png'),'png')

cor.test(xc$YOY,xc$One)

	#size frequencies
		y = x[which(x$Common.Name=='American Lobster'),]

		si = unique(y$Study.Area)

	for(s in si) {
		pdf(file.path(FigLoc,paste(s,'sizeFrequency.pdf',sep="")))
		yy = as.numeric(y[which(y$Study.Area==s),'Size'])
		hist(yy,breaks = seq(1,100,2),main=s)
		dev.off()
		}

	#lobster bay suction
	xs = read.csv(file.path(fp,'Suction2005-2013.csv')) #lobster Bay only
	xs = rename.df(xs,"Size.CL..CW.or.TL",'Size')                   
	ys = xs[which(xs$Species=='Homarus americanus'),]
	yss = ys[which(ys$Size<=100),'Size']
	a = hist(yss,breaks = seq(1,100,2),main='Lobster Bay Suction')

	require(mixtools)
		sC = normalmixEM(yss,k=4, mu=c(8,18,22,40),sigma=c(0.5,1,1,1) )
			histAllMixtures(sC,breaks=seq(1,100,2),main='LobsterBaySuction',prob=T,xlab='CL')	
			savePlot(file.path(getwd(),'LobsterBaySuction.png'),'png')


#mixture modelling for Lobster Bay, Port Latour, St Mary's Bay and Cape Breton
require(mixtools)
	s = 'Lobster Bay'
	yy = as.numeric(y[which(y$Study.Area==s),'Size'])
	hist(yy,breaks = seq(1,100,2),main=s)
	sC = normalmixEM(yy,k=3, mu=c(8,22,38),sigma=c(0.5,1,3) )
	histAllMixtures(sC,breaks=seq(1,100,2),main=s,probability=T,xlab='CL')	
	abline(v=c(11.5,32),lwd=2,col='red')
	savePlot(file.path(getwd(),'LobsterBay.png'),'png')


	s = "St Mary's Bay"
	yy = as.numeric(y[which(y$Study.Area==s),'Size'])
	hist(yy,breaks = seq(1,100,2),main=s)
	sC = normalmixEM(yy,k=3, mu=c(8,22,38),sigma=c(0.5,1,3) )
	histAllMixtures(sC,breaks=seq(1,100,2),main=s,probability=T,xlab='CL')	
	abline(v=c(12.5,33.1),lwd=2,col='red')
	
	savePlot(file.path(getwd(),'St Marys.png'),'png')


	s = 'Cape Breton'
	yy = as.numeric(y[which(y$Study.Area==s),'Size'])
	hist(yy,breaks = seq(1,100,2),main=s)
	sC = normalmixEM(yy,k=3, mu=c(8,22,38),sigma=c(0.5,1,3) )
	histAllMixtures(sC,breaks=seq(1,100,2),main=s,probability=T,,xlab='CL')	
	abline(v=c(14.2,33.85),lwd=2,col='red')

	savePlot(file.path(getwd(),'CapeBreton.png'),'png')


	s = "Port LaTour"
	yy = as.numeric(y[which(y$Study.Area==s),'Size'])
	hist(yy,breaks = seq(1,100,2),main=s,xlab='Carapace Length')
	savePlot(file.path(getwd(),'PortLatourLobster.png'),'png')


	s = 'Canso.Whitehead'
	yy = as.numeric(y[which(y$Study.Area==s),'Size'])
	hist(yy,breaks = seq(1,100,2),main=s,xlab='Carapace Length')
	savePlot(file.path(getwd(),'Canso.WhiteheadLobster.png'),'png')


#Beaver Harbour suction sampling
	require(chron)
	ws = read.csv(file.path(fp,'beaverHarbourSuction1991-2009.csv')) #lobster Bay only
	ws = ws[which(ws$Species==1),]
	ws$Date = as.Date(ws$Date,"%d-%b-%y")
	ws$Year = factor2number(years(ws$Date))
	ws$Months = factor2character(months(ws$Date))
	
	a = unique(ws$Year)
	for(y in a) {
		x11()
	 	with(ws[which(ws$Year==y & ws$Month=='October'),],hist(Size,breaks=seq(1,100,2),main=y))
		}




#temperature
		k =collectorTemperatureData()
		load(file.path(ecomod.datadirectory,'lobster','data','temperature','combinedTemperatureData.Rdata'))
			k = k[,c('Site','Site.Depth.m.','Year','Month','Date_YMD','Time_HHMMSS','Temperature')]

			l = k[which(k$Site.Depth.m. %in% 6:11),]
			l = l[which(l$Month %in% c(8,9)),]



			aggregate(Temperature~Year+Site,data=l,FUN=mean)
