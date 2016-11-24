#Pezzack Indicators

require(bio.survey)
require(bio.lobster)
require(bio.groundfish)
require(bio.polygons)

la()

###DFO Summer RV
#MAP

		LFA41 = read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","LFA41Offareas.csv"))
				LFA41 = joinPolys(as.PolySet(LFA41),operation='UNION')
				LFA41 = subset(LFA41,SID==1)
				attr(LFA41,'projection') <- 'LL'


pdf(file=file.path(project.figuredirectory('bio.lobster'),paste('pezzacksummerstratamap.pdf',sep='.')))
		LobsterMap(ylim=c(41.1,44.5),		xlim=c(-68,-63.5)	,boundaries='LFAs',addSummerStrata=T,output='bio.lobster',fname = 'summerstratamap.pdf',save=F,labcex =0.8,labels=F)
	 
 			  a = find.bio.gis('summer_strata_labels',return.one.match=F)
			  a = read.csv(a,header=T)
			  names(a)[4] <- 'label'
			  b = find.bio.gis('strat.gf',return.one.match=F)
			  b = read.table(b)
			  names(b) <- c('X','Y','PID')
					  
		  b = b[which(b$PID %in% c(477,478,480:484)),]
			  b = within(b,{POS <- ave(PID,list(PID),FUN=seq_along)})
			  addPolys(b,lty=1,border='red',col=adjustcolor('yellow',alpha.f=1))
		addPolys(LFA41,border='blue',lwd=2)
	dev.off()	

#ABUNDANCE

p = bio.lobster::load.environment()
p$libs = NULL
fp = file.path(project.datadirectory('bio.lobster'),"analysis")
load_all('~/git/bio.survey/')


    p$series =c('summer')# p$series =c('georges');p$series =c('fall')
      p$define.by.polygons = F
      p$lobster.subunits=F
      p$area = 'custom'
      p$strat = c(477,478,480:484)
      p$years.to.estimate = c(1970:2015)
      p$length.based = F
      p$by.sex = F
      p$bootstrapped.ci=T
      p$strata.files.return=F
      p$vessel.correction.fixed=1.2
      p$clusters = c( rep( "localhost", 7) )
      p$strata.efficiencies = F
      p = make.list(list(yrs=p$years.to.estimate),Y=p)

      

# DFO survey All stations including adjacent
      p$define.by.polygons = F
      p$lobster.subunits=F
      p$reweight.strata = F #this subsets 
      
      aout= dfo.rv.analysis(DS='stratified.estimates.redo',p=p)
                              p$add.reference.lines = F
                              p$time.series.start.year = p$years.to.estimate[1]
                              p$time.series.end.year = p$years.to.estimate[length(p$years.to.estimate)]
                              p$metric = 'numbers' #weights
                              p$measure = 'stratified.mean' #'stratified.total'
                              p$figure.title = ""
                              p$reference.measure = 'median' # mean, geomean
                              p$file.name = 'pezzackRVDFObasenumbers.png'

                          p$y.maximum = NULL # NULL # if ymax is too high for one year
                        p$show.truncated.numbers = F #if using ymax and want to show the numbers that are cut off as values on figure

                                p$legend = FALSE
                                p$running.median = T
                                p$running.length = 3
                                p$running.mean = F #can only have rmedian or rmean
                               p$error.polygon=F
                              p$error.bars=T

                                p$ylim=c(0,30)

                       ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p)
                      
#DFO GEORGES
#MAP

		LFA41 = read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","LFA41Offareas.csv"))
				LFA41 = joinPolys(as.PolySet(LFA41),operation='UNION')
				LFA41 = subset(LFA41,SID==1)
				attr(LFA41,'projection') <- 'LL'

pdf(file=file.path(project.figuredirectory('bio.lobster'),'pezzackgeorgesmap41.pdf'))
LobsterMap(xlim=c(-70.5,-63.5),ylim=c(38.5,45),boundaries='LFAs',addGeorgesStrata=T,output='bio.lobster',fname='georgesmap41.pdf',save=F,labcex =0.8,labels=T)
	addPolys(LFA41,border='blue',lwd=2)
	b = file.path(project.datadirectory('bio.polygons'),'data','Science','PED','GeorgesBankStrata.rdata')
	 load(b)
	 addPolys(out,border='red')
	 addPolys(subset(out,PID %in% c(1,2,3,4)),lty=1,border='red',col='yellow')
	addPolys(LFA41,border='blue',lwd=2)
	
	dev.off()

#ABUNDANCE

p = bio.lobster::load.environment()
p$libs = NULL
fp = file.path(project.datadirectory('bio.lobster'),"analysis")
load_all('~/git/bio.survey/')

  p$series =c('georges')# p$series =c('georges');p$series =c('fall')
      p$define.by.polygons = F
      p$lobster.subunits=F
      p$years.to.estimate = c(1987:2015)
      p$length.based = F
      p$by.sex = F
      p$bootstrapped.ci=T
      p$strata.files.return=F
      p$vessel.correction.fixed=1.2
      p$strat = c('5Z1','5Z2','5Z3','5Z4')
      p$clusters = c( rep( "localhost", 7) )
      p$strata.efficiencies = F
      p = make.list(list(yrs=p$years.to.estimate),Y=p)

      

# DFO survey All stations including adjacent
      p$define.by.polygons = F
      p$lobster.subunits=F
      p$area = 'custom'
      p$reweight.strata = F #this subsets 
      
      aout= dfo.rv.analysis(DS='stratified.estimates.redo',p=p)
      

	         #Figure
	                              p$add.reference.lines = F
	                              p$time.series.start.year = p$years.to.estimate[1]
	                              p$time.series.end.year = p$years.to.estimate[length(p$years.to.estimate)]
	                              p$metric = 'numbers' #weights
	                              p$measure = 'stratified.mean' #'stratified.total'
	                              p$figure.title = ""
	                              p$reference.measure = 'median' # mean, geomean
	                              p$file.name = 'pezzacklfa41georgesnumbers.png'

	                          p$y.maximum = NULL # NULL # if ymax is too high for one year
	                        p$show.truncated.numbers = F #if using ymax and want to show the numbers that are cut off as values on figure

	                                p$legend = FALSE
	                                p$running.median = T
	                                p$running.length = 3
	                                p$running.mean = F #can only have rmedian or rmean
	                               p$error.polygon=F
	                              p$error.bars=T
	                              p$ylim=c(0,10)

	                       ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p)
