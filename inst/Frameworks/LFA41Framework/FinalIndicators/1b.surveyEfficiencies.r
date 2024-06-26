require(bio.survey)
require(bio.lobster)
p = bio.lobster::load.environment()
p$libs = NULL
fp = file.path(project.datadirectory('bio.lobster'),"analysis")

#NEFSC Setup

			p$reweight.strata = F #this subsets 
			p$years.to.estimate = c(1999:2015)
			p$length.based = T
			p$size.class= c(50,300)
			p$by.sex = F
			p$sex = c(1,2) # male female berried c(1,2,3)
			p$bootstrapped.ci=F
			p$strata.files.return=F
			p$strata.efficiencies=T
			p$clusters = c( rep( "localhost", 7) )
			p = make.list(list(yrs=p$years.to.estimate),Y=p)
	
			

#Spring restratified to lfa41
			p$define.by.polygons = T
			p$lobster.subunits=F
			p$area = 'LFA41'
			p$reweight.strata = T #this subsets 
			

			aout= nefsc.analysis(DS='stratified.estimates.redo',p=p)
			save(aout,file = file.path(fp,'lfa41NEFSCSpring.polygondef.survey.efficiency.rdata'))
  			load(file = file.path(fp,'lfa41NEFSCSpring.polygondef.survey.efficiency.rdata'))
			
			surveyEfficPlot(aout,fname='lfa41NEFSCspringrestratified.pdf')


#fall restratified to lfa41
			p$season =c('fall')# p$series =c('spring');p$series =c('fall')
			p$define.by.polygons = T
			p$lobster.subunits=F
			p$area = 'LFA41'
			p$reweight.strata = T #this subsets 
			
			aout= nefsc.analysis(DS='stratified.estimates.redo',p=p)
			save(aout,file = file.path(fp,'lfa41NEFSCFall.polygondef.survey.efficiency.rdata'))
  			load(file = file.path(fp,'lfa41NEFSCFall.polygondef.survey.efficiency.rdata'))
			
			surveyEfficPlot(aout,fname='lfa41NEFSCfalrestratified.pdf')


##############################################################
#DFO RV Setup

			p$series =c('summer')# p$series =c('georges');p$series =c('fall')
			p$define.by.polygons = F
			p$lobster.subunits=F
			p$area = 'LFA41'
			p$years.to.estimate = c(1999:2015)
			p$length.based = F
			p$by.sex = F
			p$bootstrapped.ci=F
			p$strata.files.return=F
			p$vessel.correction.fixed=1.2
			p$strat = NULL
			p$clusters = c( rep( "localhost", 7) )
			p$strata.efficiencies = T
			p = make.list(list(yrs=p$years.to.estimate),Y=p)

			

#DFO restratified to lfa41
			p$define.by.polygons = T
			p$lobster.subunits=F
			p$area = 'LFA41'
			p$reweight.strata = T #this subsets 
	
			aout= dfo.rv.analysis(DS='stratified.estimates.redo',p=p)
			save(aout,file = file.path(fp,'lfa41DFOrestratified.survey.efficiency.rdata'))
  			load(file = file.path(fp,'lfa41DFOrestratified.survey.efficiency.rdata'))
			
			surveyEfficPlot(aout,fname='lfa41DFOrestratified.pdf')

###Georges Ban

	p$series =c('georges')# p$series =c('georges');p$series =c('fall')
			p$define.by.polygons = F
			p$lobster.subunits=F
			p$area = 'Georges.Canada'
			p$years.to.estimate = c(1999:2015)
			p$length.based = F
			p$by.sex = F
			p$bootstrapped.ci=F
			p$strata.files.return=F
			p$vessel.correction.fixed=1.2
			p$strat = NULL
			p$clusters = c( rep( "localhost", 7) )
			p$strata.efficiencies = T
			p = make.list(list(yrs=p$years.to.estimate),Y=p)

			p$reweight.strata =F		
	
			aout= dfo.rv.analysis(DS='stratified.estimates.redo',p=p,save=F)
			save(aout,file = file.path(fp,'lfa41georges.survey.efficiency.rdata'))
  			load(file = file.path(fp,'lfa41georges.survey.efficiency.rdata'))
			
			surveyEfficPlot(aout,fname='lfa41georgesefficiency.pdf')

