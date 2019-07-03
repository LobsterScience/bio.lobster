require(bio.survey)
require(bio.lobster)
require(bio.groundfish)

p1 = bio.lobster::load.environment()
p1$libs = NULL
fp1 = file.path(project.datadirectory('bio.lobster'),"analysis","LFA34-38")
fpf1 = file.path(project.figuredirectory('bio.lobster'),"LFA3438Framework2019")

la()
surveyEfficiencies <- function(p=p1, survey, lfa, fp=fp1, fpf = fpf1) {
#NEFSC Setup

		if(survey == 'NEFSC'){	
			p$season = 'spring'
			p$years.to.estimate = c(1999:2018)
			p$length.based = T
			p$size.class= c(50,300)
			p$by.sex = F
			p$sex = c(1,2) # male female berried c(1,2,3)
			p$bootstrapped.ci=F
			p$strata.files.return=F
			p$strata.efficiencies=T
			p$clusters = c( rep( "localhost", 7) )
			p = make.list(list(yrs=p$years.to.estimate),Y=p)
	
			

#Spring restratified to lfa34
			p$define.by.polygons = T
			p$lobster.subunits=F
			p$area = lfa
			p$reweight.strata = T #this subsets 
			

			aout= nefsc.analysis(DS='stratified.estimates.redo',p=p)
			save(aout,file = file.path(fp,paste(lfa,'NEFSCSpring.polygondef.survey.efficiency.rdata',sep="")))
  			surveyEfficPlot(aout,fp =fpf,  fname=paste(lfa,'NEFSCspringrestratified.pdf',sep=""))


#fall restratified to lfa34
			p$season =c('fall')
			p$define.by.polygons = T
			p$lobster.subunits=F
			p$area = lfa
			p$reweight.strata = T #this subsets 
			
			aout= nefsc.analysis(DS='stratified.estimates.redo',p=p)
			save(aout,file = file.path(fp,paste(lfa,'NEFSCFall.polygondef.survey.efficiency.rdata',sep="")))
			
			surveyEfficPlot(aout,fp = fpf , fname=paste(lfa,'NEFSCfalrestratified.pdf',sep=""))

			}
##############################################################
#DFO RV Setup
		if(survey == 'DFO'){
			p$series =c('summer')
			p$years.to.estimate = c(1999:2018)
			p$length.based = F
			p$by.sex = F
			p$bootstrapped.ci=F
			p$strata.files.return=F
			p$vessel.correction.fixed=1.2
			p$strat = NULL
			p$clusters = c( rep( "localhost", 7) )
			p$strata.efficiencies = T
			p = make.list(list(yrs=p$years.to.estimate),Y=p)

			

#DFO restratified to lfa34
			p$define.by.polygons = T
			p$lobster.subunits=F
			p$area = lfa
			p$reweight.strata = T #this subsets 
	
			aout= dfo.rv.analysis(DS='stratified.estimates.redo',p=p)
			save(aout,file = file.path(fp,paste(lfa,'DFOrestratified.survey.efficiency.rdata',sep="")))
  			
			surveyEfficPlot(aout,fp = fpf, fname=paste(lfa,'DFOrestratified.pdf',sep=""))
			}
		}


surveyEfficiencies(survey='NEFSC',lfa='LFA34')
surveyEfficiencies(survey='DFO',lfa='LFA34')

surveyEfficiencies(survey='DFO',lfa='LFA35')

surveyEfficiencies(survey='DFO',lfa='LFA36')

surveyEfficiencies(survey='NEFSC',lfa='LFA38')
surveyEfficiencies(survey='DFO',lfa='LFA38')
