require(bio.survey)
require(bio.lobster)
require(PBSmapping)
require(devtools)
require(sf)
require(bio.utilities)
la()
p = list()
p$libs = NULL
fpf1 = file.path(project.datadirectory('bio.lobster'),"analysis",'LFA34Update2026')
dir.create(fpf1)
p1 = p
dir.create(fpf1,showWarnings=F)
p$yrs = 1969:2026
p1 = p

stratifiedAnalyses = function(p=p1, survey,lfa, fpf = fpf1,ht=ht,wd=wd){
  if(survey == 'NEFSC'){
    p$reweight.strata = T
    p$years.to.estimate = p$yrs
    p$length.based = T
    p$size.class= c(50,300)
    p$by.sex = F
    p$sex = c(1,2) # male female berried c(1,2,3)
    p$bootstrapped.ci=T
    p$strata.files.return=F
    p$strata.efficiencies=F
    p$clusters = c( rep( "localhost", 7) )
    p$season =c('spring')
    p$define.by.polygons = T
    p$lobster.subunits=F
    p$area = lfa
    p$return.both = NULL
    p = make.list(list(yrs=p$years.to.estimate),Y=p)
    
    #Figure
    p$add.reference.lines = F
    p$time.series.start.year = p$years.to.estimate[1]
    p$time.series.end.year = p$years.to.estimate[length(p$years.to.estimate)]
    p$metric = 'numbers' #weights
    p$measure = 'stratified.mean' #'stratified.total'
    p$figure.title = ""
    p$reference.measure = 'median' # mean, geomean
    
    #2023 NEFSC survey did not go into 34 in spring
    
    aout= nefsc.analysis(DS='stratified.estimates.redo',p=p)
    write.csv(aout,file=file.path(fpf,paste(lfa,'NEFSCSpringtotalabund.csv',sep="-")))
    
    p$season =c('fall')
    p$define.by.polygons = T
    p$lobster.subunits=F
    p$area = lfa
    p$reweight.strata = T #this subsets 
    p$years.to.estimate = p$yrs[-length(p$yrs)]
    
    aout= nefsc.analysis(DS='stratified.estimates.redo',p=p)
    write.csv(aout,file=file.path(fpf,paste(lfa,'NEFSCfalltotalabund.csv',sep="-")))
    
    
    #Figure
  }
  
  if(survey == 'DFO'){
    p$series =c('summer')
    p$define.by.polygons = T
    p$lobster.subunits=F
    p$area = 'LFA34'
    p$years.to.estimate = c(1970:2026)
    p$length.based = F
    p$by.sex = F
    p$bootstrapped.ci=T
    p$strata.files.return=F
    p$vessel.correction.fixed=1.2
    p$strat = NULL
    p$clusters = c( rep( "localhost", 7) )
    p$strata.efficiencies = F
    p = bio.utilities::make.list(list(yrs=p$years.to.estimate),Y=p)
    p$reweight.strata = T #this subsets 
    
    aout= dfo.rv.analysis(DS='stratified.estimates.redo',p=p)
    write.csv(aout,file=file.path(fpf,paste(lfa,'DFOtotalabund.csv',sep="-")))
    
  }
}

stratifiedAnalyses(survey='NEFSC',lfa='LFA34',ht=8,wd=10)
stratifiedAnalyses(survey='DFO',lfa='LFA34',ht=8,wd=10)



