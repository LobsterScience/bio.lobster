#6.Environmental covs.r


require(bio.lobster)
require(bio.utilities)
require(devtools)
require(lubridate)
load_all('~/git/bio.survey/')

la()
p = bio.lobster::load.environment()
p$libs = NULL
p1 = p
fp = file.path(project.datadirectory('bio.lobster'),"analysis")
la()
load_all('~/git/bio.survey/')

wd = file.path(project.datadirectory('bio.lobster'),'PopModelInputs')


load_all('~/git/bio.survey/')
la()

stratifiedAnalysesTemperature = function( p=p1, survey,lfa, fp = wd){
  
  if(survey=='NEFSC'){
    
    p$reweight.strata = F #this subsets 
    p$years.to.estimate = c(1970:2020)
    p$length.based = F
    p$size.class= c(50,300)
    p$by.sex = F
    p$sex = c(1,2) # male female berried c(1,2,3)
    p$bootstrapped.ci=T
    p$strata.files.return=F
    p$strata.efficiencies=F
    p$clusters = c( rep( "localhost", 7) )
    
    
    
    # Spring survey All stations including adjacent
    p$season =c('spring')# p$series =c('spring');p$series =c('fall')
    p$define.by.polygons = T
    p$lobster.subunits=F
    p$area = lfa
    p$temperature=T
    p = make.list(list(yrs=p$years.to.estimate),Y=p)
    
    aout= nefsc.analysis(DS='stratified.estimates.redo',p=p,save=F)      
    write.csv(aout,file=file.path(fp,paste(lfa,'NEFSC.Spring.Temperature.csv',sep="")))
    
    p$season =c('fall')# p$series =c('spring');p$series =c('fall')
    
    aout= nefsc.analysis(DS='stratified.estimates.redo',p=p,save=F)
    write.csv(aout,file=file.path(fp,paste(lfa,'NEFSC.Fall.Temperature.csv',sep="")))
    }
  if(survey=='DFO'){
    ###dfo
    p$series =c('summer')# p$series =c('georges');p$series =c('fall')
    p$define.by.polygons = F
    p$lobster.subunits=F
    p$area = lfa
    p$years.to.estimate = c(1970:2020)
    p$length.based = F
    p$by.sex = F
    p$bootstrapped.ci=T
    p$strata.files.return=F
    p$vessel.correction.fixed=1.2
    p$strat = NULL
    p$clusters = c( rep( "localhost", 7) )
    p$strata.efficiencies = F
    p = make.list(list(yrs=p$years.to.estimate),Y=p)
    p$temperature=T
    p$reweight.strata = F #this subsets 
    la()
    aout= dfo.rv.analysis(DS='stratified.estimates.redo',p=p,save=F)
    write.csv(aout,file=file.path(fp,paste(lfa,'DFO.Temperature.csv',sep="")))
    
  }
}



stratifiedAnalysesTemperature(survey='NEFSC',lfa='EGOM')
stratifiedAnalysesTemperature(survey='DFO',lfa='EGOM')
