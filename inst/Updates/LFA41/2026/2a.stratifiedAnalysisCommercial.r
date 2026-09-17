require(bio.survey)
require(bio.lobster)
la()
p=list()
p$libs = NULL
ff = "2026"
fp1 = file.path(project.datadirectory('bio.lobster'),"assessments","LFA41",ff)
p1 = p
p1$yrs = 1969:2026

p$current.assessment.year = 2026##Check Year
p$syr = 1989
p$yrs = p$syr:assessment.year

p$lfas = c("41")


#NEFSC Setup
stratifiedAnalysesCommercial = function( p=p1, survey,lfa,  fpf = fp1,f=ff,ht=ht,wd=wd){
  if(survey=='NEFSC'){
        p$years.to.estimate = p$yrs
        p$length.based = T
        p$size.class= c(83,300)
        p$by.sex = T
        p$sex = c(1,2) # male female berried c(1,2,3)
        p$bootstrapped.ci=T
        p$strata.files.return=F
        p$strata.efficiencies=F
        p$clusters = c( rep( "localhost", 7) )
        p$season =c('spring')# p$series =c('spring');p$series =c('fall')
        p$define.by.polygons = T
        p$lobster.subunits=F
        p$area = lfa
        p$reweight.strata = T #this subsets 
p = make.list(list(yrs=p$years.to.estimate),Y=p)
    
     aout= nefsc.analysis_vh(DS='stratified.estimates.redo',p=p)
     write.csv(aout,file=file.path(fpf,'indicators',paste(lfa,'NEFSCSpringCommercialB.csv',sep="-")))

      p$season =c('fall')# p$series =c('spring');p$series =c('fall')
      p$years.to.estimate = p$yrs[-length(p$yrs)]
     
         p$metric = 'weights' #weights
      p$measure = 'stratified.total' #'stratified.total'
      p$file.name = file.path(f,paste(lfa,'NEFSCFallrestratifiedtotalweightscommercial.png',sep=""))
     aout= nefsc.analysis_vh(DS='stratified.estimates.redo',p=p)
     write.csv(aout,file=file.path(fpf,'indicators',paste(lfa,'NEFSCFallCommercialB.csv',sep="-")))
       
     }

  if(survey=='DFO'){
    p$series =c('summer')# p$series =c('georges');p$series =c('fall')
    p$years.to.estimate = c(1970:2026)
    p$length.based = T
    p$by.sex = T
    p$size.class = c(83,300)
    p$sex = c(1,2)
    p$bootstrapped.ci=T
    p$strata.files.return=F
    p$vessel.correction.fixed=1.2
    p$strat = NULL
    p$clusters = c( rep( "localhost", 7) )
    p$strata.efficiencies = F
    p = make.list(list(yrs=p$years.to.estimate),Y=p)
    p$define.by.polygons = T
    p$lobster.subunits=F
    p$area = lfa
    p$reweight.strata = T #this subsets 

    aout= dfo.rv.analysis(DS='stratified.estimates.redo',p=p)
  write.csv(aout,file=file.path(fpf,'indicators',paste(lfa,'DFOCommercialB.csv',sep="-")))
   
  p$series =c('georges')# p$series =c('georges');p$series =c('fall')
  p$years.to.estimate = c(1970:2026)
  p$define.by.polygons = F
  p$lobster.subunits=F
  p$area = lfa
  p$reweight.strata = T #this subsets 
  
  aout= dfo.rv.analysis(DS='stratified.estimates.redo',p=p)
  write.csv(aout,file=file.path(fpf,'indicators',paste(lfa,'DFOCommercialB_georges.csv',sep="-")))
  
    }     
  }



stratifiedAnalysesCommercial(survey='NEFSC',lfa='LFA41',ht=8,wd=10)
stratifiedAnalysesCommercial(survey='DFO',lfa='LFA41',ht=8,wd=10)

