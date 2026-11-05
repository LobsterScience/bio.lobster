require(bio.survey)
require(bio.lobster)
require(bio.groundfish)
la()
p=list()
p$libs = NULL
ff = "LFA34Update2026"
fpf1 = file.path(project.datadirectory('bio.lobster'),"analysis",ff)
p1 = p
p1$yrs = 1969:2026



  
stratifiedAnalysesRecruits = function(p=p1, survey,lfa, fpf = fpf1){

#NEFSC Setup
if(survey=='NEFSC'){
      p$reweight.strata = F #this subsets 
      p$years.to.estimate = p$yrs
      p$length.based = T
      p$size.class= c(70,82)
      p$by.sex = T
      p$sex = c(1,2) # male female berried c(1,2,3)
      p$bootstrapped.ci=T
      p$strata.files.return=F
      p$strata.efficiencies=F
      p$clusters = c( rep( "localhost", 7) )
    p = make.list(list(yrs=p$years.to.estimate),Y=p)
   
      p$season =c('spring')# p$series =c('spring');p$series =c('fall')
      p$define.by.polygons = T
      p$lobster.subunits=F
      p$area = lfa
      p$reweight.strata = T #this subsets 
      
   aout= nefsc.analysis(DS='stratified.estimates.redo',p=p)
  write.csv(aout,file=file.path(fpf,paste(lfa,'NEFSCSpringrecruits.csv',sep="-")))


#Fall 
      p$season =c('fall')# p$series =c('spring');p$series =c('fall')
      p$years.to.estimate = p$yrs[-length(p$yrs)]
   aout= nefsc.analysis(DS='stratified.estimates.redo',p=p)
write.csv(aout,file=file.path(fpf,paste(lfa,'NEFSCfallrecruits.csv',sep="-")))
          }
if(survey=='DFO'){
      p$series =c('summer')# p$series =c('georges');p$series =c('fall')
      p$area = lfa
      p$years.to.estimate = p$yrs[c(-1)]
      p$length.based = T
      p$by.sex = T
      p$size.class = c(70,82)
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
      p$reweight.strata = T #this subsets 
  
    aout= dfo.rv.analysis(DS='stratified.estimates.redo',p=p)
      write.csv(aout,file=file.path(fpf,paste(lfa,'DFOrecruits.csv',sep="-")))


  }
}

stratifiedAnalysesRecruits(survey='NEFSC',lfa='LFA34')
stratifiedAnalysesRecruits(survey='DFO',lfa='LFA34')
