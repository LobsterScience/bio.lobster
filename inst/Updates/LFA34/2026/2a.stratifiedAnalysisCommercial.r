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
p$yrs = 1969:2026
p1 = p

#NEFSC Setup
stratifiedAnalysesCommercial = function( p=p1, survey,lfa, fpf = fpf1){
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
    
     aout= nefsc.analysis(DS='stratified.estimates.redo',p=p)
     write.csv(aout,file=file.path(fpf,paste(lfa,'NEFSCSpringCommercialB.csv',sep="-")))

    #Figure
        p$add.reference.lines = F
        p$time.series.start.year = p$years.to.estimate[1]
        p$time.series.end.year = p$years.to.estimate[length(p$years.to.estimate)]
        p$figure.title = ""
        p$reference.measure = 'median' # mean, geomean
        p$y.maximum = NULL # NULL # if ymax is too high for one year
        p$show.truncated.weights = F #if using ymax and want to show the weights that are cut off as values on figure
        p$legend = FALSE
        p$running.median = T
        p$running.length = 3
        p$running.mean = F #can only have rmedian or rmean
        p$error.polygon=F
        p$error.bars=T
              

      p$season =c('fall')# p$series =c('spring');p$series =c('fall')
      p$years.to.estimate = p$yrs[-length(p$yrs)]
     
     aout= nefsc.analysis(DS='stratified.estimates.redo',p=p)

     write.csv(aout,file=file.path(fpf,paste(lfa,'NEFSCFallCommercialB.csv',sep="-")))
       
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
  write.csv(aout,file=file.path(fpf,paste(lfa,'DFOCommercialB.csv',sep="-")))
   
    }     
  }



stratifiedAnalysesCommercial(survey='NEFSC',lfa='LFA34')
stratifiedAnalysesCommercial(survey='DFO',lfa='LFA34')
