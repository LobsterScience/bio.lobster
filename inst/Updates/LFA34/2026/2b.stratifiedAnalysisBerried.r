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




stratifiedAnalysesBerried = function(p=p1, survey,lfa, fpf = fpf1){
      if(survey == 'NEFSC'){
                  
                  p$years.to.estimate = p$yrs
                  p$length.based = F
                  p$by.sex = T
                  p$sex = c(3) # male female berried c(1,2,3)
                  p$bootstrapped.ci=T
                  p$strata.files.return=F
                  p$strata.efficiencies=F
                  p$clusters = c( rep( "localhost", 7) )
                  p$season =c('spring')# p$series =c('spring');p$series =c('fall')
            #Spring restratified to lfa41
                  p$define.by.polygons = T
                  p$lobster.subunits=F
                  p$area = lfa
                  p$reweight.strata = T #this subsets 
                p = make.list(list(yrs=p$years.to.estimate),Y=p)
                  
                aout= nefsc.analysis(DS='stratified.estimates.redo',p=p)
                 write.csv(aout,file=file.path(fpf,paste(lfa,'NEFSCSpringberried.csv',sep="-")))

#Fall restratified to lfa41
      p$season =c('fall')
       
   aout= nefsc.analysis(DS='stratified.estimates.redo',p=p)
  write.csv(aout,file=file.path(fpf,paste(lfa,'NEFSCfallberried.csv',sep="-")))


                              #Figure
     }

     if(survey=='DFO'){

      p$series =c('summer')# p$series =c('georges');p$series =c('fall')
      p$define.by.polygons = T
      p$lobster.subunits=F
      p$area = lfa
      p$years.to.estimate = p$yrs[-1]
      p$length.based = F
      p$by.sex = T
      p$sex = c(3)
      p$bootstrapped.ci=T
      p$strata.files.return=F
      p$vessel.correction.fixed=1.2
      p$strat = NULL
      p$clusters = c( rep( "localhost", 7) )
      p$strata.efficiencies = F
      p = make.list(list(yrs=p$years.to.estimate),Y=p)
      p$reweight.strata = T #this subsets 
  

      aout= dfo.rv.analysis(DS='stratified.estimates.redo',p=p)
      write.csv(aout,file=file.path(fpf,paste(lfa,'DFOberried.csv',sep="-")))


       
      }
    }

stratifiedAnalysesBerried(survey='NEFSC',lfa='LFA34')
stratifiedAnalysesBerried(survey='DFO',lfa='LFA34')
