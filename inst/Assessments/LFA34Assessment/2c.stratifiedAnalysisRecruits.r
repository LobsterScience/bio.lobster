require(bio.survey)
require(bio.lobster)
require(bio.groundfish)
la()
p = bio.lobster::load.environment()
p$libs = NULL
ff = "LFA34Assessment"
fp1 = file.path(project.datadirectory('bio.lobster'),"analysis",ff)
fpf1 = file.path(project.figuredirectory('bio.lobster'),ff)
p1 = p
p1$yrs = 1969:2019

  
stratifiedAnalysesRecruits = function(p=p1, survey,lfa, fpf = fpf1, fp = fp1,f=ff,ht=ht,wd=wd){

#NEFSC Setup
if(survey=='NEFSC'){
      p$reweight.strata = F #this subsets 
      p$years.to.estimate = p$yrs
      p$length.based = T
      p$size.class= c(50,82)
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


                              #Figure
                              p$add.reference.lines = F
                              p$time.series.start.year = p$years.to.estimate[1]
                              p$time.series.end.year = p$years.to.estimate[length(p$years.to.estimate)]
                              p$metric = 'numbers' #numbers
                              p$measure = 'stratified.mean' #'stratified.total'
                              p$figure.title = ""
                              p$reference.measure = 'median' # mean, geomean
                              p$file.name =  file.path(f,paste(lfa,'NEFSCSpringrestratifiednumbersrecruits.png',sep=''))

                          p$y.maximum = NULL # NULL # if ymax is too high for one year
                        p$show.truncated.numbers = F #if using ymax and want to show the numbers that are cut off as values on figure

                                p$legend = FALSE
                                p$running.median = T
                                p$running.length = 3
                                p$running.mean = F #can only have rmedian or rmean
                               p$error.polygon=F
                              p$error.bars=T
                       
                       ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p,ht=ht,wd=wd,Ylab='Recruit Abundance')
              

#Fall 
      p$season =c('fall')# p$series =c('spring');p$series =c('fall')
      
   aout= nefsc.analysis(DS='stratified.estimates.redo',p=p)
write.csv(aout,file=file.path(fpf,paste(lfa,'NEFSCfallrecruits.csv',sep="-")))

            p$file.name =  file.path(f,paste(lfa,'NEFSCFallrestratifiednumbersrecruits.png',sep=""))

                          p$y.maximum = NULL # NULL # if ymax is too high for one year
                        p$show.truncated.numbers = F #if using ymax and want to show the numbers that are cut off as values on figure

                                p$legend = FALSE
                                p$running.median = T
                                p$running.length = 3
                                p$running.mean = F #can only have rmedian or rmean
                               p$error.polygon=F
                              p$error.bars=T
                           ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p,ht=ht,wd=wd,Ylab='Recruit Abundance')                 
          }
if(survey=='DFO'){
      p$series =c('summer')# p$series =c('georges');p$series =c('fall')
      p$area = lfa
      p$years.to.estimate = p$yrs[-1]
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


         #Figure
                              p$add.reference.lines = F
                              p$time.series.start.year = p$years.to.estimate[1]
                              p$time.series.end.year = p$years.to.estimate[length(p$years.to.estimate)]
                              p$metric = 'numbers' #numbers
                              p$measure = 'stratified.mean' #'stratified.total'
                              p$figure.title = ""
                              p$reference.measure = 'median' # mean, geomean
                              p$file.name =  file.path(f,paste(lfa,'DFOrestratifiednumbersrecruits.png',sep=""))
                          p$y.maximum = NULL # NULL # if ymax is too high for one year
                        p$show.truncated.numbers = F #if using ymax and want to show the numbers that are cut off as values on figure
                                p$legend = FALSE
                                p$running.median = T
                                p$running.length = 3
                                p$running.mean = F #can only have rmedian or rmean
                               p$error.polygon=F
                              p$error.bars=T
                      ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p,ht=ht,wd=wd,Ylab='Recruit Abundance')
                      p$ylim=NULL
                     
  }
}

stratifiedAnalysesRecruits(survey='NEFSC',lfa='LFA34',ht=8,wd=10)
stratifiedAnalysesRecruits(survey='DFO',lfa='LFA34',ht=8,wd=10)
