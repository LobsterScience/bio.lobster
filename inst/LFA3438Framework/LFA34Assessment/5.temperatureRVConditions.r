#environmental conditions
#temperature trends from the surveys
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


stratifiedAnalysesTemperature = function( p=p1, survey,lfa, fpf = fpf1, fp = fp1,f=ff){

if(survey=='NEFSC'){

      p$reweight.strata = F #this subsets 
      p$years.to.estimate = p$yrs
      p$length.based = T
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
                              p$add.reference.lines = F
                              p$time.series.start.year = p$years.to.estimate[1]
                              p$time.series.end.year = p$years.to.estimate[length(p$years.to.estimate)]
                              p$metric = 'temperature' #weights
                              p$measure = 'stratified.mean' #'stratified.total'
                              p$figure.title = ""
                              p$reference.measure = 'median' # mean, geomean
                              p$file.name = file.path(f,paste(lfa,'NEFSCSpringTemperature.png',sep=""))
                              p$ylim = c(4,12)
                          p$y.maximum = NULL # NULL # if ymax is too high for one year
                        p$show.truncated.numbers = F #if using ymax and want to show the numbers that are cut off as values on figure

                                p$legend = FALSE
                                p$running.median = T
                                p$running.length = 3
                                p$running.mean = F #can only have rmedian or rmean
                               p$error.polygon=F
                              p$error.bars=T

                      ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p)
 #### fall
         p$season =c('fall')# p$series =c('spring');p$series =c('fall')
	                        aout= nefsc.analysis(DS='stratified.estimates.redo',p=p,save=F)
                            p$file.name = file.path(f,paste(lfa,'NEFSCFallTemperature.png',sep=""))
                            ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p)
       }
if(survey=='DFO'){
 ###dfo
    p$series =c('summer')# p$series =c('georges');p$series =c('fall')
      p$define.by.polygons = T
      p$lobster.subunits=F
      p$area = lfa
      p$years.to.estimate = p$yrs[-1]
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
      require(bio.groundfish)
      la()
      aout= dfo.rv.analysis(DS='stratified.estimates.redo',p=p,save=F)
                                   p$add.reference.lines = F
                              p$time.series.start.year = p$years.to.estimate[1]
                              p$time.series.end.year = p$years.to.estimate[length(p$years.to.estimate)]
                              p$metric = 'temperature' #weights
                              p$measure = 'stratified.mean' #'stratified.total'
                              p$figure.title = ""
                              p$reference.measure = 'median' # mean, geomean
                            p$file.name = file.path(f,paste(lfa,'DFOTemperature.png',sep=""))
                          p$y.maximum = NULL # NULL # if ymax is too high for one year
                        p$show.truncated.numbers = F #if using ymax and want to show the numbers that are cut off as values on figure
        p$ylim = c(4,12)
                     
                                p$legend = FALSE
                                p$running.median = T
                                p$running.length = 3
                                p$running.mean = F #can only have rmedian or rmean
                               p$error.polygon=F
                              p$error.bars=T


                       ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p)
       }
     }



stratifiedAnalysesTemperature(survey='NEFSC',lfa='LFA34')
stratifiedAnalysesTemperature(survey='DFO',lfa='LFA34')
