
require(bio.survey)
require(bio.lobster)
p = bio.lobster::load.environment()
p$libs = NULL
fp1 = file.path(project.datadirectory('bio.lobster'),"analysis","LFA34-38")
fpf1 = file.path(project.figuredirectory('bio.lobster'),"LFA3438Framework2019")

la()
load_all('~/git/bio.survey/')
load_all('~/git/bio.groundfish/')
la()
#NEFSC Setup
stratifiedAnalysesCommercial = function( p=p1, survey,lfa, fpf = fpf1, fp = fp1){
  if(survey=='NEFSC'){
        p$years.to.estimate = c(1969:2018)
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


    #Figure
        p$add.reference.lines = F
        p$time.series.start.year = p$years.to.estimate[1]
        p$time.series.end.year = p$years.to.estimate[length(p$years.to.estimate)]
        p$metric = 'weights' #weights
        p$measure = 'stratified.mean' #'stratified.total'
        p$figure.title = ""
        p$reference.measure = 'median' # mean, geomean
        p$file.name = file.path('LFA3438Framework2019',paste(lfa,'NEFSCSpringrestratifiedweightscommercial.png'))
        p$y.maximum = NULL # NULL # if ymax is too high for one year
        p$show.truncated.weights = F #if using ymax and want to show the weights that are cut off as values on figure
        p$legend = FALSE
        p$running.median = T
        p$running.length = 3
        p$running.mean = F #can only have rmedian or rmean
        p$error.polygon=F
        p$error.bars=T
        p$ylim2 = c(0,500)
        xx = aggregate(ObsLobs~yr,data=aout,FUN=sum)
        names(xx) =c('x','y')
        p$ylim=c(0,50)

         ref.out= figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p, x2 = xx, sampleSizes=T)
          p$ylim=c(0,20)
          p$metric = 'numbers' #weights
        p$file.name = file.path('LFA3438Framework2019',paste(lfa,'NEFSCSpringrestratifiednumberscommercial.png'))
      ref.out= figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p, x2 = xx, sampleSizes=T)

    aout$subset = 'NEFSCSpringcommercial'
    write.csv(aout,file=file.path(fp,'indicators','NEFSC.spring.restratified.commercial.csv'))
        p$metric = 'weights' #weights
        p$measure = 'stratified.total' #'stratified.total'
        p$file.name = file.path('LFA3438Framework2019',paste(lfa,'NEFSCSpringrestratifiedtotalweightscommercial.png'))
        p$y.maximum = NULL # NULL # if ymax is too high for one year
        p$ylim2 = c(0,500)
        xx = aggregate(ObsLobs~yr,data=aout,FUN=sum)
        names(xx) =c('x','y')
        p$ylim=NULL
        ref.out= figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p, x2 = xx, sampleSizes=T)
        rm(aout)


      p$season =c('fall')# p$series =c('spring');p$series =c('fall')
       aout= nefsc.analysis(DS='stratified.estimates.redo',p=p)
      p$add.reference.lines = F
      p$time.series.start.year = p$years.to.estimate[1]
      p$time.series.end.year = p$years.to.estimate[length(p$years.to.estimate)]
      p$metric = 'weights' #weights
      p$measure = 'stratified.mean' #'stratified.total'
      p$figure.title = ""
      p$reference.measure = 'median' # mean, geomean
      p$file.name = file.path('LFA3438Framework2019',paste(lfa,'NEFSCFallrestratifiedweightscommercial.png'))

      p$y.maximum = NULL # NULL # if ymax is too high for one year
      p$show.truncated.weights = F #if using ymax and want to show the weights that are cut off as values on figure

      p$legend = FALSE
      p$running.median = T
      p$running.length = 3
      p$running.mean = F #can only have rmedian or rmean
       p$error.polygon=F
      p$error.bars=T
      p$ylim=c(0,30)

       p$ylim2 = c(0,500)
      xx = aggregate(ObsLobs~yr,data=aout,FUN=sum)
      names(xx) =c('x','y')
       
       ref.out= figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p, x2 = xx, sampleSizes=T)
       
       p$ylim=c(0,15)
       p$metric = 'numbers' #weights
       p$file.name = file.path('LFA3438Framework2019',paste(lfa,'NEFSCFallrestratifiednumberscommercial.png'))
       ref.out= figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p, x2 = xx, sampleSizes=T)
       

       #total
      #Figure
      p$add.reference.lines = F
      p$time.series.start.year = p$years.to.estimate[1]
      p$time.series.end.year = p$years.to.estimate[length(p$years.to.estimate)]
      p$metric = 'weights' #weights
      p$measure = 'stratified.total' #'stratified.total'
      p$figure.title = ""
      p$reference.measure = 'median' # mean, geomean
      p$file.name = file.path('LFA3438Framework2019',paste(lfa,'NEFSCFallrestratifiedtotalweightscommercial.png'))

      p$y.maximum = NULL # NULL # if ymax is too high for one year
      p$show.truncated.weights = F #if using ymax and want to show the weights that are cut off as values on figure

      p$legend = FALSE
      p$running.median = T
      p$running.length = 3
      p$running.mean = F #can only have rmedian or rmean
       p$error.polygon=F
      p$error.bars=T

      p$ylim=NULL
       p$ylim2 = c(0,500)
      xx = aggregate(ObsLobs~yr,data=aout,FUN=sum)
      names(xx) =c('x','y')
       

       ref.out= figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p, x2 = xx, sampleSizes=T)
       
      aout$subset = 'NEFSCFallcommercial'
      write.csv(aout,file=file.path(fp,'indicators','NEFSC.fall.restratified.commercial.csv'))
      rm(aout)
    }

  if(survey=='DFO'){
    p$series =c('summer')# p$series =c('georges');p$series =c('fall')
    p$years.to.estimate = c(1970:2018)
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

    p$add.reference.lines = F
    p$time.series.start.year = p$years.to.estimate[1]
    p$time.series.end.year = p$years.to.estimate[length(p$years.to.estimate)]
    p$metric = 'weights' #weights
    p$measure = 'stratified.mean' #'stratified.total'
    p$figure.title = ""
    p$reference.measure = 'median' # mean, geomean
    p$file.name =  file.path('LFA3438Framework2019',paste(lfa,'DFOrestratifiedweightscommercial.png'))

    p$y.maximum = NULL # NULL # if ymax is too high for one year
    p$show.truncated.weights = F #if using ymax and want to show the weights that are cut off as values on figure

    p$legend = FALSE
    p$running.median = T
    p$running.length = 3
    p$running.mean = F #can only have rmedian or rmean
     p$error.polygon=F
    p$error.bars=T


     p$ylim2 = c(0,500)
    xx = aggregate(ObsLobs~yr,data=aout,FUN=sum)
    names(xx) =c('x','y')
     p$ylim =c(0,30)

     ref.out= figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p, x2 = xx, sampleSizes=T)

    p$ylim =c(0,25)
    p$metric = 'numbers' #weights
    p$file.name =  file.path('LFA3438Framework2019',paste(lfa,'DFOrestratifiednumberscommercial.png'))
    ref.out= figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p, x2 = xx, sampleSizes=T)


 #total
    p$add.reference.lines = F
    p$time.series.start.year = p$years.to.estimate[1]
    p$time.series.end.year = p$years.to.estimate[length(p$years.to.estimate)]
    p$metric = 'weights' #weights
    p$measure = 'stratified.total' #'stratified.total'
    p$figure.title = ""
    p$reference.measure = 'median' # mean, geomean
    p$file.name =  file.path('LFA3438Framework2019',paste(lfa,'DFOrestratifiedtotalweightscommercial.png'))

    p$y.maximum = NULL # NULL # if ymax is too high for one year
    p$show.truncated.weights = F #if using ymax and want to show the weights that are cut off as values on figure

    p$legend = FALSE
    p$running.median = T
    p$running.length = 3
    p$running.mean = F #can only have rmedian or rmean
     p$error.polygon=F
    p$error.bars=T


     p$ylim2 = c(0,500)
    xx = aggregate(ObsLobs~yr,data=aout,FUN=sum)
    names(xx) =c('x','y')
     p$ylim=NULL
     ref.out= figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p, x2 = xx, sampleSizes=T)


     aout$subset = 'DFOcommercial'
    write.csv(aout,file=file.path(fp,'indicators','DFO.restratified.commercial.csv'))
 
    rm(aout)


    }     
  }



stratifiedAnalysesCommercial(survey='NEFSC',lfa='LFA34')
stratifiedAnalysesCommercial(survey='DFO',lfa='LFA34')
stratifiedAnalysesCommercial(survey='DFO',lfa='LFA35')
stratifiedAnalysesCommercial(survey='DFO',lfa='LFA36')
stratifiedAnalysesCommercial(survey='NEFSC',lfa='LFA38')####issues
stratifiedAnalysesCommercial(survey='DFO',lfa='LFA38')
stratifiedAnalysesCommercial(survey='DFO',lfa='LFA35-38')
