#redone nov 18 2016

require(bio.survey)
require(bio.lobster)
p = bio.lobster::load.environment()
p$libs = NULL
fp = file.path(project.datadirectory('bio.lobster'),"analysis")
la()
load_all('~/git/bio.survey/')
load_all('~/git/bio.groundfish/')
la()
#NEFSC Setup

        p$reweight.strata = F #this subsets 
        p$years.to.estimate = c(1969:2015)
        p$length.based = T
        p$size.class= c(83,300)
        p$by.sex = T
        p$sex = c(1,2) # male female berried c(1,2,3)
        p$bootstrapped.ci=T
        p$strata.files.return=F
        p$strata.efficiencies=F
        p$clusters = c( rep( "localhost", 7) )
        


    # Spring survey All stations including adjacent
        p$season =c('spring')# p$series =c('spring');p$series =c('fall')
        

    #Spring restratified to lfa41
        p$define.by.polygons = T
        p$lobster.subunits=F
        p$area = 'LFA41'
        p$reweight.strata = T #this subsets 

     aout= nefsc.analysis(DS='stratified.estimates.redo',p=p)


    #Figure
        p$add.reference.lines = F
        p$time.series.start.year = p$years.to.estimate[1]
        p$time.series.end.year = p$years.to.estimate[length(p$years.to.estimate)]
        p$metric = 'weights' #weights
        p$measure = 'stratified.mean' #'stratified.total'
        p$figure.title = ""
        p$reference.measure = 'median' # mean, geomean
        p$file.name = 'lfa41NEFSCSpringrestratifiedweightscommercial.png'
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
      p$file.name = 'lfa41NEFSCSpringrestratifiednumberscommercial.png'
      ref.out= figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p, x2 = xx, sampleSizes=T)

    aout$subset = 'NEFSCSpringcommercial'
    write.csv(aout,file=file.path(fp,'indicators','NEFSC.spring.restratified.commercial.csv'))


     

    #Figure
        p$add.reference.lines = F
        p$time.series.start.year = p$years.to.estimate[1]
        p$time.series.end.year = p$years.to.estimate[length(p$years.to.estimate)]
        p$metric = 'weights' #weights
        p$measure = 'stratified.total' #'stratified.total'
        p$figure.title = ""
        p$reference.measure = 'median' # mean, geomean
        p$file.name = 'lfa41NEFSCSpringrestratifiedtotalweightscommercial.png'
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
        rm(aout)

    

       
#Fall restratified to lfa41
      p$season =c('fall')# p$series =c('spring');p$series =c('fall')
      p$define.by.polygons = T
      p$lobster.subunits=F
      p$area = 'LFA41'
      p$reweight.strata = T #this subsets 

       aout= nefsc.analysis(DS='stratified.estimates.redo',p=p)


  #Figure
      p$add.reference.lines = F
      p$time.series.start.year = p$years.to.estimate[1]
      p$time.series.end.year = p$years.to.estimate[length(p$years.to.estimate)]
      p$metric = 'weights' #weights
      p$measure = 'stratified.mean' #'stratified.total'
      p$figure.title = ""
      p$reference.measure = 'median' # mean, geomean
      p$file.name = 'lfa41NEFSCFallrestratifiedweightscommercial.png'

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
       p$file.name = 'lfa41NEFSCFallrestratifiednumberscommercial.png'
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
      p$file.name = 'lfa41NEFSCFallrestratifiedtotalweightscommercial.png'

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

##############################################################
#DFO RV Setup

    p$series =c('summer')# p$series =c('georges');p$series =c('fall')
    p$define.by.polygons = F
    p$lobster.subunits=F
    p$area = 'LFA41'
    p$years.to.estimate = c(1970:2015)
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


#DFO restratified to lfa41
    p$define.by.polygons = T
    p$lobster.subunits=F
    p$area = 'LFA41'
    p$reweight.strata = T #this subsets 


    aout= dfo.rv.analysis(DS='stratified.estimates.redo',p=p)


 #Figure
    p$add.reference.lines = F
    p$time.series.start.year = p$years.to.estimate[1]
    p$time.series.end.year = p$years.to.estimate[length(p$years.to.estimate)]
    p$metric = 'weights' #weights
    p$measure = 'stratified.mean' #'stratified.total'
    p$figure.title = ""
    p$reference.measure = 'median' # mean, geomean
    p$file.name = 'lfa41DFOrestratifiedweightscommercial.png'

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
    p$file.name = 'lfa41DFOrestratifiednumberscommercial.png'
    ref.out= figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p, x2 = xx, sampleSizes=T)


 #total
    p$add.reference.lines = F
    p$time.series.start.year = p$years.to.estimate[1]
    p$time.series.end.year = p$years.to.estimate[length(p$years.to.estimate)]
    p$metric = 'weights' #weights
    p$measure = 'stratified.total' #'stratified.total'
    p$figure.title = ""
    p$reference.measure = 'median' # mean, geomean
    p$file.name = 'lfa41DFOrestratifiedtotalweightscommercial.png'

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



#DFO Georges
    p$series =c('georges')# p$series =c('georges');p$series =c('fall')
    p$define.by.polygons = F
    p$lobster.subunits=F
    p$years.to.estimate = c(2007:2015)
    p$length.based = T
    p$by.sex = T
    p$sex = c(1,2)
    p$size.class = c(83,300)
    p$bootstrapped.ci=T
    p$strata.files.return=F
    p$vessel.correction.fixed=1.2
    p$strat = NULL
    p$clusters = c( rep( "localhost", 7) )
    p$strata.efficiencies = F
    p = make.list(list(yrs=p$years.to.estimate),Y=p)



    # DFO survey All stations including adjacent
    p$define.by.polygons = T
    p$lobster.subunits=F
    p$area = 'Georges.Canada'
    p$reweight.strata = F #this subsets 

    aout= dfo.rv.analysis(DS='stratified.estimates.redo',p=p)


     #Figure
    p$add.reference.lines = F
    p$time.series.start.year = p$years.to.estimate[1]
    p$time.series.end.year = p$years.to.estimate[length(p$years.to.estimate)]
    p$metric = 'weights' #weights
    p$measure = 'stratified.mean' #'stratified.total'
    p$figure.title = ""
    p$reference.measure = 'median' # mean, geomean
    p$file.name = 'lfa41georgesweightscommercial.png'

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
     
     ref.out= figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p, x2 = xx, sampleSizes=T)
    p$metric = 'numbers'
    p$file.name = 'lfa41georgesnumberscommercial.png'
    p$ylim =c(0,10)
     
     ref.out= figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p, x2 = xx, sampleSizes=T)


     #total
     p$add.reference.lines = F
    p$time.series.start.year = p$years.to.estimate[1]
    p$time.series.end.year = p$years.to.estimate[length(p$years.to.estimate)]
    p$metric = 'weights' #weights
    p$measure = 'stratified.total' #'stratified.total'
    p$figure.title = ""
    p$reference.measure = 'median' # mean, geomean
    p$file.name = 'lfa41georgestotalweightscommercial.png'

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
     
     ref.out= figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p, x2 = xx, sampleSizes=T)


     aout$subset = 'DFOcommercial'
    write.csv(aout,file=file.path(fp,'indicators','DFO.Georges.commercial.csv'))
     
