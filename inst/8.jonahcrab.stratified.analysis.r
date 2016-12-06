
require(bio.survey)
require(bio.lobster)
require(bio.groundfish)
la()

p = bio.lobster::load.environment()
p$libs = NULL
fp = file.path(project.datadirectory('bio.lobster'),"analysis")
load_all('~/git/bio.survey/')


    p$series =c('summer')# p$series =c('georges');p$series =c('fall')
      p$define.by.polygons = F
      p$lobster.subunits=F
      p$area = 'LFA41'
      p$years.to.estimate = c(1999:2015)
      p$length.based = F
      p$by.sex = F
      p$bootstrapped.ci=T
      p$strata.files.return=F
      p$vessel.correction.fixed=1.2
      p$strat = NULL
      p$clusters = c( rep( "localhost", 7) )
      p$strata.efficiencies = F
      p = make.list(list(yrs=p$years.to.estimate),Y=p)

      

# DFO survey All stations including adjacent
      p$define.by.polygons = F
      p$lobster.subunits=F
      p$area = 'LFA41'
      p$reweight.strata = F #this subsets 
      
      aout= dfo.rv.jonah.analysis(DS='stratified.estimates.redo',p=p,save=F)
      

         #Figure
                              p$add.reference.lines = F
                              p$time.series.start.year = p$years.to.estimate[1]
                              p$time.series.end.year = p$years.to.estimate[length(p$years.to.estimate)]
                              p$metric = 'numbers' #weights
                              p$measure = 'stratified.mean' #'stratified.total'
                              p$figure.title = ""
                              p$reference.measure = 'median' # mean, geomean
                              p$file.name = 'jonahlfa41DFObasenumbers.png'

                          p$y.maximum = NULL # NULL # if ymax is too high for one year
                        p$show.truncated.numbers = F #if using ymax and want to show the numbers that are cut off as values on figure

                                p$legend = FALSE
                                p$running.median = T
                                p$running.length = 3
                                p$running.mean = F #can only have rmedian or rmean
                               p$error.polygon=F
                              p$error.bars=T


                       ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p)
                      
                       p$metric = 'weights'
                       p$file.name = 'jonahlfa41DFObaseweights.png'
                       ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p)

                       p$ylim=NULL
                       p$file.name = 'jonahlfa41DFObaseDWAO.png'
                       p$metric = 'dwao'
                       ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p)
                       p$file.name = 'jonahlfa41DFObasegini.png'
                       p$metric = 'gini'
                       p$ylim =c(0,1)
                       ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p)
                       p$ylim = NULL

                aout$subset = 'DFO.Base.All'
                
     







#DFO restratified to lfa41
      p$define.by.polygons = T
      p$lobster.subunits=F
      p$area = 'LFA41'
      p$reweight.strata = T #this subsets 
  

      aout= dfo.rv.jonah.analysis(DS='stratified.estimates.redo',p=p)
      

         #Figure
                              p$add.reference.lines = F
                              p$time.series.start.year = p$years.to.estimate[1]
                              p$time.series.end.year = p$years.to.estimate[length(p$years.to.estimate)]
                              p$metric = 'numbers' #weights
                              p$measure = 'stratified.mean' #'stratified.total'
                              p$figure.title = ""
                              p$reference.measure = 'median' # mean, geomean
                              p$file.name = 'jonahlfa41DFOrestratifiednumbers.png'

                          p$y.maximum = NULL # NULL # if ymax is too high for one year
                        p$show.truncated.numbers = F #if using ymax and want to show the numbers that are cut off as values on figure

                                p$legend = FALSE
                                p$running.median = T
                                p$running.length = 3
                                p$running.mean = F #can only have rmedian or rmean
                               p$error.polygon=F
                              p$error.bars=T

                       ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p)

                       p$ylim=NULL
                       p$box=T
                       p$file.name = 'jonahlfa41DFOrestratifiednumbersNOY.png'
                       ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p)

                       p$box=NULL
                       p$metric = 'weights'
                       p$file.name = 'jonahlfa41DFOrestratifiedweights.png'
                       ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p)

                       p$box=T
                        p$ylim=NULL
                       p$metric = 'weights'
                       p$file.name = 'jonahlfa41DFOrestratifiedweightsNOY.png'
                       ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p)

                       p$box=NULL
                       p$ylim=NULL
                       p$file.name = 'jonahlfa41DFOrestratifiedDWAO.png'
                       p$metric = 'dwao'
                       ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p)
                       p$file.name = 'jonahlfa41DFOrestratifiedgini.png'
                       p$metric = 'gini'
                       p$ylim =c(0,1)
                       ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p)
                       p$ylim = NULL


             



#DFO restratified to lfa41adjacent
      p$define.by.polygons = T
      p$lobster.subunits=F
      p$area = 'adjacentLFA41'
      p$reweight.strata = T #this subsets 


      aout= dfo.rv.jonah.analysis(DS='stratified.estimates.redo',p=p)
      

         #Figure
                              p$add.reference.lines = F
                              p$time.series.start.year = p$years.to.estimate[1]
                              p$time.series.end.year = p$years.to.estimate[length(p$years.to.estimate)]
                              p$metric = 'numbers' #weights
                              p$measure = 'stratified.mean' #'stratified.total'
                              p$figure.title = ""
                              p$reference.measure = 'median' # mean, geomean
                              p$file.name = 'jonahadjcentlfa41DFOrestratifiednumbers.png'

                          p$y.maximum = NULL # NULL # if ymax is too high for one year
                        p$show.truncated.numbers = F #if using ymax and want to show the numbers that are cut off as values on figure

                                p$legend = FALSE
                                p$running.median = T
                                p$running.length = 3
                                p$running.mean = F #can only have rmedian or rmean
                               p$error.polygon=F
                              p$error.bars=T

                       ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p)
                       p$metric = 'weights'
                       p$file.name = 'jonahadjacentlfa41DFOrestratifiedweights.png'
                       ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p)

                       p$ylim=NULL
                       p$file.name = 'jonahadjacentlfa41DFOrestratifiedDWAO.png'
                       p$metric = 'dwao'
                       ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p)
                       p$file.name = 'jonahadjacentlfa41DFOrestratifiedgini.png'
                       p$metric = 'gini'
                       p$ylim =c(0,1)
                       ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p)
                       p$ylim = NULL


               
