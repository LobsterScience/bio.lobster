

require(bio.survey)
require(bio.lobster)
require(bio.groundfish)
la()

p = bio.lobster::load.environment()
p$libs = NULL
fp = file.path(project.datadirectory('bio.lobster'),"analysis",'lfa41Assessment')
load_all('~/git/bio.survey/')




      p$reweight.strata = F #this subsets 
      p$years.to.estimate = c(1969:2016)
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
                        p$area = 'LFA41'
                        p$return.both = NULL
                      p = make.list(list(yrs=p$years.to.estimate),Y=p)
                    
                            #Figure
                              p$add.reference.lines = F
                              p$time.series.start.year = p$years.to.estimate[1]
                              p$time.series.end.year = p$years.to.estimate[length(p$years.to.estimate)]
                              p$metric = 'numbers' #weights
                              p$measure = 'stratified.mean' #'stratified.total'
                              p$figure.title = ""
                              p$reference.measure = 'median' # mean, geomean

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
                              p$metric = 'numbers' #weights
                              p$measure = 'stratified.mean' #'stratified.total'
                              p$figure.title = ""
                              p$reference.measure = 'median' # mean, geomean
                              p$file.name = 'lfa41NEFSCSpringrestratifiednumbers.png'

                              p$ylim=c(0,20)
                          p$y.maximum = NULL # NULL # if ymax is too high for one year
                        p$show.truncated.numbers = F #if using ymax and want to show the numbers that are cut off as values on figure

                                p$legend = FALSE
                                p$running.median = T
                                p$running.length = 3
                                p$running.mean = F #can only have rmedian or rmean
                               p$error.polygon=F
                              p$error.bars=T


                       ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p)
                       p$file.name = 'lfa41NEFSCSpringrestratifiednumbersNOY.png'
                       p$ylim=NULL
                       p$box=T
                       ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p)
                         
                       p$box=NULL
                       p$ylim=c(0,50)
                       p$metric = 'weights'
                       p$file.name = 'lfa41NEFSCSpringrestratifiedweights.png'
                       ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p)

                       p$box=T
                       p$ylim=NULL
                       p$metric = 'weights'
                       p$file.name = 'lfa41NEFSCSpringrestratifiedweightsNOY.png'
                       ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p)

                       p$box=NULL
                       p$ylim=NULL
                       p$file.name = 'lfa41NEFSCSpringrestratifiedDWAO.png'
                       p$metric = 'dwao'
                       ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p)
                       
                       p$file.name = 'lfa41NEFSCSpringrestratifiedgini.png'
                       p$metric = 'gini'
                       p$ylim =c(0,1)
                       ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p)
                       p$ylim = NULL

                 aout$subset = 'NEFSC.Spring.Restratified'
                write.csv(aout,file=file.path(fp,'indicators','NEFSC.Spring.Restratified.All.csv'))
          


#Fall Survey All stations not pruned by polygon
      p$season =c('fall')# p$series =c('spring');p$series =c('fall')
#Fall restratified to lfa41
      p$define.by.polygons = T
      p$lobster.subunits=F
      p$area = 'LFA41'
      p$reweight.strata = T #this subsets 
      
   aout= nefsc.analysis(DS='stratified.estimates.redo',p=p)


                              #Figure
                              p$add.reference.lines = F
                              p$time.series.start.year = p$years.to.estimate[1]
                              p$time.series.end.year = p$years.to.estimate[length(p$years.to.estimate)]
                              p$metric = 'numbers' #weights
                              p$measure = 'stratified.mean' #'stratified.total'
                              p$figure.title = ""
                              p$reference.measure = 'median' # mean, geomean
                              p$file.name = 'lfa41NEFSCFallrestratifiednumbers.png'

                          p$y.maximum = NULL # NULL # if ymax is too high for one year
                        p$show.truncated.numbers = F #if using ymax and want to show the numbers that are cut off as values on figure

                                p$legend = FALSE
                                p$running.median = T
                                p$running.length = 3
                                p$running.mean = F #can only have rmedian or rmean
                               p$error.polygon=F
                              p$error.bars=T

                              p$ylim=c(0,15)
                       ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p)

                      p$file.name = 'lfa41NEFSCFallrestratifiednumbersNOY.png'
                        p$ylim=NULL
                       ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p)
                                              
                       p$ylim=c(0,30)
                       p$metric = 'weights'
                       p$file.name = 'lfa41NEFSCFallrestratifiedweights.png'
                       ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p)

                       p$box=T
                       p$ylim=NULL
                       p$metric = 'weights'
                       p$file.name = 'lfa41NEFSCFallrestratifiedweightsNOY.png'
                       ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p)

                       p$box=NULL
                       p$file.name = 'lfa41NEFSCFallrestratifiedDWAO.png'
                       p$metric = 'dwao'
                       ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p)
                       p$file.name = 'lfa41NEFSCFallrestratifiedgini.png'
                       p$metric = 'gini'
                       p$ylim =c(0,1)
                       ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p)
                       p$ylim = NULL
                aout$subset = 'NEFSC.Fall.Restratified'
                write.csv(aout,file=file.path(fp,'indicators','NEFSC.Fall.Restratified.All.csv'))
     





##############################################################
#DFO RV Setup

      p$series =c('summer')# p$series =c('georges');p$series =c('fall')
      p$define.by.polygons = F
      p$lobster.subunits=F
      p$area = 'LFA41'
      p$years.to.estimate = c(1970:2016)
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

#DFO restratified to lfa41
      p$define.by.polygons = T
      p$lobster.subunits=F
      p$area = 'LFA41'
      p$reweight.strata = T #this subsets 
  

      aout= dfo.rv.analysis(DS='stratified.estimates.redo',p=p)
      

                              p$add.reference.lines = F
                              p$time.series.start.year = p$years.to.estimate[1]
                              p$time.series.end.year = p$years.to.estimate[length(p$years.to.estimate)]
                              p$metric = 'numbers' #weights
                              p$measure = 'stratified.mean' #'stratified.total'
                              p$figure.title = ""
                              p$reference.measure = 'median' # mean, geomean
                              p$file.name = 'lfa41DFOrestratifiednumbers.png'

                          p$y.maximum = NULL # NULL # if ymax is too high for one year
                        p$show.truncated.numbers = F #if using ymax and want to show the numbers that are cut off as values on figure

                                p$legend = FALSE
                                p$running.median = T
                                p$running.length = 3
                                p$running.mean = F #can only have rmedian or rmean
                               p$error.polygon=F
                              p$error.bars=T

                              p$ylim=c(0,30)
                       ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p)

                       p$ylim=NULL
                       p$box=T
                       p$file.name = 'lfa41DFOrestratifiednumbersNOY.png'
                       ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p)

                       p$box=NULL
                       p$ylim=c(0,32)
                       p$metric = 'weights'
                       p$file.name = 'lfa41DFOrestratifiedweights.png'
                       ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p)

                       p$box=T
                        p$ylim=NULL
                       p$metric = 'weights'
                       p$file.name = 'lfa41DFOrestratifiedweightsNOY.png'
                       ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p)

                       p$box=NULL
                       p$ylim=NULL
                       p$file.name = 'lfa41DFOrestratifiedDWAO.png'
                       p$metric = 'dwao'
                       ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p)
                       p$file.name = 'lfa41DFOrestratifiedgini.png'
                       p$metric = 'gini'
                       p$ylim =c(0,1)
                       ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p)
                       p$ylim = NULL


               aout$subset = 'DFO.restratified.All'
               write.csv(aout,file=file.path(fp,'indicators','DFO.restratified.All.csv'))
          
  #DFO Georges
      p$series =c('georges')# p$series =c('georges');p$series =c('fall')
      p$define.by.polygons = F
      p$lobster.subunits=F
      p$years.to.estimate = c(1987:2016)
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
      p$area = 'Georges.Canada'
      p$reweight.strata = F #this subsets 
      
      aout= dfo.rv.analysis(DS='stratified.estimates.redo',p=p,save=T)
      

         #Figure
                              p$add.reference.lines = F
                              p$time.series.start.year = p$years.to.estimate[1]
                              p$time.series.end.year = p$years.to.estimate[length(p$years.to.estimate)]
                              p$metric = 'numbers' #weights
                              p$measure = 'stratified.mean' #'stratified.total'
                              p$figure.title = ""
                              p$reference.measure = 'median' # mean, geomean
                              p$file.name = 'lfa41georgesnumbers.png'

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
                       p$file.name = 'lfa41georgesweights.png'
                       ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p)

                       p$file.name = 'lfa41georgesDWAO.png'
                       p$metric = 'dwao'
                       ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p)
                       p$file.name = 'lfa41georgesgini.png'
                       p$metric = 'gini'
                       p$ylim =c(0,1)
                       ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p)
                       p$ylim = NULL

aout$subset = 'DFO.Georges.All.csv'

write.csv(aout,file=file.path(fp,'indicators','DFO.Georges.All.csv'))
     
