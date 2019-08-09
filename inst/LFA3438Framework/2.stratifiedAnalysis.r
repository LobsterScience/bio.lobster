

require(bio.survey)
require(bio.lobster)
require(bio.groundfish)
la()

p = bio.lobster::load.environment()
p$libs = NULL
fp1 = file.path(project.datadirectory('bio.lobster'),"analysis","LFA34-38")
fpf1 = file.path(project.figuredirectory('bio.lobster'),"LFA3438Framework2019")

load_all('~/git/bio.survey/')
p1 = p


stratifiedAnalyses = function(p=p1, survey,lfa, fpf = fpf1, fp = fp1){
      if(survey == 'NEFSC'){
                p$reweight.strata = T
                p$years.to.estimate = c(1969:2017)
                p$length.based = T
                p$size.class= c(50,300)
                p$by.sex = F
                p$sex = c(1,2) # male female berried c(1,2,3)
                p$bootstrapped.ci=T
                p$strata.files.return=F
                p$strata.efficiencies=F
                p$clusters = c( rep( "localhost", 7) )
                p$season =c('spring')
                        p$define.by.polygons = T
                        p$lobster.subunits=F
                        p$area = lfa
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
          
       aout= nefsc.analysis(DS='stratified.estimates.redo',p=p)
                              #Figure
                              p$add.reference.lines = F
                              p$time.series.start.year = p$years.to.estimate[1]
                              p$time.series.end.year = p$years.to.estimate[length(p$years.to.estimate)]
                              p$metric = 'numbers' #weights
                              p$measure = 'stratified.mean' #'stratified.total'
                              p$figure.title = ""
                              p$reference.measure = 'median' # mean, geomean
                              p$file.name = file.path('LFA3438Framework2019',paste(lfa,'NEFSCSpringrestratifiednumbers.png',sep=""))

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
                       p$file.name = file.path('LFA3438Framework2019',paste(lfa,'NEFSCSpringrestratifiednumbersNOY.png',sep=""))
                       p$ylim=NULL
                       p$box=T
                       ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p)
                         
                       p$box=NULL
                       p$ylim=c(0,50)
                       p$metric = 'weights'
                       p$file.name = file.path('LFA3438Framework2019',paste(lfa,'NEFSCSpringrestratifiedweights.png',sep=""))
                       ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p)

                       p$box=T
                       p$ylim=NULL
                       p$metric = 'weights'
                       p$file.name = file.path('LFA3438Framework2019',paste(lfa,'NEFSCSpringrestratifiedweightsNOY.png',sep=""))
                       ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p)

                       p$box=NULL
                       p$ylim=NULL
                       p$file.name = file.path('LFA3438Framework2019',paste(lfa,'NEFSCSpringrestratifiedDWAO.png',sep=""))
                       p$metric = 'dwao'
                       ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p)
                       
                       p$file.name = file.path('LFA3438Framework2019',paste(lfa,'NEFSCSpringrestratifiedgini.png',sep=""))
                       p$metric = 'gini'
                       p$ylim =c(0,1)
                       ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p)
                       p$ylim = NULL

                 aout$subset = 'NEFSC.Spring.Restratified'
                write.csv(aout,file=file.path(fp,'indicators',paste(lfa,'NEFSC.Spring.Restratified.csv',sep="")))
          
#Fall Survey
                p$season =c('fall')
                p$define.by.polygons = T
                p$lobster.subunits=F
                p$area = lfa
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
                              p$file.name = file.path('LFA3438Framework2019',paste(lfa,'NEFSCFallrestratifiednumbers.png',sep=""))

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

                      p$file.name = file.path('LFA3438Framework2019',paste(lfa,'NEFSCFallrestratifiednumbersNOY.png',sep=""))
                        p$ylim=NULL
                       ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p)
                                              
                       p$ylim=c(0,30)
                       p$metric = 'weights'
                       p$file.name = file.path('LFA3438Framework2019',paste(lfa,'NEFSCFallrestratifiedweights.png',sep=""))
                       ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p)

                       p$box=T
                       p$ylim=NULL
                       p$metric = 'weights'
                       p$file.name = file.path('LFA3438Framework2019',paste(lfa,'NEFSCFallrestratifiedweightsNOY.png',sep=""))
                       ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p)

                       p$box=NULL
                       p$file.name = file.path('LFA3438Framework2019',paste(lfa,'NEFSCFallrestratifiedDWAO.png',sep=""))
                       p$metric = 'dwao'
                       ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p)
                       
                       p$file.name = file.path('LFA3438Framework2019',paste(lfa,'NEFSCFallrestratifiedgini.png',sep=""))
                       p$metric = 'gini'
                       p$ylim =c(0,1)
                       ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p)
                       p$ylim = NULL
                aout$subset = 'NEFSC.Fall.Restratified'
                write.csv(aout,file=file.path(fp,'indicators',paste(lfa,'NEFSC.Fall.Restratified.csv',sep="")))
                }

if(survey == 'DFO'){
                p$series =c('summer')
                p$define.by.polygons = T
                p$lobster.subunits=F
                p$area = lfa
                p$years.to.estimate = c(1970:2018)
                p$length.based = F
                p$by.sex = F
                p$bootstrapped.ci=T
                p$strata.files.return=F
                p$vessel.correction.fixed=1.2
                p$strat = NULL
                p$clusters = c( rep( "localhost", 7) )
                p$strata.efficiencies = F
                p = make.list(list(yrs=p$years.to.estimate),Y=p)
                p$reweight.strata = T #this subsets 
            

      aout= dfo.rv.analysis(DS='stratified.estimates.redo',p=p)
      

                              p$add.reference.lines = F
                              p$time.series.start.year = p$years.to.estimate[1]
                              p$time.series.end.year = p$years.to.estimate[length(p$years.to.estimate)]
                              p$metric = 'numbers' #weights
                              p$measure = 'stratified.mean' #'stratified.total'
                              p$figure.title = ""
                              p$reference.measure = 'median' # mean, geomean
                              p$file.name = file.path('LFA3438Framework2019',paste(lfa,'DFOrestratifiednumbers.png',sep=""))

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
                       p$file.name = file.path('LFA3438Framework2019',paste(lfa,'DFOrestratifiednumbersNOY.png', sep=""))
                       ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p)

                       p$box=NULL
                       p$ylim=c(0,32)
                       p$metric = 'weights'
                       p$file.name = file.path('LFA3438Framework2019',paste(lfa,'DFOrestratifiedweights.png',sep=""))
                       ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p)

                       p$box=T
                        p$ylim=NULL
                       p$metric = 'weights'
                       p$file.name = file.path('LFA3438Framework2019',paste(lfa,'DFOrestratifiedweightsNOY.png',sep=""))
                       ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p)

                       p$box=NULL
                       p$ylim=NULL
                       p$file.name = file.path('LFA3438Framework2019',paste(lfa,'DFOrestratifiedDWAO.png',sep=""))
                       p$metric = 'dwao'
                       ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p)
                       
                       p$file.name = file.path('LFA3438Framework2019',paste(lfa,'DFOrestratifiedgini.png',sep=""))
                       p$metric = 'gini'
                       p$ylim =c(0,1)
                       ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p)
                       p$ylim = NULL


               aout$subset = 'DFO.restratified.All'
               write.csv(aout,file=file.path(fp,'indicators',paste(lfa,'DFO.restratified.All.csv',sep="")))
          

}

}

stratifiedAnalyses(survey='NEFSC',lfa='LFA34')
stratifiedAnalyses(survey='DFO',lfa='LFA34')

stratifiedAnalyses(survey='DFO',lfa='LFA35')

stratifiedAnalyses(survey='DFO',lfa='LFA36')

stratifiedAnalyses(survey='NEFSC',lfa='LFA38')
stratifiedAnalyses(survey='DFO',lfa='LFA38')
stratifiedAnalyses(survey='DFO',lfa='LFA35-38')
