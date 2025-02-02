
#run Sept 17 2024
require(bio.survey)
require(bio.lobster)
require(devtools)
la()
#p = bio.lobster::load.environment()
p$libs = NULL
ff = "LFA34Update2024"
fp1 = file.path(project.datadirectory('bio.lobster'),"analysis",ff)
fpf1 = file.path(project.figuredirectory('bio.lobster'),ff)
dir.create(fpf1,showWarnings=F)
dir.create(fp1,showWarnings=F)
p$yrs = 1969:2024
p1 = p

stratifiedAnalyses = function(p=p1, survey,lfa, fpf = fpf1, fp = fp1,f=ff,ht=ht,wd=wd){
      if(survey == 'NEFSC'){
                p$reweight.strata = T
                p$years.to.estimate = p$yrs
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
   
        #2023 NEFSC survey did not go into 34 in spring
                                     
       aout= nefsc.analysis(DS='stratified.estimates.redo',p=p)
        write.csv(aout,file=file.path(fpf,paste(lfa,'NEFSCSpringtotalabund.csv',sep="-")))

                              #Figure
                              p$add.reference.lines = F
                              p$time.series.start.year = p$years.to.estimate[1]
                              p$time.series.end.year = p$years.to.estimate[length(p$years.to.estimate)]
                              p$metric = 'numbers' #weights
                              p$measure = 'stratified.mean' #'stratified.total'
                              p$figure.title = ""
                              p$reference.measure = 'median' # mean, geomean
                               p$file.name = file.path(f,paste(lfa,'NEFSCSpringrestratifiednumbersNOY.png',sep=""))

                                p$legend = FALSE
                                p$running.median = T
                                p$running.length = 3
                                p$running.mean = F #can only have rmedian or rmean
                               p$error.polygon=F
                              p$error.bars=T

                       p$ylim=NULL
                       p$box=T
                                                      p$error.polygon=F
                              p$error.bars=T

                       ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p,ht=ht,wd=wd)
                         
                       p$box=T
                       p$ylim=NULL
                       p$metric = 'weights'
                       p$file.name = file.path(f,paste(lfa,'NEFSCSpringrestratifiedweightsNOY.png',sep=""))
                       ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p,ht=ht,wd=wd)

                       p$box=NULL
                       p$ylim=NULL
                       p$file.name = file.path(f,paste(lfa,'NEFSCSpringrestratifiedDWAO.png',sep=""))
                       p$metric = 'dwao'
                       ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p,ht=ht,wd=wd)
                       
                       p$file.name = file.path(f,paste(lfa,'NEFSCSpringrestratifiedgini.png',sep=""))
                       p$metric = 'gini'
                       p$ylim =c(0,1)
                       ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p,ht=ht,wd=wd)
                       p$ylim = NULL

#Fall Survey
                p$season =c('fall')
                p$define.by.polygons = T
                p$lobster.subunits=F
                p$area = lfa
                p$reweight.strata = T #this subsets 
      p$years.to.estimate = p$yrs[-length(p$yrs)]
                
               aout= nefsc.analysis(DS='stratified.estimates.redo',p=p)
write.csv(aout,file=file.path(fpf,paste(lfa,'NEFSCfalltotalabund.csv',sep="-")))


                              #Figure
                              p$add.reference.lines = F
                              p$time.series.start.year = p$years.to.estimate[1]
                              p$time.series.end.year = p$years.to.estimate[length(p$years.to.estimate)]
                              p$metric = 'numbers' #weights
                              p$measure = 'stratified.mean' #'stratified.total'
                              p$figure.title = ""
                              p$reference.measure = 'median' # mean, geomean
                         
                                p$legend = FALSE
                                p$running.median = T
                                p$running.length = 3
                                p$running.mean = F #can only have rmedian or rmean
                               p$error.polygon=F
                              p$error.bars=T

    

                      p$file.name = file.path(f,paste(lfa,'NEFSCFallrestratifiednumbersNOY.png',sep=""))
                        p$ylim=NULL
                       ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p,ht=ht,wd=wd)
                                   
                       p$box=T
                       p$ylim=NULL
                       p$metric = 'weights'
                       p$file.name = file.path(f,paste(lfa,'NEFSCFallrestratifiedweightsNOY.png',sep=""))
                       ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p,ht=ht,wd=wd)

                       p$box=NULL
                       p$file.name = file.path(f,paste(lfa,'NEFSCFallrestratifiedDWAO.png',sep=""))
                       p$metric = 'dwao'
                       ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p,ht=ht,wd=wd)
                       
                       p$file.name = file.path(f,paste(lfa,'NEFSCFallrestratifiedgini.png',sep=""))
                       p$metric = 'gini'
                       p$ylim =c(0,1)
                       ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p,ht=ht,wd=wd)
                       p$ylim = NULL
                }

if(survey == 'DFO'){
                p$series =c('summer')
                p$define.by.polygons = T
                p$lobster.subunits=F
                p$area = 'LFA34'
                p$years.to.estimate = c(1970:2024)
                p$length.based = F
                p$by.sex = F
                p$bootstrapped.ci=T
                p$strata.files.return=F
                p$vessel.correction.fixed=1.2
                p$strat = NULL
                p$clusters = c( rep( "localhost", 7) )
                p$strata.efficiencies = F
                p = bio.utilities::make.list(list(yrs=p$years.to.estimate),Y=p)
                p$reweight.strata = T #this subsets 
            
      aout= dfo.rv.analysis(DS='stratified.estimates.redo',p=p)
browser()
            write.csv(aout,file=file.path(fpf,paste(lfa,'DFOtotalabund.csv',sep="-")))


                              p$add.reference.lines = F
                              p$time.series.start.year = p$years.to.estimate[1]
                              p$time.series.end.year = p$years.to.estimate[length(p$years.to.estimate)]
                              p$metric = 'numbers' #weights
                              p$measure = 'stratified.mean' #'stratified.total'
                              p$figure.title = ""
                              p$reference.measure = 'median' # mean, geomean
                              p$file.name = file.path(f,paste(lfa,'DFOrestratifiednumbers.png',sep=""))

                          p$y.maximum = NULL # NULL # if ymax is too high for one year
                        p$show.truncated.numbers = F #if using ymax and want to show the numbers that are cut off as values on figure

                                p$legend = FALSE
                                p$running.median = T
                                p$running.length = 3
                                p$running.mean = F #can only have rmedian or rmean
                               p$error.polygon=F
                              p$error.bars=T

                              p$ylim=c(0,30)
                       ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p,ht=ht,wd=wd)

                       p$ylim=NULL
                      if(lfa == 'LFA35-38') p$ylim=c(0,150)
                       p$box=T
                       p$file.name = file.path(f,paste(lfa,'DFOrestratifiednumbersNOY.png', sep=""))
                       ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p,ht=ht,wd=wd)

                       p$box=NULL
                       p$ylim=c(0,32)
                       p$metric = 'weights'
                       p$file.name = file.path(f,paste(lfa,'DFOrestratifiedweights.png',sep=""))
                       ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p,ht=ht,wd=wd)

                       p$box=T
                        p$ylim=NULL
                       p$metric = 'weights'
                       p$file.name = file.path(f,paste(lfa,'DFOrestratifiedweightsNOY.png',sep=""))
                       ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p,ht=ht,wd=wd)

                       p$box=NULL
                       p$ylim=NULL
                       p$file.name = file.path(f,paste(lfa,'DFOrestratifiedDWAO.png',sep=""))
                       p$metric = 'dwao'
                       ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p,ht=ht,wd=wd)
                       
                       p$file.name = file.path(f,paste(lfa,'DFOrestratifiedgini.png',sep=""))
                       p$metric = 'gini'
                       p$ylim =c(0,1)
                       ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p,ht=ht,wd=wd)
                       p$ylim = NULL
                        }

    }

stratifiedAnalyses(survey='NEFSC',lfa='LFA34',ht=8,wd=10)
stratifiedAnalyses(survey='DFO',lfa='LFA34',ht=8,wd=10)



