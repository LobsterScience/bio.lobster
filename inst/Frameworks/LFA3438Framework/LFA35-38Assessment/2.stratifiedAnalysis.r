
require(bio.survey)
require(bio.lobster)
require(bio.groundfish)

p = bio.lobster::load.environment()
p$libs = NULL
ff = "LFA35-38Assessment"
fp1 = file.path(project.datadirectory('bio.lobster'),"analysis","LFA35-38Assessment")
fpf1 = file.path(project.figuredirectory('bio.lobster'),ff)
dir.create(fpf1,showWarnings=F)
dir.create(fp1,showWarnings=F)
p1 = p
p1$yrs = 1969:2019

stratifiedAnalyses = function(p=p1, survey,lfa, fpf = fpf1, fp = fp1,f=ff,wd=10,ht=8){
                p$series =c('summer')
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
                p$reweight.strata = T #this subsets 
            

      aout= dfo.rv.analysis(DS='stratified.estimates.redo',p=p)
      write.csv(aout,file=file.path(fpf, paste(lfa,'DFOtotalabund.csv')))


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

                               p$ylim=NULL
                      if(lfa == 'LFA35-38') p$ylim=c(0,150)
                       p$box=T
                       p$file.name = file.path(f,paste(lfa,'DFOrestratifiednumbersNOY.png', sep=""))
                       ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p,wd=wd,ht=ht)

                       p$box=T
                        p$ylim=NULL
                       p$metric = 'weights'
                       p$file.name = file.path(f,paste(lfa,'DFOrestratifiedweightsNOY.png',sep=""))
                       ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p,wd=wd,ht=ht)

                       p$box=NULL
                       p$ylim=NULL
                       p$file.name = file.path(f,paste(lfa,'DFOrestratifiedDWAO.png',sep=""))
                       p$metric = 'dwao'
                       ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p,wd=wd,ht=ht)
                       
                       p$file.name = file.path(f,paste(lfa,'DFOrestratifiedgini.png',sep=""))
                       p$metric = 'gini'
                       p$ylim =c(0,1)
                       ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p,wd=wd,ht=ht)
                       p$ylim = NULL
                       return(aout)
        }

    
a = stratifiedAnalyses(survey='DFO',lfa='LFA35-38')
