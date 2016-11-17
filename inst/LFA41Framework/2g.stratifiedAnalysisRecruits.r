#redone sept 18 2016. need to contine

require(bio.survey)
require(bio.lobster)
p = bio.lobster::load.environment()
p$libs = NULL
fp = file.path(project.datadirectory('bio.lobster'),"analysis")
la()
load_all('~/git/bio.survey/')
#NEFSC Setup

base= list()
restratified = list()
adjacentrestratified = list()
      p$reweight.strata = F #this subsets 
      p$years.to.estimate = c(1969:2015)
      p$length.based = T
      p$size.class= c(0,82)
      p$by.sex = T
      p$sex = c(1,2) # male female berried c(1,2,3)
      p$bootstrapped.ci=T
      p$strata.files.return=F
      p$strata.efficiencies=F
      p$clusters = c( rep( "localhost", 7) )
  
      

# Spring survey All stations including adjacent
                        p$season =c('spring')# p$series =c('spring');p$series =c('fall')
                        p$define.by.polygons = F
                        p$lobster.subunits=F
                        p$area = 'LFA41'
                      p = make.list(list(yrs=p$years.to.estimate),Y=p)
                    
                        aout= nefsc.analysis(DS='stratified.estimates.redo',p=p)

                              #Figure
                              p$add.reference.lines = F
                              p$time.series.start.year = p$years.to.estimate[1]
                              p$time.series.end.year = p$years.to.estimate[length(p$years.to.estimate)]
                              p$metric = 'numbers' #numbers
                              p$measure = 'stratified.mean' #'stratified.total'
                              p$figure.title = ""
                              p$reference.measure = 'median' # mean, geomean
                              p$file.name = 'lfa41NEFSCSpringbasenumberscommercial.png'

                          p$y.maximum = NULL # NULL # if ymax is too high for one year
                        p$show.truncated.numbers = F #if using ymax and want to show the numbers that are cut off as values on figure

                                p$legend = FALSE
                                p$running.median = T
                                p$running.length = 3
                                p$running.mean = F #can only have rmedian or rmean
                               p$error.polygon=F
                              p$error.bars=T
                         p$ylim2 = c(0,500)
                        xx = aggregate(ObsLobs~yr,data=aout,FUN=sum)
                              names(xx) =c('x','y')
                       
                       ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p, x2 = xx, sampleSizes=T)
  

     aout$subset = 'NEFSCSpringrecruits'
       write.csv(aout,file=file.path(fp,'indicators','NEFSC.spring.base.recruits.csv'))
  

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
                              p$metric = 'numbers' #numbers
                              p$measure = 'stratified.mean' #'stratified.total'
                              p$figure.title = ""
                              p$reference.measure = 'median' # mean, geomean
                              p$file.name = 'lfa41NEFSCSpringrestratifiednumbersrecruits.png'

                          p$y.maximum = NULL # NULL # if ymax is too high for one year
                        p$show.truncated.numbers = F #if using ymax and want to show the numbers that are cut off as values on figure

                                p$legend = FALSE
                                p$running.median = T
                                p$running.length = 3
                                p$running.mean = F #can only have rmedian or rmean
                               p$error.polygon=F
                              p$error.bars=T
                       p$ylim2 = c(0,500)
                        xx = aggregate(ObsLobs~yr,data=aout,FUN=sum)
                              names(xx) =c('x','y')
                       
                       ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p, x2 = xx, sampleSizes=T)
     
aout$subset = 'NEFSCSpringrecruits'
write.csv(aout,file=file.path(fp,'indicators','NEFSC.spring.restratified.recruits.csv'))


     

#Spring restratified to adjacentlfa41
      p$define.by.polygons = T
      p$lobster.subunits=F
      p$area = 'adjacentLFA41'
      p$reweight.strata = T #this subsets 
      
   aout= nefsc.analysis(DS='stratified.estimates.redo',p=p)


                              #Figure
                              p$add.reference.lines = F
                              p$time.series.start.year = p$years.to.estimate[1]
                              p$time.series.end.year = p$years.to.estimate[length(p$years.to.estimate)]
                              p$metric = 'numbers' #numbers
                              p$measure = 'stratified.mean' #'stratified.total'
                              p$figure.title = ""
                              p$reference.measure = 'median' # mean, geomean
                              p$file.name = 'adjacentlfa41NEFSCSpringrestratifiednumbersrecruits.png'

                          p$y.maximum = NULL # NULL # if ymax is too high for one year
                        p$show.truncated.numbers = F #if using ymax and want to show the numbers that are cut off as values on figure

                                p$legend = FALSE
                                p$running.median = T
                                p$running.length = 3
                                p$running.mean = F #can only have rmedian or rmean
                               p$error.polygon=F
                              p$error.bars=T


                            p$ylim2 = c(0,500)
                        xx = aggregate(ObsLobs~yr,data=aout,FUN=sum)
                              names(xx) =c('x','y')
                       
                       ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p, x2 = xx, sampleSizes=T)
     
      aout$subset = 'NEFSCSpringrecruits'
    write.csv(aout,file=file.path(fp,'indicators','NEFSC.spring.adjrestratified.recruits.csv'))



#Fall Survey All stations not pruned by polygon
      p$season =c('fall')# p$series =c('spring');p$series =c('fall')
   # Spring survey All stations including adjacent
                        p$season =c('fall')# p$series =c('fall');p$series =c('fall')
                        p$define.by.polygons = F
                        p$lobster.subunits=F
                        p$area = 'LFA41'
                      p = make.list(list(yrs=p$years.to.estimate),Y=p)
                    
                        aout= nefsc.analysis(DS='stratified.estimates.redo',p=p)
 
                              #Figure
                              p$add.reference.lines = F
                              p$time.series.start.year = p$years.to.estimate[1]
                              p$time.series.end.year = p$years.to.estimate[length(p$years.to.estimate)]
                              p$metric = 'numbers' #numbers
                              p$measure = 'stratified.mean' #'stratified.total'
                              p$figure.title = ""
                              p$reference.measure = 'median' # mean, geomean
                              p$file.name = 'lfa41NEFSCFallbasenumbersrecruits.png'

                          p$y.maximum = NULL # NULL # if ymax is too high for one year
                        p$show.truncated.numbers = F #if using ymax and want to show the numbers that are cut off as values on figure

                                p$legend = FALSE
                                p$running.median = T
                                p$running.length = 3
                                p$running.mean = F #can only have rmedian or rmean
                               p$error.polygon=F
                              p$error.bars=T

                p$ylim2 = c(0,500)
                        xx = aggregate(ObsLobs~yr,data=aout,FUN=sum)
                              names(xx) =c('x','y')
                       
                       ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p, x2 = xx, sampleSizes=T)

      aout$subset = 'NEFSCFallrecruits'
        write.csv(aout,file=file.path(fp,'indicators','NEFSC.fall.base.recruits.csv'))
    
     
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
                              p$metric = 'numbers' #numbers
                              p$measure = 'stratified.mean' #'stratified.total'
                              p$figure.title = ""
                              p$reference.measure = 'median' # mean, geomean
                              p$file.name = 'lfa41NEFSCFallrestratifiednumbersrecruits.png'

                          p$y.maximum = NULL # NULL # if ymax is too high for one year
                        p$show.truncated.numbers = F #if using ymax and want to show the numbers that are cut off as values on figure

                                p$legend = FALSE
                                p$running.median = T
                                p$running.length = 3
                                p$running.mean = F #can only have rmedian or rmean
                               p$error.polygon=F
                              p$error.bars=T


                     p$ylim2 = c(0,500)
                        xx = aggregate(ObsLobs~yr,data=aout,FUN=sum)
                              names(xx) =c('x','y')
                       
                           ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p, x2 = xx, sampleSizes=T)
     

      aout$subset = 'NEFSCFallrecruits'
        write.csv(aout,file=file.path(fp,'indicators','NEFSC.fall.restratified.recruits.csv'))
    

#Fall restratified to adjacentlfa41
      p$define.by.polygons = T
      p$lobster.subunits=F
      p$area = 'adjacentLFA41'
      p$reweight.strata = T #this subsets 
      
   aout= nefsc.analysis(DS='stratified.estimates.redo',p=p)


                              #Figure
                              p$add.reference.lines = F
                              p$time.series.start.year = p$years.to.estimate[1]
                              p$time.series.end.year = p$years.to.estimate[length(p$years.to.estimate)]
                              p$metric = 'numbers' #numbers
                              p$measure = 'stratified.mean' #'stratified.total'
                              p$figure.title = ""
                              p$reference.measure = 'median' # mean, geomean
                              p$file.name = 'adjacentlfa41NEFSCFallrestratifiednumbersrecruits.png'

                          p$y.maximum = NULL # NULL # if ymax is too high for one year
                        p$show.truncated.numbers = F #if using ymax and want to show the numbers that are cut off as values on figure

                                p$legend = FALSE
                                p$running.median = T
                                p$running.length = 3
                                p$running.mean = F #can only have rmedian or rmean
                               p$error.polygon=F
                              p$error.bars=T


                     p$ylim2 = c(0,500)
                        xx = aggregate(ObsLobs~yr,data=aout,FUN=sum)
                              names(xx) =c('x','y')
                       
                       ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p, x2 = xx, sampleSizes=T)
     

      aout$subset = 'NEFSCFallrecruits'
      write.csv(aout,file=file.path(fp,'indicators','NEFSC.fall.adjrestratified.recruits.csv'))
    

##############################################################
#DFO RV Setup

      p$series =c('summer')# p$series =c('georges');p$series =c('fall')
      p$define.by.polygons = F
      p$lobster.subunits=F
      p$area = 'LFA41'
      p$years.to.estimate = c(1970:2015)
      p$length.based = T
      p$by.sex = T
      p$size.class = c(0,82)
      p$sex = c(1,2)
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
      
      aout= dfo.rv.analysis(DS='stratified.estimates.redo',p=p)
      

         #Figure
                              p$add.reference.lines = F
                              p$time.series.start.year = p$years.to.estimate[1]
                              p$time.series.end.year = p$years.to.estimate[length(p$years.to.estimate)]
                              p$metric = 'numbers' #numbers
                              p$measure = 'stratified.mean' #'stratified.total'
                              p$figure.title = ""
                              p$reference.measure = 'median' # mean, geomean
                              p$file.name = 'lfa41DFObasenumbersrecruits.png'

                          p$y.maximum = NULL # NULL # if ymax is too high for one year
                        p$show.truncated.numbers = F #if using ymax and want to show the numbers that are cut off as values on figure

                                p$legend = FALSE
                                p$running.median = T
                                p$running.length = 3
                                p$running.mean = F #can only have rmedian or rmean
                               p$error.polygon=F
                              p$error.bars=T


                     p$ylim2 = c(0,500)
                        xx = aggregate(ObsLobs~yr,data=aout,FUN=sum)
                              names(xx) =c('x','y')
                       
                       ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p, x2 = xx, sampleSizes=T)
     
      aout$subset = 'DFOrecruits'
      write.csv(aout,file=file.path(fp,'indicators','DFO.base.recruits.csv'))
    






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
                              p$metric = 'numbers' #numbers
                              p$measure = 'stratified.mean' #'stratified.total'
                              p$figure.title = ""
                              p$reference.measure = 'median' # mean, geomean
                              p$file.name = 'lfa41DFOrestratifiednumbersrecruits.png'

                          p$y.maximum = NULL # NULL # if ymax is too high for one year
                        p$show.truncated.numbers = F #if using ymax and want to show the numbers that are cut off as values on figure

                                p$legend = FALSE
                                p$running.median = T
                                p$running.length = 3
                                p$running.mean = F #can only have rmedian or rmean
                               p$error.polygon=F
                              p$error.bars=T


                     p$ylim2 = c(0,500)
                        xx = aggregate(ObsLobs~yr,data=aout,FUN=sum)
                              names(xx) =c('x','y')
                       
                       ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p, x2 = xx, sampleSizes=T)
    
     aout$subset = 'DFOrecruits'
      write.csv(aout,file=file.path(fp,'indicators','DFO.restratified.recruits.csv'))
 


#DFO restratified to lfa41adjacent
      p$define.by.polygons = T
      p$lobster.subunits=F
      p$area = 'adjacentLFA41'
      p$reweight.strata = T #this subsets 


      aout= dfo.rv.analysis(DS='stratified.estimates.redo',p=p)
      

         #Figure
                              p$add.reference.lines = F
                              p$time.series.start.year = p$years.to.estimate[1]
                              p$time.series.end.year = p$years.to.estimate[length(p$years.to.estimate)]
                              p$metric = 'numbers' #numbers
                              p$measure = 'stratified.mean' #'stratified.total'
                              p$figure.title = ""
                              p$reference.measure = 'median' # mean, geomean
                              p$file.name = 'adjcentlfa41DFOrestratifiednumbersrecruits.png'

                          p$y.maximum = NULL # NULL # if ymax is too high for one year
                        p$show.truncated.numbers = F #if using ymax and want to show the numbers that are cut off as values on figure

                                p$legend = FALSE
                                p$running.median = T
                                p$running.length = 3
                                p$running.mean = F #can only have rmedian or rmean
                               p$error.polygon=F
                              p$error.bars=T


                     p$ylim2 = c(0,500)
                        xx = aggregate(ObsLobs~yr,data=aout,FUN=sum)
                              names(xx) =c('x','y')
                       
                       ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p, x2 = xx, sampleSizes=T)


      aout$subset = 'DFOrecruits'
     write.csv(aout,file=file.path(fp,'indicators','DFO.adjrestratified.recruits.csv'))
 

  #DFO Georges
      p$series =c('georges')# p$series =c('georges');p$series =c('fall')
      p$define.by.polygons = F
      p$lobster.subunits=F
      p$years.to.estimate = c(2007:2015)
      p$length.based = T
      p$by.sex = T
      p$sex = c(1,2)
      p$size.class = c(0,82)
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
                              p$metric = 'numbers' #numbers
                              p$measure = 'stratified.mean' #'stratified.total'
                              p$figure.title = ""
                              p$reference.measure = 'median' # mean, geomean
                              p$file.name = 'lfa41georgesnumbersrecruits.png'

                          p$y.maximum = NULL # NULL # if ymax is too high for one year
                        p$show.truncated.numbers = F #if using ymax and want to show the numbers that are cut off as values on figure

                                p$legend = FALSE
                                p$running.median = T
                                p$running.length = 3
                                p$running.mean = F #can only have rmedian or rmean
                               p$error.polygon=F
                              p$error.bars=T


                     p$ylim2 = c(0,500)
                        xx = aggregate(ObsLobs~yr,data=aout,FUN=sum)
                              names(xx) =c('x','y')
                       
                       ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p, x2 = xx, sampleSizes=T)
      
       aout$subset = 'DFOrecruits'
      write.csv(aout,file=file.path(fp,'indicators','DFO.Georges.recruits.csv'))
 
