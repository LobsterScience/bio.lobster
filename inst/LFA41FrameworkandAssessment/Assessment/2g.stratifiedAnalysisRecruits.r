#redone sept 18 assessment.year. need to contine

require(bio.survey)
require(bio.lobster)
p = bio.lobster::load.environment()
p$libs = NULL
fp = file.path(project.datadirectory('bio.lobster'),"analysis",'lfa41Assessment')
la()
#load_all('~/git/bio.survey/')
#load_all('~/git/bio.groundfish/')
la()
#NEFSC Setup
assessment.year = p$current.assessment.year ########### check the year ############### !!!!!!!!!!!

      p$reweight.strata = F #this subsets 
      p$years.to.estimate = c(1969:assessment.year)
      p$length.based = T
      p$size.class= c(50,82)
      p$by.sex = T
      p$sex = c(1,2) # male female berried c(1,2,3)
      p$bootstrapped.ci=T
      p$strata.files.return=F
      p$strata.efficiencies=F
      p$clusters = c( rep( "localhost", 7) )
      p = make.list(list(yrs=p$years.to.estimate),Y=p)
  
      

# 
  
 #Spring restratified to lfa41
      p$season =c('spring')# p$series =c('spring');p$series =c('fall')
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
                              p$ylim=c(0,2)
                       
                       ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p, x2 = xx, sampleSizes=T)
                              p$ylim=NULL
                      p$file.name = 'NOYlfa41NEFSCSpringrestratifiednumbersrecruits.png'
                      ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p, x2 = xx, sampleSizes=T)
                      
     
aout$subset = 'NEFSCSpringrecruits'
write.csv(aout,file=file.path(fp,'indicators','NEFSC.spring.restratified.recruits.csv'))
rm(aout)

     

#Fall Survey All stations not pruned by polygon
     
#Fall restratified to lfa41
      p$season =c('fall')# p$series =c('spring');p$series =c('fall')
      p$define.by.polygons = T
      p$lobster.subunits=F
      p$area = 'LFA41'
      p$reweight.strata = T #this subsets 
      p$years.to.estimate = c(1969:(assessment.year-1)) # -1 because update is in the Fall
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
                    p$ylim=NULL
                    p$file.name = 'NOYlfa41NEFSCFallrestratifiednumbersrecruits.png'
                   ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p, x2 = xx, sampleSizes=T)
                   p$ylim=c(0,4)
      aout$subset = 'NEFSCFallrecruits'
        write.csv(aout,file=file.path(fp,'indicators','NEFSC.fall.restratified.recruits.csv'))
    rm(aout)

##############################################################
#DFO RV Setup

      p$series =c('summer')# p$series =c('georges');p$series =c('fall')
      p$define.by.polygons = F
      p$lobster.subunits=F
      p$area = 'LFA41'
      p$years.to.estimate = c(1970:assessment.year)
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
                      p$ylim=NULL
                      p$file.name = 'NOYlfa41DFOrestratifiednumbersrecruits.png'
                      ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p, x2 = xx, sampleSizes=T)
     aout$subset = 'DFOrecruits'
      write.csv(aout,file=file.path(fp,'indicators','DFO.restratified.recruits.csv'))
 rm(aout)
   p$ylim=c(0,5)


  #DFO Georges
      p$series =c('georges')# p$series =c('georges');p$series =c('fall')
      p$define.by.polygons = F
      p$lobster.subunits=F
      p$years.to.estimate = c(2007:assessment.year)
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
      p$define.by.polygons = F
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

p$ylim = NULL
                     p$ylim2 = c(0,500)
                        xx = aggregate(ObsLobs~yr,data=aout,FUN=sum)
                              names(xx) =c('x','y')
        p$ylim=c(0,1.5)               
                       ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p, x2 = xx, sampleSizes=T)
      
       aout$subset = 'DFOrecruits'
      write.csv(aout,file=file.path(fp,'indicators','DFO.Georges.recruits.csv'))
 
rm(aout)