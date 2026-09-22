
require(bio.survey)
require(bio.lobster)
library(foreign)
la()

p=list()
p$lfas = c("41") # specify lfa
p$libs = NULL
fp = file.path(project.datadirectory('bio.lobster'),"assessments",'LFA41','2026')
dir.create(fp,recursive = T)
dir.create(file.path(fp,'indicators'))
p$current.assessment.year = 2026
p$yrs = 1947:p$current.assessment.year
assessment.year = p$current.assessment.year ########### check the year ############### !!!!!!!!!!!


      p$reweight.strata = F #this subsets 
      p$years.to.estimate = c(1969:assessment.year)
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

                 aout$subset = 'NEFSC.Spring.Restratified'
                write.csv(aout,file=file.path(fp,'indicators','NEFSC.Spring.Restratified.All.csv'))
          


#Fall Survey All stations not pruned by polygon
      p$season =c('fall')# p$series =c('spring');p$series =c('fall')
#Fall restratified to lfa41
      p$define.by.polygons = T
      p$lobster.subunits=F
      p$area = 'LFA41'
      p$reweight.strata = T #this subsets 
      p$years.to.estimate = c(1969:assessment.year) # -1 because update is in the Fall
      p = make.list(list(yrs=p$years.to.estimate),Y=p)
     
   aout= nefsc.analysis_vh(DS='stratified.estimates.redo',p=p)
                aout$subset = 'NEFSC.Fall.Restratified'
                write.csv(aout,file=file.path(fp,'indicators','NEFSC.Fall.Restratified.All.csv'))
     





##############################################################
#DFO RV Setup

      p$series =c('summer')# p$series =c('georges');p$series =c('fall')
      p$define.by.polygons = F
      p$lobster.subunits=F
      p$area = 'LFA41'
      p$years.to.estimate = c(1970:assessment.year)
      #p$years.to.estimate = c(assessment.year)
      
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
      

               aout$subset = 'DFO.restratified.All'
               write.csv(aout,file=file.path(fp,'indicators','DFO.restratified.All.csv'))
          
  #DFO Georges
      p$series =c('georges')# p$series =c('georges');p$series =c('fall')
      p$define.by.polygons = F
      p$lobster.subunits=F
      p$years.to.estimate = c(1987:assessment.year)
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
      
      aout= dfo.rv.analysis(DS='stratified.estimates.redo',p=p)
aout$subset = 'DFO.Georges.All.csv'

write.csv(aout,file=file.path(fp,'indicators','DFO.Georges.All.csv'))
     
