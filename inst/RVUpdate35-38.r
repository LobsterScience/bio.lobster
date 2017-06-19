
require(bio.survey)
require(bio.lobster)
require(bio.groundfish)
la()

p = bio.lobster::load.environment()
p$libs = NULL
fp = file.path(project.datadirectory('bio.lobster'),"analysis")
load_all('~/git/bio.survey/')

if(get.odbc.data){
        p = bio.groundfish::load.groundfish.environment(assessment.year = 2016)
        # these should be run on a windows machine: NULL values get mangled for some reason
        p$odbc.data.yrs=p$assessment.year  #  <<<<< ---- DATA YEAR can be a single year update too
        groundfish.db( DS="odbc.redo", datayrs=p$odbc.data.yrs )
        groundfish.db( DS="gscat.redo" )
        groundfish.db( DS="gsdet.redo" )
        groundfish.db( DS="gsinf.redo" )
      }

##############################################################
#DFO RV Setup

      p$series =c('summer')# p$series =c('georges');p$series =c('fall')
      p$define.by.polygons = F
      p$lobster.subunits=F
      p$area = 'custom'
      p$years.to.estimate = c(1970:2016)
      p$length.based = F
      p$by.sex = F
      p$bootstrapped.ci=T
      p$strata.files.return=F
      p$vessel.correction.fixed=1.2
      p$strat = 490:495
      p$clusters = c( rep( "localhost", 7) )
      p$strata.efficiencies = F
      p = make.list(list(yrs=p$years.to.estimate),Y=p)
      p$reweight.strata = F #this subsets 
      
      aout= dfo.rv.analysis(DS='stratified.estimates.redo',p=p)
      

         #Figure
                              p$add.reference.lines = T
                              p$time.series.start.year = p$years.to.estimate[1]
                              p$time.series.end.year = p$years.to.estimate[length(p$years.to.estimate)]
                              p$metric = 'numbers' #weights
                              p$measure = 'stratified.mean' #'stratified.total'
                              p$figure.title = ""
                              p$reference.measure = 'mean' # mean, geomean
                              p$file.name = 'lfa35-38.png'
                              

                              p$y.maximum = NULL # NULL # if ymax is too high for one year
                              p$show.truncated.numbers = F #if using ymax and want to show the numbers that are cut off as values on figure

                                p$legend = FALSE
                                p$running.mean = T
                                p$running.length = 3
                                p$running.median = F #can only have rmedian or rmean
                                p$error.bars=T
                                p$reference.start.year = 1985
                                p$reference.end.year = 2016
                                p$ref.level = 1.9
                                p$add.primary.line = T
                                p$ylim=c(0,100)
                                p$custom.legend = T
                                p$legend.details = list(legend.placement='topleft',legend=c('Annual Mean','Running mean','USR'), line.types=c(1,1,1), col.types=c('black','salmon','blue'))

                       ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p)
                      
                  