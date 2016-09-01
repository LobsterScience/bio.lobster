# sex ratios surveys
require(bio.survey)
require(bio.lobster)
p = bio.lobster::load.environment()
p$libs = NULL
fp = file.path(project.datadirectory('bio.lobster'),"analysis")
la()
load_all('~/git/bio.survey/')

#by length for histograms
#DFO RV Analysis

      p$series =c('summer')# p$series =c('georges');p$series =c('fall')
      p$define.by.polygons = F
      p$lobster.subunits=F
      p$area = 'LFA41'
      p$years.to.estimate = c(1999:2015)
      p$length.based = F
      p$by.sex = T
      p$bootstrapped.ci=F
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
      p$length.based=F
      
      #full strata
        a = list(1,c(2,3))
        out = list()
        for(i in 1:length(a)) {
          p$sex=a[[i]]
          out[[i]] = dfo.rv.analysis(DS='stratified.estimates.redo',p=p,save=F)
          }

        aa = do.call(rbind,out)
        aa$sex = rep(c('male','femberr'),each=length(p$years.to.estimate))
        save(aa,file=file.path(project.datadirectory('bio.lobster'),'analysis','sexLFA41baseSummerRV.rdata  '))

#restratified
        require(bio.lobster)
        p = bio.lobster::load.environment()
        p$libs = NULL

        fp = file.path(project.datadirectory('bio.lobster'),"analysis")

        p$series =c('summer')# p$series =c('georges');p$series =c('fall')
        p$define.by.polygons = T
        p$lobster.subunits=F
        p$area = 'LFA41'
        p$reweight.strata = T #this subsets 
        p$years.to.estimate = c(1999:2015)
        p$length.based = F
        p$by.sex = T
        p$bootstrapped.ci=F
        p$strata.files.return=F
        p$vessel.correction.fixed=1.2
        p$strat = NULL
        p$clusters = c( rep( "localhost", 7) )
        p$strata.efficiencies = F
        p = make.list(list(yrs=p$years.to.estimate),Y=p)

        a = list(1,c(2,3))
        out = list()
        for(i in 1:length(a)) {
          p$sex=a[[i]]
          out[[i]] = dfo.rv.analysis(DS='stratified.estimates.redo',p=p,save=F)
          }

        aa = do.call(rbind,out)
		aa$sex = rep(c('male','femberr'),each=length(p$years.to.estimate))
        save(aa,file=file.path(project.datadirectory('bio.lobster'),'analysis','sexLFA41polygonSummerRV.rdata  '))

#adjacent
        require(bio.lobster)
        p = bio.lobster::load.environment()
        p$libs = NULL
        fp = file.path(project.datadirectory('bio.lobster'),"analysis")


        p$series =c('summer')# p$series =c('georges');p$series =c('fall')
        p$define.by.polygons = T
        p$lobster.subunits=F
        p$area = 'adjacentLFA41'
        p$reweight.strata = T #this subsets 
        p$years.to.estimate = c(1999:2015)
        p$length.based = F
        p$by.sex = T
        p$bootstrapped.ci=F
        p$strata.files.return=F
        p$vessel.correction.fixed=1.2
        p$strat = NULL
        p$clusters = c( rep( "localhost", 7) )
        p$strata.efficiencies = F
        p = make.list(list(yrs=p$years.to.estimate),Y=p)

        a = list(1,c(2,3))
        out = list()
        for(i in 1:length(a)) {
          p$sex=a[[i]]
          out[[i]] = dfo.rv.analysis(DS='stratified.estimates.redo',p=p,save=F)
          }

        aa = do.call(rbind,out)
        aa$sex = rep(c('male','femberr'),each=length(p$years.to.estimate))
        save(aa,file=file.path(project.datadirectory('bio.lobster'),'analysis','sexLFA41adjacentpolygonSummerRV.rdata  '))


#NEFSC spring


      p$reweight.strata = F #this subsets 
      p$years.to.estimate = c(1969:2015)
      p$length.based = T
      p$size.class= c(50,300)
      p$by.sex = T
      p$sex = c(1,2) # male female berried c(1,2,3)
      p$bootstrapped.ci=F
      p$strata.files.return=F
      p$strata.efficiencies=F
      p$clusters = c( rep( "localhost", 7) )
  
      

# Spring survey All stations including adjacent
                        p$season =c('spring')# p$series =c('spring');p$series =c('fall')
                        p$define.by.polygons = F
                        p$lobster.subunits=F
                        p$area = 'LFA41'
                      p = make.list(list(yrs=p$years.to.estimate),Y=p)
                    


   	    a = list(1,c(2,3))
        out = list()
        for(i in 1:length(a)) {
          p$sex=a[[i]]
          out[[i]] = nefsc.analysis(DS='stratified.estimates.redo',p=p)
          }

        aa = do.call(rbind,out)
        aa$sex = rep(c('male','femberr'),each=length(p$years.to.estimate))
        save(aa,file=file.path(project.datadirectory('bio.lobster'),'analysis','sexLFA41NEFSCspringbase.rdata  '))

# restratified
    p$define.by.polygons = T
      p$lobster.subunits=F
      p$area = 'LFA41'
      p$reweight.strata = T #this subsets 
      
   	    a = list(1,c(2,3))
        out = list()
        for(i in 1:length(a)) {
          p$sex=a[[i]]
          out[[i]] = nefsc.analysis(DS='stratified.estimates.redo',p=p)
          }

        aa = do.call(rbind,out)
        aa$sex = rep(c('male','femberr'),each=length(p$years.to.estimate))
        save(aa,file=file.path(project.datadirectory('bio.lobster'),'analysis','sexLFA41NEFSCspringrestratified.rdata  '))

# adjacent
    p$define.by.polygons = T
      p$lobster.subunits=F
      p$area = 'adjacentLFA41'
      p$reweight.strata = T #this subsets 
      
   	    a = list(1,c(2,3))
        out = list()
        for(i in 1:length(a)) {
          p$sex=a[[i]]
          out[[i]] = nefsc.analysis(DS='stratified.estimates.redo',p=p)
          }

        aa = do.call(rbind,out)
        aa$sex = rep(c('male','femberr'),each=length(p$years.to.estimate))
        save(aa,file=file.path(project.datadirectory('bio.lobster'),'analysis','sexLFA41NEFSCspringadjrestratified.rdata  '))



# fall survey All stations including adjacent
                        p$season =c('fall')# p$series =c('spring');p$series =c('fall')
                        p$define.by.polygons = F
                        p$lobster.subunits=F
                        p$area = 'LFA41'
                      p = make.list(list(yrs=p$years.to.estimate),Y=p)
                    


   	    a = list(1,c(2,3))
        out = list()
        for(i in 1:length(a)) {
          p$sex=a[[i]]

          out[[i]] = nefsc.analysis(DS='stratified.estimates.redo',p=p)
          }

        aa = do.call(rbind,out)
        aa$sex = rep(c('male','femberr'),each=length(p$years.to.estimate))
        save(aa,file=file.path(project.datadirectory('bio.lobster'),'analysis','sexLFA41NEFSCfallbase.rdata  '))

# restratified
    p$define.by.polygons = T
      p$lobster.subunits=F
      p$area = 'LFA41'
      p$reweight.strata = T #this subsets 
      
   	    a = list(1,c(2,3))
        out = list()
        for(i in 1:length(a)) {
          p$sex=a[[i]]
          out[[i]] = nefsc.analysis(DS='stratified.estimates.redo',p=p)
          }

        aa = do.call(rbind,out)
        aa$sex = rep(c('male','femberr'),each=length(p$years.to.estimate))
        save(aa,file=file.path(project.datadirectory('bio.lobster'),'analysis','sexLFA41NEFSCfallrestratified.rdata  '))

# adjacent
    p$define.by.polygons = T
      p$lobster.subunits=F
      p$area = 'adjacentLFA41'
      p$reweight.strata = T #this subsets 
      
   	    a = list(1,c(2,3))
        out = list()
        for(i in 1:length(a)) {
          p$sex=a[[i]]
          out[[i]] = nefsc.analysis(DS='stratified.estimates.redo',p=p)
          }

        aa = do.call(rbind,out)
        aa$sex = rep(c('male','femberr'),each=length(p$years.to.estimate))
        save(aa,file=file.path(project.datadirectory('bio.lobster'),'analysis','sexLFA41NEFSCfalladjrestratified.rdata  '))


#DFO Georges
      p$series =c('georges')# p$series =c('georges');p$series =c('fall')
      p$define.by.polygons = F
      p$lobster.subunits=F
      p$years.to.estimate = c(2007:2015)
      p$length.based = F
      p$by.sex = T
      p$bootstrapped.ci=F
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
      

   	    a = list(1,c(2,3))
        out = list()
        for(i in 1:length(a)) {
          p$sex=a[[i]]
          out[[i]] = dfo.rv.analysis(DS='stratified.estimates.redo',p=p)
          }

        aa = do.call(rbind,out)
        aa$sex = rep(c('male','femberr'),each=length(p$years.to.estimate))
        save(aa,file=file.path(project.datadirectory('bio.lobster'),'analysis','sexLFA41dfogeorges.rdata  '))
    
##################################### 
#####mature lobster sex ratios
# sex ratios surveys

require(bio.survey)
require(bio.lobster)
p = bio.lobster::load.environment()
p$libs = NULL
fp = file.path(project.datadirectory('bio.lobster'),"analysis")
la()
load_all('~/git/bio.survey/')

#by length for histograms
#DFO RV Analysis

      p$series =c('summer')# p$series =c('georges');p$series =c('fall')
      p$define.by.polygons = F
      p$lobster.subunits=F
      p$area = 'LFA41'
      p$years.to.estimate = c(1999:2015)
      p$length.based = T
      p$by.sex = T
      p$bootstrapped.ci=F
      p$strata.files.return=F
      p$vessel.correction.fixed=1.2
      p$strat = NULL
      p$clusters = c( rep( "localhost", 7) )
      p$strata.efficiencies = F
      p = make.list(list(yrs=p$years.to.estimate),Y=p)
      p$size.class = c(95,300)
      

# DFO survey All stations including adjacent
      p$define.by.polygons = F
      p$lobster.subunits=F
      p$area = 'LFA41'
      p$reweight.strata = F #this subsets 
      p$length.based=T
      
      #full strata
        a = list(1,c(2,3))
        out = list()
        for(i in 1:length(a)) {
          p$sex=a[[i]]
          out[[i]] = dfo.rv.analysis(DS='stratified.estimates.redo',p=p,save=F)
          }

        aa = do.call(rbind,out)
        aa$sex = rep(c('male','femberr'),each=length(p$years.to.estimate))
                          	    
   	    save(aa,file=file.path(project.datadirectory('bio.lobster'),'analysis','maturesexLFA41baseSummerRV.rdata  '))

#restratified
        require(bio.lobster)
        p = bio.lobster::load.environment()
        p$libs = NULL

        fp = file.path(project.datadirectory('bio.lobster'),"analysis")

        p$series =c('summer')# p$series =c('georges');p$series =c('fall')
        p$define.by.polygons = T
        p$lobster.subunits=F
        p$area = 'LFA41'
        p$reweight.strata = T #this subsets 
        p$years.to.estimate = c(1999:2015)
        p$length.based = T
        p$by.sex = T
        p$bootstrapped.ci=F
        p$strata.files.return=F
        p$vessel.correction.fixed=1.2
        p$strat = NULL
        p$clusters = c( rep( "localhost", 7) )
        p$strata.efficiencies = F
        p = make.list(list(yrs=p$years.to.estimate),Y=p)
        p$size.class = c(95,300)
        a = list(1,c(2,3))
        out = list()
        for(i in 1:length(a)) {
          p$sex=a[[i]]
          out[[i]] = dfo.rv.analysis(DS='stratified.estimates.redo',p=p,save=F)
          }

        aa = do.call(rbind,out)
		aa$sex = rep(c('male','femberr'),each=length(p$years.to.estimate))
        save(aa,file=file.path(project.datadirectory('bio.lobster'),'analysis','maturesexLFA41polygonSummerRV.rdata  '))

#adjacent
        require(bio.lobster)
        p = bio.lobster::load.environment()
        p$libs = NULL
        fp = file.path(project.datadirectory('bio.lobster'),"analysis")


        p$series =c('summer')# p$series =c('georges');p$series =c('fall')
        p$define.by.polygons = T
        p$lobster.subunits=F
        p$area = 'adjacentLFA41'
        p$reweight.strata = T #this subsets 
        p$years.to.estimate = c(1999:2015)
        p$length.based = T
        p$by.sex = T
        p$bootstrapped.ci=F
        p$strata.files.return=F
        p$vessel.correction.fixed=1.2
        p$strat = NULL
        p$clusters = c( rep( "localhost", 7) )
        p$strata.efficiencies = F
        p = make.list(list(yrs=p$years.to.estimate),Y=p)
        p$size.class=c(95,300)
        a = list(1,c(2,3))
        out = list()
        for(i in 1:length(a)) {
          p$sex=a[[i]]
          out[[i]] = dfo.rv.analysis(DS='stratified.estimates.redo',p=p,save=F)
          }

        aa = do.call(rbind,out)
        aa$sex = rep(c('male','femberr'),each=length(p$years.to.estimate))
        save(aa,file=file.path(project.datadirectory('bio.lobster'),'analysis','maturesexLFA41adjacentpolygonSummerRV.rdata  '))


#NEFSC spring


      p$reweight.strata = F #this subsets 
      p$years.to.estimate = c(1969:2015)
      p$length.based = T
      p$size.class= c(95,300)
      p$by.sex = T
      p$sex = c(1,2) # male female berried c(1,2,3)
      p$bootstrapped.ci=F
      p$strata.files.return=F
      p$strata.efficiencies=F
      p$clusters = c( rep( "localhost", 7) )
  
       p = make.list(list(yrs=p$years.to.estimate),Y=p)
      

# Spring survey All stations including adjacent
                        p$season =c('spring')# p$series =c('spring');p$series =c('fall')
                        p$define.by.polygons = F
     
        a = list(1,c(2,3))
        out = list()
        for(i in 1:length(a)) {
          p$sex=a[[i]]
          out[[i]] = nefsc.analysis(DS='stratified.estimates.redo',p=p)
          }

        aa = do.call(rbind,out)
        aa$sex = rep(c('male','femberr'),each=length(p$years.to.estimate))
        save(aa,file=file.path(project.datadirectory('bio.lobster'),'analysis','maturesexLFA41NEFSCspringbase.rdata  '))

# restratified
    p$define.by.polygons = T
      p$lobster.subunits=F
      p$area = 'LFA41'
      p$reweight.strata = T #this subsets 
      
   	    a = list(1,c(2,3))
        out = list()
        for(i in 1:length(a)) {
          p$sex=a[[i]]
          out[[i]] = nefsc.analysis(DS='stratified.estimates.redo',p=p)
          }

        aa = do.call(rbind,out)
        aa$sex = rep(c('male','femberr'),each=length(p$years.to.estimate))
        save(aa,file=file.path(project.datadirectory('bio.lobster'),'analysis','maturesexLFA41NEFSCspringrestratified.rdata  '))

# adjacent
    p$define.by.polygons = T
      p$lobster.subunits=F
      p$area = 'adjacentLFA41'
      p$reweight.strata = T #this subsets 
      
   	    a = list(1,c(2,3))
        out = list()
        for(i in 1:length(a)) {
          p$sex=a[[i]]
          out[[i]] = nefsc.analysis(DS='stratified.estimates.redo',p=p)
          }

        aa = do.call(rbind,out)
        aa$sex = rep(c('male','femberr'),each=length(p$years.to.estimate))
        save(aa,file=file.path(project.datadirectory('bio.lobster'),'analysis','maturesexLFA41NEFSCspringadjrestratified.rdata  '))



# Fall survey All stations including adjacent
                        p$season =c('fall')# p$series =c('spring');p$series =c('fall')
                        p$define.by.polygons = F
					     p$lobster.subunits=F
      					p$area = 'LFA41'
      					p$reweight.strata = F #this subsets 


   	    a = list(1,c(2,3))
        out = list()
        for(i in 1:length(a)) {
          p$sex=a[[i]]
          out[[i]] = nefsc.analysis(DS='stratified.estimates.redo',p=p)
          }

        aa = do.call(rbind,out)
        aa$sex = rep(c('male','femberr'),each=length(p$years.to.estimate))
        save(aa,file=file.path(project.datadirectory('bio.lobster'),'analysis','maturesexLFA41NEFSCfallbase.rdata  '))

# restratified
    p$define.by.polygons = T
      p$lobster.subunits=F
      p$area = 'LFA41'
      p$reweight.strata = T #this subsets 
      
   	    a = list(1,c(2,3))
        out = list()
        for(i in 1:length(a)) {
          p$sex=a[[i]]
          out[[i]] = nefsc.analysis(DS='stratified.estimates.redo',p=p)
          }

        aa = do.call(rbind,out)
        aa$sex = rep(c('male','femberr'),each=length(p$years.to.estimate))
        save(aa,file=file.path(project.datadirectory('bio.lobster'),'analysis','maturesexLFA41NEFSCfallrestratified.rdata  '))

# adjacent
    p$define.by.polygons = T
      p$lobster.subunits=F
      p$area = 'adjacentLFA41'
      p$reweight.strata = T #this subsets 
      
   	    a = list(1,c(2,3))
        out = list()
        for(i in 1:length(a)) {
          p$sex=a[[i]]
          out[[i]] = nefsc.analysis(DS='stratified.estimates.redo',p=p)
          }

        aa = do.call(rbind,out)
        aa$sex = rep(c('male','femberr'),each=length(p$years.to.estimate))
        save(aa,file=file.path(project.datadirectory('bio.lobster'),'analysis','maturesexLFA41NEFSCfalladjrestratified.rdata  '))





#DFO Georges
      p$series =c('georges')# p$series =c('georges');p$series =c('fall')
      p$define.by.polygons = F
      p$lobster.subunits=F
      p$years.to.estimate = c(2007:2015)
      p$length.based = T
      p$by.sex = T
      p$bootstrapped.ci=F
      p$strata.files.return=F
      p$vessel.correction.fixed=1.2
      p$strat = NULL
      p$clusters = c( rep( "localhost", 7) )
      p$strata.efficiencies = F
      p$size.class = c(95,300)
      p = make.list(list(yrs=p$years.to.estimate),Y=p)

      

# DFO survey All stations including adjacent
      p$define.by.polygons = F
      p$lobster.subunits=F
      p$area = 'Georges.Canada'
      p$reweight.strata = F #this subsets 
      

   	    a = list(1,c(2,3))
        out = list()
        for(i in 1:length(a)) {
          p$sex=a[[i]]
          out[[i]] = dfo.rv.analysis(DS='stratified.estimates.redo',p=p,save=F)
          }

        aa = do.call(rbind,out)
        aa$sex = rep(c('male','femberr'),each=length(p$years.to.estimate))
        save(aa,file=file.path(project.datadirectory('bio.lobster'),'analysis','maturesexLFA41dfogeorges.rdata  '))
    


#####immature lobster sex ratios
# sex ratios surveys

require(bio.survey)
require(bio.lobster)
p = bio.lobster::load.environment()
p$libs = NULL
fp = file.path(project.datadirectory('bio.lobster'),"analysis")
la()
load_all('~/git/bio.survey/')

#by length for histograms
#DFO RV Analysis

      p$series =c('summer')# p$series =c('georges');p$series =c('fall')
      p$define.by.polygons = F
      p$lobster.subunits=F
      p$area = 'LFA41'
      p$years.to.estimate = c(1999:2015)
      p$length.based = T
      p$by.sex = T
      p$bootstrapped.ci=F
      p$strata.files.return=F
      p$vessel.correction.fixed=1.2
      p$strat = NULL
      p$clusters = c( rep( "localhost", 7) )
      p$strata.efficiencies = F
      p = make.list(list(yrs=p$years.to.estimate),Y=p)
      p$size.class = c(0,94)
      

# DFO survey All stations including adjacent
      p$define.by.polygons = F
      p$lobster.subunits=F
      p$area = 'LFA41'
      p$reweight.strata = F #this subsets 
      p$length.based=T
      
      #full strata
        a = list(1,c(2,3))
        out = list()
        for(i in 1:length(a)) {
          p$sex=a[[i]]
          out[[i]] = dfo.rv.analysis(DS='stratified.estimates.redo',p=p,save=F)
          }

        aa = do.call(rbind,out)
        aa$sex = rep(c('male','femberr'),each=length(p$years.to.estimate))
                                
        save(aa,file=file.path(project.datadirectory('bio.lobster'),'analysis','immaturesexLFA41baseSummerRV.rdata  '))

#restratified
        require(bio.lobster)
        p = bio.lobster::load.environment()
        p$libs = NULL

        fp = file.path(project.datadirectory('bio.lobster'),"analysis")

        p$series =c('summer')# p$series =c('georges');p$series =c('fall')
        p$define.by.polygons = T
        p$lobster.subunits=F
        p$area = 'LFA41'
        p$reweight.strata = T #this subsets 
        p$years.to.estimate = c(1999:2015)
        p$length.based = T
        p$by.sex = T
        p$bootstrapped.ci=F
        p$strata.files.return=F
        p$vessel.correction.fixed=1.2
        p$strat = NULL
        p$clusters = c( rep( "localhost", 7) )
        p$strata.efficiencies = F
        p = make.list(list(yrs=p$years.to.estimate),Y=p)
        p$size.class = c(0,94)
        a = list(1,c(2,3))
        out = list()
        for(i in 1:length(a)) {
          p$sex=a[[i]]
          out[[i]] = dfo.rv.analysis(DS='stratified.estimates.redo',p=p,save=F)
          }

        aa = do.call(rbind,out)
    aa$sex = rep(c('male','femberr'),each=length(p$years.to.estimate))
        save(aa,file=file.path(project.datadirectory('bio.lobster'),'analysis','immaturesexLFA41polygonSummerRV.rdata  '))

#adjacent
        require(bio.lobster)
        p = bio.lobster::load.environment()
        p$libs = NULL
        fp = file.path(project.datadirectory('bio.lobster'),"analysis")


        p$series =c('summer')# p$series =c('georges');p$series =c('fall')
        p$define.by.polygons = T
        p$lobster.subunits=F
        p$area = 'adjacentLFA41'
        p$reweight.strata = T #this subsets 
        p$years.to.estimate = c(1999:2015)
        p$length.based = T
        p$by.sex = T
        p$bootstrapped.ci=F
        p$strata.files.return=F
        p$vessel.correction.fixed=1.2
        p$strat = NULL
        p$clusters = c( rep( "localhost", 7) )
        p$strata.efficiencies = F
        p = make.list(list(yrs=p$years.to.estimate),Y=p)
        p$size.class=c(0,94)
        a = list(1,c(2,3))
        out = list()
        for(i in 1:length(a)) {
          p$sex=a[[i]]
          out[[i]] = dfo.rv.analysis(DS='stratified.estimates.redo',p=p,save=F)
          }

        aa = do.call(rbind,out)
        aa$sex = rep(c('male','femberr'),each=length(p$years.to.estimate))
        save(aa,file=file.path(project.datadirectory('bio.lobster'),'analysis','immaturesexLFA41adjacentpolygonSummerRV.rdata  '))


#NEFSC spring


      p$reweight.strata = F #this subsets 
      p$years.to.estimate = c(1969:2015)
      p$length.based = T
      p$size.class= c(0,94)
      p$by.sex = T
      p$sex = c(1,2) # male female berried c(1,2,3)
      p$bootstrapped.ci=F
      p$strata.files.return=F
      p$strata.efficiencies=F
      p$clusters = c( rep( "localhost", 7) )
  
       p = make.list(list(yrs=p$years.to.estimate),Y=p)
      

# Spring survey All stations including adjacent
                        p$season =c('spring')# p$series =c('spring');p$series =c('fall')
                        p$define.by.polygons = F
     
        a = list(1,c(2,3))
        out = list()
        for(i in 1:length(a)) {
          p$sex=a[[i]]
          out[[i]] = nefsc.analysis(DS='stratified.estimates.redo',p=p)
          }

        aa = do.call(rbind,out)
        aa$sex = rep(c('male','femberr'),each=length(p$years.to.estimate))
        save(aa,file=file.path(project.datadirectory('bio.lobster'),'analysis','immaturesexLFA41NEFSCspringbase.rdata  '))

# restratified
    p$define.by.polygons = T
      p$lobster.subunits=F
      p$area = 'LFA41'
      p$reweight.strata = T #this subsets 
      
        a = list(1,c(2,3))
        out = list()
        for(i in 1:length(a)) {
          p$sex=a[[i]]
          out[[i]] = nefsc.analysis(DS='stratified.estimates.redo',p=p)
          }

        aa = do.call(rbind,out)
        aa$sex = rep(c('male','femberr'),each=length(p$years.to.estimate))
        save(aa,file=file.path(project.datadirectory('bio.lobster'),'analysis','immaturesexLFA41NEFSCspringrestratified.rdata  '))

# adjacent
    p$define.by.polygons = T
      p$lobster.subunits=F
      p$area = 'adjacentLFA41'
      p$reweight.strata = T #this subsets 
      
        a = list(1,c(2,3))
        out = list()
        for(i in 1:length(a)) {
          p$sex=a[[i]]
          out[[i]] = nefsc.analysis(DS='stratified.estimates.redo',p=p)
          }

        aa = do.call(rbind,out)
        aa$sex = rep(c('male','femberr'),each=length(p$years.to.estimate))
        save(aa,file=file.path(project.datadirectory('bio.lobster'),'analysis','immaturesexLFA41NEFSCspringadjrestratified.rdata  '))



# Fall survey All stations including adjacent
                        p$season =c('fall')# p$series =c('spring');p$series =c('fall')
                        p$define.by.polygons = F
               p$lobster.subunits=F
                p$area = 'LFA41'
                p$reweight.strata = F #this subsets 


        a = list(1,c(2,3))
        out = list()
        for(i in 1:length(a)) {
          p$sex=a[[i]]
          out[[i]] = nefsc.analysis(DS='stratified.estimates.redo',p=p)
          }

        aa = do.call(rbind,out)
        aa$sex = rep(c('male','femberr'),each=length(p$years.to.estimate))
        save(aa,file=file.path(project.datadirectory('bio.lobster'),'analysis','immaturesexLFA41NEFSCfallbase.rdata  '))

# restratified
    p$define.by.polygons = T
      p$lobster.subunits=F
      p$area = 'LFA41'
      p$reweight.strata = T #this subsets 
      
        a = list(1,c(2,3))
        out = list()
        for(i in 1:length(a)) {
          p$sex=a[[i]]
          out[[i]] = nefsc.analysis(DS='stratified.estimates.redo',p=p)
          }

        aa = do.call(rbind,out)
        aa$sex = rep(c('male','femberr'),each=length(p$years.to.estimate))
        save(aa,file=file.path(project.datadirectory('bio.lobster'),'analysis','immaturesexLFA41NEFSCfallrestratified.rdata  '))

# adjacent
    p$define.by.polygons = T
      p$lobster.subunits=F
      p$area = 'adjacentLFA41'
      p$reweight.strata = T #this subsets 
      
        a = list(1,c(2,3))
        out = list()
        for(i in 1:length(a)) {
          p$sex=a[[i]]
          out[[i]] = nefsc.analysis(DS='stratified.estimates.redo',p=p)
          }

        aa = do.call(rbind,out)
        aa$sex = rep(c('male','femberr'),each=length(p$years.to.estimate))
        save(aa,file=file.path(project.datadirectory('bio.lobster'),'analysis','immaturesexLFA41NEFSCfalladjrestratified.rdata  '))





#DFO Georges
      p$series =c('georges')# p$series =c('georges');p$series =c('fall')
      p$define.by.polygons = F
      p$lobster.subunits=F
      p$years.to.estimate = c(2007:2015)
      p$length.based = T
      p$by.sex = T
      p$bootstrapped.ci=F
      p$strata.files.return=F
      p$vessel.correction.fixed=1.2
      p$strat = NULL
      p$clusters = c( rep( "localhost", 7) )
      p$strata.efficiencies = F
      p$size.class = c(0,94)
      p = make.list(list(yrs=p$years.to.estimate),Y=p)

      

# DFO survey All stations including adjacent
      p$define.by.polygons = F
      p$lobster.subunits=F
      p$area = 'Georges.Canada'
      p$reweight.strata = F #this subsets 
      

        a = list(1,c(2,3))
        out = list()
        for(i in 1:length(a)) {
          p$sex=a[[i]]
          out[[i]] = dfo.rv.analysis(DS='stratified.estimates.redo',p=p,save=F)
          }

        aa = do.call(rbind,out)
        aa$sex = rep(c('male','femberr'),each=length(p$years.to.estimate))
        save(aa,file=file.path(project.datadirectory('bio.lobster'),'analysis','immaturesexLFA41dfogeorges.rdata  '))
    



#adding sex ratios combined data frames

fpp = file.path(fp,'combinedResults')

load(file.path(fpp,'CombinedBaseStratifiedResults.rdata'))
load( file.path(fpp,'CombinedReStratifiedResults.rdata'))
load( file.path(fpp,'CombinedadjacentReStratifiedResults.rdata'))


fp = file.path(project.datadirectory('bio.lobster'),"analysis")
la()


      a = c(file.path(project.datadirectory('bio.lobster'),'analysis','sexLFA41baseSummerRV.rdata  '),
         file.path(project.datadirectory('bio.lobster'),'analysis','sexLFA41NEFSCfallbase.rdata  '),
         file.path(project.datadirectory('bio.lobster'),'analysis','sexLFA41NEFSCspringbase.rdata  '))


      b = c(file.path(project.datadirectory('bio.lobster'),'analysis','sexLFA41polygonSummerRV.rdata  ') ,
file.path(project.datadirectory('bio.lobster'),'analysis','sexLFA41NEFSCspringrestratified.rdata  '),
file.path(project.datadirectory('bio.lobster'),'analysis','sexLFA41NEFSCfallrestratified.rdata  '))
        

      d = c(file.path(project.datadirectory('bio.lobster'),'analysis','sexLFA41NEFSCfalladjrestratified.rdata  '),     
         file.path(project.datadirectory('bio.lobster'),'analysis','sexLFA41NEFSCspringadjrestratified.rdata  '), 
         file.path(project.datadirectory('bio.lobster'),'analysis','sexLFA41adjacentpolygonSummerRV.rdata  ')
         )



for(i in 1:length(a)) {
  load(a[i])
    ap = aggregate(n.yst~yr,data=aa,FUN=sum)
    af = aggregate(n.yst~yr,data=subset(aa,sex=='femberr'),FUN=sum)
    apf = merge(ap,af,by='yr')
    apf$pFem = apf$n.yst.y / apf$n.yst.x
    if(grepl('SummerRV',a[i])) apf$subset = 'DFOpFemAllsize'
    if(grepl('spring',a[i]))  apf$subset = 'NEFSCSpringpFemAllsize'
    if(grepl('fall',a[i]))  apf$subset = 'NEFSCFallpFemAllsize'
    base = append(base,list(apf))
  }


for(i in 1:length(b)) {
  load(b[i])
    ap = aggregate(n.yst~yr,data=aa,FUN=sum)
    af = aggregate(n.yst~yr,data=subset(aa,sex=='femberr'),FUN=sum)
    apf = merge(ap,af,by='yr')
    apf$pFem = apf$n.yst.y / apf$n.yst.x
    if(grepl('SummerRV',b[i])) apf$subset = 'DFOpFemAllsize'
    if(grepl('spring',b[i]))  apf$subset = 'NEFSCSpringpFemAllsize'
    if(grepl('fall',b[i]))  apf$subset = 'NEFSCFallpFemAllsize'
    restratified = append(restratified,list(apf))
  }


for(i in 1:length(d)) {
  load(d[i])
    ap = aggregate(n.yst~yr,data=aa,FUN=sum)
    af = aggregate(n.yst~yr,data=subset(aa,sex=='femberr'),FUN=sum)
    apf = merge(ap,af,by='yr')
    apf$pFem = apf$n.yst.y / apf$n.yst.x
    if(grepl('SummerRV',d[i])) apf$subset = 'DFOpFemAllsize'
    if(grepl('spring',d[i]))  apf$subset = 'NEFSCSpringpFemAllsize'
    if(grepl('fall',d[i]))  apf$subset = 'NEFSCFallpFemAllsize'
    adjacentrestratified = append(adjacentrestratified,list(apf))
  }





  ###mature sex ratios
        a = c(file.path(project.datadirectory('bio.lobster'),'analysis','maturesexLFA41baseSummerRV.rdata  '),
         file.path(project.datadirectory('bio.lobster'),'analysis','maturesexLFA41NEFSCfallbase.rdata  '),
         file.path(project.datadirectory('bio.lobster'),'analysis','maturesexLFA41NEFSCspringbase.rdata  '))


      b = c(file.path(project.datadirectory('bio.lobster'),'analysis','maturesexLFA41polygonSummerRV.rdata  ') ,
file.path(project.datadirectory('bio.lobster'),'analysis','maturesexLFA41NEFSCspringrestratified.rdata  '),
file.path(project.datadirectory('bio.lobster'),'analysis','maturesexLFA41NEFSCfallrestratified.rdata  '))
        

      d = c(file.path(project.datadirectory('bio.lobster'),'analysis','maturesexLFA41NEFSCfalladjrestratified.rdata  '),     
         file.path(project.datadirectory('bio.lobster'),'analysis','maturesexLFA41NEFSCspringadjrestratified.rdata  '), 
         file.path(project.datadirectory('bio.lobster'),'analysis','maturesexLFA41adjacentpolygonSummerRV.rdata  ')
         )



for(i in 1:length(a)) {
  load(a[i])
    ap = aggregate(n.yst~yr,data=aa,FUN=sum)
    af = aggregate(n.yst~yr,data=subset(aa,sex=='femberr'),FUN=sum)
    apf = merge(ap,af,by='yr')
    apf$pFem = apf$n.yst.y / apf$n.yst.x
    if(grepl('SummerRV',a[i])) apf$subset = 'DFOpFemmaturesize'
    if(grepl('spring',a[i]))  apf$subset = 'NEFSCSpringpFemmaturesize'
    if(grepl('fall',a[i]))  apf$subset = 'NEFSCFallpFemmaturesize'
    base = append(base,list(apf))
  }


for(i in 1:length(b)) {
  load(b[i])
    ap = aggregate(n.yst~yr,data=aa,FUN=sum)
    af = aggregate(n.yst~yr,data=subset(aa,sex=='femberr'),FUN=sum)
    apf = merge(ap,af,by='yr')
    apf$pFem = apf$n.yst.y / apf$n.yst.x
    if(grepl('SummerRV',b[i])) apf$subset = 'DFOpFemmaturesize'
    if(grepl('spring',b[i]))  apf$subset = 'NEFSCSpringpFemmaturesize'
    if(grepl('fall',b[i]))  apf$subset = 'NEFSCFallpFemmaturesize'
    restratified = append(restratified,list(apf))
  }


for(i in 1:length(d)) {
  load(d[i])
    ap = aggregate(n.yst~yr,data=aa,FUN=sum)
    af = aggregate(n.yst~yr,data=subset(aa,sex=='femberr'),FUN=sum)
    apf = merge(ap,af,by='yr')
    apf$pFem = apf$n.yst.y / apf$n.yst.x
    if(grepl('SummerRV',d[i])) apf$subset = 'DFOpFemmaturesize'
    if(grepl('spring',d[i]))  apf$subset = 'NEFSCSpringpFemmaturesize'
    if(grepl('fall',d[i]))  apf$subset = 'NEFSCFallpFemmaturesize'
    adjacentrestratified = append(adjacentrestratified,list(apf))
  }

  ###immature sex ratios
        a = c(file.path(project.datadirectory('bio.lobster'),'analysis','immaturesexLFA41baseSummerRV.rdata  '),
         file.path(project.datadirectory('bio.lobster'),'analysis','immaturesexLFA41NEFSCfallbase.rdata  '),
         file.path(project.datadirectory('bio.lobster'),'analysis','immaturesexLFA41NEFSCspringbase.rdata  '))


      b = c(file.path(project.datadirectory('bio.lobster'),'analysis','immaturesexLFA41polygonSummerRV.rdata  ') ,
file.path(project.datadirectory('bio.lobster'),'analysis','immaturesexLFA41NEFSCspringrestratified.rdata  '),
file.path(project.datadirectory('bio.lobster'),'analysis','immaturesexLFA41NEFSCfallrestratified.rdata  '))
        

      d = c(file.path(project.datadirectory('bio.lobster'),'analysis','immaturesexLFA41NEFSCfalladjrestratified.rdata  '),     
         file.path(project.datadirectory('bio.lobster'),'analysis','immaturesexLFA41NEFSCspringadjrestratified.rdata  '), 
         file.path(project.datadirectory('bio.lobster'),'analysis','immaturesexLFA41adjacentpolygonSummerRV.rdata  ')
         )



for(i in 1:length(a)) {
  load(a[i])
    ap = aggregate(n.yst~yr,data=aa,FUN=sum)
    af = aggregate(n.yst~yr,data=subset(aa,sex=='femberr'),FUN=sum)
    apf = merge(ap,af,by='yr')
    apf$pFem = apf$n.yst.y / apf$n.yst.x
    if(grepl('SummerRV',a[i])) apf$subset = 'DFOpFemimmaturesize'
    if(grepl('spring',a[i]))  apf$subset = 'NEFSCSpringpFemimmaturesize'
    if(grepl('fall',a[i]))  apf$subset = 'NEFSCFallpFemimmaturesize'
    base = append(base,list(apf))
  }


for(i in 1:length(b)) {
  load(b[i])
    ap = aggregate(n.yst~yr,data=aa,FUN=sum)
    af = aggregate(n.yst~yr,data=subset(aa,sex=='femberr'),FUN=sum)
    apf = merge(ap,af,by='yr')
    apf$pFem = apf$n.yst.y / apf$n.yst.x
    if(grepl('SummerRV',b[i])) apf$subset = 'DFOpFemimmaturesize'
    if(grepl('spring',b[i]))  apf$subset = 'NEFSCSpringpFemimmaturesize'
    if(grepl('fall',b[i]))  apf$subset = 'NEFSCFallpFemimmaturesize'
    restratified = append(restratified,list(apf))
  }


for(i in 1:length(d)) {
  load(d[i])
    ap = aggregate(n.yst~yr,data=aa,FUN=sum)
    af = aggregate(n.yst~yr,data=subset(aa,sex=='femberr'),FUN=sum)
    apf = merge(ap,af,by='yr')
    apf$pFem = apf$n.yst.y / apf$n.yst.x
    if(grepl('SummerRV',d[i])) apf$subset = 'DFOpFemimmaturesize'
    if(grepl('spring',d[i]))  apf$subset = 'NEFSCSpringpFemimmaturesize'
    if(grepl('fall',d[i]))  apf$subset = 'NEFSCFallpFemimmaturesize'
    adjacentrestratified = append(adjacentrestratified,list(apf))
  }



save(base,file = file.path(fpp,'CombinedBaseStratifiedResults.rdata'))
save(restratified, file = file.path(fpp,'CombinedReStratifiedResults.rdata'))
save(adjacentrestratified,file = file.path(fpp,'CombinedadjacentReStratifiedResults.rdata'))


