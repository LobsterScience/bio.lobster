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
    
#population weighted median FLEN
	l = c()
	y = p$years.to.estimate
	for(i in 1:length(y)) {
			w = subset(aa,yr==y[i])
			l[i] = median(rep(w$FLEN,times=w$n.yst*1000))
				}

}
