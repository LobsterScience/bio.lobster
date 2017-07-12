# sex ratios surveys
require(bio.survey)
require(bio.groundfish)
require(bio.lobster)
la()
p = bio.lobster::load.environment()
p$libs = NULL
fp = file.path(project.datadirectory('bio.lobster'),"analysis")
la()
load_all('~/git/bio.survey/')

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

      

#restratified

        a = list(1,c(2,3))
        out = list()
        for(i in 1:length(a)) {
          p$sex=a[[i]]
          out[[i]] = dfo.rv.analysis(DS='stratified.estimates.redo',p=p,save=F)
          }

        aa = do.call(rbind,out)
		aa$sex = rep(c('male','femberr'),each=length(p$years.to.estimate))
        save(aa,file=file.path(project.datadirectory('bio.lobster'),'analysis','sexLFA41polygonSummerRV.rdata  '))
    write.csv(aa,file=file.path(project.datadirectory('bio.lobster'),'analysis','indicators','DFO.restratified.SexRatio.csv  '))



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
  
      

                 
# restratified
    p$season =c('spring')# p$series =c('spring');p$series =c('fall')
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
     write.csv(aa,file=file.path(project.datadirectory('bio.lobster'),'analysis','indicators','NEFSC.spring.restratified.SexRatio.csv  '))





# Fall restratified
    p$season =c('fall')# p$series =c('spring');p$series =c('fall')
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
        write.csv(aa,file=file.path(project.datadirectory('bio.lobster'),'analysis','indicators','NEFSC.fall.restratified.SexRatio.csv  '))


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
        write.csv(aa,file=file.path(project.datadirectory('bio.lobster'),'analysis','indicators','DFO.georges.SexRatio.csv  '))

##################################### 
#####mature lobster sex ratios
# sex ratios surveys


#DFO RV Analysis
#restratified
        require(bio.lobster)
        p = bio.lobster::load.environment()
        p$libs = NULL

        fp = file.path(project.datadirectory('bio.lobster'),"analysis")
        la()
        load_all('~/git/bio.survey/')

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
        p$size.class = c(92,300)
        a = list(1,c(2,3))
        out = list()
        for(i in 1:length(a)) {
          p$sex=a[[i]]
          out[[i]] = dfo.rv.analysis(DS='stratified.estimates.redo',p=p,save=F)
          }

        aa = do.call(rbind,out)
		aa$sex = rep(c('male','femberr'),each=length(p$years.to.estimate))
        save(aa,file=file.path(project.datadirectory('bio.lobster'),'analysis','maturesexLFA41polygonSummerRV.rdata  '))
    write.csv(aa,file=file.path(project.datadirectory('bio.lobster'),'analysis','indicators','DFO.restratified.Mature.SexRatio.csv  '))


#NEFSC spring


      p$reweight.strata = F #this subsets 
      p$years.to.estimate = c(1969:2015)
      p$length.based = T
      p$size.class= c(92,300)
      p$by.sex = T
      p$sex = c(1,2) # male female berried c(1,2,3)
      p$bootstrapped.ci=F
      p$strata.files.return=F
      p$strata.efficiencies=F
      p$clusters = c( rep( "localhost", 7) )
  
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
  write.csv(aa,file=file.path(project.datadirectory('bio.lobster'),'analysis','indicators','NEFSC.spring.restratified.Mature.SexRatio.csv  '))

# Fall survey 
# restratified
    p$season =c('fall')# p$series =c('spring');p$series =c('fall')
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
  write.csv(aa,file=file.path(project.datadirectory('bio.lobster'),'analysis','indicators','NEFSC.fall.restratified.Mature.SexRatio.csv  '))



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
      p$size.class = c(92,300)
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
write.csv(aa,file=file.path(project.datadirectory('bio.lobster'),'analysis','indicators','DFO.Georges.Mature.SexRatio.csv  '))
    


#####immature lobster sex ratios
# sex ratios surveys
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
        p$size.class = c(0,91)
        a = list(1,2)
        out = list()
        for(i in 1:length(a)) {
          p$sex=a[[i]]
          out[[i]] = dfo.rv.analysis(DS='stratified.estimates.redo',p=p,save=F)
          }

        aa = do.call(rbind,out)
    aa$sex = rep(c('male','femberr'),each=length(p$years.to.estimate))
        save(aa,file=file.path(project.datadirectory('bio.lobster'),'analysis','immaturesexLFA41polygonSummerRV.rdata  '))
     write.csv(aa,file=file.path(project.datadirectory('bio.lobster'),'analysis','indicators','DFO.restratified.immature.SexRatio.csv  '))

#NEFSC spring


      p$reweight.strata = F #this subsets 
      p$years.to.estimate = c(1969:2015)
      p$length.based = T
      p$size.class= c(50,91)
      p$by.sex = T
      p$sex = c(1,2) # male female berried c(1,2,3)
      p$bootstrapped.ci=F
      p$strata.files.return=F
      p$strata.efficiencies=F
      p$clusters = c( rep( "localhost", 7) )
       p = make.list(list(yrs=p$years.to.estimate),Y=p)
      

# restratified
    p$season =c('spring')# p$series =c('spring');p$series =c('fall')
    p$define.by.polygons = T
      p$lobster.subunits=F
      p$area = 'LFA41'
      p$reweight.strata = T #this subsets 
      
        a = list(1,2))
        out = list()
        for(i in 1:length(a)) {
          p$sex=a[[i]]
          out[[i]] = nefsc.analysis(DS='stratified.estimates.redo',p=p)
          }

        aa = do.call(rbind,out)
        aa$sex = rep(c('male','femberr'),each=length(p$years.to.estimate))
        save(aa,file=file.path(project.datadirectory('bio.lobster'),'analysis','immaturesexLFA41NEFSCspringrestratified.rdata  '))
     write.csv(aa,file=file.path(project.datadirectory('bio.lobster'),'analysis','indicators','NEFSC.spring.restratified.immature.SexRatio.csv  '))



# restratified
      p$season =c('fall')# p$series =c('spring');p$series =c('fall')
      p$define.by.polygons = T
      p$lobster.subunits=F
      p$area = 'LFA41'
      p$reweight.strata = T #this subsets 
      
        a = list(1,2))
        out = list()
        for(i in 1:length(a)) {
          p$sex=a[[i]]
          out[[i]] = nefsc.analysis(DS='stratified.estimates.redo',p=p)
          }

        aa = do.call(rbind,out)
        aa$sex = rep(c('male','femberr'),each=length(p$years.to.estimate))
        save(aa,file=file.path(project.datadirectory('bio.lobster'),'analysis','immaturesexLFA41NEFSCfallrestratified.rdata  '))
  write.csv(aa,file=file.path(project.datadirectory('bio.lobster'),'analysis','indicators','NEFSC.fall.restratified.immature.SexRatio.csv  '))

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
      p$size.class = c(0,91)
      p = make.list(list(yrs=p$years.to.estimate),Y=p)

      

# DFO survey All stations including adjacent
      p$define.by.polygons = F
      p$lobster.subunits=F
      p$area = 'Georges.Canada'
      p$reweight.strata = F #this subsets 
      

        a = list(1,2))
        out = list()
        for(i in 1:length(a)) {
          p$sex=a[[i]]
          out[[i]] = dfo.rv.analysis(DS='stratified.estimates.redo',p=p,save=F)
          }

        aa = do.call(rbind,out)
        aa$sex = rep(c('male','femberr'),each=length(p$years.to.estimate))
        save(aa,file=file.path(project.datadirectory('bio.lobster'),'analysis','immaturesexLFA41dfogeorges.rdata  '))
      write.csv(aa,file=file.path(project.datadirectory('bio.lobster'),'analysis','indicators','DFO.Georges.base.immature.SexRatio.csv  '))




#adding sex ratios combined data frames

fp = file.path(project.datadirectory('bio.lobster'),"analysis","indicators")
h = dir(fp)

hi = h[grep('SexRatio',h)]

for(i in hi) {
    j = read.csv(file.path(fp,i))
    g = aggregate(cbind(n.yst,ObsLobs)~yr,data=j,FUN=sum)
    k = subset(j,sex=='femberr',select=c('yr','n.yst'))
  
    l = merge(g,k,by='yr')

    l$sexratio = l$n.yst.y / l$n.yst.x
    l = l[,c('yr','sexratio','ObsLobs')]
    write.csv(l,file=file.path(fp,i))

}