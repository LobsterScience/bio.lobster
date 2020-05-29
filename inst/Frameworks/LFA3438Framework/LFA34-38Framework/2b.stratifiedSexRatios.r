# sex ratios surveys
require(bio.survey)
require(bio.groundfish)
require(bio.lobster)
la()
p = bio.lobster::load.environment()
p$libs = NULL
la()
load_all('~/git/bio.survey/')
fp1 = file.path(project.datadirectory('bio.lobster'),"analysis","LFA34-38")
fpf1 = file.path(project.figuredirectory('bio.lobster'),"LFA3438Framework2019")
p1= p 
la()
load_all('~/git/bio.survey/')



stratifiedAnalysesSexRatios = function(stage = 'all', p=p1, survey,lfa, fpf = fpf1, fp = fp1){

if(stage=='all'){
if(survey=='DFO'){
      p$series =c('summer')
      p$define.by.polygons = T
      p$lobster.subunits=F
      p$area = lfa
      p$years.to.estimate = c(1999:2018)
      p$length.based = F
      p$by.sex = T
      p$bootstrapped.ci=F
      p$strata.files.return=F
      p$vessel.correction.fixed=1.2
      p$strat = NULL
      p$clusters = c( rep( "localhost", 7) )
      p$strata.efficiencies = F
      p = make.list(list(yrs=p$years.to.estimate),Y=p)
      p$reweight.strata = T 

       a = list(1,c(2,3))
        out = list()
        for(i in 1:length(a)) {
          p$sex=a[[i]]
          out[[i]] = dfo.rv.analysis(DS='stratified.estimates.redo',p=p,save=F)
          }

        aa = do.call(rbind,out)
		
repers = dim(aa)[1]/2
   
        aa$sex = rep(c('male','femberr'),each=repers)
       
        save(aa,file=file.path(fp,'indicators',paste(lfa,'sexpolygonSummerRV.rdata',sep="")))
    write.csv(aa,file=file.path(fp,'indicators',paste(lfa,'DFO.restratified.SexRatio.csv',sep="")))
  }

if(survey=='NEFSC'){
#NEFSC spring
      p$years.to.estimate = c(1969:2018)
      p$length.based = T
      p$size.class= c(50,300)
      p$by.sex = T
      p$bootstrapped.ci=F
      p$strata.files.return=F
      p$strata.efficiencies=F
      p$clusters = c( rep( "localhost", 7) )
      p$season =c('spring')# p$series =c('spring');p$series =c('fall')
      p$define.by.polygons = T
      p$lobster.subunits=F
      p$area = lfa
      p$reweight.strata = T #this subsets 
      p = make.list(list(yrs=p$years.to.estimate),Y=p)
           
   	    a = list(1,c(2,3))
        out = list()
        for(i in 1:length(a)) {
          p$sex=a[[i]]
          out[[i]] = nefsc.analysis(DS='stratified.estimates.redo',p=p)
          }

        aa = do.call(rbind,out)
       repers = dim(aa)[1]/2
   
        aa$sex = rep(c('male','femberr'),each=repers)
       
        save(aa,file=file.path(fp,'indicators',paste(lfa,'sexNEFSCspringrestratified.rdata',sep="")))
     write.csv(aa,file=file.path(fp,'indicators',paste(lfa,'NEFSC.spring.restratified.SexRatio.csv',sep="")))


# Fall restratified
    p$season =c('fall')# p$series =c('spring');p$series =c('fall')
     
   	    a = list(1,c(2,3))
        out = list()
        for(i in 1:length(a)) {
          p$sex=a[[i]]
          out[[i]] = nefsc.analysis(DS='stratified.estimates.redo',p=p)
          }

        aa = do.call(rbind,out)
       repers = dim(aa)[1]/2
   
        aa$sex = rep(c('male','femberr'),each=repers)
       
        save(aa,file=file.path(fp,'indicators',paste(lfa,'sexNEFSCfallrestratified.rdata',sep="")))
     write.csv(aa,file=file.path(fp,'indicators',paste(lfa,'NEFSC.fall.restratified.SexRatio.csv',sep="")))
}
}

if(stage=='mature'){
  if(survey=='DFO'){
      p$series =c('summer')
      p$define.by.polygons = T
      p$lobster.subunits=F
      p$area = lfa
      p$years.to.estimate = c(1999:2018)
      p$length.based = T
      p$size.class = c(92,300)
      p$by.sex = T
      p$bootstrapped.ci=F
      p$strata.files.return=F
      p$vessel.correction.fixed=1.2
      p$strat = NULL
      p$clusters = c( rep( "localhost", 7) )
      p$strata.efficiencies = F
      p = make.list(list(yrs=p$years.to.estimate),Y=p)
      p$reweight.strata = T 

       a = list(1,c(2,3))
        out = list()
        for(i in 1:length(a)) {
          p$sex=a[[i]]
          out[[i]] = dfo.rv.analysis(DS='stratified.estimates.redo',p=p,save=F)
          }

        aa = do.call(rbind,out)
    repers = dim(aa)[1]/2
   
        aa$sex = rep(c('male','femberr'),each=repers)
       
        save(aa,file=file.path(fp,'indicators',paste(lfa,'maturesexpolygonSummerRV.rdata',sep="")))
    write.csv(aa,file=file.path(fp,'indicators',paste(lfa,'mature.DFO.restratified.SexRatio.csv',sep="")))
  }

if(survey=='NEFSC'){
#NEFSC spring
      p$years.to.estimate = c(1969:2018)
      p$length.based = T
      p$size.class= c(92,300)
      p$by.sex = T
      p$bootstrapped.ci=F
      p$strata.files.return=F
      p$strata.efficiencies=F
      p$clusters = c( rep( "localhost", 7) )
      p$season =c('spring')# p$series =c('spring');p$series =c('fall')
      p$define.by.polygons = T
      p$lobster.subunits=F
      p$area = lfa
      p$reweight.strata = T #this subsets 
       p = make.list(list(yrs=p$years.to.estimate),Y=p)
     
        a = list(1,c(2,3))
        out = list()
        for(i in 1:length(a)) {
          p$sex=a[[i]]
          out[[i]] = nefsc.analysis(DS='stratified.estimates.redo',p=p)
          }

        aa = do.call(rbind,out)
       repers = dim(aa)[1]/2
   
        aa$sex = rep(c('male','femberr'),each=repers)
       
        save(aa,file=file.path(fp,'indicators',paste(lfa,'mature.sexNEFSCspringrestratified.rdata',sep="")))
     write.csv(aa,file=file.path(fp,'indicators',paste(lfa,'mature.NEFSC.spring.restratified.SexRatio.csv',sep="")))


# Fall restratified
    p$season =c('fall')# p$series =c('spring');p$series =c('fall')
     
        a = list(1,c(2,3))
        out = list()
        for(i in 1:length(a)) {
          p$sex=a[[i]]
          out[[i]] = nefsc.analysis(DS='stratified.estimates.redo',p=p)
          }

        aa = do.call(rbind,out)
       repers = dim(aa)[1]/2
   
        aa$sex = rep(c('male','femberr'),each=repers)
       
        save(aa,file=file.path(fp,'indicators',paste(lfa,'mature.sexNEFSCfallrestratified.rdata',sep="")))
     write.csv(aa,file=file.path(fp,'indicators',paste(lfa,'mature.NEFSC.fall.restratified.SexRatio.csv',sep="")))
}
}

if(stage=='immature'){
  if(survey=='DFO'){
      p$series =c('summer')
      p$define.by.polygons = T
      p$lobster.subunits=F
      p$area = lfa
      p$years.to.estimate = c(1999:2018)
      p$length.based = T
      p$size.class = c(25,91)
      p$by.sex = T
      p$bootstrapped.ci=F
      p$strata.files.return=F
      p$vessel.correction.fixed=1.2
      p$strat = NULL
      p$clusters = c( rep( "localhost", 7) )
      p$strata.efficiencies = F
      p = make.list(list(yrs=p$years.to.estimate),Y=p)
      p$reweight.strata = T 

       a = list(1,c(2,3))
        out = list()
        for(i in 1:length(a)) {
          p$sex=a[[i]]
          out[[i]] = dfo.rv.analysis(DS='stratified.estimates.redo',p=p,save=F)
          }

        aa = do.call(rbind,out)
   repers = dim(aa)[1]/2
   
        aa$sex = rep(c('male','femberr'),each=repers)
       
        save(aa,file=file.path(fp,'indicators',paste(lfa,'immaturesexpolygonSummerRV.rdata',sep="")))
    write.csv(aa,file=file.path(fp,'indicators',paste(lfa,'immature.DFO.restratified.SexRatio.csv',sep="")))
  }

if(survey=='NEFSC'){
#NEFSC spring
      p$years.to.estimate = c(1969:2018)
      p$length.based = T
      p$size.class= c(50,91)
      p$by.sex = T
      p$bootstrapped.ci=F
      p$strata.files.return=F
      p$strata.efficiencies=F
      p$clusters = c( rep( "localhost", 7) )
      p$season =c('spring')# p$series =c('spring');p$series =c('fall')
      p$define.by.polygons = T
      p$lobster.subunits=F
      p$area = lfa
      p$reweight.strata = T #this subsets 
      p = make.list(list(yrs=p$years.to.estimate),Y=p)
      
        a = list(1,c(2,3))
        out = list()
        for(i in 1:length(a)) {
          p$sex=a[[i]]
          out[[i]] = nefsc.analysis(DS='stratified.estimates.redo',p=p)
          }

        aa = do.call(rbind,out)
        repers = dim(aa)[1]/2
        aa$sex = rep(c('male','femberr'),each=repers)
       
        save(aa,file=file.path(fp,'indicators',paste(lfa,'immature.sexNEFSCspringrestratified.rdata',sep="")))
     write.csv(aa,file=file.path(fp,'indicators',paste(lfa,'immature.NEFSC.spring.restratified.SexRatio.csv',sep="")))


# Fall restratified
    p$season =c('fall')# p$series =c('spring');p$series =c('fall')
     
        a = list(1,c(2,3))
        out = list()
        for(i in 1:length(a)) {
          p$sex=a[[i]]
          out[[i]] = nefsc.analysis(DS='stratified.estimates.redo',p=p)
          }

        aa = do.call(rbind,out)
        repers = dim(aa)[1]/2
   
        aa$sex = rep(c('male','femberr'),each=repers)
       
        save(aa,file=file.path(fp,'indicators',paste(lfa,'immature.sexNEFSCfallrestratified.rdata',sep="")))
     write.csv(aa,file=file.path(fp,'indicators',paste(lfa,'immature.NEFSC.fall.restratified.SexRatio.csv',sep="")))
}
}
}
stratifiedAnalysesSexRatios(stage='mature',survey='NEFSC',lfa='LFA34')
stratifiedAnalysesSexRatios(stage='mature',survey='DFO',lfa='LFA34')
stratifiedAnalysesSexRatios(stage='mature',survey='DFO',lfa='LFA35')
stratifiedAnalysesSexRatios(stage='mature',survey='DFO',lfa='LFA36')
stratifiedAnalysesSexRatios(stage='mature',survey='NEFSC',lfa='LFA38')
stratifiedAnalysesSexRatios(stage='mature',survey='DFO',lfa='LFA38')
stratifiedAnalysesSexRatios(stage='mature',survey='DFO',lfa='LFA35-38')


stratifiedAnalysesSexRatios(stage='all',survey='NEFSC',lfa='LFA34')
stratifiedAnalysesSexRatios(stage='all',survey='DFO',lfa='LFA34')
stratifiedAnalysesSexRatios(stage='all',survey='DFO',lfa='LFA35')
stratifiedAnalysesSexRatios(stage='all',survey='DFO',lfa='LFA36')
stratifiedAnalysesSexRatios(stage='all',survey='NEFSC',lfa='LFA38')
stratifiedAnalysesSexRatios(stage='all',survey='DFO',lfa='LFA38')
stratifiedAnalysesSexRatios(stage='all',survey='DFO',lfa='LFA35-38')


stratifiedAnalysesSexRatios(stage='immature',survey='NEFSC',lfa='LFA34')
stratifiedAnalysesSexRatios(stage='immature',survey='DFO',lfa='LFA34')
stratifiedAnalysesSexRatios(stage='immature',survey='DFO',lfa='LFA35')
stratifiedAnalysesSexRatios(stage='immature',survey='DFO',lfa='LFA36')
stratifiedAnalysesSexRatios(stage='immature',survey='NEFSC',lfa='LFA38')
stratifiedAnalysesSexRatios(stage='immature',survey='DFO',lfa='LFA38')
stratifiedAnalysesSexRatios(stage='immature',survey='DFO',lfa='LFA35-38')
