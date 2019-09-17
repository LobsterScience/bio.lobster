#run Aug 12 2019

#size frequencies females
require(bio.survey)
require(bio.lobster)
require(bio.groundfish)
p = bio.lobster::load.environment()
p$libs = NULL
p1=p

#by length for histograms
#
fp1 = file.path(project.datadirectory('bio.lobster'),"analysis","LFA34-38")
fpf1 = file.path(project.figuredirectory('bio.lobster'),"LFA3438Framework2019")

load_all('~/git/bio.survey/')
load_all('~/git/bio.groundfish/')
la()
#NEFSC Setup
stratifiedAnalysesFemale = function( p=p1, survey,lfa, fpf = fpf1, fp = fp1){

if(survey=='DFO'){
  
        p$series =c('summer')# p$series =c('georges');p$series =c('fall')
        p$define.by.polygons = T
        p$lobster.subunits=F
        p$area = lfa
        p$reweight.strata = T #this subsets 
        p$years.to.estimate = c(1999:2018)
        p$length.based = T
        p$by.sex = T
        p$sex=c(2,3)
      
        p$bootstrapped.ci=T
        p$strata.files.return=F
        p$vessel.correction.fixed=1.2
        p$strat = NULL
        p$clusters = c( rep( "localhost", 7) )
        p$strata.efficiencies = F
        p = make.list(list(yrs=p$years.to.estimate),Y=p)

        a = seq(90,200,1)
        out = list()
        for(i in 1:length(a)) {
          p$size.class=c(a[i],a[i])
          out[[i]] = dfo.rv.analysis(DS='stratified.estimates.redo',p=p,save=F)
          }

        aa = do.call(rbind,out)
        browser()
        aa$FLEN = rep(a,each=length(p$years.to.estimate))
        save(aa,file=file.path(fp, paste(lfa,'maturefemaleLengthFrequenciespolygonSummerRV.rdata',sep="")))
      }

#NEFSC spring

if(survey=='NEFSC'){
      p$years.to.estimate = c(1969:2018)
      p$length.based = T
      p$by.sex = T
      p$sex = c(2,3) # male maturefemaleLength berried c(1,2,3)
      p$bootstrapped.ci=T
      p$strata.files.return=F
      p$strata.efficiencies=F
      p$clusters = c( rep( "localhost", 7) )
 p = make.list(list(yrs=p$years.to.estimate),Y=p)
       

# restratified
      p$season =c('spring')# p$series =c('spring');p$series =c('fall')
      p$define.by.polygons = T
      p$lobster.subunits=F
      p$area = lfa
      p$reweight.strata = T #this subsets 
      
   	    a = seq(90,200,1)
        out = list()
        for(i in 1:length(a)) {
          p$size.class=c(a[i],a[i])
          kl = nefsc.analysis(DS='stratified.estimates.redo',p=p,save=F)
          kl$size = a[i]
          out[[i]] = kl
          }
        aa = do.call(rbind,out)
        save(aa,file=file.path(fp, paste(lfa,'maturefemaleLengthFrequenciespolygonNEFSCspring.rdata',sep="")))
  

# restratified
      p$season =c('fall')# p$series =c('spring');p$series =c('fall')
      
   	    a = seq(90,200,1)
        out = list()
        for(i in 1:length(a)) {
          p$size.class=c(a[i],a[i])
          kl = nefsc.analysis(DS='stratified.estimates.redo',p=p,save=F)
          kl$size = a[i]
          out[[i]] = kl
          }

        aa = do.call(rbind,out)
      save(aa,file=file.path(fp, paste(lfa,'maturefemaleLengthFrequenciespolygonNEFSCfall.rdata',sep="")))
    }
  }



stratifiedAnalysesFemale(survey='NEFSC',lfa='LFA34')
stratifiedAnalysesFemale(survey='DFO',lfa='LFA34')
stratifiedAnalysesFemale(survey='DFO',lfa='LFA35')
stratifiedAnalysesFemale(survey='DFO',lfa='LFA36')
stratifiedAnalysesFemale(survey='NEFSC',lfa='LFA38')####issues
stratifiedAnalysesFemale(survey='DFO',lfa='LFA38')
stratifiedAnalysesFemale(survey='DFO',lfa='LFA35-38')
