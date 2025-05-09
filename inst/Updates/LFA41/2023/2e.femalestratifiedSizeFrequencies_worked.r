#size frequencies females
require(bio.survey)
require(bio.lobster)

p=list()
p$libs = NULL
fp = file.path(project.datadirectory('bio.lobster'),"assessments", "LFA41", "2023")
la()





p$current.assessment.year = 2023

#by length for histograms
#
assessment.year = p$current.assessment.year ########### check the year ############### !!!!!!!!!!!
 

        p$series =c('summer')# p$series =c('georges');p$series =c('fall')
        p$define.by.polygons = T
        p$lobster.subunits=F
        p$area = 'LFA41'
        p$reweight.strata = T #this subsets 
        p$years.to.estimate = c(1999:(assessment.year))
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
        aa$FLEN = rep(a,each=length(p$years.to.estimate))
        save(aa,file=file.path(fp,'maturefemaleLengthFrequenciesLFA41polygonSummerRV.rdata'))


#NEFSC spring


      p$reweight.strata = F #this subsets 
      p$years.to.estimate = c(1969:assessment.year)
      p$length.based = T
      p$size.class= c(50,300)
      p$by.sex = T
      p$sex = c(1,2) # male maturefemaleLength berried c(1,2,3)
      p$bootstrapped.ci=T
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
      
   	    a = seq(90,200,1)
        out = list()
        for(i in 1:length(a)) {
          p$size.class=c(a[i],a[i])
          out[[i]] = nefsc.analysis(DS='stratified.estimates.redo',p=p,save=F)
          }

        aa = do.call(rbind,out)
        aa$FLEN = rep(a,each=length(p$years.to.estimate))
        save(aa,file=file.path(fp,'maturefemaleLengthFrequenciesLFA41NEFSCspringrestratified.rdata'))




# restratified
      p$season =c('fall')# p$series =c('spring');p$series =c('fall')
      p$define.by.polygons = T
      p$lobster.subunits=F
      p$area = 'LFA41'
      p$reweight.strata = T #this subsets 
      p$years.to.estimate = c(1969:(assessment.year)) # -1 because update is in the Fall
      p = make.list(list(yrs=p$years.to.estimate),Y=p)
      
   	    a = seq(90,200,1)
        out = list()
        for(i in 1:length(a)) {
          p$size.class=c(a[i],a[i])
          out[[i]] = nefsc.analysis(DS='stratified.estimates.redo',p=p,save=F)
          }

        aa = do.call(rbind,out)
        aa$FLEN = rep(a,each=length(p$years.to.estimate))
        save(aa,file=file.path(fp,'maturefemaleLengthFrequenciesLFA41NEFSCfallrestratified.rdata'))

    

#DFO Georges
      p$series =c('georges')# p$series =c('georges');p$series =c('fall')
      p$define.by.polygons = F
      p$lobster.subunits=F
      p$years.to.estimate = c(2007:assessment.year)
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

      

# DFO survey All stations including adjacent
      p$define.by.polygons = F
      p$lobster.subunits=F
      p$area = 'Georges.Canada'
      p$reweight.strata = F #this subsets 
      

   	    a = seq(90,200,1)
        out = list()
        for(i in 1:length(a)) {
          p$size.class=c(a[i],a[i])
          out[[i]] = dfo.rv.analysis(DS='stratified.estimates.redo',p=p,save=F)
          }

        aa = do.call(rbind,out)
        aa$FLEN = rep(a,each=length(p$years.to.estimate))
        save(aa,file=file.path(fp,'maturefemaleLengthFrequenciesLFA41dfogeorges.rdata'))
       
        
        
        ##Error in `$<-.data.frame`(`*tmp*`, FLEN, value = c(90, 90, 90, 90, 90,  : replacement has 1887 rows, data has 1665    AFTER LINE 145
