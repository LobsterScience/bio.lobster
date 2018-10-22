#size frequencies and sex ratios surveys

#restratified
        require(bio.lobster)
        p = bio.lobster::load.environment()
        p$libs = NULL
        fp = file.path(project.datadirectory('bio.lobster'),"analysis",'lfa41Assessment')

la()
#load_all('~/git/bio.survey/')

assessment.year = p$current.assessment.year ########### check the year ############### !!!!!!!!!!!


        p$series =c('summer')# p$series =c('georges');p$series =c('fall')
        p$define.by.polygons = T
        p$lobster.subunits=F
        p$area = 'LFA41'
        p$reweight.strata = T #this subsets 
        p$years.to.estimate = c(1999:assessment.year)
        p$length.based = T
        p$by.sex = F
        p$bootstrapped.ci=F
        p$strata.files.return=F
        p$vessel.correction.fixed=1.2
        p$strat = NULL
        p$clusters = c( rep( "localhost", 7) )
        p$strata.efficiencies = F
        p = make.list(list(yrs=p$years.to.estimate),Y=p)

        a = seq(20,200,1)
        out = list()
        for(i in 1:length(a)) {
          p$size.class=c(a[i],a[i])
          out[[i]] = dfo.rv.analysis(DS='stratified.estimates.redo',p=p,save=F)
          }

        aa = do.call(rbind,out)
        aa$FLEN = rep(a,each=length(p$years.to.estimate))
        save(aa,file=file.path(fp,'LengthFrequenciesLFA41polygonSummerRV.rdata  '))
#Done Aug 22 2017

#NEFSC spring


      p$reweight.strata = F #this subsets 
      p$years.to.estimate = c(1969:assessment.year)
      p$length.based = T
      p$size.class= c(50,300)
      p$by.sex = F
      p$sex = c(1,2,3) # male female berried c(1,2,3)
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
      
   	    a = seq(20,200,1)
        out = list()
        for(i in 1:length(a)) {
          p$size.class=c(a[i],a[i])
          out[[i]] = nefsc.analysis(DS='stratified.estimates.redo',p=p,save=F)
          }

        aa = do.call(rbind,out)
        aa$FLEN = rep(a,each=length(p$years.to.estimate))
        save(aa,file=file.path(fp, 'LengthFrequenciesLFA41NEFSCspringrestratified.rdata  '))


# fall
         p$season =c('fall')# p$series =c('spring');p$series =c('fall')
                    
      p$years.to.estimate = c(1969:(assessment.year-1)) # -1 because update is in the Fall
      p = make.list(list(yrs=p$years.to.estimate),Y=p)


   	    a = seq(20,200,1)
        out = list()
        for(i in 1:length(a)) {
          p$size.class=c(a[i],a[i])
          out[[i]] = nefsc.analysis(DS='stratified.estimates.redo',p=p,save=F)
          }

        aa = do.call(rbind,out)
        aa$FLEN = rep(a,each=length(p$years.to.estimate))
        save(aa,file=file.path(fp,'LengthFrequenciesLFA41NEFSCfallrestratified.rdata  '))


#DFO Georges
      p$series =c('georges')# p$series =c('georges');p$series =c('fall')
      p$define.by.polygons = F
      p$lobster.subunits=F
      p$years.to.estimate = c(2007:assessment.year)
      p$length.based = T
      p$by.sex = F
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
      
require(bio.groundfish)
   	    a = seq(20,200,1)
        out = list()
        for(i in 1:length(a)) {
          p$size.class=c(a[i],a[i])
          out[[i]] = dfo.rv.analysis(DS='stratified.estimates.redo',p=p,save=F)
          }

        aa = do.call(rbind,out)
        aa$FLEN = rep(a,each=length(p$years.to.estimate))
        save(aa,file=file.path(fp,'LengthFrequenciesLFA41dfogeorges.rdata  '))
