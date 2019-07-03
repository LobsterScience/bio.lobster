require(bio.lobster)
        p = bio.lobster::load.environment()
        p$libs = NULL
        fp = file.path(project.datadirectory('bio.lobster'),"analysis")
        fp1 = file.path(project.datadirectory('bio.lobster'),"analysis","LFA34-38")
        p1= p 
        la()
        load_all('~/git/bio.survey/')

stratifiedAnalysesLengthFreqs = function( p=p1, survey,lfa, fpf = fpf1, fp = fp1){
  if(survey=='DFO'){
        p$series =c('summer')
        p$define.by.polygons = T
        p$lobster.subunits=F
        p$area = lfa
        p$reweight.strata = T #this subsets 
        p$years.to.estimate = c(1999:2018)
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
        save(aa,file=file.path(fp, paste(lfa,'LengthFrequenciespolygonSummerRV.rdata',sep="")))
      }

#NEFSC spring
    if(survey=='NEFSC'){

      p$years.to.estimate = c(1969:2018)
      p$length.based = T
      p$size.class= c(50,300)
      p$by.sex = F
      p$bootstrapped.ci=F
      p$strata.files.return=F
      p$strata.efficiencies=F
      p$clusters = c( rep( "localhost", 7) )
      p$season =c('spring')
      p$define.by.polygons = T
      p$lobster.subunits=F
      p$area = lfa
      p$reweight.strata = T #this subsets 
      p = make.list(list(yrs=p$years.to.estimate),Y=p)
      
   	    a = seq(50,200,1)
        out = list()
        for(i in 1:length(a)) {
          p$size.class=c(a[i],a[i])
          out[[i]] = nefsc.analysis(DS='stratified.estimates.redo',p=p,save=F)
          }

        aa = do.call(rbind,out)
        aa$FLEN = rep(a,each=length(p$years.to.estimate))
        save(aa,file=file.path(fp,paste(lfa,'LengthFrequenciesNEFSCspringrestratified.rdata',sep="")))

# fall
         p$season =c('fall')
                    
   	    a = seq(50,200,1)
        out = list()
        for(i in 1:length(a)) {
          p$size.class=c(a[i],a[i])
          out[[i]] = nefsc.analysis(DS='stratified.estimates.redo',p=p,save=F)
          }

        aa = do.call(rbind,out)
        aa$FLEN = rep(a,each=length(p$years.to.estimate))
        save(aa,file=file.path(fp,paste(lfa,'LengthFrequenciesNEFSCfallrestratified.rdata',sep="")))
    }
}


stratifiedAnalysesLengthFreqs(survey='NEFSC',lfa='LFA34')
stratifiedAnalysesLengthFreqs(survey='DFO',lfa='LFA34')
stratifiedAnalysesLengthFreqs(survey='DFO',lfa='LFA35')
stratifiedAnalysesLengthFreqs(survey='DFO',lfa='LFA36')
stratifiedAnalysesLengthFreqs(survey='NEFSC',lfa='LFA38')
stratifiedAnalysesLengthFreqs(survey='DFO',lfa='LFA38')
stratifiedAnalysesLengthFreqs(survey='DFO',lfa='LFA35-38')
