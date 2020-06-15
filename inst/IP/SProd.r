#lobster sp modelling for the offshore

###All surveys in one model with commercial biomass partiioned by total survey area relative to stock area

# Area of each survey within LFA41

#lobster sp modelling for the offshore

require(bio.groundfish)
require(bio.lobster)
require(bio.utilities)
la()

       dir.output = file.path(project.datadirectory('bio.lobster'))
             
              require(rjags)
              rjags::load.module("dic")
              rjags::load.module("glm")



  sb = list( N= length(I = surveyT, C = C, N=nyears)

      Ku<-max(sb$I)
      k <- findMoments(lo=1/(Ku),up=1/(Ku*20),l.perc=0.25,u.perc=0.75,dist='lnorm')
      K <- findMoments(lo=(Ku),up=(Ku*20),l.perc=0.25,u.perc=0.75,dist='lnorm')
      sb$r.a<- 0.7 ; sb$r.b <- 1/0.069 
      sb$k.a<-k[[1]] ; sb$k.b <- k[[2]]
      sb$K.a <- K[[1]] ; sb$K.b <- K[[2]]
      sb$q.a<-0.001 ; sb$q.b <- 4
      sb$P0.a <- 0.0001; sb$P0.b <- 2   




#jags

    n.adapt = 3000 # burn-in  .. 4000 is enough for the full model but in case ...
    n.iter = 15000
    n.chains = 3
    n.thin = 100 # use of uniform distributions causes high autocorrelations ? 
    n.iter.final = n.iter * n.thin
    fnres = file.path( project.datadirectory("bio.lobster"), paste( "surplus.prod.mcmc", 'Trials',"rdata", sep=".") )
   
    m = jags.model( file=file.path("/home/adam/git/bio.lobster/inst/bugs",'sp3oneI.bug'), data=sb, n.chains=n.chains, n.adapt=n.adapt ) # recruitment + spring/summer q's + all observed CVs


    tomonitor <- c('sd.p','r','K','sd.o','q','B','Imean','P.res','P0','F')
   tomonitor = intersect( variable.names (m), tomonitor )
    coef(m)
    dic.samples(m, n.iter=n.iter ) # pDIC
    dir.output = file.path(project.datadirectory('bio.lobster'),'spmodelling','example')
    dir.create(dir.output,showWarnings=F,recursive=T)
  
    y = jags.samples(m, variable.names=tomonitor, n.iter=n.iter.final, thin=n.thin) # sample from posterior

save(y, file=file.path(dir.output, 'spmodelExample.rdata'))
load(file.path(dir.output, 'spmodelExample.rdata'))
 


  figure.bugs( type="timeseries", vname="biomass", y=y, sb=sb, fn=file.path(dir.output, "biomass.timeseriesI.png" ) ,save.plot=T) 
  figure.bugs( type="timeseries", vname="fishingmortality", y=y, sb=sb, fn=file.path(dir.output, "fishingmortality.timeseries.I.png" ),save.plot=T ) 
  
 figure.bugs( type="density", vname="sd.p", y=y, sb=sb, fn=file.path(dir.output, "priorposteriorsdp.png" ) ,save.plot=T,xlab='Process Error') 
 figure.bugs( type="density", vname="sd.o", y=y, sb=sb, fn=file.path(dir.output, "priorposteriorsd.oa.png" ) ,save.plot=T, xlab='Observation Error RV') 
 
 figure.bugs( type="density", vname="q", y=y, sb=sb, fn=file.path(dir.output, "priorposteriorq.png" ) ,save.plot=T,xlab='q') 
 
 figure.bugs( type="density", vname="K", y=y, sb=sb, fn=file.path(dir.output, "priorposteriorK.png" ) ,save.plot=T,xlab='K') 
 figure.bugs( type="density", vname="r", y=y, sb=sb, fn=file.path(dir.output, "priorposteriorr.png" ) ,save.plot=T,xlab='r') 
 
 graphics.off()
 B = apply(y$B,1,median)
 IR = sb$I/median(y$q)

plot(1982:2015, IR, type= 'l', lwd =2, xlab='Year',ylab = 'Biomass',lty=1,col='purple')
savePlot(file.path(dir.output,'SurveyIndicesModeledB.png'))


  F=apply(y$F,1,median)
  BMSY = median(y$K)/2
  USR = BMSY *0.8
  LRP = BMSY *0.4
  RR =median(y$r)/2




 


