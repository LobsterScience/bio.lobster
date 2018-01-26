
require(bio.lobster)
require(bio.utilities)
la()

       dir.output = file.path(project.datadirectory('bio.lobster'))
             
              require(rjags)
              rjags::load.module("dic")
              rjags::load.module("glm")

#landings and cpue
  p = lobster.db('seasonal.landings')
  g = lobster.db('annual.landings')
  g = g[-nrow(g),]
  p = p[-nrow(p),]
  p = rename.df(p,'SYEAR','YR')
#CPUE raw data
    load(file=file.path(project.datadirectory('bio.lobster'),'outputs','rawcpueIndicators27-33B.rdata')) 
  
hq = rename.df(cpueData2,'YEAR','YR')
hq$DATE = hq$CATCH = hq$EFFORT <- NULL

#LFA27

l2 = subset(hq,LFA==27 & YR>=1990)$CPUE
ll = subset(g,YR>=1990)$LFA27
   

  sb = list( N= length(ll), C = ll/100 ,yr = 1990:2016, I=as.numeric(l2  ))




      Ku<-max(ll/100*2)
      k <- findMoments(lo=1/(Ku),up=1/(Ku*6),l.perc=0.25,u.perc=0.75,dist='lnorm')
      K <- findMoments(lo=(Ku),up=(Ku*6),l.perc=0.25,u.perc=0.75,dist='lnorm')
      sb$r.a<- .8 ; sb$r.b <- 1/0.069 
      sb$k.a<-k[[1]] ; sb$k.b <- k[[2]]
     sb$K.a <- K[[1]] ; sb$K.b <- K[[2]]
      sb$q.a<-0.0001 ; sb$q.b <- 4
      sb$P0.a <- 0.0001; sb$P0.b <- 2   




#jags

    n.adapt = 500 # burn-in  .. 4000 is enough for the full model but in case ...
    n.iter = 5000
    n.chains = 3
    n.thin = 100 # use of uniform distributions causes high autocorrelations ? 
    n.iter.final = n.iter * n.thin
    fnres = file.path( project.datadirectory("bio.lobster"), paste( "surplus.prod.mcmc", 'LFA27',"rdata", sep=".") )
   
    m = jags.model( file=file.path("/home/adam/git/bio.lobster/inst/bugs",'sp3oneI.bug'), data=sb, n.chains=n.chains, n.adapt=n.adapt ) # recruitment + spring/summer q's + all observed CVs


  tomonitor <- c('sd.p','r','K','sd.o','q','Imean','P.res','P0','B','F')
   tomonitor = intersect( variable.names (m), tomonitor )
    coef(m)
    dic.samples(m, n.iter=n.iter ) # pDIC
    dir.output = file.path(project.datadirectory('bio.lobster'),'spmodelling','lfa27')
    dir.create(dir.output,showWarnings=F,recursive=T)
  
    y = jags.samples(m, variable.names=tomonitor, n.iter=n.iter.final, thin=n.thin) # sample from posterior

save(y, file=file.path(dir.output, 'spmodel27.rdata'))
load(file.path(dir.output, 'spmodel27.rdata'))
 


  figure.bugs( type="timeseries", vname="biomass", y=y, sb=sb, fn=file.path(dir.output, "biomass.timeseriesfourI.png" ),yrs=1990:2016 ,save.plot=F)
  figure.bugs( type="timeseries", vname="fishingmortality", y=y, sb=sb, fn=file.path(dir.output, "fishingmortality.timeseries.fourI.png" ),save.plot=F ) 
  
 figure.bugs( type="density", vname="sd.p", y=y, sb=sb, fn=file.path(dir.output, "priorposteriorsdp.png" ) ,save.plot=F,xlab='Process Error') 
 figure.bugs( type="density", vname="sd.o", y=y, sb=sb, fn=file.path(dir.output, "priorposteriorsd.oa.png" ) ,save.plot=F, xlab='Observation Error RV') 

  figure.bugs( type="density", vname="q", y=y, sb=sb, fn=file.path(dir.output, "priorposteriorq1.png" ) ,save.plot=F,xlab='q RV') 
 figure.bugs( type="density", vname="K", y=y, sb=sb, fn=file.path(dir.output, "priorposteriorK.png" ) ,save.plot=F,xlab='K') 
 figure.bugs( type="density", vname="r", y=y, sb=sb, fn=file.path(dir.output, "priorposteriorr.png" ) ,save.plot=F,xlab='r') 
 
 graphics.off()
 B = apply(y$B,1,median)
 IR = sb$IRV/median(y$q)
IS = sb$INS/median(y$q2)
IA = sb$INA/median(y$q3)
IG = sb$ING / median(y$q4)

plot(1982:2015, IS, type= 'l', lwd =2, xlab='Year',ylab = 'Biomass',lty=1,col='purple')
lines(1982:2015, IR, type= 'l', lwd =2, xlab='Year',ylab = 'Biomass',lty=2, col='red')
lines(1982:2015, IA, type= 'l', lwd =2, xlab='Year',ylab = 'Biomass',lty=3, col='green')
points(1982:2015, B, type= 'l', lwd=3, xlab='Year',ylab = 'Biomass',col='black')
lines(1987:2015, IG[6:length(IG)], type= 'l', lwd =2, xlab='Year',ylab = 'Biomass',lty=2, col='blue')
savePlot(file.path(dir.output,'SurveyIndicesModeledB.png'))


  F=apply(y$F,1,median)
  BMSY = median(y$K)/2
  USR = BMSY *0.8
  LRP = BMSY *0.4
  RR =median(y$r)/2
