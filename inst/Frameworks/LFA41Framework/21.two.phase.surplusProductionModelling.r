#lobster sp modelling for the offshore

###All surveys in one model with commercial biomass partiioned by total survey area relative to stock area

# Area of each survey within LFA41

#lobster sp modelling for the offshore

require(bio.groundfish)
require(bio.lobster)
require(bio.polygons)
require(PBSmapping)
require(bio.utilities)
la()

       dir.output = file.path(project.datadirectory('bio.lobster'))
             
              require(rjags)
              rjags::load.module("dic")
              rjags::load.module("glm")


          RVa = 0.446

          GBa = 0.222

          NSa = 0.594 

          NAa = 0.594 

          a = c('stratified.georges.Georges.Canada.base.length.all.not.sexed.rdata',
                'stratified.summer.LFA41.restratified.length.all.not.sexed.rdata',  
              'stratified.nefsc.fall.LFA41.restratified.length.83-300.male&female.sexed.rdata',
              'stratified.nefsc.spring.LFA41.restratified.length.83-300.male&female.sexed.rdata'
              )

          anames = c('Georges','RVsummer','fall','spring')
          dat = c()
          for(i in 1:length(a)) {
            load(file.path(dir.output,'analysis',a[i]))
            out$survey = anames[i]
            if(i %in% c(1,2)) out$w.Yst = out$w.Yst * 0.876 #proportion of legal weighted animals
            if(i == 1 ) {out$w.Yst[which(is.na(out$yr))] <- 100; out$yr[which(is.na(out$yr))] <- 2011 }#placeholder 
            out = out[,c('yr','w.Yst','survey')]
            dat = rbind(dat,out)
          }

IRV <- subset(dat,survey=='RVsummer' & yr >1981 & yr<2000,select='w.Yst')
INS <- subset(dat,survey=='spring' & yr >1981& yr<2000,select='w.Yst')
INA <- subset(dat,survey=='fall' & yr >1981& yr<2000,select='w.Yst')
ING <- subset(dat,survey=='Georges' & yr >1981& yr<2000,select='w.Yst')
#1981-2016
C = c(469,478,440,467,851,718,578,403,532,714,609,544,701,721,725,673,620,590)#,731,718,726,718,717,1010,780,691,692,541,869,752,654,746,723,680)
gyr = 5

w1 = runmed(IRV$w.Yst,k=5)
ii = which(IRV$w.Yst==0)
IRV$w.Yst[ii] <- w1[ii]


w1 = runmed(ING$w.Yst,k=5)
ii = which(ING$w.Yst==0)

ING$w.Yst[ii] <- w1[ii]



  sb = list( N= length(IRV[,1]), C = C, IRV= IRV[,1], INS=INS[,1],INA=INA[,1],ING=c(rep(0,5),ING[,1]),N1=gyr ,yr = 1981:2016, RVa = 0.446,        GBa = 0.222,        NSa = 0.594 ,  NAa = 0.594    )


      Ku<-max(sb$IRV,sb$INS,sb$INA,sb$ING)
      k <- findMoments(lo=1/(Ku),up=1/(Ku*6),l.perc=0.25,u.perc=0.75,dist='lnorm')
      K <- findMoments(lo=(Ku),up=(Ku*6),l.perc=0.25,u.perc=0.75,dist='lnorm')
      sb$r.a<- 0.5 ; sb$r.b <- 1/0.2 
      sb$k.a<-k[[1]] ; sb$k.b <- k[[2]]
      sb$K.a <- K[[1]] ; sb$K.b <- K[[2]]
      sb$q.a<-0.001 ; sb$q.b <- 4
      sb$P0.a <- 0.0001; sb$P0.b <- 2   




#jags

    n.adapt = 700 # burn-in  .. 4000 is enough for the full model but in case ...
    n.iter = 8000
    n.chains = 3
    n.thin = 25 # use of uniform distributions causes high autocorrelations ? 
    n.iter.final = n.iter * n.thin
    fnres = file.path( project.datadirectory("bio.lobster"), paste( "early.surplus.prod.mcmc", 2016,"rdata", sep=".") )
   
    m = jags.model( file=file.path("/home/adam/git/bio.lobster/inst/bugs",'sp3fourI.bug'), data=sb, n.chains=n.chains, n.adapt=n.adapt ) # recruitment + spring/summer q's + all observed CVs


    tomonitor <- c('sd.p','r','K','sd.oa','sd.ob','sd.oc','sd.od','q','q2','q3','q4','B','Imean','P.res','P0','F')
   tomonitor = intersect( variable.names (m), tomonitor )
    coef(m)
    dic.samples(m, n.iter=n.iter ) # pDIC
    dir.output = file.path(project.datadirectory('bio.lobster'),'spmodelling','lfa41')
    dir.create(dir.output,showWarnings=F,recursive=T)
  
    y = jags.samples(m, variable.names=tomonitor, n.iter=n.iter.final, thin=n.thin) # sample from posterior

save(y, file=file.path(dir.output, 'early.spmodel4IFinal.rdata'))
load(file.path(dir.output, 'early.spmodel4IFinal.rdata'))
 


  figure.bugs( type="timeseries", vname="biomass", y=y, sb=sb, fn=file.path(dir.output, "biomass.timeseriesfourI.png" ) ,save.plot=F) 
  figure.bugs( type="timeseries", vname="fishingmortality", y=y, sb=sb, fn=file.path(dir.output, "fishingmortality.timeseries.fourI.png" ),save.plot=F ) 
  
 figure.bugs( type="density", vname="sd.p", y=y, sb=sb, fn=file.path(dir.output, "priorposteriorsdp.png" ) ,save.plot=F,xlab='Process Error') 
 figure.bugs( type="density", vname="sd.oa", y=y, sb=sb, fn=file.path(dir.output, "priorposteriorsd.oa.png" ) ,save.plot=F, xlab='Observation Error RV') 
 figure.bugs( type="density", vname="sd.ob", y=y, sb=sb, fn=file.path(dir.output, "priorposteriorsd.ob.png" ) ,save.plot=F,xlab='Observation Error, NEFSC Spring') 
 figure.bugs( type="density", vname="sd.oc", y=y, sb=sb, fn=file.path(dir.output, "priorposteriorsd.oc.png" ) ,save.plot=F,xlab='Observation Error NEFSC Autumn') 
 figure.bugs( type="density", vname="sd.od", y=y, sb=sb, fn=file.path(dir.output, "priorposteriorsd.od.png" ) ,save.plot=F,xlab='Observation Error Georges Bank') 
 
 figure.bugs( type="density", vname="q", y=y, sb=sb, fn=file.path(dir.output, "priorposteriorq1.png" ) ,save.plot=F,xlab='q RV') 
 figure.bugs( type="density", vname="q2", y=y, sb=sb, fn=file.path(dir.output, "priorposteriorq2.png" ) ,save.plot=F,xlab='q NEFSC Spring') 
 figure.bugs( type="density", vname="q3", y=y, sb=sb, fn=file.path(dir.output, "priorposteriorq3.png" ) ,save.plot=F,xlab='q NEFSC Autumn') 
 figure.bugs( type="density", vname="q4", y=y, sb=sb, fn=file.path(dir.output, "priorposteriorq4.png" ) ,save.plot=F,xlab='q Georges Bank') 
 
 figure.bugs( type="density", vname="K", y=y, sb=sb, fn=file.path(dir.output, "priorposteriorK.png" ) ,save.plot=F,xlab='K') 
 figure.bugs( type="density", vname="r", y=y, sb=sb, fn=file.path(dir.output, "priorposteriorr.png" ) ,save.plot=F,xlab='r') 
 
 graphics.off()
 B = apply(y$B,1,median)
 IR = sb$IRV/median(y$q)
IS = sb$INS/median(y$q2)
IA = sb$INA/median(y$q3)
IG = sb$ING / median(y$q4)

plot(1982:1999, IS, type= 'l', lwd =2, xlab='Year',ylab = 'Biomass',lty=1,col='purple')
lines(1982:1999, IR, type= 'l', lwd =2, xlab='Year',ylab = 'Biomass',lty=2, col='red')
lines(1982:1999, IA, type= 'l', lwd =2, xlab='Year',ylab = 'Biomass',lty=3, col='green')
points(1982:1999, B, type= 'l', lwd=3, xlab='Year',ylab = 'Biomass',col='black')
lines(1987:1999, IG[6:length(IG)], type= 'l', lwd =2, xlab='Year',ylab = 'Biomass',lty=2, col='blue')
savePlot(file.path(dir.output,'SurveyIndicesModeledB.png'))


  F=apply(y$F,1,median)
  BMSY = median(y$K)/2
  USR = BMSY *0.8
  LRP = BMSY *0.4
  RR =median(y$r)/2

hcrPlot(B=B,mF=F,USR=USR,LRP=LRP,RR=RR,yrs=1982:1999)
  


 ##################LATE REFERENCE POINTS




require(bio.groundfish)
require(bio.lobster)
require(bio.polygons)
require(PBSmapping)
require(bio.utilities)
la()

       dir.output = file.path(project.datadirectory('bio.lobster'))
             
              require(rjags)
              rjags::load.module("dic")
              rjags::load.module("glm")


          RVa = 0.446

          GBa = 0.222

          NSa = 0.594 

          NAa = 0.594 

          a = c('stratified.georges.Georges.Canada.base.length.all.not.sexed.rdata',
                'stratified.summer.LFA41.restratified.length.all.not.sexed.rdata',  
              'stratified.nefsc.fall.LFA41.restratified.length.83-300.male&female.sexed.rdata',
              'stratified.nefsc.spring.LFA41.restratified.length.83-300.male&female.sexed.rdata'
              )

          anames = c('Georges','RVsummer','fall','spring')
          dat = c()
          for(i in 1:length(a)) {
            load(file.path(dir.output,'analysis',a[i]))
            out$survey = anames[i]
            if(i %in% c(1,2)) out$w.Yst = out$w.Yst * 0.876 #proportion of legal weighted animals
            if(i == 1 ) {out$w.Yst[which(is.na(out$yr))] <- 100; out$yr[which(is.na(out$yr))] <- 2011 }#placeholder 
            out = out[,c('yr','w.Yst','survey')]
            dat = rbind(dat,out)
          }

IRV <- subset(dat,survey=='RVsummer' & yr >2000 ,select='w.Yst')
INS <- subset(dat,survey=='spring' & yr >2000,select='w.Yst')
INA <- subset(dat,survey=='fall' & yr >2000,select='w.Yst')
ING <- subset(dat,survey=='Georges' & yr >2000,select='w.Yst')
#1981-2016
C = c(731,718,726,718,717,1010,780,691,692,541,869,752,654,746,723,680)
gyr = 1

w1 = runmed(IRV$w.Yst,k=5)
ii = which(IRV$w.Yst==0)
IRV$w.Yst[ii] <- w1[ii]


w1 = runmed(ING$w.Yst,k=5)
ii = which(ING$w.Yst==0)

ING$w.Yst[ii] <- w1[ii]



  sb = list( N= length(IRV[,1]), C = C, IRV= IRV[,1], INS=INS[,1],INA=INA[,1],ING=c(ING[,1]),N1=gyr ,yr = 1981:2016, RVa = 0.446,        GBa = 0.222,        NSa = 0.594 ,  NAa = 0.594    )


      Ku<-max(sb$IRV,sb$INS,sb$INA,sb$ING)
      k <- findMoments(lo=1/(Ku),up=1/(Ku*10),l.perc=0.25,u.perc=0.75,dist='lnorm')
      K <- findMoments(lo=(Ku),up=(Ku*10),l.perc=0.25,u.perc=0.75,dist='lnorm')
      sb$r.a<- 0.6 ; sb$r.b <- 1/0.06 
      sb$k.a<-k[[1]] ; sb$k.b <- k[[2]]
      sb$K.a <- K[[1]] ; sb$K.b <- K[[2]]
      sb$q.a<-0.001 ; sb$q.b <- 4
      sb$P0.a <- 0.0001; sb$P0.b <- 2   




#jags

    n.adapt = 100 # burn-in  .. 4000 is enough for the full model but in case ...
    n.iter = 1500
    n.chains = 3
    n.thin = 15 # use of uniform distributions causes high autocorrelations ? 
    n.iter.final = n.iter * n.thin
    fnres = file.path( project.datadirectory("bio.lobster"), paste( "early.surplus.prod.mcmc", 2016,"rdata", sep=".") )
   
    m = jags.model( file=file.path("/home/adam/git/bio.lobster/inst/bugs",'sp3fourIpast2000.bug'), data=sb, n.chains=n.chains, n.adapt=n.adapt ) # recruitment + spring/summer q's + all observed CVs


    tomonitor <- c('sd.p','r','K','sd.oa','sd.ob','sd.oc','sd.od','q','q2','q3','q4','B','Imean','P.res','P0','F')
   tomonitor = intersect( variable.names (m), tomonitor )
    coef(m)
    dic.samples(m, n.iter=n.iter ) # pDIC
    dir.output = file.path(project.datadirectory('bio.lobster'),'spmodelling','lfa41')
    dir.create(dir.output,showWarnings=F,recursive=T)
  
    y = jags.samples(m, variable.names=tomonitor, n.iter=n.iter.final, thin=n.thin) # sample from posterior

save(y, file=file.path(dir.output, 'late.spmodel4IFinal.rdata'))
load(file.path(dir.output, 'late.spmodel4IFinal.rdata'))
 


  figure.bugs( type="timeseries", vname="biomass", y=y, sb=sb, fn=file.path(dir.output, "biomass.timeseriesfourI.png" ) ,save.plot=F) 
  figure.bugs( type="timeseries", vname="fishingmortality", y=y, sb=sb, fn=file.path(dir.output, "fishingmortality.timeseries.fourI.png" ),save.plot=F ) 
  
 figure.bugs( type="density", vname="sd.p", y=y, sb=sb, fn=file.path(dir.output, "priorposteriorsdp.png" ) ,save.plot=F,xlab='Process Error') 
 figure.bugs( type="density", vname="sd.oa", y=y, sb=sb, fn=file.path(dir.output, "priorposteriorsd.oa.png" ) ,save.plot=F, xlab='Observation Error RV') 
 figure.bugs( type="density", vname="sd.ob", y=y, sb=sb, fn=file.path(dir.output, "priorposteriorsd.ob.png" ) ,save.plot=F,xlab='Observation Error, NEFSC Spring') 
 figure.bugs( type="density", vname="sd.oc", y=y, sb=sb, fn=file.path(dir.output, "priorposteriorsd.oc.png" ) ,save.plot=F,xlab='Observation Error NEFSC Autumn') 
 figure.bugs( type="density", vname="sd.od", y=y, sb=sb, fn=file.path(dir.output, "priorposteriorsd.od.png" ) ,save.plot=F,xlab='Observation Error Georges Bank') 
 
 figure.bugs( type="density", vname="q", y=y, sb=sb, fn=file.path(dir.output, "priorposteriorq1.png" ) ,save.plot=F,xlab='q RV') 
 figure.bugs( type="density", vname="q2", y=y, sb=sb, fn=file.path(dir.output, "priorposteriorq2.png" ) ,save.plot=F,xlab='q NEFSC Spring') 
 figure.bugs( type="density", vname="q3", y=y, sb=sb, fn=file.path(dir.output, "priorposteriorq3.png" ) ,save.plot=F,xlab='q NEFSC Autumn') 
 figure.bugs( type="density", vname="q4", y=y, sb=sb, fn=file.path(dir.output, "priorposteriorq4.png" ) ,save.plot=F,xlab='q Georges Bank') 
 
 figure.bugs( type="density", vname="K", y=y, sb=sb, fn=file.path(dir.output, "priorposteriorK.png" ) ,save.plot=F,xlab='K') 
 figure.bugs( type="density", vname="r", y=y, sb=sb, fn=file.path(dir.output, "priorposteriorr.png" ) ,save.plot=F,xlab='r') 
 
 graphics.off()
 B = apply(y$B,1,median)
 IR = sb$IRV/median(y$q)
IS = sb$INS/median(y$q2)
IA = sb$INA/median(y$q3)
IG = sb$ING / median(y$q4)

plot(2001:2015, IS, type= 'l', lwd =2, xlab='Year',ylab = 'Biomass',lty=1,col='purple')
lines(2001:2015, IR, type= 'l', lwd =2, xlab='Year',ylab = 'Biomass',lty=2, col='red')
lines(2001:2015, IA, type= 'l', lwd =2, xlab='Year',ylab = 'Biomass',lty=3, col='green')
points(2001:2015, B, type= 'l', lwd=3, xlab='Year',ylab = 'Biomass',col='black')
lines(2001:2015,IG, type= 'l', lwd =2, xlab='Year',ylab = 'Biomass',lty=2, col='blue')
savePlot(file.path(dir.output,'SurveyIndicesModeledB.png'))


  F=apply(y$F,1,median)
  BMSY = median(y$K)/2
  USR = BMSY *0.8
  LRP = BMSY *0.4
  RR =median(y$r)/2

hcrPlot(B=B,mF=F,USR=USR,LRP=LRP,RR=RR,yrs=2001:2015)
  