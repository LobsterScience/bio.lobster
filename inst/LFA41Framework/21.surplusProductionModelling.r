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

###DFORV

           fp = file.path(project.datadirectory('bio.lobster'),'analysis')
           fn = file.path( project.figuredirectory("bio.lobster"))

          	load(file=file.path(fp,'BiomassLandingsSummer1981-2015.rdata'))

          w1 = runmed(Lm$w.Yst,k=5)
          ii = which(Lm$w.Yst==0)

          Lm$w.Yst[ii] <- w1[ii]


          #Simple ML SP model
            params = c(r=0.5, K=5500, q=0.8 , B0=700 )
            
            # lognormal timeseries method
            res0 = optim( params, fn=biomass.logistic.recursion, O=Lm$w.Yst, C=Lm$landings, 
                errorType="lognormal" ) 



          N=1:length(Lm$yr)

       
              sb = list( N= length(N), C = Lm$landings, I = Lm$w.Yst,yr = Lm$yr   )




          			Ku<-max(sb$I)
          			k <- findMoments(lo=1/(Ku*4),up=1/(Ku*20),l.perc=0.25,u.perc=0.75,dist='lnorm')
          			K <- findMoments(lo=(Ku*4),up=(Ku*20),l.perc=0.25,u.perc=0.75,dist='lnorm')
                
             #   K <- findMoments(lo=Ku,up=(Ku*6),l.perc=0.05,u.perc=0.95,dist='lnorm')
          			sb$r.a<- 0.145 ; sb$r.b <- 1/0.069 
          			sb$k.a<-k[[1]] ; sb$k.b <- k[[2]]
          			sb$K.a = K[[1]] ; sb$K.b = k[[2]]
                sb$q.a<-0.05 ; sb$q.b <- 2
          			sb$P0.a <- 0.0001; sb$P0.b <- 2		





          #jags

              n.adapt = 200 # burn-in  .. 4000 is enough for the full model but in case ...
              n.iter = 1500
              n.chains = 3
              n.thin = 100 # use of uniform distributions causes high autocorrelations ? 
              n.iter.final = n.iter * n.thin
              fnres = file.path( project.datadirectory("bio.lobster"), paste( "surplus.prod.mcmc", 2016,"rdata", sep=".") )
             
              m = jags.model( file=file.path("/home/adam/git/bio.lobster/inst/bugs",'sp3oneI.bug'), data=sb, n.chains=n.chains, n.adapt=n.adapt ) # recruitment + spring/summer q's + all observed CVs


          		tomonitor <- c('sd.p','r','K','sd.o','q','B','Imean','P.res','P0','F')

              tomonitor = intersect( variable.names (m), tomonitor )
              coef(m)
              

              # ----------------

              dic.samples(m, n.iter=n.iter ) # pDIC

              
              # ----------------
              dir.output = file.path(project.datadirectory('bio.lobster'))
            y = jags.samples(m, variable.names=tomonitor, n.iter=n.iter.final, thin=n.thin) # sample from posterior

          save(y, file=file.path(dir.output, 'analysis','spmodel1I.rdata'))

          load(file.path(dir.output, 'analysis','spmodel1I.rdata'))
            
            figure.bugs( type="timeseries", vname="biomass", y=y, sb=sb, fn=file.path(dir.output, "biomass.timeseriesoneI.summer.png" ) ,save.plot=F) 
              
            figure.bugs( type="timeseries", vname="fishingmortality", y=y, sb=sb, fn=file.path(dir.output, "fishingmortality.timeseries.oneI.summer.png" ),save.plot=T ) 
            figure.bugs( type="hcr", vname="default", y=y, sb=sb, fn=file.path(dir.output, "fishingmortality.timeseries.oneI,summer.png" ),save.plot=T ) 
             
            graphics.off() ; x11()
            
              layout( matrix(c(1,2,3), 3, 1 )); par(mar = c(5, 4, 0, 2))
              hist(y$r[,,], "fd",main="",xlab='r')
                       hist(y$K[,,], "fd",main="",xlab="K")
                       hist(y$q[,,], "fd",main="",xlab='q')

#######################################################
####NSPRI
########################################################
  load(file=file.path(fp,'BiomassLandingsSpringNEF1981-2015.rdata'))


      w1 = runmed(Lm$w.Yst,k=5)
          ii = which(Lm$w.Yst==0)

          Lm$w.Yst[ii] <- w1[ii]


          #Simple ML SP model
            params = c(r=0.5, K=5500, q=0.8 , B0=700 )
            
            # lognormal timeseries method
            res0 = optim( params, fn=biomass.logistic.recursion, O=Lm$w.Yst, C=Lm$landings, 
                errorType="lognormal" ) 



          N=1:length(Lm$yr)

             
              require(rjags)
              rjags::load.module("dic")
              rjags::load.module("glm")

              sb = list( N= length(N), C = Lm$landings, I = Lm$w.Yst,yr = Lm$yr   )


              sb$C[25] <- 100 #place holder

                Ku<-max(sb$I)
                k <- findMoments(lo=1/(Ku),up=1/(Ku*3),l.perc=0.05,u.perc=0.95,dist='lnorm')
                K <- findMoments(lo=Ku,up=(Ku*6),l.perc=0.05,u.perc=0.95,dist='lnorm')
                sb$r.a<- 0.145 ; sb$r.b <- 1/0.069 
                sb$k.a<-k[[1]] ; sb$k.b <- k[[2]]
                sb$q.a<-0.05 ; sb$q.b <- 2
                sb$P0.a <- 0.0001; sb$P0.b <- 2   





          #jags

              n.adapt = 200 # burn-in  .. 4000 is enough for the full model but in case ...
              n.iter = 1500 
              n.chains = 3
              n.thin = 100 # use of uniform distributions causes high autocorrelations ? 
              n.iter.final = n.iter * n.thin
              fnres = file.path( project.datadirectory("bio.lobster"), paste( "surplus.prod.mcmc.spring", 2016,"rdata", sep=".") )
             
              m = jags.model( file=file.path("/home/adam/git/bio.lobster/inst/bugs",'sp3oneI.bug'), data=sb, n.chains=n.chains, n.adapt=n.adapt ) # recruitment + spring/summer q's + all observed CVs


              tomonitor <- c('sd.p','r','K','sd.o','q','B','Imean','P.res','P0','F')

              tomonitor = intersect( variable.names (m), tomonitor )
              coef(m)
              

              # ----------------

              dic.samples(m, n.iter=n.iter ) # pDIC

              
              # ----------------
              dir.output = file.path(project.datadirectory('bio.lobster'))
            y = jags.samples(m, variable.names=tomonitor, n.iter=n.iter.final, thin=n.thin) # sample from posterior

          save(y, file=file.path(dir.output, 'analysis','spmodel1I.spring.rdata'))

          load(file.path(dir.output, 'analysis','spmodel1I.spring.rdata'))
            
            figure.bugs( type="timeseries", vname="biomass", y=y, sb=sb, fn=file.path(dir.output, "biomass.timeseriesoneI.png" ) ,save.plot=F) 
              
            figure.bugs( type="timeseries", vname="fishingmortality", y=y, sb=sb, fn=file.path(dir.output, "fishingmortality.timeseries.oneI.png" ),save.plot=T ) 
            figure.bugs( type="hcr", vname="default", y=y, sb=sb, fn=file.path(dir.output, "fishingmortality.timeseries.oneI.png" ),save.plot=T ) 
             
            graphics.off() ; x11()
              layout( matrix(c(1,2,3), 3, 1 )); par(mar = c(5, 4, 0, 2))
              hist(y$r[,,], "fd",main="",xlab='r')
                       hist(y$K[,,], "fd",main="",xlab="K")
                       hist(y$q[,,], "fd",main="",xlab='q')

###############################
#################################################



#######################################################
####NFALL
########################################################
  load(file=file.path(fp,'BiomassLandingsFall1981-2015.rdata'))


      w1 = runmed(Lm$w.Yst,k=5)
          ii = which(Lm$w.Yst==0)

          Lm$w.Yst[ii] <- w1[ii]


          #Simple ML SP model
            params = c(r=0.5, K=5500, q=0.8 , B0=700 )
            
            # lognormal timeseries method
            res0 = optim( params, fn=biomass.logistic.recursion, O=Lm$w.Yst, C=Lm$landings, 
                errorType="lognormal" ) 



          N=1:length(Lm$yr)

             
              require(rjags)
              rjags::load.module("dic")
              rjags::load.module("glm")

              sb = list( N= length(N), C = Lm$landings, I = Lm$w.Yst,yr = Lm$yr   )


              sb$C[25] <- 100 #place holder

                Ku<-max(sb$I)
                k <- findMoments(lo=1/(Ku),up=1/(Ku*3),l.perc=0.05,u.perc=0.95,dist='lnorm')
                K <- findMoments(lo=Ku,up=(Ku*6),l.perc=0.05,u.perc=0.95,dist='lnorm')
                sb$r.a<- 0.145 ; sb$r.b <- 1/0.069 
                sb$k.a<-k[[1]] ; sb$k.b <- k[[2]]
                sb$q.a<-0.05 ; sb$q.b <- 2
                sb$P0.a <- 0.0001; sb$P0.b <- 2   





          #jags

              n.adapt = 200 # burn-in  .. 4000 is enough for the full model but in case ...
              n.iter = 1500 
              n.chains = 3
              n.thin = 100 # use of uniform distributions causes high autocorrelations ? 
              n.iter.final = n.iter * n.thin
              fnres = file.path( project.datadirectory("bio.lobster"), paste( "surplus.prod.mcmc.fall", 2016,"rdata", sep=".") )
             
              m = jags.model( file=file.path("/home/adam/git/bio.lobster/inst/bugs",'sp3oneI.bug'), data=sb, n.chains=n.chains, n.adapt=n.adapt ) # recruitment + spring/summer q's + all observed CVs


              tomonitor <- c('sd.p','r','K','sd.o','q','B','Imean','P.res','P0','F')

              tomonitor = intersect( variable.names (m), tomonitor )
              coef(m)
              

              # ----------------

              dic.samples(m, n.iter=n.iter ) # pDIC

              
              # ----------------
              dir.output = file.path(project.datadirectory('bio.lobster'))
            y = jags.samples(m, variable.names=tomonitor, n.iter=n.iter.final, thin=n.thin) # sample from posterior

          save(y, file=file.path(dir.output, 'analysis','spmodel1I.fall.rdata'))

          load(file.path(dir.output, 'analysis','spmodel1I.fall.rdata'))
            
            figure.bugs( type="timeseries", vname="biomass", y=y, sb=sb, fn=file.path(dir.output, "biomass.timeseriesoneI.png" ) ,save.plot=F) 
              
            figure.bugs( type="timeseries", vname="fishingmortality", y=y, sb=sb, fn=file.path(dir.output, "fishingmortality.timeseries.oneI.png" ),save.plot=T ) 
            figure.bugs( type="hcr", vname="default", y=y, sb=sb, fn=file.path(dir.output, "fishingmortality.timeseries.oneI.png" ),save.plot=T ) 
             
            graphics.off() ; x11()
              layout( matrix(c(1,2,3), 3, 1 )); par(mar = c(5, 4, 0, 2))
              hist(y$r[,,], "fd",main="",xlab='r')
                       hist(y$K[,,], "fd",main="",xlab="K")
                       hist(y$q[,,], "fd",main="",xlab='q')

###############################
#################################################





###All surveys in one model with commercial biomass partiioned by total survey area relative to stock area

# Area of each survey within LFA41


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

IRV <- subset(dat,survey=='RVsummer' & yr >1981,select='w.Yst')
INS <- subset(dat,survey=='spring' & yr >1981,select='w.Yst')
INA <- subset(dat,survey=='fall' & yr >1981,select='w.Yst')
ING <- subset(dat,survey=='Georges' & yr >1981,select='w.Yst')
#1981-2016
C = c(469,478,440,467,851,718,578,403,532,714,609,544,701,721,725,673,620,590,731,718,726,718,717,1010,780,691,692,541,869,752,654,746,723,680)
gyr = 5

w1 = runmed(IRV$w.Yst,k=5)
ii = which(IRV$w.Yst==0)

IRV$w.Yst[ii] <- w1[ii]



  sb = list( N= length(IRV[,1]), C = C, IRV= IRV[,1], INS=INS[,1],INA=INA[,1],ING=c(rep(0,5),ING[,1]),N1=gyr ,yr = 1981:2016, RVa = 0.446,        GBa = 0.222,        NSa = 0.594 ,  NAa = 0.594    )




      Ku<-max(sb$IRV,sb$INS,sb$INA,sb$ING)
      k <- findMoments(lo=1/(Ku*4),up=1/(Ku*10),l.perc=0.25,u.perc=0.75,dist='lnorm')
      K <- findMoments(lo=(Ku*4),up=(Ku*10),l.perc=0.25,u.perc=0.75,dist='lnorm')
      sb$r.a<- 0.7 ; sb$r.b <- 1/0.069 
      sb$k.a<-k[[1]] ; sb$k.b <- k[[2]]
      sb$K.a <- K[[1]] ; sb$K.b <- K[[2]]
      sb$q.a<-0.05 ; sb$q.b <- 2
      sb$P0.a <- 0.0001; sb$P0.b <- 2   




#jags

    n.adapt = 3000 # burn-in  .. 4000 is enough for the full model but in case ...
    n.iter = 15000
    n.chains = 3
    n.thin = 100 # use of uniform distributions causes high autocorrelations ? 
    n.iter.final = n.iter * n.thin
    fnres = file.path( project.datadirectory("bio.lobster"), paste( "surplus.prod.mcmc", 2016,"rdata", sep=".") )
   
    m = jags.model( file=file.path("/home/adam/git/bio.lobster/inst/bugs",'sp3fourI.bug'), data=sb, n.chains=n.chains, n.adapt=n.adapt ) # recruitment + spring/summer q's + all observed CVs


    tomonitor <- c('sd.p','r','K','sd.o','q','q2','q3','q4','itau2a','itau2b','itau2c','B','Imean','P.res','P0','F')

    tomonitor = intersect( variable.names (m), tomonitor )
    coef(m)
    

    # ----------------

    dic.samples(m, n.iter=n.iter ) # pDIC

    
    # ----------------
    dir.output = file.path(project.datadirectory('bio.lobster'))
  y = jags.samples(m, variable.names=tomonitor, n.iter=n.iter.final, thin=n.thin) # sample from posterior

save(y, file=file.path(dir.output, 'analysis','spmodel4I.rdata'))

load(file.path(dir.output, 'analysis','spmodel4I.rdata'))
  y$q = y$q1
  figure.bugs( type="timeseries", vname="biomass", y=y, sb=sb, fn=file.path(dir.output, "biomass.timeseriesfourI.png" ) ,save.plot=F) 
    
  figure.bugs( type="timeseries", vname="fishingmortality", y=y, sb=sb, fn=file.path(dir.output, "fishingmortality.timeseries.oneI.png" ),save.plot=T ) 
  figure.bugs( type="hcr", vname="default", y=y, sb=sb, fn=file.path(dir.output, "fishingmortality.timeseries.oneI.png" ),save.plot=T ) 
   
  graphics.off() ; x11()
    layout( matrix(c(1,2,3), 3, 1 )); par(mar = c(5, 4, 0, 2))
    hist(y$r[,,], "fd",main="",xlab='r')
             hist(y$K[,,], "fd",main="",xlab="K")
             hist(y$q[,,], "fd",main="",xlab='q')



    n.adapt = 200 # burn-in  .. 4000 is enough for the full model but in case ...
    n.iter = 1500 
    n.chains = 3
    n.thin = 100 # use of uniform distributions causes high autocorrelations ? 
    n.iter.final = n.iter * n.thin
    fnres = file.path( project.datadirectory("bio.lobster"), paste( "surplus.prod.mcmc", 2016,"rdata", sep=".") )
   
    m = jags.model( file=file.path("/home/adam/git/bio.lobster/inst/bugs",'sp3fourI.bug'), data=sb, n.chains=n.chains, n.adapt=n.adapt ) # recruitment + spring/summer q's + all observed CVs


    tomonitor <- c('sd.p','r','K','sd.o','q','q2','itau2a','itau2b','itau2c','B','Imean','P.res','P0','F')

    tomonitor = intersect( variable.names (m), tomonitor )
    coef(m)
    

    # ----------------

    dic.samples(m, n.iter=n.iter ) # pDIC

    
    # ----------------
    dir.output = file.path(project.datadirectory('bio.lobster'))
  y = jags.samples(m, variable.names=tomonitor, n.iter=n.iter.final, thin=n.thin) # sample from posterior

