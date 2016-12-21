#' @export
# Spmodelling sensitity to K prior

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


w1 = runmed(ING$w.Yst,k=5)
ii = which(ING$w.Yst==0)

ING$w.Yst[ii] <- w1[ii]




  sb = list( N= length(IRV[,1]), C = C, IRV= IRV[,1], INS=INS[,1],INA=INA[,1],ING=c(rep(0,5),ING[,1]),N1=gyr ,yr = 1981:2016, RVa = 0.446,        GBa = 0.222,        NSa = 0.594 ,  NAa = 0.594    )




      Ku<-max(sb$IRV,sb$INS,sb$INA,sb$ING)
      k <- findMoments(lo=1/(Ku),up=1/(Ku*6),l.perc=0.25,u.perc=0.75,dist='lnorm')
      K <- findMoments(lo=(Ku),up=(Ku*6),l.perc=0.25,u.perc=0.75,dist='lnorm')
      sb$k.a<-k[[1]] ; sb$k.b <- k[[2]]
      sb$K.a <- K[[1]] ; sb$K.b <- K[[2]]
      sb$q.a<-0.001 ; sb$q.b <- 4
      sb$P0.a <- 0.0001; sb$P0.b <- 2   
 	  sb$r.a<- 0.7 ; sb$r.b <- 1/0.069 
     



#jags

    n.adapt = 500 # burn-in  .. 4000 is enough for the full model but in case ...
    n.iter = 5000
    n.chains = 3
    n.thin = 50 # use of uniform distributions causes high autocorrelations ? 
    n.iter.final = n.iter * n.thin
    fnres = file.path( project.datadirectory("bio.lobster"), paste( "surplus.prod.mcmc", 2016,"rdata", sep=".") )
   
    m = jags.model( file=file.path("/home/adam/git/bio.lobster/inst/bugs",'sp3fourI.bug'), data=sb, n.chains=n.chains, n.adapt=n.adapt ) # recruitment + spring/summer q's + all 
    tomonitor <- c('sd.p','r','K','sd.oa','sd.ob','sd.oc','sd.od','q','q2','q3','q4','B','Imean','P.res','P0','F')
   	tomonitor = intersect( variable.names (m), tomonitor )
    coef(m)
    dic.samples(m, n.iter=n.iter ) # pDIC
    dir.output = file.path(project.datadirectory('bio.lobster'),'spmodelling','lfa41')
    dir.create(dir.output,showWarnings=F,recursive=T)

B = list()


Kmult = seq(1,5,by=0.5)
for(i in 1:length(Kmult)) {
	  k <- findMoments(lo=1/(Ku*Kmult[i]),up=1/(Ku*4*Kmult[i]),l.perc=0.25,u.perc=0.75,dist='lnorm')
      K <- findMoments(lo=(Ku*Kmult[i]),up=(Ku*4*Kmult[i]),l.perc=0.25,u.perc=0.75,dist='lnorm')
      sb$k.a<-k[[1]] ; sb$k.b <- k[[2]]
      sb$K.a <- K[[1]] ; sb$K.b <- K[[2]]
   		 m = jags.model( file=file.path("/home/adam/git/bio.lobster/inst/bugs",'sp3fourI.bug'), data=sb, n.chains=n.chains, n.adapt=n.adapt ) # recruitment + spring/summer q's + all 
    	dic.samples(m, n.iter=n.iter ) # pDIC
    	y = jags.samples(m, variable.names=tomonitor, n.iter=n.iter.final, thin=n.thin) # sample from posterior
    	B[[i]] <- c(apply(y$B,1,median),apply(y$F,1,median),median(y$K),median(y$r),Kmult[i])
      }

		a = as.data.frame(do.call(rbind,B))
		KK = list()
		for(i in 1:length(Kmult)) {
		      Kg <- findMoments(lo=(Ku*Kmult[i]),up=(Ku*4*Kmult[i]),l.perc=0.25,u.perc=0.75,dist='lnorm')
		      KK[[i]] <- c(Kg[[1]],Kg[[2]])
				}
		b = as.data.frame(do.call(rbind,KK))


aa = cbind(a,b)
save(aa,file=file.path(dir.output,'SensitivitytoKprior.rdata'))
load(file=file.path(dir.output,'SensitivitytoKprior.rdata'))

ik = rainbow(9)
plot(1,1,xlim=c(1982,2015),ylim=c(0,1),type='n',xlab='Year',ylab='B/K')
	for(i in 1:9){
 			lines(1982:2015,aa[i,1:34]/aa[i,69],col=ik[i])
 		}
legend('topleft',legend=round(exp(aa[,72])/1000),lty=rep(1,9),lwd=rep(2,9),col=ik,bty='n',ncol=2,title='K Prior Mean (kt)')
savePlot(file.path(dir.output,'CarringCapacityPriorBiomasstoKratio.png'))

for(i in 1:nrow(aa)) {
	bb = unlist(aa[i,])
	hcrPlot(B=bb[1:34],mF=bb[35:68],USR=bb[69]/2*0.8,LRP=bb[69]/2*0.4,RR=bb[70]/2,yrs=1982:2015,xlims=c(0,41000),ylims=c(0,0.65))
	title(paste('K prior mean = ',round(exp(aa[i,72])/1000),' kt',sep=""))
	savePlot(file.path(dir.output,paste('HCRKsens',round(exp(aa[i,72])/1000),'.png',sep='')))
}

}



