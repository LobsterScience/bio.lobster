##Design based estimates of Cunner Catch
require(bio.lobster)
require(bio.utilities)
require(RODBC)
require(lubridate)
require(devtools)
options(stringAsFactors=F)
la()

wd = ('C:\\Users\\Cooka\\OneDrive - DFO-MPO\\BycatchLobster')

setwd(wd)

##cbfha

	 b=bycatch.db('logbook.merge')
    
aS = bycatch.db('CBFHA')

 ap = aggregate(CunnerWt~TRIP+LFA+Cluster+GridGroup+Period,data=aS,FUN=mean)
 aCs = aggregate(CunnerWt~TRIP+LFA+Cluster+GridGroup+Period,data=aS,FUN=var,na.rm=T)
        aCs = rename.df(aCs,c('CunnerWt'),c('CunnerWtvar'))
        aCC = aggregate(UID~TRIP+LFA+Cluster+GridGroup+Period,data=aS,FUN=function(x) length(unique(x)))
        ap = merge(ap, aCC)
        aCp = merge(ap, aCs)
        aCp$mean =aCp$CunnerWt
        aCp$var =aCp$CunnerWtvar
        aCp$n = aCp$UID
  
 aT = aCp

 aT$Strata = paste(aT$LFA,aT$GridGroup,aT$Period,sep='_')

 op = list()
 aTs = split(aT,f=list(aT$Strata))
 aTs = rm.from.list(aTs)
  for(i in 1:length(aTs)){
              j = aTs[[i]]
              op[[i]] =c(LFA =unique(j$LFA), GridGroup = unique(j$GridGroup), Period = unique(j$Period),Strata=unique(j$Strata),clusterStatistics(j))      
            }
        aCo = as.data.frame(do.call(rbind,op))
        aCo = toNums(aCo,c(5:8))
      	
   uu = readRDS('results/BumpedUpEffortByGridGroup.rds')
   uu = subset(uu,LFA %in% 27)
	  uu$Strata = paste(uu$LFA,uu$GridGroup,uu$Period,sep='_')
   uu = aggregate(cbind(BTTH,BuTH,BlTH)~Strata+LFA+GridGroup+Period,data=uu,FUN=mean)
	  au = merge(uu,aCo,all.x=T)
ff = au
  
	  #stratified totals
	  ff$Tot = ff$BTTH * ff$mean
	  ff$var = (ff$sd)^2
	  ff$sdTH = ((ff$BTTH-ff$BlTH)/5)
	  f1 = aggregate(BTTH~LFA,data=ff,FUN=sum)
	  
	  names(f1)[2] = 'TotTr'
	  ff = merge(ff,f1)

	  fu = function(m,s,n) rnorm(n,mean=m,sd=s)
	  iter=1000
	  re = unique(ff$LFA)
	  out = list()
	  for(i in 1:length(re)){
	  		k = subset(ff,LFA==re[i])
	  		mu = sum(k$mean * k$BTTH)
	  		v  = sqrt(sum((k$BTTH)^2*(k$var/k$n)))
	  		ql = qnorm(.025,mu,v)
	  		qu = qnorm(.975,mu,v)
		  	for(j in 2:(iter)){
	  				kk = k[,c('BTTH','sdTH')]
	  				kk$n=1
	  				kw = unlist(Map(fu,kk$BTTH,kk$sdTH,kk$n))

	  				mu = c(mu,sum(k$mean * kw))
	  				v = c(v,sqrt(sum((kw)^2*(k$var/k$n))))
	  				ql =c(ql,qnorm(.025,mu[j],v[j]))
	  				qu = c(qu,qnorm(.975,mu[j],v[j]))
		  }
		out[[i]] = c(re[i],mean(mu),mean(ql),mean(qu))
		}
	  
	gs = as.data.frame(do.call(rbind,out))
	gs = toNums(gs,1:4)
	gs[,2:4] = gs[,2:4]/1000
#######################
##GCIFA
 
b=bycatch.db('logbook.merge')
    
aS = bycatch.db('GCIFA')

 ap = aggregate(CunnerWt~TRIP+LFA+Cluster+Period+SYEAR,data=aS,FUN=mean)
 aCs = aggregate(CunnerWt~TRIP+LFA+Cluster+Period+SYEAR,data=aS,FUN=var,na.rm=T)
        aCs = rename.df(aCs,c('CunnerWt'),c('CunnerWtvar'))
        aCC = aggregate(UID~TRIP+LFA+Cluster+Period+SYEAR,data=aS,FUN=function(x) length(unique(x)))
        ap = merge(ap, aCC)
        aCp = merge(ap, aCs)
        aCp$mean =aCp$CunnerWt
        aCp$var =aCp$CunnerWtvar
        aCp$n = aCp$UID
  
 aT = aCp

 aT$Strata = paste(aT$LFA,aT$Period,sep='_')

 op = list()
 aTs = split(aT,f=list(aT$SYEAR,aT$Strata))
 aTs = rm.from.list(aTs)
  for(i in 1:length(aTs)){
              j = aTs[[i]]
              op[[i]] =c(SYEAR= unique(j$SYEAR),LFA =unique(j$LFA), Period = unique(j$Period),Strata=unique(j$Strata),clusterStatistics(j))      
            }
        aCo = as.data.frame(do.call(rbind,op))
        aCo = toNums(aCo,c(5:8))
      	
      uu = readRDS('results/BumpedUpEffortByGridGroup.rds')

      uu = subset(uu,LFA %in% c('31A','31B') & SYEAR %in% c(2019,2018))
	  uu$Strata = paste(uu$LFA,uu$Period,sep='_')
	  uu = aggregate(cbind(BTTH,BlTH,BuTH)~SYEAR+LFA+Strata+Period, data=uu,FUN=sum)
	
	  au = merge(uu,aCo,all.x=T)

	  #for unsampled strata use the mean and of the time period from all others using the same clustering 
	  i = which(is.na(au$mean))
	  ff = au
	  ff = toNums(ff,8:10)
     #stratified totals
  	  ff$Tot = ff$BTTH * ff$mean
	  ff$var = (ff$sd)^2
	  ff$sdTH = ((ff$BTTH-ff$BlTH)/5)
	  f1 = aggregate(BTTH~LFA+SYEAR,data=ff,FUN=sum)
	  
	  names(f1)[3] = 'TotTr'
	  ff = merge(ff,f1)

	  fu = function(m,s,n) rnorm(n,mean=m,sd=s)
	  iter=1000
	  re = unique(cbind(ff$LFA,ff$SYEAR))
	  out = list()
	  for(i in 1:nrow(re)){
	  		k = subset(ff,LFA==re[i,1] & SYEAR==re[i,2])
	  		mu = sum(k$mean * k$BTTH)
	  		v  = sqrt(sum((k$BTTH)^2*(k$var/k$n)))
	  		ql = qnorm(.025,mu,v)
	  		qu = qnorm(.975,mu,v)
		  	for(j in 2:(iter)){
	  				kk = k[,c('BTTH','sdTH')]
	  				kk$n=1
	  				kw = unlist(Map(fu,kk$BTTH,kk$sdTH,kk$n))

	  				mu = c(mu,sum(k$mean * kw))
	  				v = c(v,sqrt(sum((kw)^2*(k$var/k$n))))
	  				ql =c(ql,qnorm(.025,mu[j],v[j]))
	  				qu = c(qu,qnorm(.975,mu[j],v[j]))
		  }
		out[[i]] = c(re[i,],mean(mu),mean(ql),mean(qu))
		}
	  
	gs = as.data.frame(do.call(rbind,out))
	names(gs)= c('LFA','YR','TotCunner','TotCunnerL','TotCunnerW')
	gs = toNums(gs,2:5)
	gs[,3:5] = gs[,3:5]/1000

