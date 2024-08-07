##Design based estimates of Lobster Catch
require(bio.lobster)
require(bio.utilities)
require(RODBC)
require(lubridate)
require(devtools)
options(stringAsFactors=F)
la()

wd = ('C:\\Users\\Cooka\\OneDrive - DFO-MPO\\BycatchLobster')

setwd(wd)

b=bycatch.db('logbook.merge')
    
aS = bycatch.db('SWLSS')
aO = bycatch.db('ISDB.reshape')

 ao = aggregate(LegalWt~TRIP+LFA+Cluster+GridGroup+Period+SYEAR,data=aO,FUN=mean)
 aCs = aggregate(LegalWt~TRIP+LFA+Cluster+GridGroup+Period+SYEAR,data=aO,FUN=var,na.rm=T)
        aCs = rename.df(aCs,c('LegalWt'),c('LegalWtvar'))
        aCC = aggregate(UID~TRIP+LFA+Cluster+GridGroup+Period+SYEAR,data=aO,FUN=function(x) length(unique(x)))
        ao = merge(ao, aCC)
        aCo = merge(ao, aCs)
        aCo$mean =aCo$LegalWt
        aCo$var =aCo$LegalWtvar
        aCo$n = aCo$UID
 
 ap = aggregate(LegalWt~TRIP+LFA+Cluster+GridGroup+Period+SYEAR,data=aS,FUN=mean)
 aCs = aggregate(LegalWt~TRIP+LFA+Cluster+GridGroup+Period+SYEAR,data=aS,FUN=var,na.rm=T)
        aCs = rename.df(aCs,c('LegalWt'),c('LegalWtvar'))
        aCC = aggregate(UID~TRIP+LFA+Cluster+GridGroup+Period+SYEAR,data=aS,FUN=function(x) length(unique(x)))
        ap = merge(ap, aCC)
        aCp = merge(ap, aCs)
        aCp$mean =aCp$LegalWt
        aCp$var =aCp$LegalWtvar
        aCp$n = aCp$UID
  
 aT = as.data.frame(rbind(aCp,aCo))       

 aT$Strata = paste(aT$LFA,aT$GridGroup,aT$Period,sep='_')

 op = list()
 aTs = split(aT,f=list(aT$SYEAR,aT$Strata))
 aTs = rm.from.list(aTs)
  for(i in 1:length(aTs)){
              j = aTs[[i]]
              op[[i]] =c(SYEAR= unique(j$SYEAR),LFA =unique(j$LFA), GridGroup = unique(j$GridGroup), Period = unique(j$Period),Strata=unique(j$Strata),clusterStatistics(j))      
            }
        aCo = as.data.frame(do.call(rbind,op))
        aCo = toNums(aCo,c(4,6:9))
      	ui = which(is.na(aCo$sd))
      	aCo$sd[ui] <- aCo$mean[ui] #assume cv=1
      	aCo$n[ui] <- 2 #assume cv=1
      	
      uu = readRDS('results/BumpedUpEffortByGridGroup.rds')

      uu = subset(uu,LFA %in% 33:35 & SYEAR %in% c(2019,2020,2021))
	  uu$Strata = paste(uu$LFA,uu$GridGroup,uu$Period,sep='_')
	  uu = subset(uu,select=c('SYEAR','LFA','Strata','Period','GridGroup','BTTH','BlTH','BuTH'))

	  au = merge(uu,aCo,all.x=T)

	  #for unsampled strata use the mean and of the time period from all others using the same clustering 
	  uy = read.csv('data/GridGroupsLocation.csv')
	  names(uy)[1] = 'LFA'
	  au = merge(au,uy)
	  au$var = (au$sd)^2
	  aug = split(au,f=list(au$Period,au$SYEAR, au$LFA))
	  ol = list()
	  aug = rm.from.list(aug)

	  for(i in 1:length(aug)){
	  		j = aug[[i]]
	  		if(all(!is.na(j$mean)) | all(is.na(j$mean))) {
	  			ol[[i]] = j
	  			} else {

	  		ii = which(is.na(j$mean))
	  		k = j[-ii,]
	  			for(l in 1:length(ii)){
	  				ll = j[ii[l],'Location']
	  				lmo = subset(k,Location == ll)
	  					if(nrow(lmo)==0){
	  							j$mean[ii[l]] <- min(j$mean,na.rm=T)
	  							j$sd[ii[l]]	<- min(j$sd,na.rm=T)
	  							j$n[ii[l]]	<- 2
	  						} else {
	  							lk = clusterStatistics(lmo)
	  							j$mean[ii[l]] <- lk[1]
	  							j$sd[ii[l]] <- lk[2]
	  						}
		  			}
		  	ol[[i]] <- j
 			}
	  }

	  ff = as.data.frame(do.call(rbind,ol))

	  fg = aggregate(mean~LFA+Period,data=ff,FUN=mean,na.rm=T)
	  fs = aggregate(mean~LFA+Period,data=ff,FUN=sd,na.rm=T)
	  names(fs)[3] = 'sd'
	  ffs = merge(fg,fs)
	  i = which(is.na(ff$mean))

	  ff$mean[i] = ffs[which(ffs$LFA==35 & ffs$Period==3),'mean']
	  ff$sd[i] = ffs[which(ffs$LFA==35 & ffs$Period==3),'sd']

	  i = which(is.na(ff$n))
	  ff$n[i] =2

	  #stratified totals
	  ff$Tot = ff$BTTH * ff$mean
	  ff$Totu = ff$BuTH*ff$mean
	  ff$Totl = ff$BlTH*ff$mean
	 gs =  aggregate(cbind(Tot,Totu,Totl)~LFA+SYEAR,data=ff,FUN=sum)

	 gg = lobster.db('seasonal.logs')

	 #so many gaps the filling kills it


	 #combine all data sets remove the syear and run it all 



	 b=bycatch.db('logbook.merge')
    
aS = bycatch.db('SWLSS')
aO = bycatch.db('ISDB.reshape')

 ao = aggregate(LegalWt~TRIP+LFA+Cluster+GridGroup+Period,data=aO,FUN=mean)
 aCs = aggregate(LegalWt~TRIP+LFA+Cluster+GridGroup+Period,data=aO,FUN=var,na.rm=T)
        aCs = rename.df(aCs,c('LegalWt'),c('LegalWtvar'))
        aCC = aggregate(UID~TRIP+LFA+Cluster+GridGroup+Period,data=aO,FUN=function(x) length(unique(x)))
        ao = merge(ao, aCC)
        aCo = merge(ao, aCs)
        aCo$mean =aCo$LegalWt
        aCo$var =aCo$LegalWtvar
        aCo$n = aCo$UID
 
 ap = aggregate(LegalWt~TRIP+LFA+Cluster+GridGroup+Period,data=aS,FUN=mean)
 aCs = aggregate(LegalWt~TRIP+LFA+Cluster+GridGroup+Period,data=aS,FUN=var,na.rm=T)
        aCs = rename.df(aCs,c('LegalWt'),c('LegalWtvar'))
        aCC = aggregate(UID~TRIP+LFA+Cluster+GridGroup+Period,data=aS,FUN=function(x) length(unique(x)))
        ap = merge(ap, aCC)
        aCp = merge(ap, aCs)
        aCp$mean =aCp$LegalWt
        aCp$var =aCp$LegalWtvar
        aCp$n = aCp$UID
  
 aT = as.data.frame(rbind(aCp,aCo))       

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
   uu = subset(uu,LFA %in% 33:35 & SYEAR %in% c(2019,2020,2021))
	  uu$Strata = paste(uu$LFA,uu$GridGroup,uu$Period,sep='_')
   uu = aggregate(cbind(BTTH,BuTH,BlTH)~Strata+LFA+GridGroup+Period,data=uu,FUN=mean)
	  au = merge(uu,aCo,all.x=T)

#4missing Strata with minimal effort

     uy = read.csv('data/GridGroupsLocation.csv')
	  names(uy)[1] = 'LFA'
	  au = merge(au,uy)
	  au$var = (au$sd)^2
	  aug = split(au,f=list(au$Period, au$LFA))
	  ol = list()
	  aug = rm.from.list(aug)

	  for(i in 1:length(aug)){
	  		j = aug[[i]]
	  		if(all(!is.na(j$mean)) | all(is.na(j$mean))) {
	  			ol[[i]] = j
	  			} else {

	  		ii = which(is.na(j$mean))
	  		k = j[-ii,]
	  			for(l in 1:length(ii)){
	  				ll = j[ii[l],'Location']
	  				lmo = subset(k,Location == ll)
	  					if(nrow(lmo)==0){
	  							j$mean[ii[l]] <- min(j$mean,na.rm=T)
	  							j$sd[ii[l]]	<- min(j$sd,na.rm=T)
	  							j$n[ii[l]]	<- 2
	  						} else {
	  							lk = clusterStatistics(lmo)
	  							j$mean[ii[l]] <- lk[1]
	  							j$sd[ii[l]] <- lk[2]
	  						}
		  			}
		  	ol[[i]] <- j
 			}
	  }

	  ff = as.data.frame(do.call(rbind,ol))
  
	  fg = aggregate(mean~LFA+Period,data=ff,FUN=mean,na.rm=T)
	  fs = aggregate(mean~LFA+Period,data=ff,FUN=sd,na.rm=T)
	  names(fs)[3] = 'sd'
	  ffs = merge(fg,fs)
	  i = which(is.na(ff$mean))

	  ff$mean[i] = ffs[which(ffs$LFA==35 & ffs$Period==3),'mean']
	  ff$sd[i] = ffs[which(ffs$LFA==35 & ffs$Period==3),'sd']

	  i = which(is.na(ff$n))
	  ff$n[i] =2

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
	gg = lobster.db('seasonal.landings')
	gg = subset(gg,SYEAR %in% c('2018-2019','2019-2020','2020-2021'),select=c(SYEAR,LFA33,LFA34,LFA35))
	gg = apply((gg[,2:4]),2,mean)

	 gg = cbind(gs,gg)
	 gg = toNums(gg,1:5)
	 gg[,2:4] = gg[,2:4]/1000
	 with(gg,plot(V1,V2,ylim=c(2000,22000)))
		with(gg,points(V1,gg,pch=16))
		with(gg,arrows(V1,y0=V3,y1=V4,length=0))


##cbfha

	 b=bycatch.db('logbook.merge')
    
aS = bycatch.db('CBFHA')

 ap = aggregate(LegalWt~TRIP+LFA+Cluster+GridGroup+Period,data=aS,FUN=mean)
 aCs = aggregate(LegalWt~TRIP+LFA+Cluster+GridGroup+Period,data=aS,FUN=var,na.rm=T)
        aCs = rename.df(aCs,c('LegalWt'),c('LegalWtvar'))
        aCC = aggregate(UID~TRIP+LFA+Cluster+GridGroup+Period,data=aS,FUN=function(x) length(unique(x)))
        ap = merge(ap, aCC)
        aCp = merge(ap, aCs)
        aCp$mean =aCp$LegalWt
        aCp$var =aCp$LegalWtvar
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
	gg = lobster.db('annual.landings')
	gg = subset(gg,YR ==2019,select=c(YR,LFA27))

	 gg = cbind(gs,gg)
	 gg = toNums(gg,1:5)
	

#######################
##GCIFA
 
b=bycatch.db('logbook.merge')
    
aS = bycatch.db('GCIFA')

 ap = aggregate(LegalWt~TRIP+LFA+Cluster+Period+SYEAR,data=aS,FUN=mean)
 aCs = aggregate(LegalWt~TRIP+LFA+Cluster+Period+SYEAR,data=aS,FUN=var,na.rm=T)
        aCs = rename.df(aCs,c('LegalWt'),c('LegalWtvar'))
        aCC = aggregate(UID~TRIP+LFA+Cluster+Period+SYEAR,data=aS,FUN=function(x) length(unique(x)))
        ap = merge(ap, aCC)
        aCp = merge(ap, aCs)
        aCp$mean =aCp$LegalWt
        aCp$var =aCp$LegalWtvar
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
	names(gs)[1:2] = c('LFA','YR')
	gg = lobster.db('annual.landings')
	gg = subset(gg,YR %in% c(2018,2019),select=c(YR,LFA31A,LFA31B))
	gg = reshape(gg,idvar='YR',varying=list(2:3),direction='long')
	gg$LFA = c('31A','31A','31B','31B')
	gg$SYEAR=gg$YR
	 gg = merge(gs,gg)
	 gg = toNums(gg,2:5)
	 gg[,3:5] = gg[,3:5]/1000
	 
