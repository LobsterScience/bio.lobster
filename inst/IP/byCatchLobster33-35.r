require(bio.lobster)
require(bio.utilities)
require(RODBC)
require(lubridate)
require(devtools)
options(stringAsFactors=F)
la()


wd = ('C:/Users/CookA/Desktop/dellshared/Bycatch in the Lobster Fishery')
setwd(wd)

#bycatch.db(DS='odbc.redo',wd=wd)

#LOADING AND MERGING LOGBOOKS with TARGETS
    #bycatch.db(DS='logbook.merge.redo',wd=wd)

        b = bycatch.db('logbook.merge',wd=wd) 
        gt = t = bycatch.db('targets',wd=wd) 
        t = subset(t,LFA%in%33:35)
        tt = aggregate(cbind(Period1,Period2,Period3)~LFA+GridGrouping,data=t,FUN=min)
        
        bA = aggregate(SD_LOG_ID~Period+GridGroup+LFA+SYEAR,data=b,FUN=function(x) length(unique(x)))
        tR = as.data.frame(reshape(tt,varying=3:5,direction = 'long',sep=""))
        names(tR) = c('LFA','GridGroup','Period','Target','Nt')
        tR$Nt = NULL
        
        bA = merge(bA,tR,all.x=T)
        
        write.csv(bA,file=file.path(wd,'results','LogbooksAgg and Targets.csv'))
        
        
#Describing the fishery from these logbooks
#LFA by LFA
        
  bW = aggregate(cbind(WEIGHT_KG,NUM_OF_TRAPS)~WOS+GRID_NUM,data=subset(b,LFA==34),FUN=sum)    
  bE = reshape(bW[,c('WOS','GRID_NUM','NUM_OF_TRAPS')],idvar='WOS',timevar = 'GRID_NUM',direction='wide')
  bE = na.zero(bE)   
  bE$WOS = NULL
        
  j = c(apply(bE,2,FUN=function(x) length(which(x>0))))
  j = which(j>15)
  
  cR = t(log(bE[,j]+0.01))
  bER = zscore(as.matrix(cR))
  gN = rownames(bER)
  
  N.ts = nrow(bER)
  nT = ncol(bER)
  
 N.tsr=8
  require(foreach)
  require(parallel)
 require(doParallel)
 registerDoParallel(10)
 
 foreach(i=1:10,.combine='rbind') %dopar%{
   require(MARSS)
   cntl.list = list(minit=200, maxit=5000, allow.degen=FALSE)
   dfa.model = list(A="zero", R='diagonal and equal',Q='diagonal and equal', m=i)
   ff = MARSS(bER, model=dfa.model, control=cntl.list,
              form="dfa", z.score=TRUE)
      data.frame(m=m,logLik=ff$logLik,K=ff$num.params,AICc=ff$AICc,stringsAsFactors=FALSE)
 }
 stopImplicitCluster()
 
 
    levels.R = c("diagonal and equal")#,"diagonal and unequal")#,"equalvarcov","unconstrained")
    levels.Q = c("diagonal and equal")#,"diagonal and unequal")
    #model.data = data.frame()
    for(R in levels.R) {
      for(Q in levels.Q){
        for(m in 3:(N.tsr)) {
        } # end m loop
      } # end Q loop
    } # end R loop
    #best model is diag and unequal R and Q with three trends
    write.csv(model.data,'results/Landings34DFAmodel.data.csv')
    
  }
  
  big.maxit.cntl.list = list(minit=200, maxit=30000, allow.degen=FALSE)
  model.list = list(m=3, R="diagonal and equal",Q = "diagonal and equal")
  the.fit = MARSS(bER, model=model.list, form="dfa", control=big.maxit.cntl.list)
  
  
  # get the inverse of the rotation matrix
  Z.est = coef(the.fit, type="matrix")$Z
  
  
  H.inv = 1
  if(ncol(Z.est)>1) H.inv = varimax(coef(the.fit, type="matrix")$Z)$rotmat
  
  clr <- c("brown","blue","darkgreen","darkred")
  ylbl = row.names(bER)
  spp = ylbl = unlist(strsplit(ylbl,'NUM_OF_TRAPS.'))[seq(2,19,2)]
  row.names(bER) = spp
  # rotate factor loadings
  Z.rot = Z.est %*% H.inv
  # rotate trends
  trends.rot = solve(H.inv) %*% the.fit$states
  tt = 1:nT
  mm=3
  
  
  #plot states and loadings
  
  layout(matrix(c(1,2,3),mm,2),widths=c(2,1))
  #par(mfcol=c(mm,2), mar=c(0.7,.5,.2,.1), omi=c(0,0,0,0))
  par(mfcol=c(mm,2),mai=c(0.5,0.5,0.5,0.1), omi=c(0,0,0,0))
  ## plot the processes
  for(i in 1:mm) {
    ylm <- c(-1,1)*max(abs(trends.rot[i,]))
    ## set up plot area
    plot(tt,trends.rot[i,], type="n", bty="L",
         ylim=ylm, xlab="", ylab="", xaxt="n")
    ## draw zero-line
    abline(h=0, col="gray")
    ## plot trend line
    lines(tt,trends.rot[i,], lwd=2)
    ## add panel labels
    mtext(paste("Trend",i), side=3, line=0.5)
    axis(1,tt)
  }
  ## plot the loadings
  minZ <- 0.01
  ylm <- c(-1,1)*max(abs(Z.rot))
  for(i in 1:mm) {
    plot(c(1:N.ts)[abs(Z.rot[,i])>minZ], as.vector(Z.rot[abs(Z.rot[,i])>minZ,i]), type="h",
         lwd=2, xlab="", ylab="", xaxt="n", ylim=ylm, xlim=c(0.5,N.ts+0.5), col=clr)
    for(j in 1:N.ts) {
      if(Z.rot[j,i] > minZ) {text(j, -0.03, ylbl[j], srt=90, adj=1, cex=1.2, col=clr[j])}
      if(Z.rot[j,i] < -minZ) {text(j, 0.03, ylbl[j], srt=90, adj=0, cex=1.2, col=clr[j])}
      abline(h=0, lwd=1.5, col="gray")
    } 
    mtext(paste("Factor loadings on trend",i),side=3,line=0.5)
  }
  
  
  #model fits
  getDFAfits <- function(MLEobj, alpha=0.05, covariates=NULL) {
    fits <- list()
    Ey <- MARSShatyt(MLEobj) # for var() calcs
    ZZ <- coef(MLEobj, type="matrix")$Z # estimated Z
    nn <- nrow(ZZ) # number of obs ts
    mm <- ncol(ZZ) # number of factors/states
    TT <- ncol(Ey$ytT)  # number of time steps
    ## check for covars
    if(!is.null(covariates)) {
      DD <- coef(MLEobj, type="matrix")$D
      cov_eff <- DD %*% covariates
    } else {
      cov_eff <- matrix(0, nn, TT)
    }
    ## model expectation
    fits$ex <- ZZ %*% MLEobj$states + cov_eff
    ## Var in model fits
    VtT <- MARSSkfss(MLEobj)$VtT
    VV <- NULL
    for(tt in 1:TT) {
      RZVZ <- coef(MLEobj, type="matrix")$R - ZZ%*%VtT[,,tt]%*%t(ZZ)
      SS <- Ey$yxtT[,,tt] - Ey$ytT[,tt,drop=FALSE] %*% t(MLEobj$states[,tt,drop=FALSE])
      VV <- cbind(VV,diag(RZVZ + SS%*%t(ZZ) + ZZ%*%t(SS)))
    }
    SE <- sqrt(VV)
    
    ## upper & lower (1-alpha)% CI
    fits$up <- qnorm(1-alpha/2)*SE + fits$ex
    fits$lo <- qnorm(alpha/2)*SE + fits$ex
    return(fits)
  }
  
  fit.b = getDFAfits(the.fit)
  
  
  #plot the factor loadings
  minZ = 0.05
  m=dim(trends.rot)[1]
  ylims = c(-1.1*max(abs(Z.rot)), 1.1*max(abs(Z.rot)))
  par(mfrow=c(ceiling(m/2),2), mar=c(3,4,1.5,0.5), oma=c(0.4,1,1,1))
  for(i in 1:m) {
    plot(c(1:N.ts)[abs(Z.rot[,i])>minZ], as.vector(Z.rot[abs(Z.rot[,i])>minZ,i]),
         type="h", lwd=2, xlab="", ylab="", xaxt="n", ylim=ylims, xlim=c(0,N.ts+1))
    for(j in 1:N.ts) {
      if(Z.rot[j,i] > minZ) {text(j, -0.05, spp[j], srt=90, adj=1, cex=0.9)}
      if(Z.rot[j,i] < -minZ) {text(j, 0.05, spp[j], srt=90, adj=0, cex=0.9)}
      abline(h=0, lwd=1, col="gray")
    } # end j loop
    mtext(paste("Factor loadings on trend",i,sep=" "),side=3,line=.5)
  } # end i loop
  
  require(broom)
  require(ggplot2)
  theme_set(theme_bw())
  d <- fitted(the.fit, interval="confidence")
  d$t = rep(1:ncol(bER),times=9)
  p1 = ggplot(data = d) +
    geom_line(aes(t, .fitted)) +
    geom_point(aes(t, y)) +
    geom_ribbon(aes(x=t, ymin=.conf.low, ymax=.conf.up), linetype=2, alpha=0.2) +
    facet_grid(~.rownames) +
    xlab("Year") + ylab("Standardized Abundance")
  
  require(ggpubr)
  ggarrange(p1,p2,ncol=1,nrow=2)
  savePlot(file.path(fpf1,'FitsDFALFA34.png'))
  ##
  
  
  
#SWLSS sea sampling
    CDa = bycatch.db(DS='SWLSS')
        
        
#Empties by month
CDa$mn = month(CDa$BOARD_DATE)
CDa$SYEAR = year(CDa$BOARD_DATE)
CDa$SYEAR = ifelse(CDa$mn>=10,CDa$SYEAR+1,CDa$SYEAR)
Emp = aggregate(cbind(Empty,P)~COMAREA_ID+mn,data=CDa,FUN=sum)
Emp$PropEmt = Emp$Empty / Emp$P

#Traps Sampled By Area Need to merge in grid2targets

CDa$GridGroup = CDa$target = CDa$Period = NA
for(i in 1:nrow(CDa)){
	pit = gt$GridGrouping[which(CDa$STRATUM_ID[i]==gt$GRID_NUM & gt$LFA==strsplit(CDa$COMAREA_ID[i],'L')[[1]][2])]
	if(length(pit)>0){
	CDa$GridGroup[i] = (pit)
	m = CDa$mn[i]
	k = subset(gt,GRID_NUM==CDa$STRATUM_ID[i] & LFA==strsplit(CDa$COMAREA_ID[i],'L')[[1]][2])
	ll = ifelse(m >=k$Period1.Start & m<=k$Period1.End,'Period1',ifelse(m >=k$Period2.Start & m<=k$Period2.End,'Period2',ifelse(m >=k$Period3.Start & m<=k$Period3.End,'Period3','Period4')))
	lll = as.numeric(strsplit(ll,'Period')[[1]][2])
	CDa$target[i] <- as.numeric(k[,ll])
	CDa$Period[i] = lll
#	rm(k,m,ll,lll)
	}
}	


#Total number of traps per Year, Period and GridGrouping
        STraps = aggregate(P~SYEAR+GridGroup+COMAREA_ID+Period+target,data=CDa,FUN=sum)
        STraps$LFA = as.numeric(unlist(lapply(strsplit(STraps$COMAREA_ID,'L'),"[[",2)))
        
        ATrips = aggregate(TRIP~SYEAR+COMAREA_ID+Period+target+GridGroup,data=CDa,FUN=function(x) length(unique(x)))
        
        ATrips$LFA = as.numeric(unlist(lapply(strsplit(ATrips$COMAREA_ID,'L'),"[[",2)))
        
        TTraps = aggregate(NUM_OF_TRAPS~SYEAR+LFA+GridGroup+Period+target,data=b,FUN=sum)
        
        STTraps = merge(TTraps,STraps, all.x=T)
        
        STTraps =merge(STTraps,ATrips,all=T)
        
        STTraps$Prp = STTraps$P / STTraps$NUM_OF_TRAPS *100
  write.csv(STTraps,file=file.path('results','SWLSSTrips2Targets.csv'))
        
        STTraps = merge(STTraps,bA,all=T)
        names(STTraps) = c('Year','LFA','GridGroup','Period','Target','CI','NUM_OF_TRAPS_FISHED','NUM_OF_TRAPS_SAMPLED_SWLSS','NUM_OF_TRIPS_SAMPLED_SWLSS','P','NUM_SDLOGS','tt')
        STTraps$CI = STTraps$tt =NULL
        
        
        write.csv(STTraps,file=file.path('results','SWLSSTrips2TargetsTrapsandTrips.csv'))
  
        STTraps  = read.csv(file=file.path('results','SWLSSTrips2TargetsTrapsandTrips.csv'))
        
  
  boxplot(Prp~SYEAR+LFA,data=STTraps,las=2,xlab='',ylab='Total Traps Hauled (%)')


hist(CDa$Lobster,breaks=0:50,main="",xlab='Number of Lobster')
savePlot('Figures/DistributionOfLobsterCatchesSampledTraps.png')




#Start with averages across traps

aC = aggregate(cbind(Lobster,Cod,Cusk,Jonah,Legal,Berried, Empty,LegalWt)~TRIP+FISHSET_ID+COMAREA_ID+STRATUM_ID+NUM_HOOK_HAUL,data=CDa,FUN=median)

aC$TotLegal = aC$LegalWt * aC$NUM_HOOK_HAUL

aCC = aggregate(cbind(TotLegal,NUM_HOOK_HAUL)~TRIP,data=aC,FUN=sum)

ms = read.csv('data/SWLSSTripmatch.csv')
ms = subset(ms,select=c(TRIP,SD_LOG_ID_1,QUALITY))

bL = merge(b,ms,by.x='SD_LOG_ID', by.y='SD_LOG_ID_1')
bLL = aggregate(cbind(WEIGHT_KG,NUM_OF_TRAPS)~SD_LOG_ID+TRIP,data=bL,FUN=sum)
SBUMP = merge(aCC,bLL)

#any bias in reported landings?
with(SBUMP,plot(TotLegal,WEIGHT_KG))
abline(b=1,a=0)

with(SBUMP,lm(log(WEIGHT_KG+.01)~log(TotLegal+.01)-1)) #treating the SWLSS data as 'truth'

#using NUM_OF_TRAPS

SBUMP$WEIGHT_ASS_LOGS = SBUMP$TotLegal/SBUMP$NUM_HOOK_HAUL * SBUMP$NUM_OF_TRAPS
with(SBUMP,plot(WEIGHT_ASS_LOGS,WEIGHT_KG))
abline(b=1,a=0)



#any bias in reported trap hauls?
#we expect NUM_HOOK_HAUL is under represtned, not entire trip being sampled.dd



#Observers
################\
#relying on atSea.clean for observer data

#gg = bycatch.db('ISDB.redo')
gg = bycatch.db('ISDB')


## Trips that dont match with grids or something
gr = subset(gg,is.na(target))
gr = as.data.frame(unique(cbind(gr$TRIPNO,gr$LFA,gr$GRIDNO)))
se = connect.command(con,'select * from lobster.issets_mv')

names(gr) = c('TRIP_ID','LFA','Grid')
ss = as.data.frame(unique(cbind(se$TRIP_ID,se$TRIP)))
names(ss) = c('TRIP_ID','TRIP')
merge(gr,ss)

###Moving on to summary of observer data update later after we get more complete info

UTraps = aggregate(UID~SYEAR+LFA+GridGroup+Period+target,data=gg,FUN=function(x) length(unique(x)))
UTrips = aggregate(TRIPNO~SYEAR+LFA+GridGroup+Period+target,data=gg,FUN=function(x) length(unique(x)))
ggg = merge(UTrips,UTraps,all=T)
names(ggg)[c(1,5:7)] = c('Year','Target','ObserverTrips','ObserverTraps')

STTraps = merge(STTraps,ggg,all=T)

write.csv(STTraps,file=file.path('results','SWLSSandObsTrips2TargetsTrapsandTrips.csv'))


####pertrap info using

ao = bycatch.db('ISDB.reshape.redo')


aCo = aggregate(cbind(Lobster,Cod,Cusk,Jonah,Legal,Berried, Empty,LegalWt)~TRIPNO+LFA+NUM_HOOK_HAUL,data=ao,FUN=median)



aCo$TotLegal = aCo$LegalWt * aCo$NUM_HOOK_HAUL

aCoC = aggregate(cbind(TotLegal,NUM_HOOK_HAUL)~TRIPNO,data=aCo,FUN=sum)


ms = read.csv('data/ISDB_Trip_id to sd_log_idFinal.csv')
ms = subset(ms,!is.na(SD_LOG_ID_1),select=c(trip_id,SD_LOG_ID_1))

bLo = merge(b,ms,by.x='SD_LOG_ID', by.y='SD_LOG_ID_1')
bLoL = aggregate(cbind(WEIGHT_KG,NUM_OF_TRAPS)~SD_LOG_ID+trip_id,data=bLo,FUN=sum)
SBUMP0 = merge(aCoC,bLoL,by.x='TRIPNO',by.y='trip_id')



###Comparison of precited v reported landings between SWLSS and Observer
with(SBUMP0,plot(WEIGHT_KG,TotLegal))
SBUMP0$TotLegalNTraps = SBUMP0$TotLegal/SBUMP0$NUM_HOOK_HAUL * SBUMP0$NUM_OF_TRAPS
require(MASS)
par(mfrow=c(1,2))
with(SBUMP,plot(WEIGHT_KG,TotLegal,ylab='Extrapolated landings from Sea Samples',main='SWLSS'))
abline(a=0,b=1)
LMSW = with(SBUMP,lm(log(TotLegal+.01)~log(WEIGHT_KG+.01)-1)) #treating the SWLSS data as 'truth'
RLMSW = with(SBUMP,rlm(log(TotLegal+.01)~log(WEIGHT_KG+.01)-1)) #treating the SWLSS data as 'truth'
with(SBUMP0,plot(WEIGHT_KG,TotLegalNTraps,ylab='Extrapolated landings from Sea Samples',main='Observer'))
abline(a=0,b=1)
LMO = (with(SBUMP0,lm(log(TotLegalNTraps+.01)~log(WEIGHT_KG+.01)-1)))
RLMO = (with(SBUMP0,rlm(log(TotLegalNTraps+.01)~log(WEIGHT_KG+.01)-1)))

#RMSE
summary(LMO)$sigma
summary(RLMO)$sigma

summary(LMSW)$sigma
summary(RLMSW)$sigma

biasSWLSS = sum(SBUMP$WEIGHT_KG - SBUMP$TotLegal)/nrow(SBUMP) 
biasOBs = sum(SBUMP0$WEIGHT_KG - SBUMP0$TotLegalNTraps )/nrow(SBUMP0) 

###
#Comparing Temporal sampling with Fishing (by LFA)

b = bycatch.db('logbook.merge',wd=wd) 
bW = aggregate(NUM_OF_TRAPS~WOS+SYEAR+LFA,data=b,FUN=sum)


aO = bycatch.db('ISDB.reshape')
aS = bycatch.db(DS='SWLSS')

ii = c(33,34,35)
jj = 2019:2021
par(mfrow=c(3,3))
for(i in ii){
  for(j in jj){
    
        bss = subset(bW,LFA==i & SYEAR==j)
        bss$CE = cumsum(bss$NUM_OF_TRAPS)/sum(bss$NUM_OF_TRAPS)
        plot(bss$WOS,bss$CE,type='l',lwd=2,xlab='WOS',ylab='Cumulative Effort',main=paste(i,j,sep="-"))
        
        aOs = subset(aO,LFA==i & SYEAR==j)
        if(nrow(aOs)>0){
          print(c(i,j))
        aOs = aggregate(UID~WOS,data=aOs,FUN=function(x) length(unique(x)))
        aOs$CE = cumsum(aOs$UID)/sum(aOs$UID)
        lines(aOs$WOS,aOs$CE,type='l',lwd=2,col='red',lty=2)
        }
        aSs = subset(aS,LFA==i & SYEAR==j)
        if(nrow(aSs)>0){
          print(c(i,j))
        aSs = aggregate(UID~WOS,data=aSs,FUN=function(x) length(unique(x)))
        aSs$CE = cumsum(aSs$UID)/sum(aSs$UID)
        lines(aSs$WOS,aSs$CE,type='l',lwd=2,col='green',lty=3)
        }
  }
}

#by year, by lfa by grid group


ii = c(33,34,35)
jj = 2019:2021
pdf('Figures/SamplingbyArea.pdf')
for(i in ii){
  for(j in jj){
    
    bss = subset(b,LFA==i & SYEAR==j)
    gg = c(na.omit(unique(bss$GridGroup)))
    for(g in gg){
    bsst = subset(bss,GridGroup==g)
    bsst = aggregate(NUM_OF_TRAPS~WOS, data=bsst,FUN=sum)
    bsst$CE = cumsum(bsst$NUM_OF_TRAPS)/sum(bsst$NUM_OF_TRAPS)
    plot(bsst$WOS,bsst$CE,type='l',lwd=2,xlab='WOS',ylab='Cumulative Effort',main=paste(i,j,g,sep="-"))
    
    aOs = subset(aO,LFA==i & SYEAR==j & GridGroup==g)
    if(nrow(aOs)>0){
      print(c(i,j))
      aOs = aggregate(UID~WOS,data=aOs,FUN=function(x) length(unique(x)))
      aOs$CE = cumsum(aOs$UID)/sum(aOs$UID)
      lines(aOs$WOS,aOs$CE,type='l',lwd=2,col='red',lty=2)
    }
    aSs = subset(aS,LFA==i & SYEAR==j& GridGroup==g)
    if(nrow(aSs)>0){
      print(c(i,j))
      aSs = aggregate(UID~WOS,data=aSs,FUN=function(x) length(unique(x)))
      aSs$CE = cumsum(aSs$UID)/sum(aSs$UID)
      lines(aSs$WOS,aSs$CE,type='l',lwd=2,col='green',lty=3)
      }
    }
  }
}
graphics.off()
#by y
