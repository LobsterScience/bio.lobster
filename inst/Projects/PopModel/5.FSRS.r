#5.FSRSCPUE

lobster.db('fsrs.redo')
fsrs = subset(fsrs,LFA %in% c(33,34,35))
FSRS.dat<-fsrs
FSRS.dat$VES_DATE<-paste(FSRS.dat$VESSEL_CD,FSRS.dat$HAUL_DATE,sep='.')
FSRS.dat$SYEAR<-FSRS.dat$HAUL_YEAR
FSRS.dat$HAUL_DATE<-as.Date(FSRS.dat$HAUL_DATE)
FSRS.dat$SYEAR[FSRS.dat$LFA%in%c("33","34","35")]<-as.numeric(substr(FSRS.dat$S_LABEL[FSRS.dat$LFA%in%c("33","34","35")],6,9))

FSRS.dat<-subset(FSRS.dat,SOAK_DAYS<6)	# Remove soak days greater than 5,  do not iclude berried females
FSRS.dat$HAUL_DATE<-as.Date(FSRS.dat$HAUL_DATE)

ii = unique(FSRS.dat[,c('SYEAR','LFA')])
FSRS.dat$DOS = NA
oo = list()
for(i in 1:nrow(ii)){
  g = subset(FSRS.dat,SYEAR ==ii[i,1] & LFA ==ii[i,2])
  g$DOS = as.numeric(g$HAUL_DATE - min(g$HAUL_DATE,na.rm=T))
  oo[[i]] = g
  rm(g)
}

FD = as.data.frame(do.call(rbind,oo))
FDM = aggregate(LOBSTER_NO~RECORD_NUMBER+TRAP_NO+LFA+VESSEL_CD+DOS+SYEAR,data=FD,FUN=max)
FDMt = aggregate(TRAP_NO~DOS+SYEAR,data=FDM,FUN=length)
FDMl = aggregate(LOBSTER_NO~DOS+SYEAR,data=FDM,FUN=sum)
FDMU = aggregate(VESSEL_CD~SYEAR,data=FD,FUN=function(x) length(unique(x)))
#Total across yrs
FDMM = merge(FDMl,FDMt)
FDMM$fSyear = as.factor(FDMM$SYEAR)
FDMM$lTrap = log(FDMM$TRAP_NO)

require(rstanarm)
options(mc.cores = parallel::detectCores())
S = stan_glm.nb(LOBSTER_NO ~ fSyear + DOS+offset(lTrap), data = FDMM,iter=3000)

P = posterior_predict(S, newdata = newd,offset=rep(3,nrow(newd)))
quants=c(0.25,0.5,0.75)
x = as.data.frame(t(apply(P,2,quantile,quants) / exp(3)))
x$SYEAR = 2000:2020
x$median = x[,2]
x$lb = x[,1]
x$ub = x[,3]
x$sd = (apply(P,2,sd)/exp(3))
x$mu = (apply(P,2,mean)/exp(3))

x = merge(x,FDMU)
x$CV=x$sd/x$mu

with(x,plot(SYEAR,mu,type='b',ylim=c(0,6)))
with(x,arrows(x0=SYEAR,y0=lb,y1=ub,length=0))

#NOT INFORMATIVE NOT USED

