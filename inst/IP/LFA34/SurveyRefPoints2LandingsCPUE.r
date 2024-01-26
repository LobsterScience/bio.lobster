require(bio.lobster)
require(devtools)
require(bio.utilities)
require(ggplot2)
require(sf)
#rerun LFA34 updates to get survey figs
ff='LFA34Update'
dadir = file.path(project.figuredirectory('bio.lobster'),ff)
fpf1 = file.path(project.figuredirectory('bio.lobster'),"LFA34Update")
fp1 = file.path(project.datadirectory('bio.lobster'),"analysis",ff)
fpf = fpf1 = file.path(project.figuredirectory('bio.lobster'),ff)


a = lobster.db('seasonal.landings')
a = subset(a,select=c(SYEAR,LFA34))
a$yr = as.numeric(substr(a$SYEAR,6,9))

#surveys
Sp = read.csv(file=file.path(fpf,paste('LFA34-NEFSCSpringCommercialB.csv',sep="-")))
DF = read.csv(file=file.path(fpf,paste('DFORelf34.csv',sep="-")))
Fa = read.csv(file=file.path(fpf,paste('LFA34-NEFSCFallCommercialB.csv',sep="-")))
IL = read.csv(file=file.path(fpf1,'ILTSCommB.csv'))
c34 = read.csv(file=file.path(fpf1,'updatedlandings.csv') )

#ref points
RP = list(c(1539,80.1),c(3232, 223), c(686,107),c(4574,1975))

#biomass panel plots
par(mfrow=c(2,2), mar=c(4,4,2,1), mai=c(0.5,1,0.4,0.4))
with(Sp,plot(yr,w.Yst,pch=16,xlab=' ',ylab = 'NEFSC Spring', ylim=c(0,8000)))
Spr = rmed(Sp$yr, Sp$w.Yst)
lines(Spr$yr, Spr$x, lwd=2, col='salmon')
with(Sp, arrows(yr, y0=w.ci.Yst.l, y1=w.ci.Yst.u, length=0))
abline(h=RP[[1]][1], lwd=2, col='green',lty=2)
abline(h=RP[[1]][2], lwd=2, col='blue',lty=3)

with(Fa,plot(yr,w.Yst,pch=16,xlab=' ',ylab = 'NEFSC Fall', ylim=c(0,30000)))
Far = rmed(Fa$yr, Fa$w.Yst)
lines(Far$yr, Far$x, lwd=2, col='salmon')
with(Fa, arrows(yr, y0=w.ci.Yst.l, y1=w.ci.Yst.u, length=0))
abline(h=RP[[2]][1], lwd=2, col='green',lty=2)
abline(h=RP[[2]][2], lwd=2, col='blue',lty=3)

with(DF,plot(yr,w.Yst,pch=1,xlab='Year',ylab = 'DFO RV Survey', ylim=c(0,8000)))
with(subset(DF,yr>1998),points(yr,w.Yst,pch=16))

DFr = rmed(DF$yr, DF$w.Yst)
lines(DFr$yr, DFr$x, lwd=2, col='salmon')
with(DF, arrows(yr, y0=w.ci.Yst.l, y1=w.ci.Yst.u, length=0))
abline(h=RP[[3]][1], lwd=2, col='green',lty=2)
abline(h=RP[[3]][2], lwd=2, col='blue',lty=3)

with(IL,plot(Year,B,pch=16,xlab='Year',ylab = 'ILTS', ylim=c(0,30000)))
DFr = rmed(IL$Year, IL$B)
lines(DFr$yr, DFr$x, lwd=2, col='salmon')
with(IL, arrows(Year, y0=lB, y1=uB, length=0))
abline(h=RP[[4]][1], lwd=2, col='green',lty=2)
abline(h=RP[[4]][2], lwd=2, col='blue',lty=3)

#what year did we go above the below

#focus on DFO and RV since others seem to crap out in recent years

dfoUSRYear = (DF$yr[which(DF$w.Yst<RP[[3]][1])])
ILTSUSRYear = IL$Year[which(IL$B<RP[[4]][1])]

c34$yr[which(c34$yr%in%dfoUSRYear)]
c34$yr[which(c34$yr%in%ILTSUSRYear)]

#using John's resdoc from 2012 on landings 8857t is USR 4428 is LRP

LandingsYrs = c34$yr[which(c34$LFA34<=8857) ]


c34$col = ifelse(c34$yr %in% LandingsYrs,'red','black')
with(c34,plot(yr,LFA34,col=col,type='p',pch=16,main='Old Landings USR'))


c34$col = ifelse(c34$yr %in% c34$yr[which(c34$yr%in%dfoUSRYear)],'red','black')
with(c34,plot(yr,LFA34,col=col,type='p',pch=16,main='DFO RV USR'))

c34$col = ifelse(c34$yr %in% c34$yr[which(c34$yr%in%ILTSUSRYear)],'red','black')
with(c34,plot(yr,LFA34,col=col,type='p',pch=16,main='ILTS USR'))


#catch per unit effort

#marfis and grid reporting from 1998 onward
lS<-lobster.db('process.logs')
lS = subset(lS,SYEAR<2024 & LFA==34)

#really old
H = lobster.db('historic.cpue')
H$CPUE = H$LBSPTRAP/2.2046

#voluntary
lobster.db('process.vlog')
V = subset(vlog,LFA=='34')
V$SYEAR = as.numeric(year(V$FDATE))
V$SYEAR = year(V$FDATE)
V$MONTH = month(V$FDATE)
ii = which(V$MONTH>8)
V$SYEAR[ii] = V$SYEAR[ii]+1 



##LFA 34

H34 = aggregate(CPUE~LFA+SYEAR+SDATE,data=subset(H, LFA==34 & SYEAR<1960),FUN=mean)
H34 = H34[order(H34$SDATE),]
names(H34) = c('LFA','SYEAR','SDATE','CPUE')

V34 = aggregate(cbind(W_KG,N_TRP)~LFA+SYEAR+FDATE, data=subset(V,LFA==34), FUN=sum)
V34$CPUE = V34$W_KG / V34$N_TRP
V34$W_KG = V34$N_TRP = NULL
names(V34) = c('LFA','SYEAR','SDATE','CPUE')


aV34 = aggregate(cbind(W_KG,N_TRP)~LFA+SYEAR, data=subset(V,LFA==34), FUN=sum)
aV34$CPUE = aV34$W_KG / aV34$N_TRP
aV34$W_KG = aV34$N_TRP = NULL
names(aV34) = c('LFA','SYEAR','CPUE')

L34 = aggregate(cbind(WEIGHT_KG,NUM_OF_TRAPS)~LFA+SYEAR+DATE_FISHED, data=subset(lS,LFA==34), FUN=sum)
L34$CPUE = L34$WEIGHT_KG / L34$NUM_OF_TRAPS
L34$WEIGHT_KG = L34$NUM_OF_TRAPS = NULL
names(L34) = c('LFA','SYEAR','SDATE','CPUE')

aL34 = aggregate(cbind(WEIGHT_KG,NUM_OF_TRAPS)~LFA+SYEAR, data=subset(lS,LFA==34), FUN=sum)
aL34$CPUE = aL34$WEIGHT_KG / aL34$NUM_OF_TRAPS
aL34$WEIGHT_KG = aL34$NUM_OF_TRAPS = NULL
names(aL34) = c('LFA','SYEAR','CPUE')

bb34 = dplyr::bind_rows(list(aV34,aL34))

b34 = as.data.frame(rbind(rbind(H34,V34),L34))

plot(bb34$SYEAR,bb34$CPUE)

##CPUE and landings

cC = merge(bb34, c34,by.x=c('SYEAR'),by.y='yr')

with(cC,plot(LFA34,CPUE))

ss = merge(merge(cC,IL[,c('Year','B')],by.x='SYEAR',by.y='Year',all.x=T),Sp[,c('yr','w.Yst')],by.x='SYEAR',by.y='yr',all.x=T)
ss = subset(ss,select=c(SYEAR,LFA34,B,w.Yst,CPUE))
ss = rename.df(ss,c('LFA34','B','w.Yst'),c('Landings','ILTS','NEFSC_Sp'))

ss = merge(ss,DF[,c('yr','w.Yst')],by.x=c('SYEAR'),by.y='yr',all.x=T)
ss = rename.df(ss,c('w.Yst'),c('DFO_RV'))

ss = merge(ss,Fa[,c('yr','w.Yst')],by.x=c('SYEAR'),by.y='yr',all.x=T)
ss = rename.df(ss,c('w.Yst'),c('NEFSC_Fa'))


###DFA
require(MARSS)

ss = zero.na(ss)
cR = t(log(ss[,2:7]+0.01))

cRR = zscore(as.matrix(cR))
spp = rownames(cRR)
par(mfcol=c(2,3), mar=c(3,4,1.5,0.5), oma=c(0.4,1,1,1))
for(i in spp){
  plot(cRR[i,],xlab="",ylab="Abundance index", bty="L", xaxt="n", pch=16, col="blue", type="b")
  title(i)
}



N.ts = nrow(cRR)
nT = ncol(cRR)

##many model comparisons

cntl.list = list(minit=200, maxit=5000, allow.degen=FALSE)
  # set up forms of R matrices
  levels.R = c("diagonal and equal","diagonal and unequal","equalvarcov","unconstrained")
  levels.Q = c("diagonal and equal","diagonal and unequal")
  model.data = data.frame()
  for(R in levels.R) {
    for(Q in levels.Q){
      for(m in 1:(N.ts-1)) {
        dfa.model = list(A="zero", R=R,Q=Q, m=m)
        ff = MARSS(cRR, model=dfa.model, control=cntl.list,
                   form="dfa", z.score=TRUE)
        
        model.data = rbind(model.data,
                           data.frame(R=R,Q=Q,m=m,logLik=ff$logLik,K=ff$num.params,AICc=ff$AICc,stringsAsFactors=FALSE))
        assign(paste("ff", m, R, Q, sep="."), ff)
      } # end m loop
    } # end Q loop
  } # end R loop
  #best model is diag and unequal R and Q with 2 trends

    write.csv(model.data,'~/tmp/model.data.csv')
  

big.maxit.cntl.list = list(minit=200, maxit=30000, allow.degen=FALSE)
model.list = list(m=2, R="diagonal and unequal",Q = "diagonal and unequal")
the.fit = MARSS(cRR, model=model.list, form="dfa", control=big.maxit.cntl.list)

# get the inverse of the rotation matrix
Z.est = coef(the.fit, type="matrix")$Z


H.inv = 1
if(ncol(Z.est)>1) H.inv = varimax(coef(the.fit, type="matrix")$Z)$rotmat

clr <- c("brown","blue","darkgreen","darkred")
ylbl = row.names(cRR)
# rotate factor loadings
Z.rot = Z.est %*% H.inv
# rotate trends
trends.rot = solve(H.inv) %*% the.fit$states
tt = ss$SYEAR
mm=2


#plot states and loadings

layout(matrix(c(1,2,3,4,5,6),mm,2),widths=c(2,1))
## par(mfcol=c(mm,2), mai=c(0.5,0.5,0.5,0.1), omi=c(0,0,0,0))
par(mai=c(0.5,0.5,0.5,0.1), omi=c(0,0,0,0))
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
minZ <- 0.05
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
