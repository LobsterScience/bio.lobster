#plots
require(bio.lobster)

##Commercial
ff = "LFA34Update2024"
fp1 = file.path(project.datadirectory('bio.lobster'),"analysis",ff)
fpf = fpf1 = file.path(project.figuredirectory('bio.lobster'),ff)


a = lobster.db('seasonal.landings')
a = subset(a,select=c(SYEAR,LFA34))
a$yr = as.numeric(substr(a$SYEAR,6,9))
write.csv(a,file=file.path(fpf1,'updatedlandings.csv') )

Sp = read.csv(file=file.path(fpf,paste('LFA34-NEFSCSpringCommercialB.csv',sep="-")))
DF = read.csv(file=file.path(fpf1,paste('DFORelf34.csv',sep="-")))
Fa = read.csv(file=file.path(fpf,paste('LFA34-NEFSCFallCommercialB.csv',sep="-")))
IL = read.csv(file=file.path(fpf1,'ILTSCommB.csv'))
c34 = read.csv(file=file.path(fpf1,'updatedlandings.csv') )

RP = list(c(1539,80.1),c(3232, 223), c(686,107),c(4574,1975))
png(file=file.path(fpf,'commb.png'),units='in',width=12,height=9,pointsize=18, res=300,type='cairo')
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

dev.off()

#Relative F

Sp = read.csv(file=file.path(fpf,paste('LFA34-NEFSCSpringCommercialB.csv',sep="-")))
DF = read.csv(file=file.path(fpf,paste('DFORelf34.csv',sep="-")))
Fa = read.csv(file=file.path(fpf,paste('LFA34-NEFSCFallCommercialB.csv',sep="-")))
IL = read.csv(file=file.path(fpf1,'ILTSCommB.csv'))
c34 = read.csv(file=file.path(fpf1,'updatedlandings.csv') )


RRs = list(.9113,10.5713,.97063,.83955)
png(file=file.path(fpf,'relf.png'),units='in',width=10,height=8,pointsize=18, res=300,type='cairo')
par(mfrow=c(2,2), mar=c(4,4,2,1), mai=c(0.5,1,0.4,0.4))

Sp = Sp[,c('yr','w.Yst','w.ci.Yst.l','w.ci.Yst.u')]
Sp  =merge(Sp,c34,by.x='yr',by.y='yr')

Sp$rL = Sp$LFA34/(Sp$w.ci.Yst.l+Sp$LFA34)
Sp$rU =Sp$LFA34/ (Sp$w.ci.Yst.u+Sp$LFA34)
Sp$rM = Sp$LFA34/(Sp$w.Yst+Sp$LFA34)

Sp[which(!is.finite(Sp[,7])),7] <- NA
Sp[which(!is.finite(Sp[,8])),8] <- NA
Sp[which(!is.finite(Sp[,9])),9] <- NA

rf = median(Sp$rM, an.rm=T)
rl = mean(subset(Sp,yr %in% 1970:1998,select=rM)[,1],na.rm=T)

rMM = rmed(Sp$yr, Sp$rM)
wMM = rmed(Sp$yr, Sp$w.Yst)

x1 = data.frame(yr=wMM$yr,wMM=wMM$x,rMM = rMM$x)

Sp = merge(Sp,x1,by=c('yr'),all.x=T)

with(Sp,plot(yr,rM,pch=16,xlab=' ',ylab='NEFSC Spring',ylim=c(0.6,1)))
with(Sp,arrows(yr,y0=rU,y1=rL, length=0))
xx = rmed(Sp$yr,Sp$rM)
with(xx,lines(yr,x,col='salmon',lwd=2))
abline(h=RRs[[1]],col='blue',lwd=2)


Fa = Fa[,c('yr','w.Yst','w.ci.Yst.l','w.ci.Yst.u')]
Fa  =merge(Fa,c34,by.x='yr',by.y='yr')

Fa$rL = Fa$LFA34/(Fa$w.ci.Yst.l)
Fa$rU =Fa$LFA34/ (Fa$w.ci.Yst.u)
Fa$rM = Fa$LFA34/(Fa$w.Yst)

Fa[which(!is.finite(Fa[,7])),7] <- NA
Fa[which(!is.finite(Fa[,8])),8] <- NA
Fa[which(!is.finite(Fa[,9])),9] <- NA

rf = median(Fa$rM, an.rm=T)
rl = mean(subset(Fa,yr %in% 1970:1998,select=rM)[,1],na.rm=T)

rMM = as.data.frame(do.call(cbind,rmed(Fa$yr, Fa$rM)))
wMM = as.data.frame(do.call(cbind,rmed(Fa$yr, Fa$w.Yst)))

names(rMM)[2] = 'rMM'
names(wMM)[2] = 'wMM'

Fa = merge(Fa,rMM, all.x=T)
Fa = merge(Fa,wMM,all.x=T)

with(Fa,plot(yr,rM,pch=16,xlab=' ',ylab='NEFSC Fall'))
with(Fa,arrows(yr,y0=rU,y1=rL, length=0))
xx = rmed(Fa$yr,Fa$rM)
with(xx,lines(yr,x,col='salmon',lwd=2))
abline(h=rl,col='blue',lwd=2)



DF = DF[,c('yr','w.Yst','w.ci.Yst.l','w.ci.Yst.u')]
DF  =merge(DF,c34,by.x='yr',by.y='yr')

DF$rL = DF$LFA34/(DF$w.ci.Yst.l+DF$LFA34)
DF$rU =DF$LFA34/ (DF$w.ci.Yst.u+DF$LFA34)
DF$rM = DF$LFA34/(DF$w.Yst+DF$LFA34)

DF[which(!is.finite(DF[,7])),7] <- NA
DF[which(!is.finite(DF[,8])),8] <- NA
DF[which(!is.finite(DF[,9])),9] <- NA

rf = median(DF$rM, an.rm=T)
rl = mean(subset(DF,yr %in% 1970:1998,select=rM)[,1],na.rm=T)

DF$rMM = rmed(DF$yr, DF$rM)[[2]]
DF$wMM = rmed(DF$yr, DF$w.Yst)[[2]]

DF$rMM = rmed(DF$yr, DF$rM)[[2]]
DF$wMM = rmed(DF$yr, DF$w.Yst)[[2]]

with(DF,plot(yr,rM,pch=16,xlab=' ',ylab='DFO RV Survey'))
with(DF,arrows(yr,y0=rU,y1=rL, length=0))
xx = rmed(DF$yr,DF$rM)
with(xx,lines(yr,x,col='salmon',lwd=2))
abline(h=rl,col='blue',lwd=2)


names(IL) = c('x','yr','w.Yst','w.ci.Yst.l','w.ci.Yst.u')

IL = IL[,c('yr','w.Yst','w.ci.Yst.l','w.ci.Yst.u')]
IL  =merge(IL,c34,by.x='yr',by.y='yr')

IL$rL = IL$LFA34/(IL$w.ci.Yst.l+IL$LFA34)
IL$rU =IL$LFA34/ (IL$w.ci.Yst.u+IL$LFA34)
IL$rM = IL$LFA34/(IL$w.Yst+IL$LFA34)

IL[which(!is.finite(IL[,7])),7] <- NA
IL[which(!is.finite(IL[,8])),8] <- NA
IL[which(!is.finite(IL[,9])),9] <- NA

rf = median(IL$rM, an.rm=T)
rl = mean(subset(IL,yr %in% 1970:1998,select=rM)[,1],na.rm=T)

IL$rMM = rmed(IL$yr, IL$rM)[[2]]
IL$wMM = rmed(IL$yr, IL$w.Yst)[[2]]

IL$rMM = rmed(IL$yr, IL$rM)[[2]]
IL$wMM = rmed(IL$yr, IL$w.Yst)[[2]]

with(IL,plot(yr,rM,pch=16,xlab=' ',ylab='ILTS',ylim=c(0.3,1)))
with(IL,arrows(yr,y0=rU,y1=rL, length=0))
xx = rmed(IL$yr,IL$rM)
with(xx,lines(yr,x,col='salmon',lwd=2))
abline(h=rl,col='blue',lwd=2)
dev.off()





png(file=file.path(fpf,'phaseplots.png'),units='in',width=12,height=9,pointsize=18, res=300,type='cairo')
par(mfrow=c(2,2), mar=c(5,4,2,1), mai=c(0.5,1,0.4,0.4),oma=c(3,3,0,0))


hcrPlot(B=Sp$wtm/1000,mF=Sp$rf,USR=RP[[1]][1]/1000,LRP=RP[[1]][2]/1000,RR=RRs[[1]],labels=c(),yrs=Sp$Label,ylim=c(0.5,1.1),area.cols=rev(gray.colors(3,start=.4,end=1)),xlab=' ',ylab='NEFSC Spring',yr.ends=F); box();
hcrPlot(B=Fa$wMM/1000,mF=Fa$rMM,USR=RP[[2]][1]/1000,LRP=RP[[2]][2]/1000,RR=RRs[[2]],labels=c(),yrs=c(rep('',nrow(Fa)-2),2022,""),area.cols=rev(gray.colors(3,start=.4,end=1)),xlab=' ',ylab='NEFSC Fall',yr.ends=F);box();
hcrPlot(B=DF$wMM/1000,mF=DF$rMM,USR=RP[[3]][1]/1000,LRP=RP[[3]][2]/1000,RR=RRs[[3]],labels=c(),yrs=c(rep('',length(DF$yr)-1),2023),ylim=c(0.5,1.1),area.cols=rev(gray.colors(3,start=.4,end=1)),xlab=' ',ylab='DFO RV Survey',yr.ends=T);box();
hcrPlot(B=IL$wMM/1000,mF=IL$rMM,USR=RP[[4]][1]/1000,LRP=RP[[4]][2]/1000,RR=RRs[[4]],labels=c(),yrs=c(rep('',length(IL$yr)-1),2023),ylim=c(0.5,1.1),area.cols=rev(gray.colors(3,start=.4,end=1)),xlab=' ',ylab='ILTS',yr.ends=T);box();

mtext("Commercial Biomass",side=1, line=1, outer=TRUE, cex=1.1)
mtext("Relative Fishing Mortality", side=2,line=1,outer=TRUE, cex=1.1, las=0)


graphics.off()

(Sp$rf[47]-RRs[[1]])/RRs[[1]]
(Fa$rMM[46]-RRs[[2]])/RRs[[2]]
(DF$rMM[47]-RRs[[3]])/RRs[[3]]
(IL$rMM[27]-RRs[[4]])/RRs[[4]]



png(file=file.path(fpf,'phaseplotsALT.png'),units='in',width=12,height=9,pointsize=18, res=300,type='cairo')
par(mfrow=c(2,2), mar=c(5,4,2,1), mai=c(0.5,1,0.4,0.4),oma=c(3,3,0,0))


hcrPlot(B=Sp$wMM/1000,mF=Sp$rMM,USR=RP[[1]][1]/1000,LRP=RP[[1]][2]/1000,RR=RRs[[1]],labels=c(),yrs=substr(Sp$yr,3,4),ylim=c(0.5,1.1),area.cols=rev(gray.colors(3,start=.4,end=1)),xlab=' ',ylab='NEFSC Spring',yr.ends=F); box();
hcrPlot(B=Fa$wMM/1000,mF=Fa$rMM,USR=RP[[2]][1]/1000,LRP=RP[[2]][2]/1000,RR=RRs[[2]],labels=c(),yrs=substr(Fa$yr,3,4),area.cols=rev(gray.colors(3,start=.4,end=1)),xlab=' ',ylab='NEFSC Fall',yr.ends=F);box();
hcrPlot(B=DF$wMM/1000,mF=DF$rMM,USR=RP[[3]][1]/1000,LRP=RP[[3]][2]/1000,RR=RRs[[3]],labels=c(),yrs=substr(DF$yr,3,4),ylim=c(0.5,1.1),area.cols=rev(gray.colors(3,start=.4,end=1)),xlab=' ',ylab='DFO RV Survey',yr.ends=F);box();
hcrPlot(B=IL$wMM/1000,mF=IL$rMM,USR=RP[[4]][1]/1000,LRP=RP[[4]][2]/1000,RR=RRs[[4]],labels=c(),yrs=substr(DF$yr,3,4),ylim=c(0.5,1.1),area.cols=rev(gray.colors(3,start=.4,end=1)),xlab=' ',ylab='ILTS',yr.ends=F);box();

mtext("Commercial Biomass",side=1, line=1, outer=TRUE, cex=1.1)
mtext("Relative Fishing Mortality", side=2,line=1,outer=TRUE, cex=1.1, las=0)


graphics.off()

##plots for FSAR

#plotting as per csasdown 4 panel plot
#add in the theme_csas
theme_csas <- function(base_size = 11, base_family = "", text_col = "grey20",
                       panel_border_col = "grey70") {
  half_line <- base_size / 2
  theme_light(base_size = base_size, base_family = "") +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks.length = unit(half_line / 2.2, "pt"),
      strip.background = element_rect(fill = NA, colour = NA),
      strip.text.x = element_text(colour = text_col),
      strip.text.y = element_text(colour = text_col),
      axis.text = element_text(colour = text_col),
      axis.title = element_text(colour = text_col),
      legend.title = element_text(colour = text_col, size = rel(0.9)),
      panel.border = element_rect(fill = NA, colour = panel_border_col, linewidth = 1),
      legend.key.size = unit(0.9, "lines"),
      legend.text = element_text(size = rel(0.7), colour = text_col),
      legend.key = element_rect(colour = NA, fill = NA),
      legend.background = element_rect(colour = NA, fill = NA),
      plot.title = element_text(colour = text_col, size = rel(1)),
      plot.subtitle = element_text(colour = text_col, size = rel(.85))
    )
}

##########################making FSAR plots

a = lobster.db('seasonal.landings')
a = subset(a,select=c(SYEAR,LFA34))
a$yr = as.numeric(substr(a$SYEAR,6,9))
write.csv(a,file=file.path(fpf1,'updatedlandings.csv') )

Sp = read.csv(file=file.path(fpf,paste('LFA34-NEFSCSpringCommercialB.csv',sep="-")))
DF = read.csv(file=file.path(fpf1,paste('DFORelf34.csv',sep="-")))
Fa = read.csv(file=file.path(fpf,paste('LFA34-NEFSCFallCommercialB.csv',sep="-")))
IL = read.csv(file=file.path(fpf1,'ILTSCommB.csv'))
c34 = read.csv(file=file.path(fpf1,'updatedlandings.csv') )

RP = list(c(1539,80.1,'NEFSC_Spring'),c(3232, 223,'NEFSC_Fall'), c(686,107,'DFO'),c(4574,1975,'ILTS'))
RP = as.data.frame(do.call(rbind,RP))
names(RP)=c('USI','LRI','source')
RP$USI = as.numeric(RP$USI)
RP$LRI = as.numeric(RP$LRI)


sp = subset(Sp,select=c(yr,w.Yst,w.ci.Yst.l,w.ci.Yst.u))
fa = subset(Fa,select=c(yr,w.Yst,w.ci.Yst.l,w.ci.Yst.u))
df = subset(DF,select=c(yr,w.Yst,w.ci.Yst.l,w.ci.Yst.u))
il = subset(IL,select=c(Year,B,lB,uB))

names(sp)=names(fa)=names(df)=names(il)
il$source='ILTS'
df$source='DFO'
fa$source='NEFSC_Fall'
sp$source='NEFSC_Spring'

al = dplyr::bind_rows(list(il,df,fa,sp))

Spr = as.data.frame(t(do.call(rbind,rmed(Sp$yr, Sp$w.Yst))))
Far = as.data.frame(t(do.call(rbind,rmed(Fa$yr, Fa$w.Yst))))
DFr = as.data.frame(t(do.call(rbind,rmed(DF$yr, DF$w.Yst))))
ILr = as.data.frame(t(do.call(rbind,rmed(IL$Year,IL$B))))
ILr$source = 'ILTS'
DFr$source='DFO'
Far$source='NEFSC_Fall'
Spr$source='NEFSC_Spring'

alr = dplyr::bind_rows(list(ILr,DFr,Far,Spr))
require(ggplot2)

#plot2
ggplot(data = al, aes(x = Year,y = B)) +
  geom_point(size=2)+
  geom_errorbar(aes(ymin = lB, ymax = uB), width = 0)+ 
  facet_wrap(~source,scales='free_y')+
  geom_line(data=alr,aes(x=yr,y=x),colour='red',lwd=1.25)+
  geom_hline(data=RP,aes(yintercept=USI),linetype='dotted',colour='blue',linewidth=1.3)+
  geom_hline(data=RP,aes(yintercept=LRI),linetype='dashed',colour='green',linewidth=1.3)+
      labs(x = "Year", y = "Commercial Biomass (t)") +
  theme_csas(base_size = 14)

#plot1

#landings and effort
a = lobster.db('process.logs')
a = subset(a,SYEAR %in% 2004:2024& LFA %in% '34') 

aa = split(a,f=list(a$LFA,a$SYEAR))
aa = rm.from.list(aa)
cpue.lst<-list()
#by time
for(i in 1:length(aa)){
  tmp<-aa[[i]]
  tmp = tmp[,c('DATE_FISHED','WEIGHT_KG','NUM_OF_TRAPS')]
  names(tmp)<-c('time','catch','effort')
  tmp$date<-as.Date(tmp$time)
  first.day<-min(tmp$date)
  tmp$time<-julian(tmp$date,origin=first.day-1)
  tmp = tmp[order(tmp$time),]
  tmp$time = ceiling(tmp$time) #convert to week of season
  g<-as.data.frame(biasCorrCPUE(tmp,by.time=F))
  g$lfa=unique(aa[[i]]$LFA)
  g$yr = unique(aa[[i]]$SYEAR)
  g = t(g)[,1]
  cpue.lst[[i]] <- g
}

cc =as.data.frame(do.call(rbind,cpue.lst))
cc$CPUE = as.numeric(cc$`biasCorrCPUE(tmp, by.time = F)`)
cc$yr = as.numeric(cc$yr)
cp = as.data.frame(do.call(cbind,rmed(cc$yr,cc$CPUE)))
cp$x = as.numeric(cp$x)
cp$yr = as.numeric(cp$yr)
ccp = cc[nrow(cc),]
ccf = cc[-nrow(cc),]
g1a <- ggplot(data = ccf, aes(x = yr,y = CPUE)) +
  geom_point(size=2)+
  geom_point(data=ccp,aes(x=yr,y=CPUE),colour='grey66',shape=17,size=3)+
  geom_line(data=cp,aes(x=yr,y=x),colour='gray45',lwd=1.25)+
 labs(x = "Year", y = "CPUE") +
  theme_csas(base_size = 12)

g1a.fr <- ggplot(data = ccf, aes(x = yr,y = CPUE)) +
  geom_point(size=2)+
  geom_point(data=ccp,aes(x=yr,y=CPUE),colour='grey66',shape=17,size=3)+
  geom_line(data=cp,aes(x=yr,y=x),colour='gray45',lwd=1.25)+
  theme_csas(base_size = 12)
  labs(x = "Année", y = "CPUE") +


a=lobster.db('seasonal.landings')
a = subset(a,select=c(SYEAR,LFA34))
a = subset(a,!is.na(LFA34))
a$yr= as.numeric(substr(a$SYEAR,6,9))
aaa = a
ef = merge(cc,aaa)
ef$Effort = ef$LFA34/(ef$CPUE)

aap = aaa[nrow(aaa),]
aaa = aaa[1:(nrow(aaa)-1),]

efp = ef[nrow(ef),]
ef = ef[-nrow(ef),]
ymax=30000
scaleright = max(ef$Effort)/ymax
g1 <- ggplot(data = subset(aaa,yr>1990), aes(x = yr,y=LFA34)) +
  geom_bar(stat='identity',fill='black') +
  geom_bar(data=aap,aes(x=yr,y=LFA34),stat='identity',fill='gray66') +
  geom_point(data=efp,aes(x=yr,y=Effort/scaleright),colour='grey66',shape=17,size=2)+
  geom_line(data=ef,aes(x=yr,y=Effort/scaleright),colour='black',lwd=1,linetype='dashed')+
  geom_point(data=ef,aes(x=yr,y=Effort/scaleright),colour='black',shape=16,size=1)+
  scale_y_continuous(name='Landings', sec.axis= sec_axis(~.*scaleright, name= 'Effort',breaks = seq(0,22000,by=5000)))+
  labs(x = "Year") +
  theme_csas(base_size=12)

g1.fr <- ggplot(data = subset(aaa,yr>1990), aes(x = yr,y=LFA34)) +
  geom_bar(stat='identity',fill='black') +
  geom_bar(data=aap,aes(x=yr,y=LFA34),stat='identity',fill='gray66') +
  geom_point(data=efp,aes(x=yr,y=Effort/scaleright),colour='grey66',shape=17,size=2)+
  geom_line(data=ef,aes(x=yr,y=Effort/scaleright),colour='black',lwd=1,linetype='dashed')+
  geom_point(data=ef,aes(x=yr,y=Effort/scaleright),colour='black',shape=16,size=1)+
  labs(x = "Year") +
  theme_csas(base_size=12)
  scale_y_continuous(name='Débarquements', sec.axis= sec_axis(~.*scaleright, name= 'Effort',breaks = seq(0,22000,by=5000)))+

g2 = ggplot(data = subset(al,source=='ILTS'), aes(x = Year,y = B)) +
  geom_point(size=2)+
  geom_errorbar(aes(ymin = lB, ymax = uB), width = 0)+ 
#  facet_wrap(~source,scales='free_y')+
  geom_line(data=subset(alr,source=='ILTS'),aes(x=yr,y=x),colour='grey66',lwd=1.25)+
  geom_hline(data=subset(RP,source=='ILTS'),aes(yintercept=USI),linetype='dotted',colour='grey66',linewidth=1.3)+
  geom_hline(data=subset(RP,source=='ILTS'),aes(yintercept=LRI),linetype='dashed',colour='grey66',linewidth=1.3)+
  labs(x = "Year", y = "ILTS Commercial Biomass") +
  theme_csas(base_size = 12)

  g2.fr = ggplot(data = subset(al,source=='ILTS'), aes(x = Year,y = B)) +
    geom_point(size=2)+
    geom_errorbar(aes(ymin = lB, ymax = uB), width = 0)+ 
    #  facet_wrap(~source,scales='free_y')+
    geom_line(data=subset(alr,source=='ILTS'),aes(x=yr,y=x),colour='grey66',lwd=1.25)+
    geom_hline(data=subset(RP,source=='ILTS'),aes(yintercept=USI),linetype='dotted',colour='grey66',linewidth=1.3)+
    geom_hline(data=subset(RP,source=='ILTS'),aes(yintercept=LRI),linetype='dashed',colour='grey66',linewidth=1.3)+
    theme_csas(base_size = 12)
    labs(x = "Année", y = "Biomasse commerciale ILTS") +

g3 = ggplot(data = subset(al,source=='DFO'), aes(x = Year,y = B)) +
  geom_point(size=2)+
  geom_errorbar(aes(ymin = lB, ymax = uB), width = 0)+ 
  #  facet_wrap(~source,scales='free_y')+
  geom_line(data=subset(alr,source=='DFO'),aes(x=yr,y=x),colour='grey66',lwd=1.25)+
  geom_hline(data=subset(RP,source=='DFO'),aes(yintercept=USI),linetype='dotted',colour='grey66',linewidth=1.3)+
  geom_hline(data=subset(RP,source=='DFO'),aes(yintercept=LRI),linetype='dashed',colour='grey66',linewidth=1.3)+
  
  labs(x = "Year", y = "DFO Commercial Biomass") +
  theme_csas(base_size = 12)

    g3.fr = ggplot(data = subset(al,source=='DFO'), aes(x = Year,y = B)) +
      geom_point(size=2)+
      geom_errorbar(aes(ymin = lB, ymax = uB), width = 0)+ 
      #  facet_wrap(~source,scales='free_y')+
      geom_line(data=subset(alr,source=='DFO'),aes(x=yr,y=x),colour='grey66',lwd=1.25)+
      geom_hline(data=subset(RP,source=='DFO'),aes(yintercept=USI),linetype='dotted',colour='grey66',linewidth=1.3)+
      geom_hline(data=subset(RP,source=='DFO'),aes(yintercept=LRI),linetype='dashed',colour='grey66',linewidth=1.3)+
      
      theme_csas(base_size = 12)
      labs(x = "Année", y = "Biomasse commerciale MPO") +

       
cowplot::plot_grid(g1, g1a, g2, g3, ncol = 2, labels = "AUTO", align = "hv") #drop into document from 

#Francais labels on figure. Will save to wd by default. You can drop into the CSAS folder from there
png(filename=file.path(getwd(), "fsrpanel plots.figure1.french.png"), width=1200, height=900, res=125) #modify file path as required
cowplot::plot_grid(g1.fr, g1a.fr, g2.fr, g3.fr, ncol = 2, labels = "AUTO", align = "hv") 
print(paste0("french figure can be found here:", file.path(getwd(), "fsr.figure1.french.png"))) #modify file path as required
dev.off()


##################################################################
#############PHASE PLOTS IN GG

Sp = read.csv(file=file.path(fpf,paste('LFA34-NEFSCSpringCommercialB.csv',sep="-")))
DF = read.csv(file=file.path(fpf,paste('DFORelf34.csv',sep="-")))
Fa = read.csv(file=file.path(fpf,paste('LFA34-NEFSCFallCommercialB.csv',sep="-")))
IL = read.csv(file=file.path(fpf1,'ILTSCommB.csv'))
c34 = read.csv(file=file.path(fpf1,'updatedlandings.csv') )



RP$RI = c(.9113,10.5713,.97063,.83955)
Sp = Sp[,c('yr','w.Yst','w.ci.Yst.l','w.ci.Yst.u')]

Sp  =merge(Sp,c34)

Sp$rL = Sp$LFA34/(Sp$w.ci.Yst.l+Sp$LFA34)
Sp$rU =Sp$LFA34/ (Sp$w.ci.Yst.u+Sp$LFA34)
Sp$rM = Sp$LFA34/(Sp$w.Yst+Sp$LFA34)

Sp[which(!is.finite(Sp[,7])),7] <- NA
Sp[which(!is.finite(Sp[,8])),8] <- NA
Sp[which(!is.finite(Sp[,9])),9] <- NA

rf = median(Sp$rM, an.rm=T)
rl = mean(subset(Sp,yr %in% 1970:1998,select=rM)[,1],na.rm=T)

rMM = rmed(Sp$yr, Sp$rM)
wMM = rmed(Sp$yr, Sp$w.Yst)

x1 = data.frame(yr=wMM$yr,wMM=wMM$x,rMM = rMM$x)

Sp = merge(Sp,x1,by=c('yr'),all.x=T)

Fa = Fa[,c('yr','w.Yst','w.ci.Yst.l','w.ci.Yst.u')]
Fa  =merge(Fa,c34,by.x='yr',by.y='yr')

Fa$rL = Fa$LFA34/(Fa$w.ci.Yst.l)
Fa$rU =Fa$LFA34/ (Fa$w.ci.Yst.u)
Fa$rM = Fa$LFA34/(Fa$w.Yst)

Fa[which(!is.finite(Fa[,7])),7] <- NA
Fa[which(!is.finite(Fa[,8])),8] <- NA
Fa[which(!is.finite(Fa[,9])),9] <- NA

rf = median(Fa$rM, an.rm=T)
rl = mean(subset(Fa,yr %in% 1970:1998,select=rM)[,1],na.rm=T)

rMM = as.data.frame(do.call(cbind,rmed(Fa$yr, Fa$rM)))
wMM = as.data.frame(do.call(cbind,rmed(Fa$yr, Fa$w.Yst)))

names(rMM)[2] = 'rMM'
names(wMM)[2] = 'wMM'

Fa = merge(Fa,rMM, all.x=T)
Fa = merge(Fa,wMM,all.x=T)

DF = DF[,c('yr','w.Yst','w.ci.Yst.l','w.ci.Yst.u')]
DF  =merge(DF,c34,by.x='yr',by.y='yr')

DF$rL = DF$LFA34/(DF$w.ci.Yst.l+DF$LFA34)
DF$rU =DF$LFA34/ (DF$w.ci.Yst.u+DF$LFA34)
DF$rM = DF$LFA34/(DF$w.Yst+DF$LFA34)

DF[which(!is.finite(DF[,7])),7] <- NA
DF[which(!is.finite(DF[,8])),8] <- NA
DF[which(!is.finite(DF[,9])),9] <- NA

rf = median(DF$rM, an.rm=T)
rl = mean(subset(DF,yr %in% 1970:1998,select=rM)[,1],na.rm=T)

DF$rMM = rmed(DF$yr, DF$rM)[[2]]
DF$wMM = rmed(DF$yr, DF$w.Yst)[[2]]

DF$rMM = rmed(DF$yr, DF$rM)[[2]]
DF$wMM = rmed(DF$yr, DF$w.Yst)[[2]]

names(IL) = c('x','yr','w.Yst','w.ci.Yst.l','w.ci.Yst.u')

IL = IL[,c('yr','w.Yst','w.ci.Yst.l','w.ci.Yst.u')]
IL  =merge(IL,c34,by.x='yr',by.y='yr')

IL$rL = IL$LFA34/(IL$w.ci.Yst.l+IL$LFA34)
IL$rU =IL$LFA34/ (IL$w.ci.Yst.u+IL$LFA34)
IL$rM = IL$LFA34/(IL$w.Yst+IL$LFA34)

IL[which(!is.finite(IL[,7])),7] <- NA
IL[which(!is.finite(IL[,8])),8] <- NA
IL[which(!is.finite(IL[,9])),9] <- NA

rf = median(IL$rM, an.rm=T)
rl = mean(subset(IL,yr %in% 1970:1998,select=rM)[,1],na.rm=T)

IL$rMM = rmed(IL$yr, IL$rM)[[2]]
IL$wMM = rmed(IL$yr, IL$w.Yst)[[2]]

IL$rMM = rmed(IL$yr, IL$rM)[[2]]
IL$wMM = rmed(IL$yr, IL$w.Yst)[[2]]


###
spa = subset(Sp,!is.na(w.Yst))
lareas = data.frame(LRI=c(0,RP$LRI[which(RP$source=='NEFSC_Spring')]),ys = rep(max(spa$rMM)*1.1,2))
uareas = data.frame(USI=c(RP$LRI[which(RP$source=='NEFSC_Spring')],RP$USI[which(RP$source=='NEFSC_Spring')]),ys = rep(max(spa$rMM)*1.1,2))
rline = data.frame(x=c(RP$USI[which(RP$source=='NEFSC_Spring')],max(spa$wMM)*1.1),ys = rep(RP$RI[which(RP$source=='NEFSC_Spring')],2))

phase.panels=function(French=FALSE) {
  
  xlb="Commercial Biomass"
  ylb="Relative Fishing Mortality"
  
  if(French){
    xlb="Biomasse commerciale"
    ylb="Mortalité par pêche relative" 
  }
  
   g1 = ggplot(spa,aes(x=wMM,y=rMM))+
   geom_point()+
   geom_path()+
   labs(x=xlb,y=ylb)+
   geom_text(data=subset(spa,yr %in% c(min(yr),max(yr))),aes(label=yr,x=wMM,y=rMM),position = position_nudge(y=c(-.023,.023)))+
   geom_area(data=lareas,aes(x=LRI,y=ys),fill='grey30',alpha=.25)+
   geom_area(data=uareas,aes(x=USI,y=ys),fill='grey70',alpha=.25)+
   coord_cartesian(ylim=c(0.5,1.1))+
   theme_csas()
 
 Fpa = subset(Fa,!is.na(w.Yst))
 lareas = data.frame(LRI=c(0,RP$LRI[which(RP$source=='NEFSC_Fall')]),ys = rep(max(Fpa$rMM)*1.1,2))
 uareas = data.frame(USI=c(RP$LRI[which(RP$source=='NEFSC_Fall')],RP$USI[which(RP$source=='NEFSC_Fall')]),ys = rep(max(Fpa$rMM)*1.1,2))
 rline = data.frame(x=c(RP$USI[which(RP$source=='NEFSC_Fall')],max(Fpa$wMM)*1.1),ys = rep(RP$RI[which(RP$source=='NEFSC_Fall')],2))
 
 g2 = ggplot(Fpa,aes(x=wMM,y=rMM))+
   geom_point()+
   geom_path()+
   labs(x=xlb,y=ylb)+
   geom_text(data=subset(Fpa,yr %in% c(min(yr),max(yr))),aes(label=yr,x=wMM,y=rMM),position = position_nudge(y=c(-.023,.7),x=c(-300,0)))+
   geom_area(data=lareas,aes(x=LRI,y=ys),fill='grey30',alpha=.25)+
   geom_area(data=uareas,aes(x=USI,y=ys),fill='grey70',alpha=.25)+
   geom_segment(data=rline,aes(x=x[1],xend=x[2],y=ys[1],yend=ys[2]),linetype='dashed') +
   coord_cartesian(ylim=c(0,30))+
   theme_csas()
 
 
 DFa = subset(DF,!is.na(w.Yst))
 lareas = data.frame(LRI=c(0,RP$LRI[which(RP$source=='DFO')]),ys = rep(max(DFa$rMM)*1.1,2))
 uareas = data.frame(USI=c(RP$LRI[which(RP$source=='DFO')],RP$USI[which(RP$source=='DFO')]),ys = rep(max(DFa$rMM)*1.1,2))
 rline = data.frame(x=c(RP$USI[which(RP$source=='DFO')],max(DFa$wMM)*1.1),ys = rep(RP$RI[which(RP$source=='DFO')],2))
 
g3 = ggplot(DFa,aes(x=wMM,y=rMM))+
   geom_point()+
   geom_path()+
  labs(x=xlb,y=ylb)+
   geom_text(data=subset(DFa,yr %in% c(min(yr),max(yr))),aes(label=yr,x=wMM,y=rMM),position = position_nudge(y=c(0.01,-.01),x=c(0,0)))+
   geom_area(data=lareas,aes(x=LRI,y=ys),fill='grey30',alpha=.25)+
   geom_area(data=uareas,aes(x=USI,y=ys),fill='grey70',alpha=.25)+
   geom_segment(data=rline,aes(x=x[1],xend=x[2],y=ys[1],yend=ys[2]),linetype='dashed') +
   coord_cartesian(ylim=c(0.5,1.1))+
   theme_csas()
 

ILa = subset(IL,!is.na(w.Yst))
lareas = data.frame(LRI=c(0,RP$LRI[which(RP$source=='ILTS')]),ys = rep(max(ILa$rMM)*1.1,2))
uareas = data.frame(USI=c(RP$LRI[which(RP$source=='ILTS')],RP$USI[which(RP$source=='ILTS')]),ys = rep(max(ILa$rMM)*1.1,2))
rline = data.frame(x=c(RP$USI[which(RP$source=='ILTS')],max(ILa$wMM)*1.1),ys = rep(RP$RI[which(RP$source=='ILTS')],2))

g4 = ggplot(ILa,aes(x=wMM,y=rMM))+
  geom_point()+
  geom_path()+
  labs(x=xlb,y=ylb)+
  geom_text(data=subset(ILa,yr %in% c(min(yr),max(yr))),aes(label=yr,x=wMM,y=rMM),position = position_nudge(y=c(0.01,-.01),x=c(0,0)))+
  geom_area(data=lareas,aes(x=LRI,y=ys),fill='grey30',alpha=.25)+
  geom_area(data=uareas,aes(x=USI,y=ys),fill='grey70',alpha=.25)+
  geom_segment(data=rline,aes(x=x[1],xend=x[2],y=ys[1],yend=ys[2]),linetype='dashed') +
  coord_cartesian(ylim=c(0.5,.95))+
  theme_csas()

cowplot::plot_grid(g1, g2, g3, g4, ncol = 2, labels = "AUTO", align = "hv")
}

phase.panels () #Save into document

#Francais labels on figure. Will save to wd by default. You can drop into the CSAS folder from there
png(filename=file.path(getwd(), "fsr.phase.panel.french.png"), width=1200, height=900, res=125) #modify file path as required
phase.panels(French=TRUE)
print(paste0("french figure can be found here:", file.path(getwd(), "fsr.phase.panel.french.png"))) #modify file path as required
dev.off()
