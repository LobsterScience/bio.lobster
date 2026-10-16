
require(devtools)
require(bio.lobster)
p = bio.lobster::load.environment()
p$libs = NULL
require(PBSmapping)
require(bio.lobster)
require(bio.utilities)
require(bio.survey)
la()
ff = "LFA34Update2025"
dadir = file.path(project.figuredirectory('bio.lobster'),ff)
fpf1 = file.path(project.figuredirectory('bio.lobster'),"LFA34Update")



##landings

####LFA 34

require(bio.lobster)
require(PBSmapping)

a = lobster.db('annual.landings')
b = lobster.db('seasonal.landings')
d = lobster.db('historic.landings')
l34 = c("YARMOUTH","DIGBY")

d = subset(d, !is.na(LFA))
d = aggregate(LANDINGS_MT~YEAR+LFA,data=d,FUN=sum)
d = subset(d, YEAR<1947)

names(d) = c('YR','LFA','LAND')

b$YR = substr(b$SYEAR,6,9)
a = subset(a,YR<1976)
b = subset(b,YR>1975 & YR<=2025)
a34 = a[,c('YR','LFA34')]
b34 = b[,c('YR','LFA34')]
c34 = rbind(a34,b34)
c34 = subset(c34,YR>1969)
c34$yr = c34$YR
write.csv(c34,file=file.path(fpf1,'updatedlandings.csv') )
##

######DFO
df =  read.csv(file.path(dadir,'LFA34-DFOtotalabund.csv'))
df2 = read.csv(file.path(dadir,'LFA34-DFOCommercialB.csv'))
df$X = df2$X = NULL
df = subset(df,yr<1999)
df2 = subset(df2,yr>1998)
df = as.data.frame(rbind(df,df2))
df = df[,c('yr','w.Yst','w.ci.Yst.l','w.ci.Yst.u')] #proportion of total weight that is commercial
df$w.Yst[which(df$yr<1999)] <- df$w.Yst[which(df$yr<1999)]*0.71
df$w.ci.Yst.l[which(df$yr<1999)] <- df$w.ci.Yst.l[which(df$yr<1999)]*0.71
df$w.ci.Yst.u[which(df$yr<1999)] <- df$w.ci.Yst.u[which(df$yr<1999)]*0.71
#write.csv(df,file=file.path(dadir,'LFA34-DFOCommercialB.csv'))
aout = df
png(file.path(dadir,'LFA34-DFOCommercialB.png'))

   with(df,plot(yr,w.Yst/1000,pch=1,xlab='Year',ylab='Commerical Biomass (t x000)',ylim=c(0,9)))
   with(df,arrows(yr,y0=w.ci.Yst.u/1000,y1=w.ci.Yst.l/1000, length=0))
   with(subset(df,yr>1998),points(yr,w.Yst/1000,pch=16))
   xx = rmed(df$yr,df$w.Yst/1000)
   xx = as.data.frame(do.call(cbind,xx))
   with(subset(xx,yr<1999),lines(yr,x,col='salmon',lwd=2))
   with(subset(xx,yr>1998),lines(yr,x,col='salmon',lwd=3))
  
      lb = median(subset(df,yr %in% 1970:1999,select=w.Yst)[,1],na.rm=T)/1000
              ub = median(subset(df,yr %in% 2000:2018,select=w.Yst)[,1],na.rm=T)/1000 * 0.4
              nub = median(subset(df,yr %in% 1970:2018,select=w.Yst)[,1],na.rm=T)/1000
              llb = aout$w.Yst[which(df$w.Yst>0)]
              llb = median(sort(llb)[1:5])/1000
abline(h = ub,lwd=2,col='green')
abline(h = llb,lwd=2,col='blue')
dev.off()
df = df[,c('yr','w.Yst','w.ci.Yst.l','w.ci.Yst.u')]
df  =merge(df,c34)

df$rL = df$LFA34/(df$w.ci.Yst.l+df$LFA34)
df$rU =df$LFA34/ (df$w.ci.Yst.u+df$LFA34)
df$rM = df$LFA34/(df$w.Yst+df$LFA34)
 df[which(!is.finite(df[,7])),7] <- NA
 df[which(!is.finite(df[,8])),8] <- NA
 df[which(!is.finite(df[,9])),9] <- NA

rf = median(df$rM, an.rm=T)
rl = median(subset(df,yr %in% 1970:1998,select=rM)[,1])

df$rMM = rmed(df$yr, df$rM)[[2]]
df$wMM = rmed(df$yr, df$w.Yst)[[2]]
 
df$rMM = rmed(df$yr, df$rM)[[2]]
df$wMM = rmed(df$yr, df$w.Yst)[[2]]
png(file.path(fpf1,'LFA34-DFORelativeF.png'))
   with(df,plot(yr,rM,pch=16,xlab='Year',ylab='Relative F',ylim=c(0.6,1)))
   with(df,arrows(yr,y0=rU,y1=rL, length=0))
   xx = rmed(df$yr,df$rM)
   with(xx,lines(yr,x,col='salmon',lwd=2))
   abline(h=rl,col='blue',lwd=2)
dev.off()   
#HCR plots
       hcrPlot(B=df$wMM/1000,mF=df$rMM,USR=ub,LRP=llb,RR=rl,labels=c(),yrs=c(rep('',length(df$yr)-3),c(2020,2022,2023)),ylim=c(0.5,1.1),area.cols=rev(gray.colors(3,start=.4,end=1)),xlab='Commercial Biomass',ylab='Relative Fishing Mortality')
box()
 
savePlot(file.path(fpf1,'HCRDFo.png'))


######ILTS



df = read.csv(file.path(fpf1,'ILTSCommB.csv'))
names(df) = c('x','yr','w.Yst','w.ci.Yst.l','w.ci.Yst.u')



aout = df
png(file.path(fpf1,'LFA34-ILTSCommercialB.png'))
   with(df,plot(yr,w.Yst/1000,pch=16,xlab='Year',ylab='Commerical Biomass',ylim=c(0,30)))
   with(df,arrows(yr,y0=w.ci.Yst.u/1000,y1=w.ci.Yst.l/1000, length=0))
  xx = rmed(df$yr,df$w.Yst/1000)
   xx = as.data.frame(do.call(cbind,xx))
   with(xx,lines(yr,x,col='salmon',lwd=3))
   
      lb = median(subset(aout,yr %in% 1996:1999,select=w.Yst)[,1],na.rm=T)/1000
              ub = median(subset(aout,yr %in% 2000:2018,select=w.Yst)[,1],na.rm=T)/1000 * 0.4
              nub = median(subset(aout,yr %in% 1996:2018,select=w.Yst)[,1],na.rm=T)/1000
              llb = aout$w.Yst[which(aout$w.Yst>0)]
              llb = median(sort(llb)[1:4])/1000
abline(h = ub,lwd=2,col='green')
abline(h = llb,lwd=2,col='blue')
dev.off()
savePlot(file.path(fpf1,'LFA34-ILTSCommercialB.png'))

df  =merge(df,c34)

df$rL = df$LFA34/(df$w.ci.Yst.l+df$LFA34)
df$rU =df$LFA34/ (df$w.ci.Yst.u+df$LFA34)
df$rM = df$LFA34/(df$w.Yst+df$LFA34)
 df[which(!is.finite(df[,7])),7] <- NA
 df[which(!is.finite(df[,8])),8] <- NA
 df[which(!is.finite(df[,9])),9] <- NA

rf = median(df$rM, an.rm=T)
rl = median(subset(df,yr %in% 1970:1998,select=rM)[,1])


  png(file=file.path(fpf1,'ILTSRelFREFS.png'),units='in',width=15,height=12,pointsize=18, res=300,type='cairo')
    plot(df$yr,df$rM,type='p',pch=16,col='black',xlab='Year',ylab='Relative F')
    arrows(df$yr,y0 = df$rL,y1 = df$rU,length=0)
    with(rmed(df$yr,df$rM),lines(yr,x,col='salmon',lwd=3))
    abline(h=rl , col='blue',lwd=2)
dev.off()


df$rMM = rmed(df$yr, df$rM)[[2]]
df$wMM = rmed(df$yr, df$w.Yst)[[2]]

#HCR plots
         hcrPlot(B=df$wMM/1000,mF=df$rMM,USR=ub,LRP=llb,RR=rl,labels=c(),yrs=c(rep('',length(df$yr)-3),2020:2022),ylim=c(0.5,1.1),area.cols=rev(gray.colors(3,start=.4,end=1)),xlab='Commercial Biomass',ylab='Relative Fishing Mortality')
box()
 
savePlot(file.path(fpf1,'HCRILTS.png'))
