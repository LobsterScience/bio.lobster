

require(bio.lobster)
p = bio.lobster::load.environment()
require(bio.polygons)
p$libs = NULL
require(PBSmapping)
require(bio.lobster)
require(bio.utilities)
la()

##landings

####LFA 34

require(bio.lobster)
require(PBSmapping)

a = lobster.db('annual.landings')
b = lobster.db('seasonal.landings')
d = lobster.db('historic.landings')
l34 = c("YARMOUTH","DIGBY")
l35 = c("KINGS","ANNAPOLIS", "COLCHESTER" , "CUMBERLAND")
l36 = c('ALBERT','SAINT JOHN','CHARLOTTE')
l38 = c('CHARLOTTE')

d$LFA = ifelse(d$COUNTY %in% l34, 'LFA34',NA)
d = subset(d, !is.na(LFA))
d = aggregate(LANDINGS_MT~SYEAR+LFA,data=d,FUN=sum)
d = subset(d, SYEAR<1947)

names(d) = c('YR','LFA','LAND')

b$YR = substr(b$SYEAR,6,9)
a = subset(a,YR<1976)
b = subset(b,YR>1975 & YR<2019)
a34 = a[,c('YR','LFA34')]
b34 = b[,c('YR','LFA34')]
c34 = rbind(a34,b34)
c34 = subset(c34,YR>1969)
c34$yr = c34$YR

##

fpf1 = file.path(project.figuredirectory('bio.lobster'),"LFA3438Framework2019")

                p$reweight.strata = T
                p$years.to.estimate = c(1969:2018)
                p$length.based = T
                p$size.class= c(83,300)
                p$by.sex = F
                p$sex = c(1,2) # male female berried c(1,2,3)
                p$bootstrapped.ci=T
                p$strata.files.return=F
                p$strata.efficiencies=F
                p$clusters = c( rep( "localhost", 7) )
                p$season =c('spring')
                        p$define.by.polygons = T
                        p$lobster.subunits=F
                        p$area ='LFA34'
                        p$return.both = NULL
                      p = make.list(list(yrs=p$years.to.estimate),Y=p)
                    
                            #Figure
                              p$add.reference.lines = F
                              p$time.series.start.year = p$years.to.estimate[1]
                              p$time.series.end.year = p$years.to.estimate[length(p$years.to.estimate)]
                              p$metric = 'numbers' #weights
                              p$measure = 'stratified.mean' #'stratified.total'
                              p$figure.title = ""
                              p$reference.measure = 'median' # mean, geomean
          
                  aout= nefsc.analysis(DS='stratified.estimates.redo',p=p)

#ao is full time series of biomasses
      require(bcp)
      ao = aout[,c('yr','w.Yst')]
      h = subset(ao,w.Yst>0) #need to remove the zeros for bcp to function correctly
      b = bcp(log(h$w.Yst),w0 = 0.2, p0 = 0.05)
      plot(b,xaxlab=h$yr,xlab='Year')
      savePlot(file.path(fpf1,'BCPNSprL34.png'))

      cbind(h$yr,b$posterior.prob)

#change in 2000



                              #Figure
                              p$add.reference.lines = F
                              p$time.series.start.year = p$years.to.estimate[1]
                              p$time.series.end.year = p$years.to.estimate[length(p$years.to.estimate)]
                              p$metric = 'weights' #weights
                              p$measure = 'stratified.total' #'stratified.total'
                              p$figure.title = ""
                              p$reference.measure = 'median' # mean, geomean
                              p$file.name = file.path('LFA3438Framework2019',paste('LFA34','NEFSCSpringrestratifiedweightcommercial.png',sep=""))
                                p$legend = FALSE
                                p$running.median = T
                                p$running.length = 3
                                p$running.mean = F #can only have rmedian or rmean
                               p$error.polygon=F
                              p$error.bars=T

                       ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p, save=F)
        
      
          #based on bcp              
              lb = median(subset(aout,yr %in% 1970:1998,select=w.Yst)[,1])/1000
              ub = median(subset(aout,yr %in% 1999:2018,select=w.Yst)[,1])/1000 * 0.4
              nub = median(subset(aout,yr %in% 1970:2018,select=w.Yst)[,1])/1000
              llb = aout$w.Yst[which(aout$w.Yst>0)]
              llb = median(sort(llb)[1:5])/1000

      
              abline(h=llb,col='orange',lwd=2)
              abline(h=lb,col='blue',lwd=2)
              abline(h=ub,col='green',lwd=2)
              abline(h=nub,col='purple',lwd=2)

              savePlot(file.path(fpf1,'NEFSCspringReferencePoints.png'))

########
#relative F 
df = aout
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
  png(file=file.path(fpf1,'NEFSCSpringRelFREFS.png'),units='in',width=15,height=12,pointsize=18, res=300,type='cairo')
    plot(df$yr,df$rM,type='p',pch=16,col='black',xlab='Year',ylab='Relative F')
    arrows(df$yr,y0 = df$rL,y1 = df$rU,length=0)
    with(rmed(df$yr,df$rM),lines(yr,x,col='salmon',lwd=3))
    abline(h=rf, col='purple',lwd=2)
    abline(h=rl , col='blue',lwd=2)
dev.off()


df$rMM = rmed(df$yr, df$rM)[[2]]
df$wMM = rmed(df$yr, df$w.Yst)[[2]]

#HCR plots
    
hcrPlot(B=df$wMM/1000,mF=df$rMM,USR=ub,LRP=llb,RR=rl,yrs=c(rep('',length(df$yr)-3),2016:2018),ylim=c(0,1.1))
savePlot(file.path(fpf1,'HCRNSpr.png'))


####################
###NEFSC Fall
#####################
   
                p$reweight.strata = T
                p$years.to.estimate = c(1969:2018)
                p$length.based = T
                p$size.class= c(83,300)
                p$by.sex = F
                p$sex = c(1,2) # male female berried c(1,2,3)
                p$bootstrapped.ci=T
                p$strata.files.return=F
                p$strata.efficiencies=F
                p$clusters = c( rep( "localhost", 7) )
                p$season =c('fall')
                        p$define.by.polygons = T
                        p$lobster.subunits=F
                        p$area ='LFA34'
                        p$return.both = NULL
                      p = make.list(list(yrs=p$years.to.estimate),Y=p)
                    
                  aout= nefsc.analysis(DS='stratified.estimates.redo',p=p)

#ao is full time series of biomasses
      require(bcp)
      ao = aout[,c('yr','w.Yst')]
      h = subset(ao,w.Yst>0) #need to remove the zeros for bcp to function correctly
      b = bcp(log(h$w.Yst),w0 = 0.2, p0 = 0.05)
      plot(b,xaxlab=h$yr,xlab='Year')
      savePlot(file.path(fpf1,'BCPNfalL34.png'))

      cbind(h$yr,b$posterior.prob)

#change in 1998



                              #Figure
                              p$add.reference.lines = F
                              p$time.series.start.year = p$years.to.estimate[1]
                              p$time.series.end.year = p$years.to.estimate[length(p$years.to.estimate)]
                              p$metric = 'weights' #weights
                              p$measure = 'stratified.total' #'stratified.total'
                              p$figure.title = ""
                              p$reference.measure = 'median' # mean, geomean
                              p$file.name = file.path('LFA3438Framework2019',paste('LFA34','NEFSCSpringrestratifiedweightcommercial.png',sep=""))
                                p$legend = FALSE
                                p$running.median = T
                                p$running.length = 3
                                p$running.mean = F #can only have rmedian or rmean
                               p$error.polygon=F
                              p$error.bars=T

                       ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p, save=F)
        
      
          #based on bcp              
              lb = median(subset(aout,yr %in% 1970:1998,select=w.Yst)[,1],na.rm=T)/1000
              ub = median(subset(aout,yr %in% 1999:2018,select=w.Yst)[,1],na.rm=T)/1000 * 0.4
              nub = median(subset(aout,yr %in% 1970:2018,select=w.Yst)[,1],na.rm=T)/1000
              llb = aout$w.Yst[which(aout$w.Yst>0)]
              llb = median(sort(llb)[1:5])/1000

      
              abline(h=llb,col='orange',lwd=2)
              abline(h=lb,col='blue',lwd=2)
              abline(h=ub,col='green',lwd=2)
              abline(h=nub,col='purple',lwd=2)

              savePlot(file.path(fpf1,'NEFSCfallReferencePoints.png'))
df = aout
df = df[,c('yr','w.Yst','w.ci.Yst.l','w.ci.Yst.u')]
df  =merge(df,c34)
df = subset(df,yr<2018)
df$rL = df$LFA34/(df$w.ci.Yst.l)
df$rU =df$LFA34/ (df$w.ci.Yst.u)
df$rM = df$LFA34/(df$w.Yst)
 df[which(!is.finite(df[,7])),7] <- NA
 df[which(!is.finite(df[,8])),8] <- NA
 df[which(!is.finite(df[,9])),9] <- NA

rf = median(df$rM, na.rm=T)
rl = median(subset(df,yr %in% 1970:1998,select=rM)[,1],na.rm=T)

rrr = rmed(df$yr, df$rM)
rrr = as.data.frame(cbind(rrr$yr, rrr$x))
names(rrr) = c('yr','rMM')
df = merge(df,rrr,by='yr',all.x=T)
df$wMM = rmed(df$yr, df$w.Yst)[[2]]

  png(file=file.path(fpf1,'NEFSCFallRelFREFS.png'),units='in',width=15,height=12,pointsize=18, res=300,type='cairo')
    plot(df$yr,df$rM,type='p',pch=16,col='black',xlab='Year',ylab='Relative F')
    arrows(df$yr,y0 = df$rL,y1 = df$rU,length=0)
    with(rmed(df$yr,df$rM),lines(yr,x,col='salmon',lwd=3))
    abline(h=rf, col='purple',lwd=2)
    abline(h=rl , col='blue',lwd=2)
dev.off()


#HCR plots
    
hcrPlot(B=df$wMM/1000,mF=df$rMM,USR=ub,LRP=llb,RR=rl,yrs=c(rep('',length(df$yr)-3),2015:2017),ylim=c(0,33))
savePlot(file.path(fpf1,'HCRFal.png'))


######DFO

dadir = file.path(project.datadirectory('bio.lobster'),'analysis','LFA34-38','indicators')
df =  read.csv(file.path(dadir,'LFA34DFO.restratified.All.csv'))
df2 = read.csv(file.path(dadir,'LFA34DFO.restratified.commercial.csv'))
df = subset(df,yr<1999)
df2 = subset(df2,yr>1998)
df = as.data.frame(rbind(df,df2))
df = df[,c('yr','w.Yst','w.ci.Yst.l','w.ci.Yst.u')] #proportion of total weight that is commercial
df$w.Yst[which(df$yr<1999)] <- df$w.Yst[which(df$yr<1999)]*0.71
df$w.ci.Yst.l[which(df$yr<1999)] <- df$w.ci.Yst.l[which(df$yr<1999)]*0.71
df$w.ci.Yst.u[which(df$yr<1999)] <- df$w.ci.Yst.u[which(df$yr<1999)]*0.71

  ao =df[,c('yr','w.Yst')]
      h = subset(ao,w.Yst>0) #need to remove the zeros for bcp to function correctly
      b = bcp(log(h$w.Yst),w0 = 0.2, p0 = 0.05)
      plot(b,xaxlab=h$yr,xlab='Year')
      savePlot(file.path(fpf1,'BCPDFOL34.png'))

aout = df
   with(df,plot(yr,w.Yst/1000,pch=1,xlab='Year',ylab='Commerical Biomass (t x000)',ylim=c(0,9)))
   with(df,arrows(yr,y0=w.ci.Yst.u/1000,y1=w.ci.Yst.l/1000, length=0))
   with(subset(df,yr>1998),points(yr,w.Yst/1000,pch=16))
   xx = rmed(df$yr,df$w.Yst/1000)
   xx = as.data.frame(do.call(cbind,xx))
   with(subset(xx,yr<1999),lines(yr,x,col='salmon',lwd=2))
   with(subset(xx,yr>1998),lines(yr,x,col='salmon',lwd=3))
   
      lb = median(subset(aout,yr %in% 1970:1999,select=w.Yst)[,1],na.rm=T)/1000
              ub = median(subset(aout,yr %in% 2000:2018,select=w.Yst)[,1],na.rm=T)/1000 * 0.4
              nub = median(subset(aout,yr %in% 1970:2018,select=w.Yst)[,1],na.rm=T)/1000
              llb = aout$w.Yst[which(aout$w.Yst>0)]
              llb = median(sort(llb)[1:5])/1000

      
              abline(h=llb,col='orange',lwd=2)
              abline(h=lb,col='blue',lwd=2)
              abline(h=ub,col='green',lwd=2)
              abline(h=nub,col='purple',lwd=2)

              savePlot(file.path(fpf1,'DFOReferencePoints.png'))

df = aout
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
  png(file=file.path(fpf1,'DFORelFREFS.png'),units='in',width=15,height=12,pointsize=18, res=300,type='cairo')
    plot(df$yr,df$rM,type='p',pch=16,col='black',xlab='Year',ylab='Relative F')
    arrows(df$yr,y0 = df$rL,y1 = df$rU,length=0)
    with(rmed(df$yr,df$rM),lines(yr,x,col='salmon',lwd=3))
    abline(h=rf, col='purple',lwd=2)
    abline(h=rl , col='blue',lwd=2)
dev.off()


df$rMM = rmed(df$yr, df$rM)[[2]]
df$wMM = rmed(df$yr, df$w.Yst)[[2]]

#HCR plots
    
hcrPlot(B=df$wMM/1000,mF=df$rMM,USR=ub,LRP=llb,RR=rl,yrs=c(rep('',length(df$yr)-3),2016:2018),ylim=c(0.5,1.1))
savePlot(file.path(fpf1,'HCRDFo.png'))






######ILTS


dadir = file.path(project.datadirectory('bio.lobster'),'analysis','LFA34-38','indicators')

df = read.csv(file.path(dadir,'ILTScommercialBiomass.csv'))
df = df[,c('yr','w.Yst','w.ci.Yst.l','w.ci.Yst.u')]

  ao =df[,c('yr','w.Yst')]
      h = subset(ao,w.Yst>0) #need to remove the zeros for bcp to function correctly
      b = bcp(log(h$w.Yst),w0 = 0.2, p0 = 0.05)
      plot(b,xaxlab=h$yr,xlab='Year')
      savePlot(file.path(fpf1,'BCPILTSL34.png'))

aout = df
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

      
              abline(h=llb,col='orange',lwd=2)
              abline(h=lb,col='blue',lwd=2)
              abline(h=ub,col='green',lwd=2)
              abline(h=nub,col='purple',lwd=2)

              savePlot(file.path(fpf1,'ILTSReferencePoints.png'))
df = aout
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

   png(file=file.path(fpf1,'ILTSRelF.png'),units='in',width=15,height=12,pointsize=18, res=300,type='cairo')
    plot(df$yr,df$rM,type='p',pch=16,col='black',xlab='Year',ylab='Relative F')
    arrows(df$yr,y0 = df$rL,y1 = df$rU,length=0)
    with(rmed(df$yr,df$rM),lines(yr,x,col='salmon',lwd=3))
dev.off()

  png(file=file.path(fpf1,'ILTSRelFREFS.png'),units='in',width=15,height=12,pointsize=18, res=300,type='cairo')
    plot(df$yr,df$rM,type='p',pch=16,col='black',xlab='Year',ylab='Relative F')
    arrows(df$yr,y0 = df$rL,y1 = df$rU,length=0)
    with(rmed(df$yr,df$rM),lines(yr,x,col='salmon',lwd=3))
    abline(h=rf, col='purple',lwd=2)
    abline(h=rl , col='blue',lwd=2)
dev.off()


df$rMM = rmed(df$yr, df$rM)[[2]]
df$wMM = rmed(df$yr, df$w.Yst)[[2]]

#HCR plots
    
hcrPlot(B=df$wMM/1000,mF=df$rMM,USR=ub,LRP=llb,RR=rl,yrs=c(rep('',length(df$yr)-3),2016:2018),ylim=c(0.5,1.1))
savePlot(file.path(fpf1,'HCRILTS.png'))



####LFA 35

cp = read.csv(file.path(project.datadirectory('bio.lobster'),'analysis','LFA34-38','indicators','CPUEmodelindex.csv'))
c5 = subset(cp, LFA==35)

require(bcp)

   b = bcp((c5$mu),w0 = 0.2, p0 = 0.05)
      plot(b,xaxlab=c5$YEAR,xlab='Year')
      savePlot(file.path(fpf1,'BCPCPUE35.png'))

png(file=file.path(fpf1,'LFA35CPUERefs.png'),units='in',width=15,height=12,pointsize=18, res=300,type='cairo')
with(c5, plot(YEAR, mu, xlab='Year',ylab='Modelled CPUE', pch=16 ,type='p', ylim=c(0,5) ))
with(c5, arrows(YEAR, y0=ub,y1=lb, length=0))
K = median(subset(c5,YEAR>2010)$mu)
USR = K*.4
LRP = K*.2
abline(h=USR, col='green',lwd=2)
abline(h=LRP, col='blue',lwd=2)
xx = rmed(c5$YEAR, c5$mu)
lines(xx$yr,xx$x, lwd=2,col='salmon')
dev.off()



####LFA 36

c6 = subset(cp, LFA==36)

require(bcp)

   b = bcp((c6$mu),w0 = 0.2, p0 = 0.05)
      plot(b,xaxlab=c6$YEAR,xlab='Year')
      savePlot(file.path(fpf1,'BCPCPUE36.png'))

png(file=file.path(fpf1,'LFA36CPUERefs.png'),units='in',width=15,height=12,pointsize=18, res=300,type='cairo')
with(c6, plot(YEAR, mu, xlab='Year',ylab='Modelled CPUE', pch=16 ,type='p', ylim=c(0,5) ))
with(c6, arrows(YEAR, y0=ub,y1=lb, length=0))
K = median(subset(c6,YEAR>2012)$mu)
USR = K*.4
LRP = K*.2
abline(h=USR, col='green',lwd=2)
abline(h=LRP, col='blue',lwd=2)
xx = rmed(c6$YEAR, c6$mu)
lines(xx$yr,xx$x, lwd=2,col='salmon')
dev.off()




####LFA 38

c8 = subset(cp, LFA==38)

require(bcp)

   b = bcp((c8$mu),w0 = 0.2, p0 = 0.05)
      plot(b,xaxlab=c6$YEAR,xlab='Year')
      savePlot(file.path(fpf1,'BCPCPUE38.png'))

png(file=file.path(fpf1,'LFA38CPUERefs.png'),units='in',width=15,height=12,pointsize=18, res=300,type='cairo')
with(c8, plot(YEAR, mu, xlab='Year',ylab='Modelled CPUE', pch=16 ,type='p', ylim=c(0,6) ))
with(c8, arrows(YEAR, y0=ub,y1=lb, length=0))
K = median(subset(c8,YEAR>2013)$mu)
USR = K*.4
LRP = K*.2
abline(h=USR, col='green',lwd=2)
abline(h=LRP, col='blue',lwd=2)
xx = rmed(c8$YEAR, c8$mu)
lines(xx$yr,xx$x, lwd=2,col='salmon')
dev.off()