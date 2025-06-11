#landings

require(bio.lobster)
require(PBSmapping)
la()
a = lobster.db('annual.landings')
b = lobster.db('seasonal.landings')
d = lobster.db('historic.landings')
l34 = c("YARMOUTH","DIGBY")
l35 = c("KINGS","ANNAPOLIS", "COLCHESTER" , "CUMBERLAND",'WESTMORLAND','ALBERT')
l36_38 = c('ALBERT','SAINT JOHN','CHARLOTTE')
l27 = c('CAPE BRETON','VICTORIA')
l28_29_30 = 'RICHMOND'
l31  ='GUYSBOROUGH'

l33 = c('SHELBURNE','LUNENBERG','QUEENS')

d$LFA = ifelse(d$COUNTY %in% l34, 'LFA34',NA)
d = subset(d, !is.na(LFA))
d = aggregate(LANDINGS_MT~SYEAR+LFA,data=d,FUN=sum)
d = subset(d, SYEAR<1947)

names(d) = c('YR','LFA','LAND')

b$YR = substr(b$SYEAR,6,9)
a = subset(a,YR<1976)
b = subset(b,YR>1975 & YR<=2019)
fpf1 = file.path(project.figuredirectory('bio.lobster'),"LFA3438Framework2019")

LFA = c('LFA34','LFA35','LFA36','LFA38')
for(i in LFA){
		aa = a[,c('YR',i)]
		bb = b[,c('YR',i)]
		dd = subset(d,LFA==i)
		dd$LFA <- NULL
		names(dd)[2] <- i
		aa = rbind(rbind(aa,bb),dd)
		aa = aa[order(aa$YR),]
		file.name = paste('Landings',i,'.png',sep="")
		# png(file=file.path(fpf1,'LandingsL3538.png'),units='in',width=15,height=12,pointsize=18, res=300,type='cairo')
		plot(aa$YR,aa[,i],type='h',lwd=4,col='black',xlab='Year',ylab='Landings (t)')
		lines(aa$YR,runmed(aa[,i],3),col='salmon',lwd=3)
		#dev.off()
		}

#relative F LFA 34
a34 = a[,c('YR','LFA34')]
b34 = b[,c('YR','LFA34')]
c34 = rbind(a34,b34)
c34 = subset(c34,YR>1969)
c34$yr = c34$YR
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



 	 png(file=file.path(fpf1,'LFA34CommBDFOextended.png'),units='in',width=15,height=12,pointsize=18, res=300,type='cairo')
 	 with(df,plot(yr,w.Yst,pch=1,xlab='Year',ylab='Commerical Biomass (t)',ylim=c(0,8500)))
	 with(df,arrows(yr,y0=w.ci.Yst.u,y1=w.ci.Yst.l, length=0))
	 with(subset(df,yr>1998),points(yr,w.Yst,pch=16))
	 xx = rmed(df$yr,df$w.Yst)
	 xx = as.data.frame(do.call(cbind,xx))
	 with(subset(xx,yr<1999),lines(yr,x,col='salmon',lwd=1))
	 with(subset(xx,yr>1998),lines(yr,x,col='salmon',lwd=3))
	 dev.off()
	 

df  =merge(df,c34)
df$rL = df$LFA34/(df$w.ci.Yst.l+df$LFA34)
df$rU =df$LFA34/ (df$w.ci.Yst.u+df$LFA34)
df$rM = df$LFA34/(df$w.Yst+df$LFA34)
 df[which(!is.finite(df[,7])),7] <- NA
 df[which(!is.finite(df[,8])),8] <- NA
 df[which(!is.finite(df[,9])),9] <- NA


 	 png(file=file.path(fpf1,'LFA34RelFDFO.png'),units='in',width=15,height=12,pointsize=18, res=300,type='cairo')
		plot(df$yr,df$rM,type='p',pch=16,col='black',xlab='Year',ylab='Relative F')
		arrows(df$yr,y0 = df$rL,y1 = df$rU,length=0)
		with(rmed(df$yr,df$rM),lines(yr,x,col='salmon',lwd=3))
dev.off()

####aim for dfo
	
	replacementRatio.relF(df$LFA34,df$w.Yst+df$LFA34, savePlot=F, years.lagged.replacement=8)

###

df = read.csv(file.path(dadir,'LFA34NEFSC.fall.restratified.commercial.csv'))
df = df[,c('yr','w.Yst','w.ci.Yst.l','w.ci.Yst.u')]
df  =merge(df,c34)

df$rL = df$LFA34/df$w.ci.Yst.l
df$rU =df$LFA34/ df$w.ci.Yst.u
df$rM = df$LFA34/df$w.Yst
 df[which(!is.finite(df[,7])),7] <- NA
 df[which(!is.finite(df[,8])),8] <- NA
 df[which(!is.finite(df[,9])),9] <- NA

 	 png(file=file.path(fpf1,'NEFSCFallRelFDFO.png'),units='in',width=15,height=12,pointsize=18, res=300,type='cairo')
		plot(df$yr,df$rM,type='p',pch=16,col='black',xlab='Year',ylab='Relative F')
		arrows(df$yr,y0 = df$rL,y1 = df$rU,length=0)
		with(rmed(df$yr,df$rM),lines(yr,x,col='salmon',lwd=3))
dev.off()


df = read.csv(file.path(dadir,'LFA34NEFSC.spring.restratified.commercial.csv'))
df = df[,c('yr','w.Yst','w.ci.Yst.l','w.ci.Yst.u')]
df  =merge(df,c34)

df$rL = df$LFA34/(df$w.ci.Yst.l+df$LFA34)
df$rU =df$LFA34/ (df$w.ci.Yst.u+df$LFA34)
df$rM = df$LFA34/(df$w.Yst+df$LFA34)
 df[which(!is.finite(df[,7])),7] <- NA
 df[which(!is.finite(df[,8])),8] <- NA
 df[which(!is.finite(df[,9])),9] <- NA

 	 png(file=file.path(fpf1,'NEFSCSpringRelFDFO.png'),units='in',width=15,height=12,pointsize=18, res=300,type='cairo')
		plot(df$yr,df$rM,type='p',pch=16,col='black',xlab='Year',ylab='Relative F')
		arrows(df$yr,y0 = df$rL,y1 = df$rU,length=0)
		with(rmed(df$yr,df$rM),lines(yr,x,col='salmon',lwd=3))
dev.off()



il = file.path(dadir,'ILTScommercialBiomass.csv')

df = read.csv(file.path(dadir,'ILTScommercialBiomass.csv'))
df = df[,c('yr','w.Yst','w.ci.Yst.l','w.ci.Yst.u')]
df  =merge(df,c34)

df$rL = df$LFA34/(df$w.ci.Yst.l+df$LFA34)
df$rU =df$LFA34/ (df$w.ci.Yst.u+df$LFA34)
df$rM = df$LFA34/(df$w.Yst+df$LFA34)
 df[which(!is.finite(df[,7])),7] <- NA
 df[which(!is.finite(df[,8])),8] <- NA
 df[which(!is.finite(df[,9])),9] <- NA

 	 png(file=file.path(fpf1,'ILTSRelF.png'),units='in',width=15,height=12,pointsize=18, res=300,type='cairo')
		plot(df$yr,df$rM,type='p',pch=16,col='black',xlab='Year',ylab='Relative F')
		arrows(df$yr,y0 = df$rL,y1 = df$rU,length=0)
		with(rmed(df$yr,df$rM),lines(yr,x,col='salmon',lwd=3))
dev.off()


replacementRatio.relF(df$LFA34,df$LFA34+df$w.Yst, years.lagged.replacement=7)


ccirs = read.csv(file.path(dadir,'ExploitationRefs34.csv'))

plot(df$yr,df$rM,pch=16,col='black',xlab='Year',ylab='Relative F',ylim=c(0.45,1),type='b')
points(ccirs$Yr,ccirs$ERfm,type='b')
legend('topleft',pch=c(1,16),legend=c('CCIR','ILTSRelF'))
savePlot('~/tmp/Exploitation34.png')




###lfa 35-38
dadir = file.path(project.datadirectory('bio.lobster'),'analysis','LFA34-38','indicators')
df =  read.csv(file.path(dadir,'LFA35-38DFO.restratified.All.csv'))
df2 = read.csv(file.path(dadir,'LFA35-38DFO.restratified.commercial.csv'))
df = subset(df,yr<1999)
df2 = subset(df2,yr>1998)
df = as.data.frame(rbind(df,df2))
df = df[,c('yr','w.Yst','w.ci.Yst.l','w.ci.Yst.u')] #proportion of total weight that is commercial
df$w.Yst[which(df$yr<1999)] <- df$w.Yst[which(df$yr<1999)]*0.746
df$w.ci.Yst.l[which(df$yr<1999)] <- df$w.ci.Yst.l[which(df$yr<1999)]*0.746
df$w.ci.Yst.u[which(df$yr<1999)] <- df$w.ci.Yst.u[which(df$yr<1999)]*0.746



 	 png(file=file.path(fpf1,'LFA35-38CommBDFOextended.png'),units='in',width=15,height=12,pointsize=18, res=300,type='cairo')
 	 with(df,plot(yr,w.Yst,pch=1,xlab='Year',ylab='Commerical Biomass (t)',ylim=c(0,9500)))
	 with(df,arrows(yr,y0=w.ci.Yst.u,y1=w.ci.Yst.l, length=0))
	 with(subset(df,yr>1998),points(yr,w.Yst,pch=16))
	 xx = rmed(df$yr,df$w.Yst)
	 xx = as.data.frame(do.call(cbind,xx))
	 with(subset(xx,yr<1999),lines(yr,x,col='salmon',lwd=1))
	 with(subset(xx,yr>1998),lines(yr,x,col='salmon',lwd=3))
	 dev.off()
	 
a$L3538 = rowSums(a[,12:14])
b$L3538 = rowSums(b[,4:6])
c358 = as.data.frame(rbind(a[,c('YR','L3538')],b[,c('YR','L3538')]))
names(c358)[1] = 'yr'
df  =merge(df,c358)
df$rL = df$L3538/(df$w.ci.Yst.l+df$L3538)
df$rU =df$L3538/ (df$w.ci.Yst.u+df$L3538)
df$rM = df$L3538/(df$w.Yst+df$L3538)


 	 png(file=file.path(fpf1,'LFA3538RelFDFO.png'),units='in',width=15,height=12,pointsize=18, res=300,type='cairo')
		plot(df$yr,df$rM,type='p',pch=16,col='black',xlab='Year',ylab='Relative F')
		arrows(df$yr,y0 = df$rL,y1 = df$rU,length=0)
		with(rmed(df$yr,df$rM),lines(yr,x,col='salmon',lwd=3))
dev.off()

####aim for dfo
	
	replacementRatio.relF(df$L3538,df$w.Yst+df$L3538, savePlot=F, years.lagged.replacement=8)









playing=F
if(playingIn34){

				#cycles
				require(forecast)
					i = "LFA34"
					 aa = b[,c('YR',i)]
					bb = subset(aa,YR>1980)	
					x = bb[,i]
				 	n <- length(x)
				    x <- as.ts(x)
				    x <- residuals(tslm(x ~ trend)) #removing time series trend
				    n.freq <- 500
				    spec <- spec.ar(c(na.contiguous(x)), plot = T, n.freq = n.freq) # spectral analysis of cycle
				    period <- floor(1/spec$freq[which.max(spec$spec)] + 0.5) #determiing the most likely period

				    plot( 1/spec$freq,spec$spec,type= 'l',xlim=c(0,20),xlab='Frequency', ylab='Spectral Density')
				    abline(h=max(spec$spec),v=1/spec$freq[which.max(spec$spec)],col='red',lwd=2)
				    savePlot(file.path(fpf1,'CyclesInLandingsLFA34.png'))
				 
				 #rate of increase
				           	ag <- coef(lm(log(LFA34)~YR,data=bb))
						 round((1-exp(ag[2]*nrow(bb))),digits=1)
					

				   x = trends.rot[1,]
				   n <- length(x)
				    x <- as.ts(x)
				    x <- residuals(tslm(x ~ trend)) #removing time series trend
				    n.freq <- 500
				    spec <- spec.ar(c(na.contiguous(x)), plot = T, n.freq = n.freq) # spectral analysis of cycle
				    period <- floor(1/spec$freq[which.max(spec$spec)] + 0.5) #determiing the most likely period

						a= read.table('~/tmp/sunsptdata.dat',header=T)
						b = subset(a,Year>1980 & Year<2019)

				   y = b$TSI
				   n <- length(y)
				    y <- as.ts(y)
				    y <- residuals(tslm(y ~ trend)) #removing time series trend
				  plot(y)
				 par(new=T)
				plot(x,col='red')

}

if(playingIn35){
nothin
				#cycles
				require(forecast)
					i = "LFA35"
					 aa = a[,c('YR',i)]
					bb = subset(aa,YR>1980)	
					x = bb[,i]
				 	n <- length(x)
				    x <- as.ts(x)
				    x <- residuals(tslm(x ~ trend)) #removing time series trend
				    n.freq <- 500
				    spec <- spec.ar(c(na.contiguous(x)), plot = T, n.freq = n.freq) # spectral analysis of cycle
				    period <- floor(1/spec$freq[which.max(spec$spec)] + 0.5) #determiing the most likely period

				    plot( 1/spec$freq,spec$spec,type= 'l',xlim=c(0,20),xlab='Frequency', ylab='Spectral Density')
				    abline(h=max(spec$spec),v=1/spec$freq[which.max(spec$spec)],col='red',lwd=2)
				    savePlot(file.path(fpf1,'CyclesInLandingsLFA34.png'))
				 
				 #rate of increase
				           	ag <- coef(lm(log(LFA34)~YR,data=bb))
						 round((1-exp(ag[2]*nrow(bb))),digits=1)
					

				   x = trends.rot[1,]
				   n <- length(x)
				    x <- as.ts(x)
				    x <- residuals(tslm(x ~ trend)) #removing time series trend
				    n.freq <- 500
				    spec <- spec.ar(c(na.contiguous(x)), plot = T, n.freq = n.freq) # spectral analysis of cycle
				    period <- floor(1/spec$freq[which.max(spec$spec)] + 0.5) #determiing the most likely period

						a= read.table('~/tmp/sunsptdata.dat',header=T)
						b = subset(a,Year>1980 & Year<2019)

				   y = b$TSI
				   n <- length(y)
				    y <- as.ts(y)
				    y <- residuals(tslm(y ~ trend)) #removing time series trend
				  
}