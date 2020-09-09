#landings

require(bio.lobster)
require(PBSmapping)

a = lobster.db('annual.landings')
b = lobster.db('seasonal.landings')
ff = "LFA35-38Assessment"

b$YR = substr(b$SYEAR,6,9)
a = subset(a,YR<1976)
b = subset(b,YR>1975 & YR<=2020)
fpf1 = file.path(project.figuredirectory('bio.lobster'),ff)
fp1 = file.path(project.datadirectory('bio.lobster'),"analysis",ff)

LFA = c('LFA35','LFA36','LFA38')
for(i in LFA){
		aa = a[,c('YR',i)]
		bb = b[,c('YR',i)]
		aa = (rbind(aa,bb))
		aa = aa[order(aa$YR),]
		file.name = paste('Landings',i,'.png',sep="")
		png(file=file.path(fpf1,'LandingsL3538.png'),units='in',width=15,height=12,pointsize=18, res=300,type='cairo')
		plot(aa$YR,aa[,i],type='h',lwd=4,col='black',xlab='Year',ylab='Landings (t)')
		lines(aa$YR,runmed(aa[,i],3),col='salmon',lwd=3)
		dev.off()
		}


###lfa 35-38

df2 = read.csv(file.path(fpf1,'LFA35-38 DFOCommB.csv'))
df =  read.csv(file.path(fpf1,'LFA35-38 DFOtotalabund.csv'))
df = subset(df,yr<1999)
df2 = subset(df2,yr>1998)
df = as.data.frame(rbind(df,df2))
df = df[,c('yr','w.Yst','w.ci.Yst.l','w.ci.Yst.u')] #proportion of total weight that is commercial
df$w.Yst[which(df$yr<1999)] <- df$w.Yst[which(df$yr<1999)]*0.746
df$w.ci.Yst.l[which(df$yr<1999)] <- df$w.ci.Yst.l[which(df$yr<1999)]*0.746
df$w.ci.Yst.u[which(df$yr<1999)] <- df$w.ci.Yst.u[which(df$yr<1999)]*0.746



 	 #png(file=file.path(fpf1,'LFA35-38CommBDFOextended.png'),units='in',width=10,height=8,pointsize=18, res=300,type='cairo')
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


 	 png(file=file.path(fpf1,'LFA3538RelFDFO.png'),units='in',width=10,height=8,pointsize=18, res=300,type='cairo')
		plot(df$yr,df$rM,type='p',pch=16,col='black',xlab='Year',ylab='Relative F')
		arrows(df$yr,y0 = df$rL,y1 = df$rU,length=0)
		with(rmed(df$yr,df$rM),lines(yr,x,col='salmon',lwd=3))
dev.off()
