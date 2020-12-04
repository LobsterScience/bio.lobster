	
	require(bio.lobster)
	require(forecast)
	require(bio.utilities)
	require(MARSS)
	fpf1 = file.path('~','tmp','LandingEAFMAnalysis')

					i = "LFA34"
					a=lobster.db('annual.landings')
					a = a[order(a$YR),]
					 aa = a[,c('YR',i)]
					bb = subset(aa,YR>1981 & YR<2019)	
					x = bb[,i]
					x=mave(x,c(1,1,1))

					xx = (x-mean(x))/sd(x)
				 	n <- length(x)
				    x <- as.ts(x)
				    x34 = x <- residuals(tslm(x ~ trend)) #removing time series trend
				    n.freq <- 500
				    spec <- spec.ar(c(na.contiguous(x)), plot = T, n.freq = n.freq) # spectral analysis of cycle
				    period <- floor(1/spec$freq[which.max(spec$spec)] + 0.5) #determiing the most likely period

				    plot( 1/spec$freq,spec$spec,type= 'l',xlim=c(0,20),xlab='Frequency', ylab='Spectral Density')
				    abline(h=max(spec$spec),v=1/spec$freq[which.max(spec$spec)],col='red',lwd=2)
				    savePlot(file.path(fpf1,'CyclesInLandingsLFA34.png'))

				 #1/spec$freq[which.max(spec$spec)]
				  11.88
				 #rate of increase
				           	ag <- coef(lm(log(LFA34)~YR,data=bb))
						 round((1-exp(ag[2]*nrow(bb))),digits=1)
					

#US data

	d = read.csv(file.path(project.datadirectory('bio.lobster'),'data','uslandings.csv'))
	x = d$A513    			   
    			   n <- length(x)
    			   x=mave(x,c(1,1,1))
				    xx = (x-mean(x))/sd(x)
				 	
				    x <- as.ts(x)
				    x513 = x <- residuals(tslm(x ~ trend)) #removing time series trend
				    n.freq <- 500
				    spec <- spec.ar(c(na.contiguous(x)), plot = T, n.freq = n.freq) # spectral analysis of cycle
				    period <- floor(1/spec$freq[which.max(spec$spec)] + 0.5) #determiing the most likely period
   plot( 1/spec$freq,spec$spec,type= 'l',xlim=c(0,20),xlab='Frequency', ylab='Spectral Density')
				    abline(h=max(spec$spec),v=1/spec$freq[which.max(spec$spec)],col='red',lwd=2)
				 #1/spec$freq[which.max(spec$spec)]
				  10.73
		
				#		a= read.table('~/tmp/sunsptdata.dat',header=T)
			#			b = subset(a,Year>1980 & Year<2019)

				   y = b$TSI
				   n <- length(y)
				    y <- as.ts(y)
				    y <- residuals(tslm(y ~ trend)) #removing time series trend
				  

#SGSL

	w = read.csv(file.path(project.datadirectory('bio.lobster'),'data','SGSL_LobsterLandings.csv'))
	w = subset(w, Year>1981)
	x = w$L24    			  
    			   n <- length(x)
    			   x=mave(x,c(1,1,1))
				    xx = (x-mean(x))/sd(x)
				 	
				    x <- as.ts(x)
				    x24 = x <- residuals(tslm(x ~ trend)) #removing time series trend
				    n.freq <- 500
				    spec <- spec.ar(c(na.contiguous(x)), plot = T, n.freq = n.freq) # spectral analysis of cycle
				    period <- floor(1/spec$freq[which.max(spec$spec)] + 0.5) #determiing the most likely period
   plot( 1/spec$freq,spec$spec,type= 'l',xlim=c(0,20),xlab='Frequency', ylab='Spectral Density')
				    abline(h=max(spec$spec),v=1/spec$freq[which.max(spec$spec)],col='red',lwd=2)
			#1/spec$freq[which.max(spec$spec)]
			#	  12.79
		

##looking at cycles from bnam Outputs
os = read.csv(file='~/tmp/Bnamtrends.csv')
rownames(os) = os$X
os$X = NULL

#L34
x = unlist(os[which(rownames(os)=='34'),])
   n <- length(x)
    			   x=mave(x,c(1,1,1))
				    xx = (x-mean(x))/sd(x)
				 	
				    x <- as.ts(x)
				    t34 = x <- residuals(tslm(x ~ trend)) #removing time series trend
				    n.freq <- 500
				    spec <- spec.ar(c(na.contiguous(x)), plot = T, n.freq = n.freq) # spectral analysis of cycle
				    period <- floor(1/spec$freq[which.max(spec$spec)] + 0.5) #determiing the most likely period
   plot( 1/spec$freq,spec$spec,type= 'l',xlim=c(0,300),xlab='Frequency', ylab='Spectral Density')
				    abline(h=max(spec$spec),v=1/spec$freq[which.max(spec$spec)],col='red',lwd=2)
		
	#1/spec$freq[which.max(spec$spec)]/12 to get to years
			#	  5.94
		


#L24
x = unlist(os[which(rownames(os)=='24'),])
   n <- length(x)
    			   x=mave(x,c(1,1,1))
				    xx = (x-mean(x))/sd(x)
				 	
				    x <- as.ts(x)
				    t24 = x <- residuals(tslm(x ~ trend)) #removing time series trend
				    n.freq <- 500
				    spec <- spec.ar(c(na.contiguous(x)), plot = T, n.freq = n.freq) # spectral analysis of cycle
				    period <- floor(1/spec$freq[which.max(spec$spec)] + 0.5) #determiing the most likely period
   plot( 1/spec$freq,spec$spec,type= 'l',xlim=c(0,300),xlab='Frequency', ylab='Spectral Density')
				    abline(h=max(spec$spec),v=1/spec$freq[which.max(spec$spec)],col='red',lwd=2)
		
	#1/spec$freq[which.max(spec$spec)]/12 #to get to years
			#	  6.93
		
#L513
x = unlist(os[which(rownames(os)=='513'),])
   n <- length(x)
    			   x=mave(x,c(1,1,1))
				    xx = (x-mean(x))/sd(x)
				 	
				    x <- as.ts(x)
				    t513 = x <- residuals(tslm(x ~ trend)) #removing time series trend
				    n.freq <- 500
				    spec <- spec.ar(c(na.contiguous(x)), plot = T, n.freq = n.freq) # spectral analysis of cycle
				    period <- floor(1/spec$freq[which.max(spec$spec)] + 0.5) #determiing the most likely period
   plot( 1/spec$freq,spec$spec,type= 'l',xlim=c(0,300),xlab='Frequency', ylab='Spectral Density')
				    abline(h=max(spec$spec),v=1/spec$freq[which.max(spec$spec)],col='red',lwd=2)
		
	#1/spec$freq[which.max(spec$spec)]/12 #to get to years
			#	  6.39

#cR = cR[,c(-1,-2,-3,-4,-5,-6,-343,-344,-345,-346,-347,-348)]
cRR = zscore(as.matrix(cR))



ccf



########################################################################################################################

###DFA
	a=lobster.db('annual.landings')
	a = a[order(a$YR),]
	b = subset(a,YR>1981 & YR<2019)	
	d = read.csv(file.path(project.datadirectory('bio.lobster'),'data','uslandings.csv'))
	w = read.csv(file.path(project.datadirectory('bio.lobster'),'data','SGSL_LobsterLandings.csv'))
	y = read.csv(file.path(project.datadirectory('bio.lobster'),'data','QCLandings1945-2018.csv'))
	y1 = reshape(y[,c('an','debarq','LFA')],direction='wide',timevar='LFA',idvar='an')
	names(y1) = c('Year','L17','L151618','L15161718','L192021','L22','L22N','L22S','LQC','L19','L20','L21A','L21BA','L21BP')
	y = y1[order(y1$Year),]
	y = subset(y,Year>1981 & Year<2019 )
	u = read.csv(file.path(project.datadirectory('bio.lobster'),'data','LandingsLFAs_NL_NS_bis.csv'))
	u1 = reshape(u,direction='wide',timevar='LFA',idvar='Year')
	names(u1) = c('Year','L3_4A','L4B','L5','L6','L7','L8_AB','L10','L11_12','L13A','L13B','L14A','L14BC')

	w = subset(w, Year>1981)
	w$TOTAL=NULL

	cR = merge(b,d,by.x='YR',by.y='Year')
	cR = merge(cR,w,by.x='YR',by.y='Year')
	cR = merge(cR,y,by.x='YR',by.y='Year')
	cR = merge(cR,u1,by.x='YR',by.y='Year')


	cR$L192021 = cR$L15161718 =  cR$L22 = cR$LQC = cR$YR = cR$LFA41= cR$LFA31= cR$LFA28=NULL
	cR = t(log(cR+.001))

cRR = zscore(as.matrix(cR))
spp = rownames(cRR)
par(mfcol=c(8,5), mar=c(3,4,1.5,0.5), oma=c(0.4,1,1,1))
for(i in spp){
  plot(cRR[i,],xlab="",ylab="Landings", bty="L", xaxt="n", pch=16, col="blue", type="b")
  title(i)
  }


N.ts = nrow(cRR)
nT = ncol(cRR)
N.ts = c(5,6,7)
cntl.list = list(minit=200, maxit=5000, allow.degen=FALSE)
find.best=T
if(find.best){
# set up forms of R matrices
levels.R = c("diagonal and equal","diagonal and unequal")#,"equalvarcov","unconstrained")
model.data = data.frame()
	for(R in levels.R) {
		for(m in 1:length(N.ts)) {
				dfa.model = list(A="zero", R=R, m=N.ts[m])
				ff = MARSS(cRR, model=dfa.model, control=cntl.list,
						form="dfa", z.score=TRUE)
		
				model.data = rbind(model.data,
							data.frame(R=R,m=N.ts[m],logLik=ff$logLik,K=ff$num.params,AICc=ff$AICc,stringsAsFactors=FALSE))
				assign(paste("ff", m, R, sep="."), ff)
		} # end m loop
} # end R loop
#best model is diag and unequal R and Q with three trends
write.csv(model.data,'~/tmp/full.model.data.allData.csv')

}

model.data89101112=model.data

big.maxit.cntl.list = list(minit=200, maxit=30000, allow.degen=FALSE)
model.list = list(m=7, R="diagonal and unequal",Q = "diagonal and unequal")
the.fit = MARSS(cRR, model=model.list, form="dfa", control=big.maxit.cntl.list)
save(the.fit,file='~/tmp/full.model.output.allData.rdata')
#load(file='/tmp/full.model.output.allData.rdata')

# get the inverse of the rotation matrix
Z.est = coef(the.fit, type="matrix")$Z


H.inv = 1
if(ncol(Z.est)>1) H.inv = varimax(coef(the.fit, type="matrix")$Z)$rotmat

clr <- c("brown","blue","darkgreen","darkred",'orange','purple')
ylbl = row.names(cRR)
# rotate factor loadings
Z.rot = Z.est %*% H.inv
# rotate trends
trends.rot = solve(H.inv) %*% the.fit$states
tt = 1982:2018
mm=7


#plot states and loadings
N.ts=nrow(cRR)
#par(mai=c(0.5,0.5,0.5,0.1), omi=c(0,0,0,0))
## plot the processes
minZ <- 0.05

pdf('~/tmp/FullModelTrends.pdf')
 par(mfcol=c(2,1), mai=c(0.5,0.5,0.5,0.1), omi=c(0,0,0,0))

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
  ylm <- c(-1,1)*max(abs(Z.rot[,i]))

  plot(c(1:N.ts)[abs(Z.rot[,i])>minZ], as.vector(Z.rot[abs(Z.rot[,i])>minZ,i]), type="h",
       lwd=2, xlab="", ylab="", xaxt="n", ylim=ylm, xlim=c(0.5,N.ts+0.5), col=clr)
	
    for(j in 1:N.ts) {
      if(Z.rot[j,i] > minZ) {text(j, -0.03, ylbl[j], srt=90, adj=1, cex=1.2, col=clr[j])}
      if(Z.rot[j,i] < -minZ) {text(j, 0.03, ylbl[j], srt=90, adj=0, cex=1.2, col=clr[j])}
      abline(h=0, lwd=1.5, col="gray")
      } 
  mtext(paste("Factor loadings on trend",i),side=3,line=0.5)
}
graphics.off()

fpf1 = file.path(project.figuredirectory('bio.lobster'))
savePlot(file.path(fpf1,'CanNGULNLSGSLUSLandLoadings.png'))

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
spp = rownames(cRR)
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
d <- augment(the.fit, interval="confidence")


d$t = rep(1982:2018,times=20)

d0 = subset(d,.rownames %in% c ("L3_4A","L4B","L5","L6"))
p.0 = ggplot(data = d0) +
geom_line(aes(t, .fitted)) +
geom_point(aes(t, y)) +
geom_ribbon(aes(x=t, ymin=.conf.low, ymax=.conf.up), linetype=2, alpha=0.2) +
facet_grid(~.rownames) +
xlab("Year") + ylab("Standardized Landings")
d0 = subset(d,.rownames %in% c ("L7","L8_AB","L10","L11_12"))
p.1 = ggplot(data = d0) +
geom_line(aes(t, .fitted)) +
geom_point(aes(t, y)) +
geom_ribbon(aes(x=t, ymin=.conf.low, ymax=.conf.up), linetype=2, alpha=0.2) +
facet_grid(~.rownames) +
xlab("Year") + ylab("Standardized Landings")
d0 = subset(d,.rownames %in% c ("L13A","L13B","L14A","L14BC"))
p.2 = ggplot(data = d0) +
geom_line(aes(t, .fitted)) +
geom_point(aes(t, y)) +
geom_ribbon(aes(x=t, ymin=.conf.low, ymax=.conf.up), linetype=2, alpha=0.2) +
facet_grid(~.rownames) +
xlab("Year") + ylab("Standardized Landings")

d0 = subset(d,.rownames %in% c ("L19","L20","L21A","L21BA"))
p.3 = ggplot(data = d0) +
geom_line(aes(t, .fitted)) +
geom_point(aes(t, y)) +
geom_ribbon(aes(x=t, ymin=.conf.low, ymax=.conf.up), linetype=2, alpha=0.2) +
facet_grid(~.rownames) +
xlab("Year") + ylab("Standardized Landings")


d0 = subset(d,.rownames %in% c ("L21BP","L22N","L22S"))
p.4 = ggplot(data = d0) +
geom_line(aes(t, .fitted)) +
geom_point(aes(t, y)) +
geom_ribbon(aes(x=t, ymin=.conf.low, ymax=.conf.up), linetype=2, alpha=0.2) +
facet_grid(~.rownames) +
xlab("Year") + ylab("Standardized Landings")


d0 = subset(d,.rownames %in% c ("L23","L24","L25","L26A"))
p0 = ggplot(data = d0) +
geom_line(aes(t, .fitted)) +
geom_point(aes(t, y)) +
geom_ribbon(aes(x=t, ymin=.conf.low, ymax=.conf.up), linetype=2, alpha=0.2) +
facet_grid(~.rownames) +
xlab("Year") + ylab("Standardized Landings")

d1 = subset(d,.rownames %in% c ('L26B',"LFA27","LFA29","LFA30"))
p1 = ggplot(data = d1) +
geom_line(aes(t, .fitted)) +
geom_point(aes(t, y)) +
geom_ribbon(aes(x=t, ymin=.conf.low, ymax=.conf.up), linetype=2, alpha=0.2) +
facet_grid(~.rownames) +
xlab("Year") + ylab("Standardized Landings")

d2 = subset(d,.rownames %in% c ("LFA31A","LFA31B","LFA32","LFA33"))
p2 = ggplot(data = d2) +
geom_line(aes(t, .fitted)) +
geom_point(aes(t, y)) +
geom_ribbon(aes(x=t, ymin=.conf.low, ymax=.conf.up), linetype=2, alpha=0.2) +
facet_grid(~.rownames) +
xlab("Year") + ylab("Standardized Landings")

d3 = subset(d,.rownames %in% c ("LFA34","LFA35","LFA36","LFA38"))
p3 = ggplot(data = d3) +
geom_line(aes(t, .fitted)) +
geom_point(aes(t, y)) +
geom_ribbon(aes(x=t, ymin=.conf.low, ymax=.conf.up), linetype=2, alpha=0.2) +
facet_grid(~.rownames) +
xlab("Year") + ylab("Standardized Landings")



d4 = subset(d,.rownames %in% c ("A511","A512","A513","A514"))
p4 = ggplot(data = d4) +
geom_line(aes(t, .fitted)) +
geom_point(aes(t, y)) +
geom_ribbon(aes(x=t, ymin=.conf.low, ymax=.conf.up), linetype=2, alpha=0.2) +
facet_grid(~.rownames) +
xlab("Year") + ylab("Standardized Landings")



require(ggpubr)
ggarrange(p.0,p.1,p.2,p.3,p.4,p0,p1,p2,p3,p4, ncol=3,nrow=4)
savePlot(file.path('~/tmp/','FitsDFALandingsCanUSSGSLNLNGSL.png'))


############################################
cyclicity in trends

#1
				    x <- as.ts(trends.rot[3,])
				   # t513 = x <- residuals(tslm(x ~ trend)) #removing time series trend
				    n.freq <- 500
				    spec <- spec.ar(c(na.contiguous(mave(x,c(1,1,1,1,1)))), plot = T, n.freq = n.freq) # spectral analysis of cycle
				    #spec <- spec.ar(c(na.contiguous(x)), plot = T, n.freq = n.freq) # spectral analysis of cycle
				    floor(1/spec$freq[which.max(spec$spec)] + 0.5) #determiing the most likely period
   plot( 1/spec$freq,spec$spec,type= 'l',xlim=c(0,30),xlab='Frequency', ylab='Spectral Density')
				    abline(h=max(spec$spec),v=1/spec$freq[which.max(spec$spec)],col='red',lwd=2)

#AMO
options(stringsAsFactors=F)

url <- "http://www.esrl.noaa.gov/psd/data/correlation//amon.us.long.data"
a = readLines(url,skip=1)
k = length(a)
a = a[-c(1,(k-4):k)]
g = length(a)

a = matrix(scan(url,skip=1,nlines=g),nrow=g,ncol=13,byrow=T)
p = rowMeans(a[115:163,2:13])
plot(1970:2018,p,type='h',ylab='AMO anomaly',xlab='Year')
	    x <- as.ts(p)
				    t513 = x <- residuals(tslm(x ~ trend)) #removing time series trend
				    n.freq <- 500
				    spec <- spec.ar(c(na.contiguous(mave(x,c(1,1,1,1,1)))), plot = T, n.freq = n.freq) # spectral analysis of cycle
				   # spec <- spec.ar(c(na.contiguous(x)), plot = T, n.freq = n.freq) # spectral analysis of cycle
				    floor(1/spec$freq[which.max(spec$spec)] + 0.5) #determiing the most likely period
   plot( 1/spec$freq,spec$spec,type= 'l',xlim=c(0,30),xlab='Frequency', ylab='Spectral Density')
				    abline(h=max(spec$spec),v=1/spec$freq[which.max(spec$spec)],col='red',lwd=2)

#nao

 u = 'https://www.ncdc.noaa.gov/teleconnections/nao/data.csv'
 l = read.csv(u,header=T)
 l = as.data.frame(l[-1,])
 l$yr = c(rep(1950:2019, each=12),rep(2020,1))
names(l)[1] <- 'NAO'
l$NAO = as.numeric(l$NAO)
a = aggregate(NAO~yr,data=l,FUN=mean)
x = ma(a$NAO[which(a$yr>1970)],1)
    x <- as.ts(x)
				    t513 = x <- residuals(tslm(x ~ trend)) #removing time series trend
				    x = ma(x,1)
				    n.freq <- 500
				    spec <- spec.ar(c(na.contiguous(mave(x,c(1,1,1,1,1)))), plot = T, n.freq = n.freq) # spectral analysis of cycle
				   # spec <- spec.ar(c(na.contiguous(x)), plot = T, n.freq = n.freq) # spectral analysis of cycle
				    floor(1/spec$freq[which.max(spec$spec)] + 0.5) #determiing the most likely period
   plot( 1/spec$freq,spec$spec,type= 'l',xlim=c(0,30),xlab='Frequency', ylab='Spectral Density')
				    abline(h=max(spec$spec),v=1/spec$freq[which.max(spec$spec)],col='red',lwd=2)


#


###############################################################################
###temperature DFA
os = read.csv(file='~/tmp/Bnamtrends.csv')
rownames(os) = os$X
os$X = NULL
cR = os
#cR = cR[,c(-1,-2,-3,-4,-5,-6,-343,-344,-345,-346,-347,-348)]
cRR = zscore(as.matrix(cR))

spp = rownames(cRR)
par(mfcol=c(5,4), mar=c(3,4,1.5,0.5), oma=c(0.4,1,1,1))
for(i in spp){
  plot(cRR[i,],xlab="",ylab="Temperature", bty="L", xaxt="n", pch=16, col="blue", type="b")
  title(i)
  }

N.ts = nrow(cRR)
nT = ncol(cRR)
##need to redo
nTR = F
if(nTR){
cntl.list = list(minit=200, maxit=5000, allow.degen=FALSE)
find.best=T
if(find.best){
# set up forms of R matrices
levels.R = c("diagonal and equal","equalvarcov")#,"diagonal and unequal","unconstrained")
model.data = data.frame()
	for(R in levels.R) {
		#for(m in 1:(N.ts-1)) {
			for(m in 1:19) {
				dfa.model = list(A="zero", R=R, m=m)
				ff = MARSS(cRR, model=dfa.model, control=cntl.list,
						form="dfa", z.score=TRUE,silent=T)
		
				model.data = rbind(model.data,
							data.frame(R=R,m=m,logLik=ff$logLik,K=ff$num.params,AICc=ff$AICc,stringsAsFactors=FALSE))
				assign(paste("ff", m, R, sep="."), ff)
				print(model.data)
		} # end m loop
} # end R loop
write.csv(model.data,'~/tmp/model.data.TempsData16-22.csv')
}
}

#best m Oct 2020
big.maxit.cntl.list = list(minit=200, maxit=30000, allow.degen=FALSE)
model.list = list(m=17, R="equalvarcov")
the.fit = MARSS(cRR, model=model.list, form="dfa", control=big.maxit.cntl.list)
save(the.fit,file='/tmp/modeltemp.output.rdata')

# get the inverse of the rotation matrix
Z.est = coef(the.fit, type="matrix")$Z


H.inv = 1
if(ncol(Z.est)>1) H.inv = varimax(coef(the.fit, type="matrix")$Z)$rotmat

clr <- c("brown","blue","darkgreen","darkred",'orange','purple')
ylbl = row.names(cRR)
# rotate factor loadings
Z.rot = Z.est %*% H.inv
# rotate trends
trends.rot = solve(H.inv) %*% the.fit$states
tt = seq(1990,2019,length.out=348)
mm=c(1,2,6,12)


#plot states and loadings

layout(matrix(c(1:4),4,2),widths=c(2,1))
 par(mfcol=c(4,2), mai=c(0.5,0.5,0.5,0.1), omi=c(0,0,0,0))
par(mai=c(0.5,0.5,0.5,0.1), omi=c(0,0,0,0))
 #plot the processes

for(i in 1:length(mm)) {
  ylm <- c(-1,1)*max(abs(trends.rot[mm[i],]))
  ## set up plot area
    plot(tt,trends.rot[mm[i],], type="n", bty="L",
         ylim=ylm, xlab="", ylab="", xaxt="n")
    ## draw zero-line
    abline(h=0, col="gray")
    ## plot trend line
    lines(tt,trends.rot[mm[i],], lwd=2)
    ## add panel labels
    mtext(paste("Trend",mm[i]), side=3, line=0.5)
    axis(1,tt)
}
## plot the loadings
FminZ <- 0.05
ylm <- c(-1,1)*max(abs(Z.rot))
for(i in 1:length(mm)) {
	
  plot(c(1:N.ts)[abs(Z.rot[,mm[i]])>minZ], as.vector(Z.rot[abs(Z.rot[,mm[i]])>minZ,mm[i]]), type="h",
       lwd=2, xlab="", ylab="", xaxt="n", ylim=ylm, xlim=c(0.5,N.ts+0.5), col=clr)
    for(j in 1:N.ts) {
      if(Z.rot[j,mm[i]] > minZ) {text(j, -0.03, ylbl[j], srt=90, adj=1, cex=1.2, col=clr[j])}
      if(Z.rot[j,mm[i]] < -minZ) {text(j, 0.03, ylbl[j], srt=90, adj=0, cex=1.2, col=clr[j])}
      abline(h=0, lwd=1.5, col="gray")
      } 
  mtext(paste("Factor loadings on trend",i),side=3,line=0.5)
}

fpf1 = file.path(project.figuredirectory('bio.lobster'))
savePlot(file.path('~/tmp','CanSGSLUSTempLoadings.png'))

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
spp = rownames(cRR)
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
d <- augment(the.fit, interval="confidence")
d$t = tt
d0 = subset(d,.rownames %in% c ("23","24","25","261"))
p0 = ggplot(data = d0) +
geom_line(aes(t, .fitted)) +
#geom_point(aes(t, y)) +
geom_ribbon(aes(x=t, ymin=.conf.low, ymax=.conf.up), linetype=2, alpha=0.2) +
facet_grid(~.rownames) +
xlab("Year") + ylab("Detrended Temperature")

d1 = subset(d,.rownames %in% c ('262',"27","29","30"))
p1 = ggplot(data = d1) +
geom_line(aes(t, .fitted)) +
#geom_point(aes(t, y)) +
geom_ribbon(aes(x=t, ymin=.conf.low, ymax=.conf.up), linetype=2, alpha=0.2) +
facet_grid(~.rownames) +
xlab("Year") + ylab("Detrended Temperature")

d2 = subset(d,.rownames %in% c ("311","312","32","33"))
p2 = ggplot(data = d2) +
geom_line(aes(t, .fitted)) +
#geom_point(aes(t, y)) +
geom_ribbon(aes(x=t, ymin=.conf.low, ymax=.conf.up), linetype=2, alpha=0.2) +
facet_grid(~.rownames) +
xlab("Year") + ylab("Detrended Temperature")

d3 = subset(d,.rownames %in% c ("34","35","36","38"))
p3 = ggplot(data = d3) +
geom_line(aes(t, .fitted)) +
#geom_point(aes(t, y)) +
geom_ribbon(aes(x=t, ymin=.conf.low, ymax=.conf.up), linetype=2, alpha=0.2) +
facet_grid(~.rownames) +
xlab("Year") + ylab("Detrended Temperature")



d4 = subset(d,.rownames %in% c ("511","512","513","514"))
p4 = ggplot(data = d4) +
geom_line(aes(t, .fitted)) +
#geom_point(aes(t, y)) +
geom_ribbon(aes(x=t, ymin=.conf.low, ymax=.conf.up), linetype=2, alpha=0.2) +
facet_grid(~.rownames) +
xlab("Year") + ylab("Standardized Landings")



require(ggpubr)
ggarrange(p0,p1,p2,p3,p4, ncol=2,nrow=3)


