#DFA using MARSS on survey data
require(bio.lobster)
require(bio.utilities)
require(MARSS)

#recruit survey trends

cR = read.csv(file.path(project.datadirectory('bio.lobster'),'analysis','LFA34-38','indicators','recruitAbund34Combined.csv'))
names(cR) = c('YR','NSpr','NFal','ILTS','DFO','FSRS','SPA3','SFA29','Landings')
cR = subset(cR,YR>1995)
cR$Landings <- NULL
xx = sample(2:8,7)
xx = c(8,5,7,6,3,4,2)
cR = t(log(cR[,xx]+0.01))

cRR = zscore(as.matrix(cR))
spp = rownames(cRR)
par(mfcol=c(3,3), mar=c(3,4,1.5,0.5), oma=c(0.4,1,1,1))
for(i in spp){
  plot(cRR[i,],xlab="",ylab="Abundance index", bty="L", xaxt="n", pch=16, col="blue", type="b")
  title(i)
  }


N.ts = nrow(cRR)
nT = ncol(cRR)

#seven time series three trends
#z12, z13 and z23 need to be set to 0 for identifiability
example=F
if(example){
							Z.vals = list(
							"z11", 0 , 0 ,
							"z21","z22", 0 ,
							"z31","z32","z33",
							"z41","z42","z43",#"z44",
							"z51","z52","z53",#"z54","z55",
							"z61","z62","z63",#"z64","z65","z66",
							"z71","z72","z73",#"z74","z75","z76","z77")
							"z81","z82","z83")

							Z = matrix(Z.vals, nrow=N.ts, ncol=3, byrow=TRUE)

							#
							Q = B = diag(1,3)

							#indep var no covar
							R.vals = list(
							"r11",0,0,0,0,0,0,0,
							0,"r22",0,0,0,0,0,0,
							0,0,"r33",0,0,0,0,0,
							0,0,0,"r44",0,0,0,0,
							0,0,0,0,"r55",0,0,0,
							0,0,0,0,0,"r66",0,0,
							0,0,0,0,0,0,"r77",0,
							0,0,0,0,0,0,0,"r88"
							)

							R = matrix(R.vals, nrow=N.ts, ncol=N.ts, byrow=TRUE)
							x0 = U = A = "zero"
							V0 = diag(5,3)

							dfa.model = list(Z=Z, A="zero", R=R, B=B, U=U, Q=Q, x0=x0, V0=V0)
							cntl.list = list(maxit=5000)
							fit1 = MARSS(cRR, model=dfa.model, control=cntl.list)

							par(mfcol=c(3,3), mar=c(3,4,1.5,0.5), oma=c(0.4,1,1,1))
							for(i in 1:length(spp)){
							        plot(cRR[i,],xlab="",ylab="abundance index",bty="L", xaxt="n", ylim=c(-4,3), pch=16, col="blue")
							        par.mat=coef(fit1,type="matrix")
							        lines(as.vector(par.mat$Z[i,,drop=FALSE]%*%fit1$states+par.mat$A[i,]), lwd=2)
							        title(spp[i])
							        }
}
##many model comparisons

cntl.list = list(minit=200, maxit=5000, allow.degen=FALSE)
find.best=F
if(find.best){
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
#best model is diag and unequal R and Q with three trends
write.csv(model.data,'~/tmp/model.data.csv')

}
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
tt = 1996:2017
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

fpf1 = file.path(project.figuredirectory('bio.lobster'),"LFA3438Framework2019")
savePlot(file.path(fpf1,'LFA34RecruitDFAStates.png'))

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
d$t = rep(1996:2017,times=7)
d1 = subset(d,.rownames %in% c ("SFA29","NFal","NSpr","SPA3"))
p1 = ggplot(data = d1) +
geom_line(aes(t, .fitted)) +
geom_point(aes(t, y)) +
geom_ribbon(aes(x=t, ymin=.conf.low, ymax=.conf.up), linetype=2, alpha=0.2) +
facet_grid(~.rownames) +
xlab("Year") + ylab("Standardized Abundance")

d2 = subset(d,.rownames %in% c ("ILTS","DFO","FSRS","Landings"))
p2 = ggplot(data = d2) +
geom_line(aes(t, .fitted)) +
geom_point(aes(t, y)) +
geom_ribbon(aes(x=t, ymin=.conf.low, ymax=.conf.up), linetype=2, alpha=0.2) +
facet_grid(~.rownames) +
xlab("Year") + ylab("Standardized Abundance")
require(ggpubr)
ggarrange(p1,p2,ncol=1,nrow=2)
savePlot(file.path(fpf1,'FitsDFALFA34.png'))
##
##
##
cR = read.csv('~/tmp/recruitAbund.csv')
cR = subset(cR,select=c(YR, landings))

cR = subset(cR,YR>1995)
cR = (zscore(t(cR[,2])))

cor.test(cR[1,],trends.rot[1,])##-0.004
cor.test(cR[1,],trends.rot[2,])##0.886 p <<0.001

plot(cR[1,],trends.rot[2,],xlab='Landings',ylab='Trend 2', pch=16)##0.886 p <<0.001
savePlot(file.path(fpf1,'Trend2vLand.png'))



########################################################################################################################################
##total abundance

require(bio.lobster)
require(bio.utilities)
require(MARSS)

#total abund

cR = read.csv(file.path(project.datadirectory('bio.lobster'),'analysis','LFA34-38','indicators','TotalAbund34Combined.csv'))
xx = sample(2:7,6)
cR = t(log(cR[,xx]+0.01))

cRR = zscore(as.matrix(cR))
spp = rownames(cRR)
par(mfcol=c(3,3), mar=c(3,4,1.5,0.5), oma=c(0.4,1,1,1))
for(i in spp){
  plot(cRR[i,],xlab="",ylab="Abundance index", bty="L", xaxt="n", pch=16, col="blue", type="b")
  title(i)
  }


N.ts = nrow(cRR)
nT = ncol(cRR)


							dfa.model = list( A="zero", R='diagonal and unequal', m=3)
							cntl.list = list(maxit=5000)
							fit1 = MARSS(cRR, model=dfa.model, control=cntl.list, form='dfa', z.score=T)

							par(mfcol=c(3,3), mar=c(3,4,1.5,0.5), oma=c(0.4,1,1,1))
							for(i in 1:length(spp)){
							        plot(cRR[i,],xlab="",ylab="abundance index",bty="L", xaxt="n", ylim=c(-4,3), pch=16, col="blue")
							        par.mat=coef(fit1,type="matrix")
							        lines(as.vector(par.mat$Z[i,,drop=FALSE]%*%fit1$states+par.mat$A[i,]), lwd=2)
							        title(spp[i])
							        }

##many model comparisons

cntl.list = list(minit=200, maxit=5000, allow.degen=FALSE)
find.best=F
if(find.best){
# set up forms of R matrices
levels.R = c("diagonal and equal","diagonal and unequal","equalvarcov","unconstrained")
#levels.Q = c("diagonal and equal","diagonal and unequal")
model.data = data.frame()
	for(R in levels.R) {
		for(m in 1:(N.ts-1)) {
				dfa.model = list(A="zero", R=R, m=m)
				ff = MARSS(cRR, model=dfa.model, control=cntl.list,
						form="dfa", z.score=TRUE)
		
				model.data = rbind(model.data,
							data.frame(R=R,m=m,logLik=ff$logLik,K=ff$num.params,AICc=ff$AICc,stringsAsFactors=FALSE))
				assign(paste("ff", m, R, sep="."), ff)
		} # end m loop
	} # end R loop

#best model is diag and unequal R and Q with three trends
write.csv(model.data,'~/tmp/model.data.totabund.csv')

}
big.maxit.cntl.list = list(minit=200, maxit=30000, allow.degen=FALSE)
model.list = list(m=2, R="diagonal and unequal")
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
tt = 1969:2018
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

fpf1 = file.path(project.figuredirectory('bio.lobster'),"LFA3438Framework2019")
savePlot(file.path(fpf1,'LFA34totDFAStates.png'))

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
d$t = rep(1969:2018,times=6)
d1 = subset(d,.rownames %in% c ("Nfal","Nspr","SPA3"))
p1 = ggplot(data = d1) +
geom_line(aes(t, .fitted)) +
geom_point(aes(t, y)) +
geom_ribbon(aes(x=t, ymin=.conf.low, ymax=.conf.up), linetype=2, alpha=0.2) +
facet_grid(~.rownames) +
xlab("Year") + ylab("Standardized Abundance")

d2 = subset(d,.rownames %in% c ("ILTS","DFO","SFA29"))
p2 = ggplot(data = d2) +
geom_line(aes(t, .fitted)) +
geom_point(aes(t, y)) +
geom_ribbon(aes(x=t, ymin=.conf.low, ymax=.conf.up), linetype=2, alpha=0.2) +
facet_grid(~.rownames) +
xlab("Year") + ylab("Standardized Abundance")
require(ggpubr)
ggarrange(p1,p2,ncol=1,nrow=2)
savePlot(file.path(fpf1,'FitstotalDFALFA34.png'))
##
###################################################
#temperature


########################################################################################################################################
##total abundance

require(bio.lobster)
require(bio.utilities)
require(MARSS)

#total abund

cR = read.csv(file.path(project.datadirectory('bio.lobster'),'analysis','LFA34-38','indicators','TemperatureL34Combined.csv'))
xx = sample(2:4,3)
cR = t(log(cR[,xx]+0.01))

cRR = zscore(as.matrix(cR))
spp = rownames(cRR)
par(mfcol=c(2,2), mar=c(3,4,1.5,0.5), oma=c(0.4,1,1,1))
for(i in spp){
  plot(cRR[i,],xlab="",ylab="Bottom Temperature", bty="L", xaxt="n", pch=16, col="blue", type="b")
  title(i)
  }


N.ts = nrow(cRR)
nT = ncol(cRR)

##many model comparisons

cntl.list = list(minit=200, maxit=5000, allow.degen=FALSE)
find.best=F
if(find.best){
# set up forms of R matrices
levels.R = c("diagonal and equal","diagonal and unequal","equalvarcov","unconstrained")
#levels.Q = c("diagonal and equal","diagonal and unequal")
model.data = data.frame()
	for(R in levels.R) {
		for(m in 1:(N.ts-1)) {
				dfa.model = list(A="zero", R=R, m=m)
				ff = MARSS(cRR, model=dfa.model, control=cntl.list,
						form="dfa", z.score=TRUE)
		
				model.data = rbind(model.data,
							data.frame(R=R,m=m,logLik=ff$logLik,K=ff$num.params,AICc=ff$AICc,stringsAsFactors=FALSE))
				assign(paste("ff", m, R, sep="."), ff)
		} # end m loop
	} # end R loop

#best model is diag and unequal R and Q with three trends
write.csv(model.data,'~/tmp/model.data.temperature.csv')

}
big.maxit.cntl.list = list(minit=200, maxit=30000, allow.degen=FALSE)
model.list = list(m=1, R="equalvarcov")
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
tt = 1969:2018
mm=1


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

fpf1 = file.path(project.figuredirectory('bio.lobster'),"LFA3438Framework2019")
savePlot(file.path(fpf1,'LFA34tempDFAStates.png'))

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
d$t = rep(1969:2018,times=3)
d1 = subset(d,.rownames %in% c ("Nfal","NSpr","DFO"))
 ggplot(data = d1) +
geom_line(aes(t, .fitted)) +
geom_point(aes(t, y)) +
geom_ribbon(aes(x=t, ymin=.conf.low, ymax=.conf.up), linetype=2, alpha=0.2) +
facet_grid(~.rownames) +
xlab("Year") + ylab("Temperature")

savePlot(file.path(fpf1,'FitstempDFALFA34.png'))



###recruitment and temperature 1996:2017
ccf(atemp[27:48],diff(trends.rot[1,]))
atemp = (diff(trends.rot[1,],1))
##

###########################
###commercial
###############


require(bio.lobster)
require(bio.utilities)
require(MARSS)

#total abund

cR = read.csv(file.path(project.datadirectory('bio.lobster'),'analysis','LFA34-38','indicators','AllComm.csv'))
cR$FSRS = NULL
cR = t(log(cR[,2:5]+0.01))

cRR = zscore(as.matrix(cR))
spp = rownames(cRR)
par(mfcol=c(2,3), mar=c(3,4,1.5,0.5), oma=c(0.4,1,1,1))
for(i in spp){
  plot(cRR[i,],xlab="",ylab="Commerical Biomass", bty="L", xaxt="n", pch=16, col="blue", type="b")
  title(i)
  }


N.ts = nrow(cRR)
nT = ncol(cRR)

##many model comparisons

cntl.list = list(minit=200, maxit=5000, allow.degen=FALSE)
find.best=T
if(find.best){
# set up forms of R matrices
levels.R = c("diagonal and equal","diagonal and unequal","equalvarcov","unconstrained")
#levels.Q = c("diagonal and equal","diagonal and unequal")
model.data = data.frame()
  for(R in levels.R) {
    for(m in 1:(N.ts-1)) {
        dfa.model = list(A="zero", R=R, m=m)
        ff = MARSS(cRR, model=dfa.model, control=cntl.list,
            form="dfa", z.score=TRUE)
    
        model.data = rbind(model.data,
              data.frame(R=R,m=m,logLik=ff$logLik,K=ff$num.params,AICc=ff$AICc,stringsAsFactors=FALSE))
        assign(paste("ff", m, R, sep="."), ff)
    } # end m loop
  } # end R loop

#best model is diag and unequal R and Q with three trends
write.csv(model.data,'~/tmp/model.data.commercial.csv')
}

big.maxit.cntl.list = list(minit=200, maxit=30000, allow.degen=FALSE)
model.list = list(m=2, R="equalvarcov")
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
tt = 1969:2018
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

fpf1 = file.path(project.figuredirectory('bio.lobster'),"LFA3438Framework2019")
savePlot(file.path(fpf1,'LFA34CommDFAStates.png'))

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
d$t = rep(1969:2018,times=4)

 ggplot(data = d) +
geom_line(aes(t, .fitted)) +
geom_point(aes(t, y)) +
geom_ribbon(aes(x=t, ymin=.conf.low, ymax=.conf.up), linetype=2, alpha=0.2) +
facet_grid(~.rownames) +
xlab("Year") + ylab("Commercial Biomass")

savePlot(file.path(fpf1,'FitsCommDFALFA34.png'))

##########################
######rel f ccir
##


require(bio.lobster)
require(bio.utilities)
require(MARSS)


cR = read.csv(file.path(project.datadirectory('bio.lobster'),'analysis','LFA34-38','indicators','AllRelF.csv'))
cR = cR[,-1]
cR = t(log(cR[,2:6]+0.01))

cRR = zscore(as.matrix(cR))
spp = rownames(cRR)
par(mfcol=c(2,3), mar=c(3,4,1.5,0.5), oma=c(0.4,1,1,1))
for(i in spp){
  plot(cRR[i,],xlab="",ylab="Relative F / Exploitation Index", bty="L", xaxt="n", pch=16, col="blue", type="b")
  title(i)
  }


N.ts = nrow(cRR)
nT = ncol(cRR)

##many model comparisons

cntl.list = list(minit=200, maxit=5000, allow.degen=FALSE)
find.best=T
if(find.best){
# set up forms of R matrices
levels.R = c("diagonal and equal","diagonal and unequal","equalvarcov","unconstrained")
#levels.Q = c("diagonal and equal","diagonal and unequal")
model.data = data.frame()
  for(R in levels.R) {
    for(m in 1:(N.ts-1)) {
        dfa.model = list(A="zero", R=R, m=m)
        ff = MARSS(cRR, model=dfa.model, control=cntl.list,
            form="dfa", z.score=TRUE)
    
        model.data = rbind(model.data,
              data.frame(R=R,m=m,logLik=ff$logLik,K=ff$num.params,AICc=ff$AICc,stringsAsFactors=FALSE))
        assign(paste("ff", m, R, sep="."), ff)
    } # end m loop
  } # end R loop

#best model is diag and unequal R and Q with three trends
write.csv(model.data,'~/tmp/model.data.relf.csv')
}

big.maxit.cntl.list = list(minit=200, maxit=30000, allow.degen=FALSE)
model.list = list(m=1, R="diagonal and equal")
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
tt = 1970:2018
mm=1


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

fpf1 = file.path(project.figuredirectory('bio.lobster'),"LFA3438Framework2019")
savePlot(file.path(fpf1,'LFA34RELFDFAStates.png'))

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
d$t = rep(1970:2018,times=5)

 ggplot(data = d) +
geom_line(aes(t, .fitted)) +
geom_point(aes(t, y)) +
geom_ribbon(aes(x=t, ymin=.conf.low, ymax=.conf.up), linetype=2, alpha=0.2) +
facet_grid(~.rownames) +
xlab("Year") + ylab("RelF / Exploitation Index")

savePlot(file.path(fpf1,'FitsRelfDFALFA34.png'))
