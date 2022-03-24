#DFA using MARSS on survey data
require(bio.lobster)
require(bio.utilities)
require(MARSS)

#FULL DATA

cR = read.csv('C:/Users/Cooka/Downloads/PCA.ess_2021.csv')
cR = cR[,c(-17,-18,-19,-20)] #removing some extra columns that were in the csv but are  not needed

rownames(cR) = cR$YEAR
cR$YEAR <- NULL

cR = t(cR)

cRR = zscore(as.matrix(cR))
spp = rownames(cRR)
par(mfcol=c(3,5), mar=c(3,4,1.5,0.5), oma=c(0.4,1,1,1))
for(i in spp){
  plot(cRR[i,],xlab="",ylab="Index", bty="L", xaxt="n", pch=16, col="blue", type="b")
  title(i)
  }


N.ts = nrow(cRR)
nT = ncol(cRR)


cntl.list = list(minit=200, maxit=5000, allow.degen=FALSE)
find.best=T
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
write.csv(model.data,'C:/Users/Cooka/Documents/tmp/shrimp.model.data.csv')
}

big.maxit.cntl.list = list(minit=200, maxit=30000, allow.degen=FALSE)
model.list = list(m=3, R="diagonal and unequal",Q = "diagonal and equal")
the.fit = MARSS(cRR, model=model.list, form="dfa", control=big.maxit.cntl.list)

# get the inverse of the rotation matrix
Z.est = coef(the.fit, type="matrix")$Z


H.inv = 1
if(ncol(Z.est)>1) H.inv = varimax(coef(the.fit, type="matrix")$Z)$rotmat

clr <- c("brown","blue","darkgreen")
ylbl = row.names(cRR)
# rotate factor loadings
Z.rot = Z.est %*% H.inv
# rotate trends
trends.rot = solve(H.inv) %*% the.fit$states
tt = 1995:2021
mm=3


#plot states and loadings

layout(matrix(c(1,2,3),mm,2),widths=c(2,1))
 par(mfcol=c(mm,2), mai=c(0.5,0.5,0.5,0.1), omi=c(0,0,0,0))
#par(mai=c(0.5,0.5,0.5,0.1), omi=c(0,0,0,0))
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

fpf1 = file.path("C:/Users/Cooka/Documents/tmp/")
savePlot(file.path(fpf1,'DFAShrimpDFAStates.png'))

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
d <- fitted(the.fit, interval="confidence")
d$t = rep(1995:2021,times=15)
d1 = d
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
##############################lets try it as a state space model to look at factors effect on 



require(bio.lobster)
require(bio.utilities)
require(MARSS)

#FULL DATA

cR = read.csv('C:/Users/Cooka/Downloads/PCA.ess_2021.csv')
cR = cR[,c(-17,-18,-19,-20)] #removing some extra columns that were in the csv but are  not needed

rownames(cR) = cR$YEAR
cR$YEAR <- NULL

