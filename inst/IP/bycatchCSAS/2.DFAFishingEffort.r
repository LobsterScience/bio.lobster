############################################################################################################################        
#Describing the fishery from these logbooks
#LFA by LFA

require(bio.lobster)
require(bio.utilities)
require(RODBC)
require(lubridate)
require(devtools)
options(stringAsFactors=F)
la()
  require(foreach)
  require(parallel)
 require(doParallel)


wd = ('C:/Users/CookA/Desktop/dellshared/Bycatch in the Lobster Fishery')
setwd(wd)

#bycatch.db(DS='odbc.redo',wd=wd)

#LOADING AND MERGING LOGBOOKS with TARGETS
    #bycatch.db(DS='logbook.merge.redo',wd=wd)

        b = bycatch.db('logbook.merge',wd=wd) 

        
  bW = aggregate(cbind(WEIGHT_KG,NUM_OF_TRAPS)~WOS+GRID_NUM,data=subset(b,LFA==34),FUN=sum)    
  bE = reshape(bW[,c('WOS','GRID_NUM','NUM_OF_TRAPS')],idvar='WOS',timevar = 'GRID_NUM',direction='wide')
  bE = na.zero(bE)   
  bE$WOS = NULL
        
  j = c(apply(bE,2,FUN=function(x) length(which(x>0))))
  j = which(j>15)
  
  cR = t(log(bE[,j]+0.01))
  bER = zscore(as.matrix(cR))
  gN = rownames(bER)
  
  N.ts = nrow(bER)
  nT = ncol(bER)
  
 N.tsr=8
 registerDoParallel(10)
 
 foreach(i=1:10,.combine='rbind') %dopar%{
   require(MARSS)
   cntl.list = list(minit=200, maxit=5000, allow.degen=FALSE)
   dfa.model = list(A="zero", R='diagonal and equal',Q='diagonal and equal', m=i)
   ff = MARSS(bER, model=dfa.model, control=cntl.list,
              form="dfa", z.score=TRUE)
      data.frame(m=m,logLik=ff$logLik,K=ff$num.params,AICc=ff$AICc,stringsAsFactors=FALSE)
 }
 stopImplicitCluster()
 
 
    levels.R = c("diagonal and equal")#,"diagonal and unequal")#,"equalvarcov","unconstrained")
    levels.Q = c("diagonal and equal")#,"diagonal and unequal")
    #model.data = data.frame()
    for(R in levels.R) {
      for(Q in levels.Q){
        for(m in 3:(N.tsr)) {
        } # end m loop
      } # end Q loop
    } # end R loop
    #best model is diag and unequal R and Q with three trends
    write.csv(model.data,'results/Landings34DFAmodel.data.csv')
    

  
  big.maxit.cntl.list = list(minit=200, maxit=30000, allow.degen=FALSE)
  model.list = list(m=3, R="diagonal and equal",Q = "diagonal and equal")
  the.fit = MARSS(bER, model=model.list, form="dfa", control=big.maxit.cntl.list)
  
  
  # get the inverse of the rotation matrix
  Z.est = coef(the.fit, type="matrix")$Z
  
  
  H.inv = 1
  if(ncol(Z.est)>1) H.inv = varimax(coef(the.fit, type="matrix")$Z)$rotmat
  
  clr <- c("brown","blue","darkgreen","darkred")
  ylbl = row.names(bER)
  spp = ylbl = unlist(strsplit(ylbl,'NUM_OF_TRAPS.'))[seq(2,19,2)]
  row.names(bER) = spp
  # rotate factor loadings
  Z.rot = Z.est %*% H.inv
  # rotate trends
  trends.rot = solve(H.inv) %*% the.fit$states
  tt = 1:nT
  mm=3
  
  
  #plot states and loadings
  
  layout(matrix(c(1,2,3),mm,2),widths=c(2,1))
  #par(mfcol=c(mm,2), mar=c(0.7,.5,.2,.1), omi=c(0,0,0,0))
  par(mfcol=c(mm,2),mai=c(0.5,0.5,0.5,0.1), omi=c(0,0,0,0))
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
  minZ <- 0.01
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
  d$t = rep(1:ncol(bER),times=9)
  p1 = ggplot(data = d) +
    geom_line(aes(t, .fitted)) +
    geom_point(aes(t, y)) +
    geom_ribbon(aes(x=t, ymin=.conf.low, ymax=.conf.up), linetype=2, alpha=0.2) +
    facet_grid(~.rownames) +
    xlab("Year") + ylab("Standardized Abundance")
  
  require(ggpubr)
  ggarrange(p1,p2,ncol=1,nrow=2)
  savePlot(file.path(fpf1,'FitsDFALFA34.png'))
  
