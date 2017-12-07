#' @export
CPUEmodel<-function(CPUE,redo=T,interaction=F, theta =1){
	
  lfa = ifelse(!is.na(unique(CPUE$subarea)),unique(CPUE$subarea),unique(CPUE$LFA))

  fn.root =  file.path( project.datadirectory('bio.lobster'), "R", "CPUE", "ModelResults")
  # create factor year
  CPUE$fYEAR=as.factor(CPUE$SYEAR)


  CPUE = na.omit(CPUE)
  CPUE = subset(CPUE,TOTAL_WEIGHT_KG>0)

  # create log traps
  CPUE$logTRAPS=log(CPUE$NUM_OF_TRAPS)


  
 
  library(rstanarm)
  options(mc.cores = parallel::detectCores())

  if(redo){
      print(Sys.time())
#browser()
      S <- stan_glm(log(TOTAL_WEIGHT_KG) ~ fYEAR + DOS + TEMP , offset= NUM_OF_TRAPS, data = CPUE)
      #S2 <- glmer(SHORTS ~ fYEAR + LEGALS + TEMP + offset(logTRAPS) + (1 | VESSEL),family = MASS::negative.binomial(theta = 1), data = CPUE)
      print(Sys.time())
      print(summary(S))
    
    pData<-with(S$data,data.frame(fYEAR=sort(unique(fYEAR)),TEMP= mean(TEMP),logTRAPS= log(2),LEGALS= mean(LEGALS), VESSEL = unique(VESSEL)[1]))
    P <- posterior_predict(S, newdata = pData)
 

    pData<-with(S2@frame,data.frame(fYEAR=sort(unique(fYEAR)),TEMP= mean(TEMP),logTRAPS= log(2),LEGALS= mean(LEGALS), VESSEL = unique(VESSEL)[1]))
    P2 <- predict(S2, newdata = pData, type = 'response')
  
    #confint(S)
    #PI <- predictInterval(S, newdata = pData, which = 'fixed', type='linear.prediction', stat = 'median')
    #PI.arm.sims = arm::sim(S,1000)
    #PI.arm <- data.frame(  
    #  fit=apply(fitted(PI.arm.sims, S), 1, function(x) quantile(x, 0.500)),
    #  upr=apply(fitted(PI.arm.sims, S), 1, function(x) quantile(x, 0.975)),
    #  lwr=apply(fitted(PI.arm.sims, S), 1, function(x) quantile(x, 0.025)))

   #boots<- lme4::bootMer(S, function(x) predict(x, newdata = pData, re.form = NULL, type = 'response'), nsim = 10,use.u=T,parallel = 'multicore', ncpus = 4,type='parametric')
   # 
   #data.frame(fit = apply(boot$t, 2, function(x) mean(x)),
   #           lower = apply(boot$t, 2, function(x) as.numeric(quantile(x, probs=.05, na.rm=TRUE))),
   #           upper = apply(boot$t, 2, function(x) as.numeric(quantile(x, probs=.95, na.rm=TRUE)))) -> PI.boot
   
    
      #P <- confint(S, newdata = pData, method = "Wald")
      pData$YEAR <- as.numeric(as.character(pData$fYEAR))
      pData$mu <- P
      #pData$ub <- exp(P$fit + 1.96 * P$se.fit)
      #pData$lb <- exp(P$fit - 1.96 * P$se.fit)
      print(Sys.time())
      output<-list(model=S,pData=pData)
      save( output, file=file.path( fn.root, paste0(lfa,response,"glmm.rdata")), compress=T)
    }
    else {
      load(file.path( fn.root, paste0(lfa,response,"glmm.rdata")))
    }
    
    return(output)
  }
