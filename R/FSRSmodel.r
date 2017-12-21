#' @export
FSRSmodel<-function(FSRS,response="SHORTS",redo=T,interaction=F, theta =1){
	
  lfa = ifelse(!is.na(unique(FSRS$subarea)),unique(FSRS$subarea),unique(FSRS$LFA))

  fn.root =  file.path( project.datadirectory('bio.lobster'), "R", "FSRS", "ModelResults")
  # create factor year
  FSRS$fYEAR=as.factor(FSRS$SYEAR)

  # create log traps
  FSRS$logTRAPS=log(FSRS$TOTAL_TRAPS)
  FSRS$LEGALS=as.numeric(FSRS$LEGALS)
  
  # create factor vessel
  FSRS$VESSEL=as.factor(FSRS$VESSEL_CD)

  # remove temp nas
  FSRS$TEMP[FSRS$TEMP==-99]=NA
 
  FSRS=subset(FSRS,!is.na(TEMP)&TEMP>(-5))



  #Do a NB GLMM using glmer
  require(MASS)

  if(response=="SHORTS"){
    if(redo){

      if(type=="base"){
        if(interaction==F)  S = glm.nb(SHORTS ~ fYEAR + LEGALS + TEMP + offset(logTRAPS), data = FSRS)
        if(interaction==T)  S = glm.nb(SHORTS ~ fYEAR + LEGALS + TEMP + LEGALS*TEMP + offset(logTRAPS), data = FSRS)

        print(summary(S))
        
        pData=with(FSRS,data.frame(fYEAR=sort(unique(fYEAR)),TEMP= pTemp,logTRAPS= log(1),LEGALS= pLegals))
        P = predict(S, newdata = pData, type = 'response',se.fit=T)

        pData$YEAR = as.numeric(as.character(pData$fYEAR))
        pData$mu = P$fit
        pData$ub = P$fit + 1.96 * P$se.fit
        pData$lb = P$fit - 1.96 * P$se.fit
      }

      if(type=="mixed"){
        require(lme4)
        if(interaction==F)  S = glmer(SHORTS ~ fYEAR + LEGALS + TEMP + (1 | VESSEL), offset=logTRAPS,family = MASS::negative.binomial(theta = S$theta), data = FSRS)
        if(interaction==T)  S = glmer(SHORTS ~ fYEAR + LEGALS + TEMP + LEGALS*TEMP + (1 | VESSEL), offset=logTRAPS,family = MASS::negative.binomial(theta = S$theta), data = FSRS)
        pData=with(FSRS,data.frame(fYEAR=sort(unique(fYEAR)),TEMP= pTemp,LEGALS= pLegals))#, VESSEL = unique(VESSEL)[1]))
        P = predict(S, newdata = pData, type = 'response',re.form=NA)
        pData$YEAR = as.numeric(as.character(pData$fYEAR))
        pData$mu = P
      }

      if(type=="bayesian"){
        require(rstanarm)
        options(mc.cores = parallel::detectCores())

        if(interaction==F)  S = stan_glm.nb(SHORTS ~ fYEAR + LEGALS + TEMP, offset=logTRAPS, data = FSRS,iter=iter)
        if(interaction==T)  S = stan_glm.nb(SHORTS ~ fYEAR + LEGALS + TEMP + LEGALS*TEMP , offset=logTRAPS, data = FSRS)
        pData=with(FSRS,data.frame(fYEAR=sort(unique(fYEAR)),TEMP= pTemp,LEGALS= pLegals))#, VESSEL = unique(VESSEL)[1]))
        P = posterior_predict(S, newdata = pData,offset=rep(log(1),nrow(pData)))
        x = apply(P,2,quantile,quants)
        pData$YEAR = as.numeric(as.character(pData$fYEAR))
        pData$median = x[2,]
        pData$q1 = x[1,]
        pData$q2 = x[3,]
        pData$mu = apply(P,2,mean)
        pData$ub = pData$mu + apply(P,2,sd)
        pData$lb = pData$mu - apply(P,2,sd)


      }

      output=list(model=S,pData=pData)

      save( output, file=file.path( fn.root, paste0(lfa,response,type,"glm.rdata")), compress=T)
      print(Sys.time())
browser()
      if(interaction==F)S <- glmer(SHORTS ~ fYEAR + LEGALS + TEMP + offset(logTRAPS) + (1 | VESSEL), family = MASS::negative.binomial(theta = theta), data = FSRS)
      if(interaction==T)S <- glmer(SHORTS ~ fYEAR + LEGALS + TEMP + LEGALS*TEMP + offset(logTRAPS) + (1 | VESSEL),family = MASS::negative.binomial(theta = theta), data = FSRS)
      print(Sys.time())
      print(summary(S))
    
    pData<-with(S@frame,data.frame(fYEAR=sort(unique(fYEAR)),TEMP= mean(TEMP),logTRAPS= log(2),LEGALS= mean(LEGALS), VESSEL = unique(VESSEL)[1]))
    P <- predict(S, newdata = pData, type = 'response')
   
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
    else {
      load(file.path( fn.root, paste0(lfa,response,type,"glm.rdata")))
    }
    
    return(output)
  
  if(response=="LEGALS"){
    if(redo){
      #browser()
      if(type=="base"){
        if(interaction==F)L = glm.nb(LEGALS ~ fYEAR + DOS + TEMP + offset(logTRAPS), data = FSRS)
        if(interaction==T)L = glm.nb(LEGALS ~ fYEAR + DOS + TEMP + DOS*TEMP + offset(logTRAPS), data = FSRS)
       
        print(summary(L))
        
        pData=with(FSRS,data.frame(fYEAR=sort(unique(fYEAR)), TEMP= pTemp, logTRAPS=log(1), DOS=round(max(DOS)*pDos)))
        P = predict(L, newdata = pData, type = 'response',se.fit=T)

        pData$YEAR = as.numeric(as.character(pData$fYEAR))
        pData$mu = P$fit
        pData$ub = P$fit + 1.96 * P$se.fit
        pData$lb = P$fit - 1.96 * P$se.fit
      }

      if(type=="mixed"){
        if(interaction==F)  L = glmer(LEGALS ~ fYEAR + DOS + TEMP + (1 | VESSEL), offset=logTRAPS,family = MASS::negative.binomial(theta = L$theta), data = FSRS)
        if(interaction==T)  L = glmer(LEGALS ~ fYEAR + DOS + TEMP + DOS*TEMP + (1 | VESSEL), offset=logTRAPS,family = MASS::negative.binomial(theta = L$theta), data = FSRS)
        pData=with(FSRS,data.frame(fYEAR=sort(unique(fYEAR)),TEMP= pTemp, DOS=round(max(DOS)*pDos)))#, VESSEL = unique(VESSEL)[1]))
        P = predict(L, newdata = pData, type = 'response',re.form=NA)
        pData$YEAR = as.numeric(as.character(pData$fYEAR))
        pData$mu = P
      }

      if(type=="bayesian"){
        require(rstanarm)
        options(mc.cores = parallel::detectCores())

        if(interaction==F)  L = stan_glm.nb(LEGALS ~ fYEAR + DOS + TEMP, offset=logTRAPS, data = FSRS,iter=iter)
        if(interaction==T)  L = stan_glm.nb(LEGALS ~ fYEAR + DOS + TEMP + DOS*TEMP , offset=logTRAPS, data = FSRS)
        pData=with(FSRS,data.frame(fYEAR=sort(unique(fYEAR)),TEMP= pTemp,DOS= round(max(DOS)*pDos)))#, VESSEL = unique(VESSEL)[1]))
        P = posterior_predict(L, newdata = pData,offset=rep(log(1),nrow(pData)))
        x = apply(P,2,quantile,quants)
        pData$YEAR = as.numeric(as.character(pData$fYEAR))
        pData$median = x[2,]
        pData$q1 = x[1,]
        pData$q2 = x[3,]
        pData$mu = apply(P,2,mean)
        pData$ub = pData$mu + apply(P,2,sd)
        pData$lb = pData$mu - apply(P,2,sd)


      }


      output=list(model=L,pData=pData)

      save( output, file=file.path( fn.root, paste0(lfa,response,type,"glm.rdata")), compress=T)
    }
    else {
      load(file.path( fn.root, paste0(lfa,response,type,"glm.rdata")))
    }
    
    return(output)
  }
    if(response == "RECRUITS"){
    if(redo){

      if(type=="base"){
        if(interaction==F)  S = glm.nb(RECRUITS ~ fYEAR + LEGALS + TEMP + offset(logTRAPS), data = FSRS)
        if(interaction==T)  S = glm.nb(RECRUITS ~ fYEAR + LEGALS + TEMP + LEGALS*TEMP + offset(logTRAPS), data = FSRS)

        print(summary(S))
        
        pData=with(FSRS,data.frame(fYEAR=sort(unique(fYEAR)),TEMP= pTemp,logTRAPS= log(1),LEGALS= pLegals))
        P = predict(S, newdata = pData, type = 'response',se.fit=T)

        pData$YEAR = as.numeric(as.character(pData$fYEAR))
        pData$mu = P$fit
        pData$ub = P$fit + 1.96 * P$se.fit
        pData$lb = P$fit - 1.96 * P$se.fit
      }

      if(type=="mixed"){
        require(lme4)
        if(interaction==F)  S = glmer(RECRUITS ~ fYEAR + LEGALS + TEMP + (1 | VESSEL), offset=logTRAPS,family = MASS::negative.binomial(theta = S$theta), data = FSRS)
        if(interaction==T)  S = glmer(RECRUITS ~ fYEAR + LEGALS + TEMP + LEGALS*TEMP + (1 | VESSEL), offset=logTRAPS,family = MASS::negative.binomial(theta = S$theta), data = FSRS)
        pData=with(FSRS,data.frame(fYEAR=sort(unique(fYEAR)),TEMP= pTemp,LEGALS= pLegals))#, VESSEL = unique(VESSEL)[1]))
        P = predict(S, newdata = pData, type = 'response',re.form=NA)
        pData$YEAR = as.numeric(as.character(pData$fYEAR))
        pData$mu = P
      }

      if(type=="bayesian"){
        require(rstanarm)
        options(mc.cores = parallel::detectCores())

        if(interaction==F)  S = stan_glm.nb(RECRUITS ~ fYEAR + LEGALS + TEMP, offset=logTRAPS, data = FSRS,iter=iter)
        if(interaction==T)  S = stan_glm.nb(RECRUITS ~ fYEAR + LEGALS + TEMP + LEGALS*TEMP , offset=logTRAPS, data = FSRS)
        pData=with(FSRS,data.frame(fYEAR=sort(unique(fYEAR)),TEMP= pTemp,LEGALS= pLegals))#, VESSEL = unique(VESSEL)[1]))
        P = posterior_predict(S, newdata = pData,offset=rep(log(1),nrow(pData)))
        x = apply(P,2,quantile,quants)
        pData$YEAR = as.numeric(as.character(pData$fYEAR))
        pData$median = x[2,]
        pData$q1 = x[1,]
        pData$q2 = x[3,]
        pData$mu = apply(P,2,mean)
        pData$ub = pData$mu + apply(P,2,sd)
        pData$lb = pData$mu - apply(P,2,sd)


      }

      output=list(model=S,pData=pData)

      save( output, file=file.path( fn.root, paste0(lfa,response,type,"glm.rdata")), compress=T)
    }
    else {
      load(file.path( fn.root, paste0(lfa,response,type,"glm.rdata")))
    }
    
    return(output)


    }

      
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
	#Do a NB GLMM using glmmadmb
#	library(glmmADMB)

#	if(response=="RECRUITS"){
#		if(redo){
#			print(Sys.time())
#			if(interaction==F)S = glmmadmb(SHORTS ~ fYEAR + LEGALS + TEMP + offset(logTRAPS) + (1 | VESSEL), family = "nbinom", data = FSRS,save.dir="tmp")
#			if(interaction==T)S = glmmadmb(SHORTS ~ fYEAR + LEGALS + TEMP + LEGALS*TEMP + offset(logTRAPS) + (1 | VESSEL), family = "nbinom", data = FSRS,save.dir="tmp")
#			print(Sys.time())
#			print(summary(S))
#			pData=with(S$frame,data.frame(fYEAR=sort(unique(fYEAR)),TEMP= mean(TEMP),logTRAPS= log(2),LEGALS= mean(LEGALS)))
#			P = predict(S, newdata = pData, se = TRUE)
#			pData$YEAR = as.numeric(as.character(pData$fYEAR))
#			pData$mu = exp(P$fit)
#			pData$ub = exp(P$fit + 1.96 * P$se.fit)
#			pData$lb = exp(P$fit - 1.96 * P$se.fit)
#			print(Sys.time())
#			S$LFA=lfa
#			output=list(model=S,pData=pData)
#			save( output, file=file.path( fn.root, paste0(lfa,response,"glmm.rdata")), compress=T)
#		}
#		else {
#			load(file.path( fn.root, paste0(lfa,response,"glmm.rdata")))
#		}

#		return(output)
#	}

#	if(response=="LEGALS"){
#		if(redo){
#			print(Sys.time())
#			if(interaction==F)L = glmmadmb(LEGALS ~ fYEAR + DOS + TEMP + offset(logTRAPS) + (1 | VESSEL), family = "nbinom", data = FSRS,save.dir="tmp")
#			if(interaction==T)L = glmmadmb(LEGALS ~ fYEAR + DOS + TEMP + DOS*TEMP + offset(logTRAPS) + (1 | VESSEL), family = "nbinom", data = FSRS,save.dir="tmp")
#			print(Sys.time())
#			print(summary(L))
#			pData=with(L$frame,data.frame(fYEAR=sort(unique(fYEAR)),TEMP= mean(TEMP),logTRAPS= log(2),DOS= mean(DOS)))
#			P = predict(L, newdata = pData, se = TRUE)
#			pData$YEAR = as.numeric(as.character(pData$fYEAR))
	#		pData$mu = exp(P$fit)
	#		pData$ub = exp(P$fit + 1.96 * P$se.fit)
	#		pData$lb = exp(P$fit - 1.96 * P$se.fit)
	#		print(Sys.time())
	#		L$LFA=lfa
	#		output=list(model=L,pData=pData)
	#		save( output, file=file.path( fn.root, paste0(lfa,response,"glmm.rdata")), compress=T)
	#	}
	#	else {
	#		load(file.path( fn.root, paste0(lfa,response,"glmm.rdata")))
	#	}
	#	return(output)
	#}


#}
