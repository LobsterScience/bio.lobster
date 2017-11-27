#' @export
FSRSmodel<-function(FSRS,response="SHORTS",redo=T,interaction=F, theta =1){
	
  lfa = ifelse(!is.na(unique(FSRS$subarea)),unique(FSRS$subarea),unique(FSRS$LFA))

  fn.root =  file.path( project.datadirectory('bio.lobster'), "R", "FSRS", "ModelResults")
  # create factor year
  FSRS$fYEAR=as.factor(FSRS$SYEAR)

  # create log traps
  FSRS$logTRAPS=log(FSRS$TOTAL_TRAPS)
  
  # create factor vessel
  FSRS$VESSEL=as.factor(FSRS$VESSEL_CD)

  # remove temp nas
  FSRS$TEMP[FSRS$TEMP==-99]<-NA
 
  FSRS<-subset(FSRS,!is.na(TEMP)&TEMP>(-5))


  #Do a NB GLMM using glmer
  library(lme4,MASS)
  if(response=="SHORTS"){
    if(redo){
      print(Sys.time())
#browser()
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
    }
    else {
      load(file.path( fn.root, paste0(lfa,response,"glmm.rdata")))
    }
    
    return(output)
  }
  
  if(response=="LEGALS"){
    if(redo){
      print(Sys.time())
      browser()
      if(interaction==F)L <- glmer(LEGALS ~ fYEAR + DOS + TEMP + offset(logTRAPS) + (1 | VESSEL), family = MASS::negative.binomial(theta = theta), data = FSRS)
      if(interaction==T)L <- glmer(LEGALS ~ fYEAR + DOS + TEMP + DOS*TEMP + offset(logTRAPS) + (1 | VESSEL), family = MASS::negative.binomial(theta = theta), data = FSRS)
      print(Sys.time())
      print(summary(L))
      
      pData<-with(L@frame,data.frame(fYEAR=sort(unique(fYEAR)),TEMP= mean(TEMP),logTRAPS= log(2),DOS= mean(DOS)))
      P <- predict(L, newdata = pData, se = TRUE)
      #P <- confint(L, newdata = pData, method = "Wald")
      pData$YEAR <- as.numeric(as.character(pData$fYEAR))
      	pData$mu <- exp(P$fit)
      		#pData$ub <- exp(P$fit + 1.96 * P$se.fit)
      		#pData$lb <- exp(P$fit - 1.96 * P$se.fit)
      		#print(Sys.time())
      	
      		output<-list(model=L,pData=pData)
      		save( output, file=file.path( fn.root, paste0(lfa,response,"glmm.rdata")), compress=T)
      	}
      	else {
      		load(file.path( fn.root, paste0(lfa,response,"glmm.rdata")))
       	}
      	return(output)
      }
      
      
      } 
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
	#Do a NB GLMM using glmmadmb
#	library(glmmADMB)

#	if(response=="SHORTS"){
#		if(redo){
#			print(Sys.time())
#			if(interaction==F)S <- glmmadmb(SHORTS ~ fYEAR + LEGALS + TEMP + offset(logTRAPS) + (1 | VESSEL), family = "nbinom", data = FSRS,save.dir="tmp")
#			if(interaction==T)S <- glmmadmb(SHORTS ~ fYEAR + LEGALS + TEMP + LEGALS*TEMP + offset(logTRAPS) + (1 | VESSEL), family = "nbinom", data = FSRS,save.dir="tmp")
#			print(Sys.time())
#			print(summary(S))
#			pData<-with(S$frame,data.frame(fYEAR=sort(unique(fYEAR)),TEMP= mean(TEMP),logTRAPS= log(2),LEGALS= mean(LEGALS)))
#			P <- predict(S, newdata = pData, se = TRUE)
#			pData$YEAR <- as.numeric(as.character(pData$fYEAR))
#			pData$mu <- exp(P$fit)
#			pData$ub <- exp(P$fit + 1.96 * P$se.fit)
#			pData$lb <- exp(P$fit - 1.96 * P$se.fit)
#			print(Sys.time())
#			S$LFA=lfa
#			output<-list(model=S,pData=pData)
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
#			if(interaction==F)L <- glmmadmb(LEGALS ~ fYEAR + DOS + TEMP + offset(logTRAPS) + (1 | VESSEL), family = "nbinom", data = FSRS,save.dir="tmp")
#			if(interaction==T)L <- glmmadmb(LEGALS ~ fYEAR + DOS + TEMP + DOS*TEMP + offset(logTRAPS) + (1 | VESSEL), family = "nbinom", data = FSRS,save.dir="tmp")
#			print(Sys.time())
#			print(summary(L))
#			pData<-with(L$frame,data.frame(fYEAR=sort(unique(fYEAR)),TEMP= mean(TEMP),logTRAPS= log(2),DOS= mean(DOS)))
#			P <- predict(L, newdata = pData, se = TRUE)
#			pData$YEAR <- as.numeric(as.character(pData$fYEAR))
	#		pData$mu <- exp(P$fit)
	#		pData$ub <- exp(P$fit + 1.96 * P$se.fit)
	#		pData$lb <- exp(P$fit - 1.96 * P$se.fit)
	#		print(Sys.time())
	#		L$LFA=lfa
	#		output<-list(model=L,pData=pData)
	#		save( output, file=file.path( fn.root, paste0(lfa,response,"glmm.rdata")), compress=T)
	#	}
	#	else {
	#		load(file.path( fn.root, paste0(lfa,response,"glmm.rdata")))
	#	}
	#	return(output)
	#}


#}