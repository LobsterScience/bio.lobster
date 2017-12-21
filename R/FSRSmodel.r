#' @export
FSRSmodel=function(FSRS,response="SHORTS",redo=T,interaction=F,type="base" ,tag='', pTemp=7, pLegals=3, pDos=0.5,quants=c(0.25,0.5,0.75),iter=2000,ptraps=100){
	
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

    if(type=="base"){
      if(redo){
        if(interaction==F)  S = glm.nb(SHORTS ~ fYEAR + LEGALS + TEMP + offset(logTRAPS), data = FSRS)
        if(interaction==T)  S = glm.nb(SHORTS ~ fYEAR + LEGALS + TEMP + LEGALS*TEMP + offset(logTRAPS), data = FSRS)
        save( S, file=file.path( fn.root, paste0(lfa,response,type,tag,"glm.rdata")), compress=T)
      }
      else load(file.path( fn.root, paste0(lfa,response,type,tag,"glm.rdata")))
      print(summary(S))
      
      pData=with(FSRS,data.frame(fYEAR=sort(unique(fYEAR)),TEMP= pTemp,logTRAPS= log(1),LEGALS= pLegals))
      P = predict(S, newdata = pData, type = 'response',se.fit=T)

      pData$YEAR = as.numeric(as.character(pData$fYEAR))
      pData$mu = P$fit
      pData$ub = P$fit + 1.96 * P$se.fit
      pData$lb = P$fit - 1.96 * P$se.fit
    }

    if(type=="mixed"){
      if(redo){
        require(lme4)
        if(interaction==F)  S = glmer(SHORTS ~ fYEAR + LEGALS + TEMP + (1 | VESSEL), offset=logTRAPS,family = MASS::negative.binomial(theta = S$theta), data = FSRS)
        if(interaction==T)  S = glmer(SHORTS ~ fYEAR + LEGALS + TEMP + LEGALS*TEMP + (1 | VESSEL), offset=logTRAPS,family = MASS::negative.binomial(theta = S$theta), data = FSRS)
        save( S, file=file.path( fn.root, paste0(lfa,response,type,tag,"glm.rdata")), compress=T)
      }
      else load(file.path( fn.root, paste0(lfa,response,type,tag,"glm.rdata")))
      print(summary(S))
      
      pData=with(FSRS,data.frame(fYEAR=sort(unique(fYEAR)),TEMP= pTemp,LEGALS= pLegals))#, VESSEL = unique(VESSEL)[1]))
      P = predict(S, newdata = pData, type = 'response',re.form=NA)
      pData$YEAR = as.numeric(as.character(pData$fYEAR))
      pData$mu = P
    }

    if(type=="bayesian"){
      require(rstanarm)
      if(redo){
        options(mc.cores = parallel::detectCores())
        if(interaction==F)  S = stan_glm.nb(SHORTS ~ fYEAR + LEGALS + TEMP, offset=logTRAPS, data = FSRS,iter=iter)
        if(interaction==T)  S = stan_glm.nb(SHORTS ~ fYEAR + LEGALS + TEMP + LEGALS*TEMP , offset=logTRAPS, data = FSRS)
        save( S, file=file.path( fn.root, paste0(lfa,response,type,tag,"glm.rdata")), compress=T)
      }
      else load(file.path( fn.root, paste0(lfa,response,type,tag,"glm.rdata")))
      print(summary(S))
      
      pData=with(FSRS,data.frame(fYEAR=sort(unique(fYEAR)),TEMP= pTemp,LEGALS= pLegals))#, VESSEL = unique(VESSEL)[1]))
      P = posterior_predict(S, newdata = pData,offset=rep(log(ptraps),nrow(pData)))
      x = apply(P,2,quantile,quants)/ptraps
      pData$YEAR = as.numeric(as.character(pData$fYEAR))
      pData$median = x[2,]
      pData$lb = x[1,]
      pData$ub = x[3,]
      pData$mu = apply(P,2,mean)/ptraps
      pData$sd =apply(P,2,sd)/ptraps


    }

    output=list(model=S,pData=pData)

  }
  
  if(response=="LEGALS"){
    #browser()
    if(type=="base"){
      if(redo){
        if(interaction==F)L = glm.nb(LEGALS ~ fYEAR + DOS + TEMP + offset(logTRAPS), data = FSRS)
        if(interaction==T)L = glm.nb(LEGALS ~ fYEAR + DOS + TEMP + DOS*TEMP + offset(logTRAPS), data = FSRS)
        save( L, file=file.path( fn.root, paste0(lfa,response,type,tag,"glm.rdata")), compress=T)
      }
      else load(file.path( fn.root, paste0(lfa,response,type,tag,"glm.rdata")))
      print(summary(L))
    
      
      pData=with(FSRS,data.frame(fYEAR=sort(unique(fYEAR)), TEMP= pTemp, logTRAPS=log(1), DOS=round(max(DOS)*pDos)))
      P = predict(L, newdata = pData, type = 'response',se.fit=T)

      pData$YEAR = as.numeric(as.character(pData$fYEAR))
      pData$mu = P$fit
      pData$ub = P$fit + 1.96 * P$se.fit
      pData$lb = P$fit - 1.96 * P$se.fit
    }

    if(type=="mixed"){
      if(redo){
        if(interaction==F)  L = glmer(LEGALS ~ fYEAR + DOS + TEMP + (1 | VESSEL), offset=logTRAPS,family = MASS::negative.binomial(theta = L$theta), data = FSRS)
        if(interaction==T)  L = glmer(LEGALS ~ fYEAR + DOS + TEMP + DOS*TEMP + (1 | VESSEL), offset=logTRAPS,family = MASS::negative.binomial(theta = L$theta), data = FSRS)
        save( L, file=file.path( fn.root, paste0(lfa,response,type,tag,"glm.rdata")), compress=T)
      }
      else load(file.path( fn.root, paste0(lfa,response,type,tag,"glm.rdata")))
      print(summary(L))

      pData=with(FSRS,data.frame(fYEAR=sort(unique(fYEAR)),TEMP= pTemp, DOS=round(max(DOS)*pDos)))#, VESSEL = unique(VESSEL)[1]))
      P = predict(L, newdata = pData, type = 'response',re.form=NA)
      pData$YEAR = as.numeric(as.character(pData$fYEAR))
      pData$mu = P
    }

    if(type=="bayesian"){
      require(rstanarm)
      if(redo){
        options(mc.cores = parallel::detectCores())

        if(interaction==F)  L = stan_glm.nb(LEGALS ~ fYEAR + DOS + TEMP, offset=logTRAPS, data = FSRS,iter=iter)
        if(interaction==T)  L = stan_glm.nb(LEGALS ~ fYEAR + DOS + TEMP + DOS*TEMP , offset=logTRAPS, data = FSRS)
        save( L, file=file.path( fn.root, paste0(lfa,response,type,tag,"glm.rdata")), compress=T)
      }
      else load(file.path( fn.root, paste0(lfa,response,type,tag,"glm.rdata")))
      print(summary(L))

      pData=with(FSRS,data.frame(fYEAR=sort(unique(fYEAR)),TEMP= pTemp,DOS= round(max(DOS)*pDos)))#, VESSEL = unique(VESSEL)[1]))
      P = posterior_predict(L, newdata = pData,offset=rep(log(ptraps),nrow(pData)))
      x = apply(P,2,quantile,quants)/ptraps
      pData$YEAR = as.numeric(as.character(pData$fYEAR))
      pData$median = x[2,]
      pData$lb = x[1,]
      pData$ub = x[3,]
      pData$mu = apply(P,2,mean)/ptraps
      pData$sd =apply(P,2,sd)/ptraps


    }


    output=list(model=L,pData=pData)
    
  }
  if(response == "RECRUITS"){

    if(type=="base"){
      if(redo){
        if(interaction==F)  R = glm.nb(RECRUITS ~ fYEAR + LEGALS + TEMP + offset(logTRAPS), data = FSRS)
        if(interaction==T)  R = glm.nb(RECRUITS ~ fYEAR + LEGALS + TEMP + LEGALS*TEMP + offset(logTRAPS), data = FSRS)
        save( R, file=file.path( fn.root, paste0(lfa,response,type,tag,"glm.rdata")), compress=T)
      }
      else load(file.path( fn.root, paste0(lfa,response,type,tag,"glm.rdata")))
      print(summary(R))
      
      pData=with(FSRS,data.frame(fYEAR=sort(unique(fYEAR)),TEMP= pTemp,logTRAPS= log(1),LEGALS= pLegals))
      P = predict(R, newdata = pData, type = 'response',se.fit=T)

      pData$YEAR = as.numeric(as.character(pData$fYEAR))
      pData$mu = P$fit
      pData$ub = P$fit + 1.96 * P$se.fit
      pData$lb = P$fit - 1.96 * P$se.fit
    }

    if(type=="mixed"){
      if(redo){
        require(lme4)
        if(interaction==F)  R = glmer(RECRUITS ~ fYEAR + LEGALS + TEMP + (1 | VESSEL), offset=logTRAPS,family = MASS::negative.binomial(theta = S$theta), data = FSRS)
        if(interaction==T)  R = glmer(RECRUITS ~ fYEAR + LEGALS + TEMP + LEGALS*TEMP + (1 | VESSEL), offset=logTRAPS,family = MASS::negative.binomial(theta = S$theta), data = FSRS)
        save( R, file=file.path( fn.root, paste0(lfa,response,type,tag,"glm.rdata")), compress=T)
      }
      else load(file.path( fn.root, paste0(lfa,response,type,tag,"glm.rdata")))
      print(summary(R))
      pData=with(FSRS,data.frame(fYEAR=sort(unique(fYEAR)),TEMP= pTemp,LEGALS= pLegals))#, VESSEL = unique(VESSEL)[1]))
      P = predict(R, newdata = pData, type = 'response',re.form=NA)
      pData$YEAR = as.numeric(as.character(pData$fYEAR))
      pData$mu = P
    }

    if(type=="bayesian"){
      require(rstanarm)
      if(redo){
        options(mc.cores = parallel::detectCores())

        if(interaction==F)  R = stan_glm.nb(RECRUITS ~ fYEAR + LEGALS + TEMP, offset=logTRAPS, data = FSRS,iter=iter)
        if(interaction==T)  R = stan_glm.nb(RECRUITS ~ fYEAR + LEGALS + TEMP + LEGALS*TEMP , offset=logTRAPS, data = FSRS)
        save( R, file=file.path( fn.root, paste0(lfa,response,tag,type,"glm.rdata")), compress=T)
      }
      else load(file.path( fn.root, paste0(lfa,response,type,tag,"glm.rdata")))
      print(summary(R))

      pData=with(FSRS,data.frame(fYEAR=sort(unique(fYEAR)),TEMP= pTemp,LEGALS= pLegals))#, VESSEL = unique(VESSEL)[1]))
      P = posterior_predict(R, newdata = pData,offset=rep(log(ptraps),nrow(pData)))
      x = apply(P,2,quantile,quants)/ptraps
      pData$YEAR = as.numeric(as.character(pData$fYEAR))
      pData$median = x[2,]
      pData$lb = x[1,]
      pData$ub = x[3,]
      pData$mu = apply(P,2,mean)/ptraps
      pData$sd =apply(P,2,sd)/ptraps


    }

    output=list(model=R,pData=pData)

  }

  print(paste(lfa,response,type))

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