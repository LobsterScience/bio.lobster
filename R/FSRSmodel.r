#' @export
FSRSmodel<-function(FSRS,response="SHORTS",redo=T,interaction=F){
	
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


	#Do a NB GLMM using glmmadmb
	library(glmmADMB)

	if(response=="SHORTS"){
		if(redo){
			print(Sys.time())
			if(interaction==F)S <- glmmadmb(SHORTS ~ fYEAR + LEGALS + TEMP + offset(logTRAPS) + (1 | VESSEL), family = "nbinom", data = FSRS,save.dir="tmp")
			if(interaction==T)S <- glmmadmb(SHORTS ~ fYEAR + LEGALS + TEMP + LEGALS*TEMP + offset(logTRAPS) + (1 | VESSEL), family = "nbinom", data = FSRS,save.dir="tmp")
			print(Sys.time())
			print(summary(S))
			pData<-with(S$frame,data.frame(fYEAR=sort(unique(fYEAR)),TEMP= mean(TEMP),logTRAPS= log(2),LEGALS= mean(LEGALS)))
			P <- predict(S, newdata = pData, se = TRUE)
			pData$YEAR <- as.numeric(as.character(pData$fYEAR))
			pData$mu <- exp(P$fit)
			pData$ub <- exp(P$fit + 1.96 * P$se.fit)
			pData$lb <- exp(P$fit - 1.96 * P$se.fit)
			print(Sys.time())
			S$LFA=lfa
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
			if(interaction==F)L <- glmmadmb(LEGALS ~ fYEAR + DOS + TEMP + offset(logTRAPS) + (1 | VESSEL), family = "nbinom", data = FSRS,save.dir="tmp")
			if(interaction==T)L <- glmmadmb(LEGALS ~ fYEAR + DOS + TEMP + DOS*TEMP + offset(logTRAPS) + (1 | VESSEL), family = "nbinom", data = FSRS,save.dir="tmp")
			print(Sys.time())
			print(summary(L))
			pData<-with(L$frame,data.frame(fYEAR=sort(unique(fYEAR)),TEMP= mean(TEMP),logTRAPS= log(2),DOS= mean(DOS)))
			P <- predict(L, newdata = pData, se = TRUE)
			pData$YEAR <- as.numeric(as.character(pData$fYEAR))
			pData$mu <- exp(P$fit)
			pData$ub <- exp(P$fit + 1.96 * P$se.fit)
			pData$lb <- exp(P$fit - 1.96 * P$se.fit)
			print(Sys.time())
			L$LFA=lfa
			output<-list(model=L,pData=pData)
			save( output, file=file.path( fn.root, paste0(lfa,response,"glmm.rdata")), compress=T)
		}
		else {
			load(file.path( fn.root, paste0(lfa,response,"glmm.rdata")))
		}
		return(output)
	}


}