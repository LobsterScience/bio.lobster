#' @export
CPUEmodel=function(CPUE,redo=T,interaction=F, dos=1){
	
  lfa = ifelse(!is.na(unique(CPUE$subarea)),unique(CPUE$subarea),unique(CPUE$LFA))

  fn.root =  file.path( project.datadirectory('bio.lobster'), "R", "CPUE", "ModelResults")
  
  if(redo){
    
    # create factor year
    CPUE$fYEAR=as.factor(CPUE$SYEAR)

    # filter out NAs and zero catches
    CPUE = na.omit(CPUE)
    CPUE = subset(CPUE,TOTAL_WEIGHT_KG>0)

    # create log traps
    CPUE$logTRAPS=log(CPUE$NUM_OF_TRAPS)
  

    if(interaction==F)G = glm(log(TOTAL_WEIGHT_KG) ~ fYEAR + DOS + TEMP , offset= logTRAPS, family=gaussian(link='identity'),data = CPUE)
    if(interaction==T)G = glm(log(TOTAL_WEIGHT_KG) ~ fYEAR + DOS + TEMP + DOS * TEMP , offset= logTRAPS, family=gaussian(link='identity'),data = CPUE)



    pData=with(G$data,data.frame(fYEAR=sort(unique(fYEAR)),TEMP= mean(TEMP),DOS=dos,logTRAPS=log(1)))
    PG = predict(G, newdata = pData, type = 'response',se.fit=T)
  
      pData$YEAR = as.numeric(as.character(pData$fYEAR))
      pData$mu = exp(PG$fit)
      pData$ub = exp(PG$fit + 1.96 * PG$se.fit)
      pData$lb = exp(PG$fit - 1.96 * PG$se.fit)
      output = list(model=G,pData=pData)
      save( output, file=file.path( fn.root, paste0(lfa,"glm.rdata")), compress=T)
    }
    else {
      load(file.path( fn.root, paste0(lfa,"glm.rdata")))
    }
    
    return(output)
  }
