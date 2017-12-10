#' @export
CPUEModelPlot = function(CPUEModelResults, TempModelling, lfa, graphic='R', wd=8, ht=11, se=T, path=file.path(project.figuredirectory("bio.lobster"),"figures"), ...){
	
  if(missing(lfa))lfa = names(CPUEModelResults)


  CPUEModelResults = CPUEModelResults[which(names(CPUEModelResults)%in%lfa)]
  pData.list = list()

  if(graphic=='pdf')pdf(file.path( path,paste0("CPUE",lab,".pdf")),width=wd,height=ht)
  if(graphic=='R')x11(width=wd,height=ht)

  par(mfrow=c(length(lfa),1),mar=c(0,0,0,0),omi=c(0.5,1,0.5,0.5),las=1)

  for(i in 1:length(lfa)){

    M = CPUEModelResults[[ lfa[i] ]]$model
    M$data$CPUE = M$data$TOTAL_WEIGHT_KG/M$data$NUM_OF_TRAPS
    MD = subset(M$data,!duplicated(y))
    MD = MD[order(MD$y),]
    D = median(M$data$DEPTH)
    Temp = predict(TempModelling$Model, newdata = data.frame(y=MD$y, cos.y=cos(2*pi*MD$y), sin.y=sin(2*pi*MD$y), DEPTH=D, area=lfa[i]), type='response')

    pData=data.frame(DOS=MD$DOS,TEMP= Temp,logTRAPS=log(1), fYEAR=MD$fYEAR)
    PM = predict(M, newdata = pData, type = 'response',se.fit=T)

 
      pData$YEAR = as.numeric(as.character(pData$fYEAR))
      pData$mu = exp(PM$fit)
      pData$ub = exp(PM$fit + 1.96 * PM$se.fit)
      pData$lb = exp(PM$fit - 1.96 * PM$se.fit)
      pData$y = MD$y
      pData$LFA = lfa[i]

      pData = merge(pData,data.frame(y=seq(min(pData$YEAR)+0.6,max(pData$YEAR)+0.6,1)),all=T)


    plot(CPUE~y,M$data,pch=16,cex=0.2,col=rgb(0,0,0,0.1),...)
    lines(mu~y,pData,lwd=2,col='red')
    if(se){
      lines(ub~y,pData,lty=2,col='red')
      lines(lb~y,pData,lty=2,col='red')
    }
    mtext(paste("LFA",lfa[i]),adj=0.95,line=-4,cex=1.5)

    pData.list[[i]] = pData

   }
   if(graphic!='R')dev.off()

   return(do.call("rbind",pData.list))

 }
