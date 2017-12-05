#' @export
moltPrModel = function(p,redo.dd=T){

	tagging.data = read.csv(file.path(project.datadirectory('bio.lobster'),'data','inputs','Tagging','tagging.csv'))
	tagging.data$TagDate = as.Date(tagging.data$TagDate)
	tagging.data$CapDate = as.Date(tagging.data$CapDate)
	
	if(redo.dd){

		if(!"TempModel"%in%names(p)) {
			TempModelling = TempModel()
			p$TempModel = TempModelling$Model
		}

		degreedays=c()
		cat('|-')
		for(i in 1:nrow(tagging.data)){
			p$Depth = ifelse(tagging.data$z[i]<100,tagging.data$z[i],100)
			p$Area = tagging.data$subarea[i]
			degreedays[i] = sum(TempModelPredict(p, start=tagging.data$TagDate[i], end=tagging.data$CapDate[i]))
			if(i %in% (floor(nrow(tagging.data)/20)*1:20))cat('-')
		}
		cat('-|')

		tagging.data$degreedays = degreedays

		write.csv(tagging.data,file.path(project.datadirectory('bio.lobster'),'data','inputs','Tagging','tagging.csv'),row.names=F)

	}
	tagging.data$CL = tagging.data$TagCL

	moltPrModel = glm(Molted ~ degreedays + CL , data = tagging.data, family = binomial(link = "logit"))
	print(summary(moltPrModel))

	tagging.data$MoltProb = moltPrModel$fitted.values

	moltincr.data = subset(tagging.data,Molted==1&MoltProb<0.9)

	moltIncrModel = glm(CapCL ~ CL + offset(CL), data = moltincr.data)
	moltIncrModel = glm(MoltIncr ~ CL , data = moltincr.data, family=gaussian(link='log'))
	print(summary(moltIncrModel))

	#lens = seq(50,200,5)
	#P = predict(moltIncrModel,newdata=data.frame(CL=lens),type='response',se.fit=T)
#
	#with(moltincr.data,plot(CL,MoltIncr),ylim=c(0,0.6))
	#lines(lens,P$fit,col='red',lwd=2)
	#lines(lens,P$fit + P$se.fit * sqrt(nrow(moltincr.data)),col='red',lty=2)
	#lines(lens,P$fit - P$se.fit * sqrt(nrow(moltincr.data)),col='red',lty=2)

    #pData$ub <- exp(P$fit + 1.96 * P$se.fit)
      #pData$lb <- exp(P$fit - 1.96 * P$se.fit)
 

	return(list(moltPrModel=moltPrModel,moltIncrModel=moltIncrModel))
}