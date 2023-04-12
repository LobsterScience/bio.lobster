#' @export
moltModel = function(p,redo.dd=T, redo=F,outdir =file.path(project.datadirectory('bio.lobster'),'analysis','Tagging') ){
if(redo){
    require(rstanarm)
	tagging.data = read.csv(file.path(outdir,'tagging.csv'))
	tagging.data$TagDate = as.Date(tagging.data$TagDate)
	tagging.data$CapDate = as.Date(tagging.data$CapDate)
	tagging.data$TagQ = quarter(tagging.data$TagDate)
	tagging.data$CapQ = quarter(tagging.data$CapDate)

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

		write.csv(tagging.data,file.path(outdir,'tagging.csv'),row.names=F)

	}
	tagging.data$CL = tagging.data$TagCL
  tagging.data$days = tagging.data$CapDate - tagging.data$TagDate
	moltPrModel = glm(Molted ~ degreedays + CL , data = tagging.data, family = binomial(link = "logit"))

	print(summary(moltPrModel))

	tagging.data$MoltProb = moltPrModel$fitted.values

	malemoltincr.data = subset(tagging.data,Molted==1&MoltProb<0.9&TagSex==1&CL>50)
	femalemoltincr.data = subset(tagging.data,Molted==1&MoltProb<0.9&TagSex%in%2:3&CL>50)

	maleMoltIncrModel = stan_glm(log(SizeDiff) ~ CL , data = malemoltincr.data, family=gaussian(link='identity'),iter=20000)
	femaleMoltIncrModel = stan_glm(log(SizeDiff) ~ CL , data = femalemoltincr.data, family=gaussian(link='identity'),iter=20000)
 	print(summary(maleMoltIncrModel))
 	print(summary(femaleMoltIncrModel))
 	ou = list(moltPrModel=moltPrModel,maleMoltIncrModel=maleMoltIncrModel,femaleMoltIncrModel=femaleMoltIncrModel,tagging.data=tagging.data,malemoltincr.data=malemoltincr.data,femalemoltincr.data=femalemoltincr.data)
  saveRDS(ou, file=file.path(outdir,'moltModel_moltIncre.rds'))

}
  ou = readRDS(file=file.path(outdir,'moltModel_moltIncre.rds'))

	return(ou)
}
