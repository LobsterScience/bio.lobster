moltPrModel = function(p){

	tagging.data = read.csv(file.path(project.datadirectory('bio.lobster'),'data','inputs','Tagging','tagging.csv'))
	

	moltPrModel = glm(Molted ~ degreedays + TagCL , data = tagging.data, family = binomial(link = "logit"))
	print(summary(moltPrModel))

	return(moltPrModel)
}