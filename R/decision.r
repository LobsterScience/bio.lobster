decision<-function(area,model.out,mu=0.15,pr=seq(0.1,0.6,0.1),vers=1,post.survey.C=0,index,refs='varied'){
		
	require(xtable)
	if(missing(index))index<-1:length(model.out$median$B)

	# Ref Points
	if(refs[1]!='varied')RatioURP<-sweep(model.out$sims.list$B.p,1,FUN='/',refs[1])
	if(refs[1]=='varied')RatioURP<-sweep(model.out$sims.list$B.p,1,FUN='/',(apply(model.out$sims.list$B[,index],1,mean)*0.8))
	PrHealth<-sapply(1:model.out$data$NC,function(x){sum(RatioURP[,x]>1)/nrow(RatioURP)})

	if(refs[1]!='varied')RatioLRP<-sweep(model.out$sims.list$B.p,1,FUN='/',refs[2])
	if(refs[1]=='varied')RatioLRP<-sweep(model.out$sims.list$B.p,1,FUN='/',(apply(model.out$sims.list$B[,index],1,mean)*0.3))
	PrCrit<-sapply(1:model.out$data$NC,function(x){sum(RatioLRP[,x]>1)/nrow(RatioLRP)})

	# Ref Points
	if(refs[1]=='varied'){
		print(paste("URP =",round(mean(model.out$median$B)*0.8)))
		RatioURP.current<-model.out$sims.list$B[,model.out$data$NY]/(apply(model.out$sims.list$B[,index],1,mean)*0.8)
	}
	if(refs[1]!='varied')RatioURP.current<-model.out$sims.list$B[,model.out$data$NY]/refs[1]
	PrHealth.current<-sum(RatioURP.current>1)/length(RatioURP.current)
	print(PrHealth.current)

	if(refs[1]=='varied'){
		print(paste("LRP =",round(mean(model.out$median$B)*0.3)))
		RatioLRP.current<-model.out$sims.list$B[,model.out$data$NY]/(apply(model.out$sims.list$B[,index],1,mean)*0.3)
	}
	if(refs[1]!='varied')RatioLRP.current<-model.out$sims.list$B[,model.out$data$NY]/refs[2]
	PrCrit.current<-sum(RatioLRP.current>1)/length(RatioLRP.current)
	print(PrCrit.current)

#browser()
		# Decision table
	if(vers==1)tab1<-cbind(data.frame(Catch=model.out$data$C.p-post.survey.C,mu=model.out$median$mu.p,B.change=model.out$median$B.change,p.decline=model.out$mean$pB0,Pr.above.URP=PrHealth,Pr.above.LRP=PrCrit),t(sapply(1:length(model.out$data$C.p),function(i){quantile(mu*model.out$sims.list$B.p2[,i],pr)})))

	if(vers==2)tab1<-data.frame(Catch=model.out$data$C.p-post.survey.C,mu=model.out$median$mu.p,B.change=model.out$median$B.change,p.decline=model.out$mean$pB0,Pr.above.URP=PrHealth,Pr.above.LRP=PrCrit)
	tabtex <- xtable(tab1)
	print(tabtex, type='latex', file=paste('Decision',area,'tex',sep='.'), include.rownames=F)
	tab1
}

		