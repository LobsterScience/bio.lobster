#' @export
TempModelPlot = function(TempModel,graphic='png',depths=c(5,25,50,100),wd=10,ht=6,...){

	if(graphic=='png')png(file.path(project.datadirectory('bio.lobster'),'figures','TempModel.png'),width=wd,height=ht,units='in',res=200)
	if(graphic=='R')x11()

	y =	seq(min(TempModel$Data$y),max(TempModel$Data$y),0.01)

	t = list()

	for(i in 1:length(depths)){

		t[[i]]=predict(TempModel$Model,data.frame(y=y,cos.y=cos(2*pi*y),sin.y=sin(2*pi*y),DEPTH=depths[i],subarea='33W'),type='response')


	}

	 
	 plot(TEMPERATURE~y,data=TempModel$Data,pch='.',col=rgb(0,0,0,0.1),...)
	 for(i in 1:length(depths))lines(y,t[[i]],col=i+1)

	 if(graphic!="R")dev.off()


	if(graphic=='png')png(file.path(project.datadirectory('bio.lobster'),'figures','TempDataMap.png'))
	if(graphic=='R')x11()
	LobsterMap('27-33')
	with(TempModel$Data,points(X,Y,pch='.',col=rgb(1,0,0,0.1)))
	 if(graphic!="R")dev.off()

	

}
	