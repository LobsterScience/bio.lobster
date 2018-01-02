#' @export
TempModelPlot = function(TempModel,graphic='R',depths=c(5,25,50,100),wd=12,ht=5,Area="33W",colors=tim.colors(length(Area)),...){

	for(a in 1:length(Area)){

		if(graphic=='png')png(file.path(project.datadirectory('bio.lobster'),'figures',paste0('TempModel',Area[a],'.png')),width=wd,height=ht,units='in',res=200)
		if(graphic=='R')x11( width=wd, height=ht)

		y =	seq(min(TempModel$Data$y),max(TempModel$Data$y),0.01)


		plot(TEMPERATURE~y,data=TempModel$Data,pch='.',col=rgb(0.5,0.5,0.5,0.05),xlab='',main=Area[a],...)
	

		points(TEMPERATURE~y,data=subset(TempModel$Data,area==Area[a]),pch=16,cex=0.2,col=scales::alpha(colors[a],0.2))

		for(i in 1:length(depths)){

			t=predict(TempModel$Model,data.frame(y=y,cos.y=cos(2*pi*y),sin.y=sin(2*pi*y),DEPTH=depths[i],area=Area[a]),type='response')
			lines(y,t,lty=i+1,col=colors[a])

		}
		legend('topright',legend=depths,lty=1:length(depths)+1,col=colors[a],bty='n',title="Depth (m)")
		if(graphic!="R")dev.off()

	}



	if(graphic=='png')png(file.path(project.datadirectory('bio.lobster'),'figures','TempDataMap.png'))
	if(graphic=='R')x11()
	LobsterMap('27-33')
	with(TempModel$Data,points(X,Y,pch='.',col=rgb(1,0,0,0.1)))
	 if(graphic!="R")dev.off()

	

}
	