TempModelPlot = function(TempModel){

	y=	seq(min(TempData$y),max(TempData$y),0.01)

	#points(TEMPERATURE~y,data=subset(TempData,subarea=='27N'),pch=16,col=rgb(1,0,0,0.2),cex=0.5)
	t5=predict(TempModel,data.frame(y=y,cos.y=cos(2*pi*y),sin.y=sin(2*pi*y),DEPTH=5,subarea='33W'),type='response')
	t25=predict(TempModel,data.frame(y=y,cos.y=cos(2*pi*y),sin.y=sin(2*pi*y),DEPTH=25,subarea='33W'),type='response')
	t50=predict(TempModel,data.frame(y=y,cos.y=cos(2*pi*y),sin.y=sin(2*pi*y),DEPTH=50,subarea='33W'),type='response')
	t100=predict(TempModel,data.frame(y=y,cos.y=cos(2*pi*y),sin.y=sin(2*pi*y),DEPTH=100,subarea='33W'),type='response')
	t200=predict(TempModel,data.frame(y=y,cos.y=cos(2*pi*y),sin.y=sin(2*pi*y),DEPTH=200,subarea='33W'),type='response')
	t500=predict(TempModel,data.frame(y=y,cos.y=cos(2*pi*y),sin.y=sin(2*pi*y),DEPTH=500,subarea='33W'),type='response')
	 
	 plot(TEMPERATURE~y,data=TempData,pch='.',col=rgb(0,0,0,0.1),ylim=c(-2,20),xlim=range(y))
	 lines(y,t5,col=2)
	 lines(y,t25,col=3)
	 lines(y,t50,col=4)
	 lines(y,t100,col=6)

	

}
	