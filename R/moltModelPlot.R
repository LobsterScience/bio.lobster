#' @export
moltModelPlot = function(MoltModel, dx = 1:3600,  lens = c(30,50,70,90,110,130,150,170,190),graphic="R",...){

	p = list()
	p$ddoy = dx
	p$moltPrModel = MoltModel$moltPrModel

	if(graphic=="R")x11()

	plot(range(dx),0:1,type='n',...)

	for(i in 1:length(lens)){
		
		py2 = pPrMolt(p,cl=lens[i]) 
		lines(p$ddoy,py2,col='blue',lty=2)


	}
	grid()
	if(graphic!="R")dev.off()

	lens=seq(52.5,200,5)
	Incr=posterior_predict(MoltModel$moltIncrMode,newdata=data.frame(CL=lens),fun=exp)
	mu = apply(Incr,2,mean)
	ub = apply(Incr,2,quantile,0.975)
	lb = apply(Incr,2,quantile,0.025)

	#lens = seq(50,200,5)
	#P = predict(moltIncrModel,newdata=data.frame(CL=lens),type='response',se.fit=T)
#
	with(moltincr.data,plot(CL,SizeDiff,type='n',xlim=c(0,200),xlab="Carapace Length (mm)",ylab = "Size Difference (mm)"))
	with(subset(moltincr.data,TagSex%in%2:3),points(CL,SizeDiff,pch=16,col=rgb(1,0,0,0.2)))
	with(subset(moltincr.data,TagSex==1),points(CL,SizeDiff,pch=16,col=rgb(0,0,1,0.2)))
	with(moltincr.data,plot(CL,MoltIncr*100,ylim=c(0,60),type='n',xlim=c(0,200),xlab="Carapace Length (mm)",ylab = "Molt Increment (%)"))
	with(subset(moltincr.data,TagSex%in%2:3),points(CL,MoltIncr*100,pch=16,col=rgb(1,0,0,0.2)))
	with(subset(moltincr.data,TagSex==1),points(CL,MoltIncr*100,pch=16,col=rgb(0,0,1,0.2)))
	lines(lens,mu*100,col='red',lwd=2)
	lines(lens,ub*100,col='red',lty=2)
	lines(lens,lb*100,col='red',lty=2)



}