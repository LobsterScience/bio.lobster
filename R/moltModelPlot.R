#' @export
moltModelPlot = function(MoltModel, dx = 1:5000,  lens = c(50,70,90,110,130,150,170,190),graphic="R",wd=8,ht=8,...){

	p = list()
	p$ddoy = dx
	p$moltModel = MoltModel

	if(graphic=="R")x11()
	if(graphic=='png')png(file.path(project.datadirectory('bio.lobster'),'figures','MoltProbModel.png'),width=wd,height=ht,units='in',res=200)

	plot(range(dx),0:1,type='n',xlab='Degree Days',ylab="Molt Probability",...)

	cols = brewer.pal(length(lens)+1,"YlOrRd")[-1]
	for(i in 1:length(lens)){
		
		py2 = pPrMolt(p,cl=lens[i]) 
		lines(p$ddoy,py2,col=cols[i])


	}
	legend('bottomright',legend=lens,title="Carapace Length \n (mm)",col=cols,pch=15,bty='n')
	grid()
	if(graphic!="R")dev.off()

	lens=seq(52.5,200,5)
	m.Incr=posterior_predict(MoltModel$maleMoltIncrModel,newdata=data.frame(CL=lens),fun=exp)
	m.mu = apply(m.Incr,2,mean)
	m.ub = apply(m.Incr,2,quantile,0.975)
	m.lb = apply(m.Incr,2,quantile,0.025)

	f.Incr=posterior_predict(MoltModel$femaleMoltIncrModel,newdata=data.frame(CL=lens),fun=exp)
	f.mu = apply(f.Incr,2,mean)
	f.ub = apply(f.Incr,2,quantile,0.975)
	f.lb = apply(f.Incr,2,quantile,0.025)

	#lens = seq(50,200,5)
	#P = predict(moltIncrModel,newdata=data.frame(CL=lens),type='response',se.fit=T)
#
	if(graphic=="R")x11()
	if(graphic=='png')png(file.path(project.datadirectory('bio.lobster'),'figures','MoltIncrModel.png'),width=wd,height=ht,units='in',res=200)

	with(MoltModel$malemoltincr.data,plot(CL,SizeDiff,type='n',xlim=c(50,200),xlab="Carapace Length (mm)",ylab = "Size Difference (mm)"))
	
	#females
	with(MoltModel$femalemoltincr.data,points(CL,SizeDiff,pch=16,col=rgb(1,0,0,0.2)))
	lines(lens,f.mu,col='red',lwd=2)
	lines(lens,f.ub,col='red',lty=2)
	lines(lens,f.lb,col='red',lty=2)

	# males
	with(MoltModel$malemoltincr.data,points(CL,SizeDiff,pch=16,col=rgb(0,0,1,0.2)))
	lines(lens,m.mu,col='blue',lwd=2)
	lines(lens,m.ub,col='blue',lty=2)
	lines(lens,m.lb,col='blue',lty=2)

	if(graphic!="R")dev.off()


}