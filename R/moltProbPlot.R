#' @export
moltProbPlot = function(p, dx = 1:660,  lens = c(30,50,70,90,110,130,150,170,190), gdd = F, temps = rDailyTemps(x=dx,b=10,m=10,s=84),...){

	if(gdd==T) dx = dx * temps

	plot(range(dx),0:1,type='n',...)

	for(i in 1:length(lens)){
		
		py2 = pPrMolt(cw=lens[i],a=p$moltPr$a,b=p$moltPr$b,x=p$moltPr$x,d=dx) 
		lines(dx,py2,col='blue',lty=2)


	}
	grid()
}