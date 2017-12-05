#' @export
moltProbPlot = function(p, dx = 1:3600,  lens = c(30,50,70,90,110,130,150,170,190),...){

	p$ddoy = dx


	plot(range(dx),0:1,type='n',...)

	for(i in 1:length(lens)){
		
		py2 = pPrMolt(p,cl=lens[i]) 
		lines(p$ddoy,py2,col='blue',lty=2)


	}
	grid()
}