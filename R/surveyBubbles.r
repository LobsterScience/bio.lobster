#' @export
surveyBubbles = function(data,scaler=1,pie=F){

	if(pie) {
		require(mapplots)
		z = data[c("denm","denf","denb")]
		sex.cols = c(rgb(0,0,1,0.7),rgb(1,0,0,0.7),rgb(0,1,0,0.7))
		with(data,draw.pie(z=as.matrix(z),x=x,y=y,radius = scaler, col=sex.cols))
	} else {
		data$den = sqrt(data$den)
		with(data,symbols(x=x, y=y, circles = den, fg=line.cols, bg= fill.cols, add=T,inches=scaler))
	}


}