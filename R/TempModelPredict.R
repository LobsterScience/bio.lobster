#' @export
TempModelPredict = function(p, start = p$startDate, end = p$startDate+(p$nt*p$timestep) ){

	y =	decimal_date(seq(start,end,1))
	
	t = predict(p$TempModel, data.frame(y=y, cos.y=cos(2*pi*y), sin.y=sin(2*pi*y), DEPTH=p$Depth, area=p$Area), type='response')

	return(t)
	
}