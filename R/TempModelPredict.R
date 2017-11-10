TempModelPredict = function(p){

	y=	decimal_date(seq(p$startDate,p$startDate+(p$nt*p$timestep),1))
	t=predict(p$TempModel,data.frame(y=y,cos.y=cos(2*pi*y),sin.y=sin(2*pi*y),subarea=p$Area),type='response')

	return(t)
	
}