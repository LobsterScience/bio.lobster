#' findValue
#' 
#' Finds the corresponding x value to match a reference y value
#' @param x is the xaxis 
#' @param y is the yaxis
#' @param ref is the level of y you want to find the corresponding x value for  
#' @return x value

findValue <- function(x,y,ref=0.4){
	
		np 		= length(x)
		s0 		= max(y)
		s0.4	= s0*ref
		ix 		= which.min(abs(s0.4 - y))
		ixx 	= min(np-1,max(2,ix))
		tx 		= x[ixx + c(-1,0,1)]
		ty 		= y[ixx + c(-1,0,1)]
		cou 	= lm(ty ~ tx + I(tx^2))
		co      = data.frame(tx=seq(tx[1],tx[3],length.out=100))
		f0.4	= predict(cou,newdata=co)
		ix 		= co[which.min(abs(s0.4 - f0.4)),1]
		return(ix)
}


