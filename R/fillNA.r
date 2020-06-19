#' @export
fillNA <- function(x,y){  
			np 	= length(x)
			ix 	= which(is.na(y))
			for(i in 1:length(ix)){
			ixx = min(np-1,max(2,ix[i]))
			tx 	= as.numeric(x[ixx + c(,-1,0,1)])
			ty 	= y[ixx + c(-1,0,1)]
			co 	= lm(ty ~ tx )
			y[ix[i]] = predict(co,newdata=data.frame(tx=tx[2]))
			}
		return(y)
	}