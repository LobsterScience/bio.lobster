#' @export
SoMLeBris <- function(p,cl,T){
	with(p,{
	
	Y = 2015
	
	Lm = 459.794-0.169*Y-2.79*T
	
	B = -22.073-0.012*Y-0.027*T
	o = 1 / (1+exp(-B*(cl-Lm)))
	return(o)

	})
}