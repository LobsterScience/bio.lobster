#' @export

getM = function(CL,t,Minf=0.1,K,t0,a=0.0008,b=3, maxCL=200, age=F){

	if(age) Mt = Minf*(1-exp(-K(t-t0)))^(-b*0.305)
	else    Mt = Minf*(a*CL^b/(a*maxCL^b))^(-0.305)
	
	return(Mt)
}
