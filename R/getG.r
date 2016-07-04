#' @export
getG<-function(CF,l.bar,b=3,VB=c(144,0.4,0)){

		# growth by height adjusted for condition
		waa.tm1 <- CF*(l.bar/100)^b
		laa.t <- VB[1]*(1-exp(-VB[2]))+exp(-VB[2])*l.bar
		waa.t <- c(CF[-1],CF[length(CF)])*(laa.t/100)^b
		waa.t2 <- CF*(laa.t/100)^b
		g <- waa.t/waa.tm1
		g2 <- waa.t2/waa.tm1
		list(g=g,g2=g2)
	
}
