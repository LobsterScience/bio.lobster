#' @export
	
	stochasticReferencePoints<-function(type=c('MSY','BMSY','FMSY'),r,K,err) {
		#transform model estimates from state space schaefer to stochastic reference points after Bousquet et al .2008, sd.p is the error term
			if(type=='MSY') {
				b<-r*K/4*(1-err^2/(r*(1-r/4))+(4*err^4)/(r^2*(4-r)^4)*(r^4-4*r^3-12*r^2+48*r-16))
				}
			if(type=='FMSY') {
				b<-r/2-(2*(2-r)*err^2)/(4-r)^2
				}
			if(type=='BMSY') {	
				b <- K/2*(1-(8*err^2)/(r*(4-r)^4)-(8*err^4)/(r*(4-r)^5)*(3*r^3-18*r^2-12*r+32))
			}
			return(b)
	}
	