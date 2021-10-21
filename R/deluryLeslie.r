#' @export
#delury estimate
deluryLeslie <-	function(y,estimate=c('delury','leslie'),method=c('ordinary','robust'),weight=NULL,plot=F) {
dl = list(coef=1234)
if(nrow(y)>5) {
	#x$lncpue <- log(x$cpue)
	
		#if sum across week before average cpue
			y$lncpue <- log(y$cpue)
			y$sumef <- cumsum(y$effort)
			y$sumlan <- cumsum(y$catch)
			y$yr = unique(y$season)
			
			if(estimate=='delury') {
				delu <- summary(lm(lncpue~sumef,data=y))$coefficients
				if(method=='robust'){
					require(MASS)
					delu <- summary(rlm(lncpue~sumef,data=y))$coefficients
						if(!is.null(weight)){
							delu <- summary(rlm(lncpue~sumef,data=y,weights=weight))$coefficients
							}
					}
				qq = -1 * delu[2,1]
				N = exp(delu[1,1]) / qq 
				if(plot){
				with(y,plot(sumef,lncpue,xlab='Effort',ylab='ln(CPUE)'))#,main=paste('Delury',unique(x$yr),sep="-")
				abline(a=delu[1,1],b=delu[2,1],col='red')
				mtext(paste0('q = ',qq,"; N0 = ",round(N,0)), side=1,line=4,cex=0.7) # display equation
				}
				return(list(coef = delu, q = qq, N0=N))
				}
		
		if(estimate=='leslie') {
			
		lesl <- summary(lm(lncpue~sumlan,data=y))$coefficients
		if(method=='robust'){
					require(MASS)
					lesl <- summary(rlm(lncpue~sumlan,data=y))$coefficients
						if(!is.null(weight)){
							lesl <- summary(rlm(lncpue~sumlan,data=y,weights=weight))$coefficients
							}
					}
				qq = -1 * lesl[2,1]
				N = exp(lesl[1,1]) / qq 
				
				if(plot){
				with(y,plot(sumlan,lncpue,xlab='Landings',ylab='ln(CPUE)'))#main=paste('Leslie',unique(x$yr),sep="-")
				abline(a=lesl[1,1],b=lesl[2,1],col='red')
				mtext(paste0('q = ',qq,"; N0 = ",round(N,0)), side=1,line=4,cex=0.7) # display equation
				}
				return(list(coef = lesl, q = qq, N0=N))
		}
	}
	return(dl)
	}
