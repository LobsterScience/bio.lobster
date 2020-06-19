#' @export
skewness <- function(x,na.rm=T,size.adjusted=T,type=NULL) {
	if(na.rm) x <- x[!is.na(x)]
		 n <- length(x)
if(!is.null(type)) {
			 if(type=='Fisher'){
			        if(!size.adjusted) a = (sum((x - mean(x))^3)/n)/(sum((x - mean(x))^2)/n)^(3/2)
					if(size.adjusted) a =  sqrt(n*(n-1))/(n-1) * (sum((x - mean(x))^3)/n)/(sum((x - mean(x))^2)/n)^(3/2)
					return(a)
						}
			
			if(type=='Galton') {
					xx = quantile(x,probs=c(0.25,0.5,0.75))
					return(as.numeric(((xx[1]+xx[3])-2*xx[2]) / (xx[3]-xx[1])))
					}
			if(type=='Pearson2') {
					return(3*(mean(x)-median(x)) / sd(x))
					}
			}

return(list(Fisher=skewness(x,type = 'Fisher'),Galton=skewness(x,type='Galton'),Pearson2=skewness(x,type='Pearson2')))

		}