#' @export
kurtosis <- function(x,std.norm=T, na.rm=T) {
	
     if (na.rm) x <- x[!is.na(x)]
        n <- length(x)
        a = (sum((x - mean(x))^4)/n)/((sum((x - mean(x))^2)/n)^2) 
        if(std.norm) a = a - 3 #kurtosis of norm dist is 3 just zero centers
        return(a)
    }
  

