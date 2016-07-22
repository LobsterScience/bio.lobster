#' @title dicusum
#' @description decision interval CUSUM control chart analysis 
#' @param \code{x} :vector of time series data
#' @param \code{ref.period} : index used for reference period
#' @param \code{k} : threshold differences to detect a change, also termed allowance 
#' @return returns a list of positive and negative deviations as well as reference index
#' @examples
#' x = c(1,2,3,4,5,3,2,1,2,6)
#' dicusum(x,1:3,0.2)
#' @author  Adam Cook, \email{Adam.Cook@@dfo-mpo.gc.ca}
#' @references Mesnil and Pettigas 2008 Aquat Living Resour 22 187-192
#' @export


dicusum <- function(x,ref.index,k) {
        mean.ref<-mean(x[ref.index])
        sd.ref<-sd(x[ref.index])
        z<-(x-mean.ref)/sd.ref
        pos.dev<-neg.dev<-rep(0,length(x))
        for(i in 2:length(x))
                {
                pos.dev[i]<-max(0,pos.dev[i-1]+z[i]-k)
                neg.dev[i]<-min(0,neg.dev[i-1]+z[i]+k)
                }
    
    list(pos.dev=pos.dev,neg.dev=neg.dev,ref.index = ref.index)
  
}



