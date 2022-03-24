#' @export

clusterStatistics<- function(x){
    #x is a data frame with mean, var, n
     #x is a data frame with mean, var, n
  if(nrow(x)>1){
  xc = sum(x$n*x$mean) / sum(x$n)
  xx = x
  if(any(x$n==1)) xx = subset(x,n>1)
    if(nrow(xx)>1)   xvar=sum((xx$n-1)*xx$var+(xx$n*(xx$mean-xc)^2)) / (sum(xx$n)-1) 
    if(nrow(xx)<=1)  xvar = var(x$mean)
    if(is.na(xvar)) browser()
  return(c(mean=xc,sd=sqrt(xvar),n=sum(x$n),nc=nrow(x)))
  } else {

  return(c(mean=x$mean,sd=sqrt(x$var),n=sum(x$n),nc=1))
  }
 
}