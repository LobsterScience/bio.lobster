#' @export
mavg<-function(x,n=3){stats::filter(x,rep(1/n,n),sides=2)}
