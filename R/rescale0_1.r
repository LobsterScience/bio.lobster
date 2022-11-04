#' @export
rescale0_1 <- function(x){
	return((x-min(x,na.rm=T))/(max(x,na.rm=T)-min(x,na.rm=T)))
}