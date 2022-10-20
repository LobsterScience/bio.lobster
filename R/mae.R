#' @export
mae<- function(x,y){
	sum(abs(x-y))/length(x)
}

