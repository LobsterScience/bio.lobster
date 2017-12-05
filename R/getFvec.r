#' @export
getFvec = function(f, LS, lens){

	fl=c()
	by = diff(lens)[1]
	x = ceiling(LS/by)
	i = x - lens[1]/by
	p =	(x*by - LS)/by
	#nf = -log(1 - p * (1-exp(-f)))
	
	fl[1:i-1] = 0
	fl[i] = f * p
	fl[(i+1):length(lens)] = f

	return(fl)
}
