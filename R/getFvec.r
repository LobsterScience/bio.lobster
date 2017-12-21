#' @export
getFvec = function(f, LS, lens, window=NULL){

	fl=rep(0,length(lens))
	by = diff(lens)[1]
	x = ceiling(LS/by)
	i = x - lens[1]/by
	p =	(x*by - LS)/by
	#nf = -log(1 - p * (1-exp(-f)))
	fl[i] = f * p
	fl[(i+1):length(lens)] = f
	
	if(!is.null(window)){
		w = ceiling(window/by)
		j = w - lens[1]/by
		j2 = seq(j[1],j[2],1)
		p1 = 1-(w[1]*by - window[1])/by
		p2 = (w[2]*by - window[2])/by
		fl[j2] = 0
		fl[j] = f * c(p1,p2)

	}

	return(fl)
}
