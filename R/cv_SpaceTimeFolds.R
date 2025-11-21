#' @export
#making fold ids
cv_SpaceTimeFolds = function(dat,idCol='TRIP',nfolds=8){
	dat$fold_id = NA
if(length(unique(dat[,idCol]))>1){
		x = split(dat,f=dat[,idCol])
	o = list()
	s = 1:nfolds
	for(i in 1:length(x)){
		m = x[[i]]
		n = nrow(m)
		f=rep(s,each=floor(n/nfolds))
		if(length(f)!=n) f=c(f,s[1:(n-length(f))])
		m$fold_id = sample(f,size=n)
		o[[i]] = m
	}
	return(do.call(rbind,o))
}
	if(length(unique(dat[,idCol]))==1){
	  dat$fold_id <- sample(rep(1:nfolds,length.out=nrow(dat)))
	return(dat)
	}
}
