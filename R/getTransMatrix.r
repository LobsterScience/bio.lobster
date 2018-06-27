#' @export
getTransMatrix = function(rlist,lfa="33W",f=4,s=1){
	
	plst = rlist$plist[[lfa]]
	males = rlist$mlist[[lfa]]
	females = rlist$flist[[lfa]]
	transMats = list(males=list(),females=list())
	dims = dim(rlist$mlist$"33W"$totalPop)
	w = array(dim=dims)

	#males

	for(i in 1:dims[3]){
		for(j in 1:dims[4]){
			w[,,i,j] = as.matrix(males$totalPop[,,i,j]/males$finalPop)
		}
	}
	w[is.na(w)] = 0
	weighted = w * males$moltProbs
	mP = apply(weighted,c(1,2),sum)/apply(w,c(1,2),sum)
	moltProbs = list()
	for(i in 1:f){
		q = seq(s,dims[1],f) + (i-1)
		moltProbs[[i]] = colMeans(mP[q,],na.rm=T)

	}

	plst$sex = 1

	mat=getIncr(plst)
	mat[is.na(mat)] = 0
	for(i in 1:f){
		transMats$males[[i]] = moltProbs[[i]] * mat
		diag(transMats$males[[i]]) = diag(transMats$males[[i]]) + (1- moltProbs[[i]])
	}

	#females

	for(i in 1:dims[3]){
		for(j in 1:dims[4]){
			w[,,i,j] = as.matrix(females$totalPop[,,i,j]/females$finalPop)
		}
	}
	w[is.na(w)] = 0
	weighted = w * females$moltProbs
	mP = apply(weighted,c(1,2),sum)/apply(w,c(1,2),sum)
	moltProbs = list()
	for(i in 1:f){
		q = seq(s,dims[1],f) + (i-1)
		moltProbs[[i]] = colMeans(mP[q,],na.rm=T)

	}

	plst$sex = 2

	mat=getIncr(plst)
	mat[is.na(mat)] = 0
	for(i in 1:f){
		transMats$females[[i]] = moltProbs[[i]] * mat
		diag(transMats$females[[i]]) = diag(transMats$females[[i]]) + (1- moltProbs[[i]])
	}

	x11()
	par(mfrow=c(2,1))

	plot(1:30,rep(0:1,15),type='n',ylab="Molt Prob",xlab='CL',main='Males',xaxt='n',las=1)
	axis(1,at=1:30,lab=1:30*5+50)
	mps = lapply(transMats[[1]],diag)
	do.call("rbind",mps)
	lines(1-mps[[1]],lty=1)
	lines(1-mps[[2]],lty=2)
	lines(1-mps[[3]],lty=3)
	lines(1-mps[[4]],lty=4)
	lines(4-colSums(do.call("rbind",mps)),lwd=2)
	legend('topright',legend=1:4,lty=1:4)

	plot(1:30,rep(0:1,15),type='n',ylab="Molt Prob",xlab='CL',main='Females',xaxt='n',las=1)
	axis(1,at=1:30,lab=1:30*5+50)
	fps = lapply(transMats[[2]],diag)
	lines(1-fps[[1]],lty=1)
	lines(1-fps[[2]],lty=2)
	lines(1-fps[[3]],lty=3)
	lines(1-fps[[4]],lty=4)
	lines(4-colSums(do.call("rbind",fps)),lwd=2)
	
	return(transMats)

}