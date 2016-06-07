projections<-function(model.out,C.p=c(200,300)){
	

	model.out$data$C.p<-C.p
	model.out$data$NC<-length(C.p)
	
	Pmed.p<-list()
	P.p<-list()
	B.p<-list()
	Bmed.p<-list()
	mu.p<-list()
	B.change<-list()
	pB0<-list()
	Pmed.p2<-list()
	P.p2<-list()
	B.p2<-list()
	

	d<-c(model.out$data,model.out$sims.list)
	for(i in 1:length(C.p)){
		
		# year 1
		Pmed.p[[i]]<- log(exp(-d$m[,d$NY])*(d$g[d$NY])*(d$P[,d$NY]-C.p[i]/d$K)+exp(-d$m[,d$NY])*(d$gR[d$NY])*d$r[,d$NY])
		P.p[[i]] <- sapply(1:length(d$K),function(x){rlnorm(1,Pmed.p[[i]][x], d$sigma[x])})
		B.p[[i]] <- P.p[[i]] * d$K
		Bmed.p[[i]] <- exp(Pmed.p[[i]]) * d$K
		mu.p[[i]] <- C.p[i] / (B.p[[i]] + C.p[i])
		B.change[[i]] <- (B.p[[i]] - d$B[,d$NY]) / d$B[,d$NY] * 100
		pB0[[i]] <- 0 > (B.p[[i]]-d$B[,d$NY])
		
		# year 2
		Pmed.p2[[i]]<-log(exp(-d$m[,d$NY])*(d$g[d$NY])*P.p[[i]]+exp(-d$m[,d$NY])*(d$gR[d$NY])*d$r[,d$NY])
		P.p2[[i]] <- sapply(1:length(d$K),function(x){rlnorm(1,Pmed.p2[[i]][x], d$sigma[x])})
		B.p2[[i]] <- P.p2[[i]] * d$K
		#browser()
	}
	
	

	
	#browser()	
		
    model.out$sims.list$B.p<-do.call("cbind",B.p)
    model.out$sims.list$B.p2<-do.call("cbind",B.p2)
    model.out$sims.list$Bmed.p<-do.call("cbind",Bmed.p)
    model.out$sims.list$mu.p<-do.call("cbind",mu.p)
    model.out$sims.list$B.change<-do.call("cbind",B.change)
    
	model.out$mean$B.p<-unlist(lapply(B.p,mean))
	model.out$mean$B.p2<-unlist(lapply(B.p2,mean))
	model.out$mean$Bmed.p<-unlist(lapply(Bmed.p,mean))
	model.out$mean$mu.p<-unlist(lapply(mu.p,mean))
	model.out$mean$B.change<-unlist(lapply(B.change,mean))
	model.out$mean$pB0<-unlist(lapply(pB0,mean))
    
	model.out$median$B.p<-unlist(lapply(B.p,median))
	model.out$median$B.p2<-unlist(lapply(B.p2,median))
	model.out$median$Bmed.p<-unlist(lapply(Bmed.p,median))
	model.out$median$mu.p<-unlist(lapply(mu.p,median))
	model.out$median$B.change<-unlist(lapply(B.change,median))


	model.out
	}
	