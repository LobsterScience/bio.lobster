#' @export
surveyHistMap = function(data,size=c(1,1),pars=NULL,bw="nrd0",minx=0){

	require(Hmisc)
	
	plotDensity = function(CL){

		x=as.numeric(substr(names(CL),3,5))-2.5
		d = density(rep(x,round(CL)),bw=bw)
		plot(d,xlab='',ylab='',yaxt='n',xaxt='n',main='',axes=F,xlim=c(0,200))
		polygon(d,col='grey')
		abline(v=82.5,col='red')
	}


	locs = subset(data,!duplicated(SID),c("SID","X","Y"))
	locs = locs[order(locs$SID),]


	for(i in 1:nrow(locs)){

		#print(locs$SID[i])

		CLdata = subset(data,SID==locs$SID[i],which(substr(names(surveyLobsters34),1,2)=="CL"))
		CL=colSums(CLdata,na.rm=T)
		#browser()
	
		if(sum(CL)>minx)subplot(plotDensity(CL),x=locs$X[i],y=locs$Y[i],size=size,pars=pars)


	}



}