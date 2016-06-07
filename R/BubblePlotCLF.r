BubblePlotCLF<-function(CLF,bins=seq(0,220,5),yrs=2005:2014,log.trans=F,filen='',prop=F,LS=82.5,...){

	mids<-bins[-1]-diff(bins)/2

	pdf(file.path(project.datadirectory('lobster'),'figures',paste0('LC',filen,'.pdf')),11,8)
	for(i in 1:length(CLF)){
		if(prop)CLF[[i]]<-sweep(CLF[[i]],1,FUN="/", rowSums(CLF[[i]]))
		z=as.vector(CLF[[i]])
		z[z==0]<-NA
		if(log.trans)z=log(z)
		#browser()
		symbols(rep(yrs,length(mids)),rep(mids,each=length(yrs)),circles=z,ylab="Carapace Length (mm)",xlab="Year",main=names(CLF)[i],...)
		abline(h=83,col='red')
	}
	dev.off()
}
