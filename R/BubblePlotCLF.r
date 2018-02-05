#' @export
BubblePlotCLF<-function(CLF,bins=seq(0,220,5),yrs=2005:2014,log.trans=F,filen='',prop=F,LS=82.5,graphic='pdf',wd=11,ht=8,path=file.path(project.datadirectory('bio.lobster'),'figures'),label=NULL,...){

	mids<-bins[-1]-diff(bins)/2

	if(graphic=='pdf')pdf(file.path(path,paste0('LC',filen,'.pdf')),width=wd,height=ht)
	if(graphic=='png')png(file.path(path,paste0('LC',filen,'.png')),width=wd,height=ht,units='in',res=200)
	if(graphic=='R')x11()
	par(mar=c(5, 5, 4, 2))
	for(i in 1:length(CLF)){
		if(prop)CLF[[i]][rowSums(CLF[[i]])>0,]<-sweep(CLF[[i]][rowSums(CLF[[i]])>0,],1,FUN="/", rowSums(CLF[[i]]))
		z=as.vector(unlist(CLF[[i]]))
		z[z==0]<-NA
		if(log.trans)z=log(z)
		#browser()
		symbols(rep(yrs,length(mids)),rep(mids,each=length(yrs)),circles=z,ylab="Carapace Length (mm)",xlab="Year",main=names(CLF)[i],...)
		abline(h=LS,col='red')
		text(max(yrs),max(mids),label,cex=2)
	}
	if(graphic!='R')dev.off()
}
