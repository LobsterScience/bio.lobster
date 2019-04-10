#' @export

SoMplot = function(Areas=c("27","29","30","32","33"),cl=50:130,ltys=1:length(Areas),cols=1:length(Areas), graphic="R",version=1,alpha=0.5,pm=0.5,...){

	if(graphic=="R")x11()
	if(graphic=="png")png(file.path(project.datadirectory('bio.lobster'),'figures','LFA2733Framework2018',paste0('SoM',version,'.png')),width=8,height=8,units='in',res=200)

	par(...)
	som=list()

	if(version==1){
		plot(cl,seq(0,1,l=length(cl)),type='n',xlab='Carapace Length (mm)',ylab='Probability of Maturity')

		for(i in 1:length(Areas)){

			som[[i]] = pMat(p = list(Area=Areas[i]),cl)

			lines(cl,som[[i]],lty=ltys[i],col=cols[i])
		}

		legend('bottomright',Areas,lty=ltys,col=cols,title="LFA")

	}
	if(version==2){
		plot(1:length(Areas),seq(min(cl),max(cl),l=length(Areas)),xaxt='n',type='n',ylab='Carapace Length (mm)',xlab='Lobster Fishing Area')

		for(i in 1:length(Areas)){

			som[[i]] = pMat(p = list(Area=Areas[i]),cl)
			m = range(cl[som[[i]]>(0+alpha/2)&som[[i]]<(1-alpha/2)])
			l = legal(Areas[i])
			arrows(i,m[1],i,m[2],code=3,angle=90,length=0.1)
			points(i,l,pch='-',col='red',cex=3)
		}
	axis(1,at=1:length(Areas),lab=Areas)
	}



	if(graphic!="R")dev.off()

	somdif = lapply(som,'-',pm)
	mins = lapply(lapply(somdif,abs),min)
	sa50m=c()
	for(i in 1:length(som)){
		sa50m[i] = cl[which(abs(somdif[[i]])==mins[[i]])]
	}
	names(sa50m) = Areas

	return(sa50m)

}

