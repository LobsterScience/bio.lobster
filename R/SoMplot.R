#' @export

SoMplot = function(Areas=c("27","29","30","32","33"),cl=50:130,ltys=1:length(Areas),cols=1:length(Areas),LS=NULL, legend=T,graphic="R",fp=file.path(project.datadirectory('bio.lobster'),'figures','LFA2733Framework2018'),...){

	if(graphic=="R")x11()
	if(graphic=="png")png(file.path(fp,'SoM.png'),width=8,height=8,units='in',res=200)

	par(...)

	plot(cl,seq(0,1,l=length(cl)),type='n',xlab='Carapace Length (mm)',ylab='Probability of Maturity')

	for(i in 1:length(Areas)){

		som = pMat(p = list(Area=Areas[i]),cl)

		lines(cl,som,lty=ltys[i],col=cols[i])

	}

	if(!is.null(LS))abline(v=LS,col='red')
	if(legend)legend('bottomright',Areas,lty=ltys,col=cols,title="LFA")

	if(graphic!="R")dev.off()

}

