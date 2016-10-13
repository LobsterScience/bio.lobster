#' @export

replacementRatio.relF <- function(landings,survey.biomass,fn=project.figuredirectory('bio.lobster'),file.name='trial.png',raw=F,running.length=3,bootstrap=T,nreps=10000,trim=T,robust.reg=T,savePlot=T) {
		require(MASS)

		if(raw) running.length=1

		xmean = x = landings
		ymean = y = survey.biomass

		if(!raw) {
						#Applegate et al. 1998. Evaluation of existing overfishing definitions and recommendations for new overfishing definitions to comply with the Sustainable #Fisheries Act. Final Report, Overfishing Definition Review Panel. New England Fishery management Council, Newburyport,Massachusetts, 179 p
  					xmean = apply(embed(x,running.length),1,mean)
  					ymean = apply(embed(y,running.length),1,mean)
  		
	      		}

		relF = x[-1:-(running.length-1)] / ymean
		rR = y[-1:-(running.length)] / ymean[-length(ymean)]

		if(any(rR==0)) {
				k = which(rR==0)
				rR = rR[-k]
				relF = relF[-k]
				}
		if(robust.reg) a = rlm(log(rR)~log(relF[-1]))
		if(!robust.reg) a = lm(log(rR)~log(relF[-1]))
		if(savePlot) png(file=file.path(fn,file.name),units='in',width=15,height=12,pointsize=18, res=300,type='cairo')
		plot(log(relF[-1]),log(rR),xlab='log(relative F)',ylab = 'log(Replacement Ratio)',pch=16)
		abline(h=0,col='red')
		abline(a=coef(a)[1],b=coef(a)[2],col='black',lwd=2)
		rRef = -1 * coef(a)[1] / coef(a)[2]
		lines(x=c(rRef,rRef),y=c(-10,0),col='red',lwd=2)
		aa = robust.regression.r2(a,weighted=T)
		legend('topright',paste(expression(r^2),(aa)),bty='n',cex=1)
		if(savePlot) dev.off()
		print(exp(rRef)) ##matches Rago's AIM if use non robust regression

		if(bootstrap) {
			out <- c()

			g = predict(a)
			r = residuals(a)
			if(robust.reg) r = a$wresid

			for(i in 1:nreps) {
					gp = g + sample(r,length(g))
					if(robust.reg) gl = coef(rlm(gp~log(relF[-1])))
					if(!robust.reg) gl = coef(lm(gp~log(relF[-1])))
					out = c(out, as.numeric(exp(-1*gl[1] / gl[2])[1]))
			}
			if(trim) out = quantileBreak2NA(out,c(0.05,0.95))

			return(out)

		}

			
}