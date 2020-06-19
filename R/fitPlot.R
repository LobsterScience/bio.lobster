#' @export
fitPlot <- function(data.out,name="", CI=F,CV=F,SE=F,Iadj=1,Uadj=1,graphic='R',ymaxB,ymaxR,alpha=0.05,path=''){

# Fit plots


if(graphic=='pdf')pdf(file.path(path,paste("fit", name, ".pdf", sep="")))
if(graphic=="R")windows()

if("qU"%in%names(data.out$median))par(mfrow = c(3,1), mar = c(1, 4, 1, 1), omi = c(0.2, 0.3, 0, 0.2))
else par(mfrow = c(2,1), mar = c(1, 4, 1, 1), omi = c(0.2, 0.3, 0, 0.2))

if(missing(ymaxB))ymax<-ifelse(CI,max(pmax(apply(sweep(data.out$sims.list$B,2,FUN='*',data.out$median$q/Iadj), 2, quantile, 1-alpha/2), data.out$data$I/Iadj)),max(c((data.out$median$B[1:length(data.out$data$iyr)]*data.out$median$q)/Iadj,data.out$data$I/Iadj)))
else ymax<-ymaxB
plot(data.out$data$iyr, (data.out$median$B[1:length(data.out$data$iyr)]*data.out$median$q)/Iadj, type = 'l', lwd = 2, ylim = c(0, ymax), ylab = "", las = 1, xlim = c(min(data.out$data$iyr)-1, max(data.out$data$iyr)+1), mgp = c(0.5, 0.5, 0), xlab = "", tcl = -0.3, asp = 'xy', cex.axis=1.2)
axis(4, lab = F, tcl = -0.3)
if(Iadj!=1)mtext("Survey Biomass \n (kg/tow)", 2, 2.5, cex = 1.25)
if(Iadj==1)mtext("Survey Biomass", 2, 3.5, cex = 1.25)
if(CI){
	lines(data.out$data$iyr, apply(sweep(data.out$sims.list$B,2,FUN='*',data.out$median$q/Iadj), 2, quantile, alpha/2), lty = 2)
	lines(data.out$data$iyr, apply(sweep(data.out$sims.list$B,2,FUN='*',data.out$median$q/Iadj), 2, quantile, 1-alpha/2), lty = 2)
}
points(data.out$data$iyr, data.out$data$I/Iadj, col = 'red', pch = 16,cex=1.2)
if(CV)segments(data.out$data$iyr, data.out$data$I/Iadj+data.out$data$I.cv*(data.out$data$I/Iadj), data.out$data$iyr, data.out$data$I/Iadj-data.out$data$I.cv*(data.out$data$I/Iadj),col='red')

#browser()

if("ratiolined"%in%names(data.out$data))qR<-q*data.out$data$ratiolined
if(missing(ymaxR))ymax<-ifelse(CI,max(pmax(apply(sweep(data.out$sims.list$R,2,FUN='*',data.out$median$qR/Iadj), 2, quantile, 1-alpha/2), data.out$data$IR/Iadj)),max(c((data.out$median$R[1:length(data.out$data$iyr)]*data.out$median$qR)/Iadj,data.out$data$IR/Iadj)))
else ymax<-ymaxR
plot(data.out$data$iyr, (data.out$median$R[1:length(data.out$data$iyr)]*data.out$median$qR)/Iadj, type = 'l', lwd=2, ylim = c(0,ymax), ylab = "", las = 1, xlim = c(min(data.out$data$iyr)-1, max(data.out$data$iyr)+1), mgp = c(0.5, 0.5, 0), xlab = "", tcl = -0.3, asp = 'xy', cex.axis=1.2)
axis(4, lab = F, tcl = -0.3)
if(Iadj!=1)mtext("Survey Recruit Biomass \n (kg/tow)", 2, 2.5, cex = 1.25)
if(Iadj==1)mtext("Survey Recruit Biomass", 2, 3.5, cex = 1.25)
if(CI){
	lines(data.out$data$iyr, apply(sweep(data.out$sims.list$R,2,FUN='*',data.out$median$qR/Iadj), 2, quantile, alpha/2), lty = 2)
	lines(data.out$data$iyr, apply(sweep(data.out$sims.list$R,2,FUN='*',data.out$median$qR/Iadj), 2, quantile, 1-alpha/2), lty = 2)
}
points(data.out$data$iyr, data.out$data$IR/Iadj, col = 'red',pch=16,cex=1.2)
if(CV)segments(data.out$data$iyr, data.out$data$IR/Iadj+data.out$data$IR.cv*(data.out$data$IR/Iadj), data.out$data$iyr, data.out$data$IR/Iadj-data.out$data$IR.cv*(data.out$data$IR/Iadj),col='red')


if("qU"%in%names(data.out$median)){

		
	ymax<-ifelse(CI,max(pmax(apply(sweep(data.out$sims.list$B,2,FUN='*',data.out$median$qU/Uadj), 2, quantile, 1-alpha/2), data.out$data$U+data.out$data$U.cv*data.out$data$U)),max(c((data.out$median$B[1:length(data.out$data$iyr)]*data.out$median$qU)/Uadj,data.out$U)))

	plot(data.out$data$iyr, (data.out$median$B[1:length(data.out$data$iyr)]*data.out$median$qU)/Uadj, type = 'l', lwd=2, ylim = c(0, ymax), ylab = "", las = 1, xlim = c(min(data.out$data$iyr)-1, max(data.out$data$iyr)+1), mgp = c(0.5, 0.5, 0), xlab = "", tcl = -0.3, asp = 'xy', cex.axis=1.2)
	axis(4, lab = F, tcl = -0.3)
	mtext("Commercial CPUE \n (kg/hm)", 2, 2.5, cex = 1.25)
	if(CI){
		lines(data.out$data$iyr, apply(sweep(data.out$sims.list$B,2,FUN='*',data.out$median$qU/Uadj), 2, quantile, alpha/2), lty = 2)
		lines(data.out$data$iyr, apply(sweep(data.out$sims.list$B,2,FUN='*',data.out$median$qU/Uadj), 2, quantile, 1-alpha/2), lty = 2)
	}
	points(data.out$data$iyr, data.out$data$U, col = 'red',pch=16,cex=1.5)
	if(CV)segments(data.out$data$iyr, data.out$data$U+data.out$data$U.cv*data.out$data$U, data.out$data$iyr, data.out$data$U-data.out$data$U.cv*data.out$data$U,col='red')
	}

if(graphic!="R")dev.off()
}


