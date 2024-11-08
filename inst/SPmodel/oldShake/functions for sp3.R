#functions for sp3
#source(C:/Documents and Settings/cooka/Desktop/Scripts/Surplus Production dec 2011/bugs/uniform error/functions for sp3.R')
source('C:/Documents and Settings/cooka/Desktop/Scripts/Surplus Production dec 2011/bugs/finding moments of distributions.R')
source('C:/Documents and Settings/cooka/Desktop/Scripts/Surplus Production dec 2011/data in/cameo data scripts.R')

	plott<-function(a2=a2,namess,dputs=F){
	if(dputs) {dput(a2,paste(namess,".txt",sep=""))}
	
	pdf(paste("",namess,".pdf",sep=""))
	#plot fits
	plot(c(0,1),c(0,1),ann=F,bty='n',type='n',xaxt='n',yaxt='n')
	text(0.5,0.5,paste('O + P Error af=',af),cex=2)
	
	plot(input1$I)
	lines(exp(a2$median$Imean),type='l')


	#plot priors and posteriors
			op<-par(mfrow=c(3,2),mar=c(4,4,3,1))
			# r
			plot(density(rweibull(10000,input1$r.a,1/(input1$r.b))),xlab='r',xlim=c(0,1.5),lty=2,main='')
			lines(density(a2$sims.list$r1),col='blue',lty=1)
			lines(density(a2$sims.list$r2),col='green',lty=1)

			
			#K
			plot(density(rlnorm(10000,K1s[[1]],sqrt(K1s[[2]]))),xlab='K',lty=2,main='')
			lines(density(a2$sims.list$K1),col='blue',lty=1)
			lines(density(a2$sims.list$K2),col='green',lty=1)

			#q
			plot(1,1,type='n',xlim=c(input1$q.a,input1$q.b*1.1),ylim=c(0,3),xlab='q',main='',ylab='Density')
			polygon(x=c(input1$q.a,input1$q.a,input1$q.b,input1$q.b),y=c(0,dunif(c(input1$q.a,input1$q.b),input1$q.a,input1$q.b),0))
			lines(density(a2$sims.list$q1),col='blue',lty=1)
			lines(density(a2$sims.list$q2),col='green',lty=1)

			#process errors
			plot(1,1,type='n',xlim=c(0,1),xlab='Process Variance',main='',ylab='Density',ylim=c(0,0.6))
			polygon(x=c(0.25^2,0.25^2,1,1),y=c(0,0.4,0.4,0),border='black',lty=2)
			lines(density(sqrt(a2$sims.list$Sigma2)),col='blue',lty=1,lwd=1)
			lines(density(sqrt(a2$sims.list$Sigma21)),col='green',lty=1,lwd=1)
			
			plot(1,1,type='n',xlim=c(0,1),xlab='Observation Variance',main='',ylab='Density',ylim=c(0,0.6))
			polygon(x=c(0.25^2,0.25^2,1,1),y=c(0,0.4,0.4,0),border='black',lty=2)
			lines(density(sqrt(a2$sims.list$Tau2)),col='blue',lty=1,lwd=1)
			lines(density(sqrt(a2$sims.list$Tau21)),col='green',lty=1,lwd=1)
			
			
			plot(1970:2011,a2$median$P.res,type='h',ylab='Residuals',xlab='Year')
			abline(h=0,col='red')
		
			par(op)

	#using biomass from model fits

				dat1<-data.frame(years=seq(1970,1970+(length(a2$median$B)-1)),Biomass=0,Landings=0)
				dat1$Biomass<-a2$median$B*10^3
				dat1$Landings<-input1$C*10^3
				
						dat1$ASP <- 0
						for(i in 1:(nrow(dat1)-1)) {
							dat1[i,'ASP'] <- dat1[i+1,'Biomass'] - dat1[i,'Biomass'] +dat1[i,'Landings'] 
						}

				#MSY V B plots
				plot.arr(dat1$ASP,dat1$Biomass)
									
				lines(x=c(0,a2$median$BMSPc*10^3),y=c(a2$median$MSPc*10^3,a2$median$MSPc*10^3),lwd=2,col='blue')
				lines(x=c(a2$median$BMSPc*10^3,a2$median$BMSPc*10^3),y=c(0,a2$median$MSPc*10^3),lwd=2,col='blue')
				lines(x=c(0,a2$median$BMSP*10^3),y=c(a2$median$MSP*10^3,a2$median$MSP*10^3),lwd=2,col='blue')
				lines(x=c(a2$median$BMSP*10^3,a2$median$BMSP*10^3),y=c(0,a2$median$MSP*10^3),lwd=2,col='blue')
				
				legend('topleft',c('Sto','Det'),col=c('black','blue'),lty=c(1,1),bty='n',cex=0.8)

				
					#MSY plots with 40 and 80 BMSY
					af=0
					fit <- lower <- upper <- numeric()
					N<-seq(1970,1970+(length(a2$median$B)-1)-3)
					n<-length(N)
					for(i in 1:length(N)) {
					fit[i]<-median(a2$sims.list$B[,i]*10^af)
					lower[i]<-quantile(a2$sims.list$B[,i]*10^af,0.10)
					upper[i]<-quantile(a2$sims.list$B[,i]*10^af,0.90)
					}
					#yl <- c(min(c(fit,lower,upper)),max(c(fit,lower,upper)))
					yl <- c(75,1000)
					#plot(1,1, type='n',ylab='Biomass',xlab='Year',ylim=c(25,1000),xlim=c(1970,2015))
					
					lines(N,fit)
					lines(N,lower,lty=2)
					lines(N,upper,lty=2)
					lines(c(1970,1992),c(a$median$K1,a$median$K1),col='blue',lwd=3)
					lines(c(1993,2011),c(a$median$K2,a$median$K2),col='green',lwd=3)					
					
					#3 year projections
					
					af=0
					fit <- lower <- upper <- numeric()
					N <- seq(1970,1970+(length(a2$median$B)-1))
					n<-length(N)
					for(i in (length(N)-3):length(N)) {
					fit[i]<-median(a2$sims.list$B[,i]*10^af)
					lower[i]<-quantile(a2$sims.list$B[,i]*10^af,0.10)
					upper[i]<-quantile(a2$sims.list$B[,i]*10^af,0.90)
					}
					
					lines(N+1,fit,col='red')
					lines(N+1,lower,lty=2,col='red')
					lines(N+1,upper,lty=2,col='red')
					
					
					polygon(c(1970,1992,1992,1970),c(0.8*a2$median$K1/2,0.8*a2$median$K1/2,0.4*a2$median$K1/2,0.4*a2$median$K1/2),density=10,col='blue')									
					polygon(c(1993,2011,2011,1993),c(0.8*a2$median$K2/2,0.8*a2$median$K2/2,0.4*a2$median$K2/2,0.4*a2$median$K2/2),density=10,col='green')
					#legend('topright',fill=c('orange','blue'),c('Det','Sto'),bty='n',cex=0.8,density=20)
					
					
			#dat<-cbind(dat,a2$median$P.res)
			panel.cor <- function(x, y, digits=2, prefix="", cex.cor)
					{
					    usr <- par("usr"); on.exit(par(usr))
					    par(usr = c(0, 1, 0, 1))
					    r <- abs(cor(x, y))
					    txt <- format(c(r, 0.123456789), digits=digits)[1]
					    txt <- paste(prefix, txt, sep="")
					    if(missing(cex.cor)) cex <- 0.8/strwidth(txt)
					    
					    test <- cor.test(x,y)
					    # borrowed from printCoefmat
					    Signif <- symnum(test$p.value, corr = FALSE, na = FALSE,
					                  cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
					                  symbols = c("***", "**", "*", ".", " "))
					    
					    text(0.5, 0.5, txt, cex = cex * r)
					    text(.8, .8, Signif, cex=cex, col=2)
					}
					cor.prob <- function(X, dfr = nrow(X) - 2) {
			 R <- cor(X)
	 		above <- row(R) < col(R)
	 		r2 <- R[above]^2
	 		Fstat <- r2 * dfr / (1 - r2)
	 			R[above] <- 1 - pf(Fstat, 1, dfr)
	 			R
}


par(mfrow=c(2,2))
			
	plot(density(a2$sims.list$BMSP*10^af),main='',xlab='BMSP')
	lines(density(a2$sims.list$BMSPc*10^af),col='red')
	
	plot(density(a2$sims.list$MSP*10^af),main='',xlab='MSP')
	lines(density(a2$sims.list$MSPc*10^af),col='red')

	plot(c(0,1),c(0,1),ann=F,bty='n',type='n',xaxt='n',yaxt='n')
	legend(0.2,0.7,c('Det','Sto'),lty=c(1,1),col=c('black','red'),lwd=2,bty='n')
	
dev.off()
	}
#---------------------------

