#silver hake plots from winbugs
source('C:/Documents and Settings/cooka/Desktop/Scripts/Surplus Production dec 2011/bugs/finding moments of distributions.R')
source('C:/Documents and Settings/cooka/Desktop/Scripts/Surplus Production dec 2011/data in/cameo data scripts.R')

			op<-par(mfrow=c(3,2),mar=c(4,4,3,1))
			# r
			plot(density(rnorm(10000,input1$r.a,sqrt(input1$r.b))),xlab='r',xlim=c(0,1.5),lty=2,main='')
			lines(density(a$sims.list$r),col='red',lty=1)
			
			#K
			plot(density(rlnorm(10000,Ks[[1]],sqrt(Ks[[2]]))),xlab='K',lty=2,main='')
			lines(density(a$sims.list$K),col='red',lty=1)
			
			#q
			plot(1,1,type='n',xlim=c(input1$q.a/1.1,input1$q.b*1.1),xlab='q',main='',ylab='Density')
			polygon(x=c(input1$q.a,input1$q.a,input1$q.b,input1$q.b),y=c(0,dunif(c(input1$q.a,input1$q.b),input1$q.a,input1$q.b),0))
			lines(density(a$sims.list$q),col='red',lty=1)
	
			#process errors
			plot(1,1,type='n',xlim=c(0,1),xlab='Process Variance',main='',ylab='Density',ylim=c(0,0.6))
			polygon(x=c(0.25^2,0.25^2,1,1),y=c(0,0.4,0.4,0),border='black',lty=2)
			lines(density(sqrt(a$sims.list$Sigma)),col='red',lty=1,lwd=1)
			
			plot(1,1,type='n',xlim=c(0,1),xlab='Observation Variance',main='',ylab='Density',ylim=c(0,0.6))
			polygon(x=c(0.25^2,0.25^2,1,1),y=c(0,0.4,0.4,0),border='black',lty=2)
			lines(density(sqrt(a$sims.list$Tau2)),col='red',lty=1,lwd=1)
			
			
			plot(1993:2011,a$median$P.res,type='h',ylab='Residuals',xlab='Year')
			abline(h=0,col='red')
		
			par(op)

	#using biomass from model fits

				dat1<-data.frame(years=seq(1993,1993+(length(a$median$B)-1)),Biomass=0,Landings=0)
				dat1$Biomass<-a$median$B*10^3
				dat1$Landings<-input1$C*10^3
				
						dat1$ASP <- 0
						for(i in 1:(nrow(dat1)-1)) {
							dat1[i,'ASP'] <- dat1[i+1,'Biomass'] - dat1[i,'Biomass'] +dat1[i,'Landings'] 
						}

				#MSY V B plots
				plot.arr(dat1$ASP,dat1$Biomass)
									
				lines(x=c(0,a$median$BMSPc*10^3),y=c(a$median$MSPc*10^3,a$median$MSPc*10^3),lwd=2,col='blue')
				lines(x=c(a$median$BMSPc*10^3,a$median$BMSPc*10^3),y=c(0,a$median$MSPc*10^3),lwd=2,col='blue')
				lines(x=c(0,a$median$BMSP*10^3),y=c(a$median$MSP*10^3,a$median$MSP*10^3),lwd=2,col='blue')
				lines(x=c(a$median$BMSP*10^3,a$median$BMSP*10^3),y=c(0,a$median$MSP*10^3),lwd=2,col='blue')
				
				legend('topleft',c('Sto','Det'),col=c('black','blue'),lty=c(1,1),bty='n',cex=0.8)

				
					#MSY plots with 40 and 80 BMSY
						af=3
						fit <- lower <- upper <- numeric()
						N<-seq(1993,1993+(length(a$median$B)-1))
						n<-length(N)
						for(i in 1:length(N)) {
						fit[i]<-median(a$sims.list$B[,i])
						lower[i]<-quantile(a$sims.list$B[,i],0.25)
						upper[i]<-quantile(a$sims.list$B[,i],0.75)
						}
						yl <- c(min(c(fit,lower,upper)),max(c(fit,lower,upper)))
						plot(1,1, type='n',ylab='Biomass',xlab='Year',ylim=yl,xlim=c(1993,2015))
						#polygon(c(1992,2011,2011,1992),c(0.8*a$median$BMSP,0.8*a$median$BMSP,0.4*a$median$BMSP,0.4*a$median$BMSP),density=10,col='orange')
						lines(N[1:(length(N)-3)],fit[1:(length(N)-3)])
						lines(N[1:(length(N)-3)],lower[1:(length(N)-3)],lty=2)
						lines(N[1:(length(N)-3)],upper[1:(length(N)-3)],lty=2)
						
						lines(N[(length(N)-3):N]+0.5,fit[(length(N)-3):N],col='red')
						lines(N[(length(N)-3):N]+0.5,lower[(length(N)-3):N],lty=2,col='red')
						lines(N[(length(N)-3):N]+0.5,upper[(length(N)-3):N],lty=2,col='red')
						
						
						
						polygon(c(1993,2012,2012,1993),c(0.8*a$median$BMSPc,0.8*a$median$BMSPc,0.4*a$median$BMSPc,0.4*a$median$BMSPc),density=10,col='blue')
						#legend('topright',fill=c('orange','blue'),c('Det','Sto'),bty='n',cex=0.8,density=20)
						

par(mfrow=c(2,2))
			
	plot(density(a$sims.list$BMSP*10^af),main='',xlab='BMSP')
	lines(density(a$sims.list$BMSPc*10^af),col='red')
	
	plot(density(a$sims.list$MSP*10^af),main='',xlab='MSP')
	lines(density(a$sims.list$MSPc*10^af),col='red')

	plot(c(0,1),c(0,1),ann=F,bty='n',type='n',xaxt='n',yaxt='n')
	legend(0.2,0.7,c('Det','Sto'),lty=c(1,1),col=c('black','red'),lwd=2,bty='n')
	
dev.off()
	}
#---------------------------

