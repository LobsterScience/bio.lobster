#silver hake
setwd('C:/Documents and Settings/cooka/Desktop/Scripts/silver hake/surplus production')
source('C:/Documents and Settings/cooka/Desktop/Scripts/Surplus Production dec 2011/bugs/finding moments of distributions.R')
dat <- read.csv('dat.csv',header=T)
dat <- dat[dat$Year>=1970,]
			input1 <-  list()
			input1$N <- nrow(dat)
			input1$I <- dat[,3]
			#input1$I2 <- dat[,4]
			input1$C <- c(dat[,2]) # the rep is the landings for next 3 years
			input1$Cmat <- matrix(c(0,0,0,5,5,5,10,10,10,15,15,15,20,20,20,25,25,25),ncol=3,nrow=6,byrow=T)
			input1$cats <- 6
			input1$yrs<-3

			
			
#Two phase K
#Add in Priors
			Ku<-max(input1$I)
 			#rs <- find.moments(lo=0.1,up=0.8,l.perc=0.05,u.perc=0.95,dist='norm')
 			
			k1s <- find.moments(lo=1/Ku,up=1/(Ku*5),l.perc=0.05,u.perc=0.95,dist='lnorm')
			k2s <- find.moments(lo=1/Ku,up=1/(Ku*5),l.perc=0.05,u.perc=0.95,dist='lnorm')
			K1s <<- find.moments(lo=Ku,up=(Ku*5),l.perc=0.05,u.perc=0.95,dist='lnorm')
			K2s <<- find.moments(lo=Ku,up=(Ku*5),l.perc=0.05,u.perc=0.95,dist='lnorm')
			
			input1$r.a<-8.941450203 ; input1$r.b <- 1/0.993347893 #informative prior from mcallister work
			input1$k1.a<-k1s[[1]] ; input1$k1.b <- k1s[[2]]
			input1$k2.a<-k2s[[1]] ; input1$k2.b <- k2s[[2]]
			input1$q.a<-0.05 ; input1$q.b <- 1
			#Process = a b observation=c d
			input1$a0<-0.000001;  input1$b0<-5; 
			input1$c0<-0.000001; input1$d0<-5;
			
			#input1$nyrs=3 #for projections


		
		
spBUGS3 <- function(input=input1,inits=inits,n = 100000, burn = 20000, thin = 100, debug = F, wd="C:/Documents and Settings/cooka/Desktop/Scripts/silver hake/surplus production/"){
		require(R2WinBUGS)
			
	#	Initial values for WinBUGS one for each chain
		inits <- list(list(P=rep(0.75,times=input$N),
						r1=0.8,
						k1=0.003,
        				q1=0.4,
        				P.p = matrix(0.1,ncol=4,nrow=6)
   						     					),
				list(P=rep(0.8,times=input$N),
						r1=0.9,
						k1=0.005,
        				q1=0.4,
        				P.p = matrix(0.1,ncol=4,nrow=6)
        				       				
     					))
			
		# Parameters to be returned 
		parameters <- c('itau2','isigma2','Tau2','Sigma2','r1','r2','K1','K2','q1','q2','B','Imean','P.res','Bmat')
					
		
		## Call to WinBUGS ##
		tmp <- bugs(data = input, inits, parameters.to.save = parameters, model.file = paste(wd,"sp32K2r2q2tau2sig.bug",sep=""), 
		n.chains = 2, n.iter = n, n.burnin = burn, n.thin = thin, bugs.directory = "C:/Program Files/WinBUGS14/", debug = debug)
		return(tmp)
	} 

a<- spBUGS3(debug=T)
dput(a,'sp model Silverhake 19702011 2k2r2q final.txt')

setwd('C:/Documents and Settings/cooka/Desktop/Scripts/silver hake/surplus production')
a <- dget('sp model Silverhake 19702011 2k2r2q final.txt')



#3 qs
setwd('C:/Documents and Settings/cooka/Desktop/Scripts/silver hake/surplus production')
source('C:/Documents and Settings/cooka/Desktop/Scripts/Surplus Production dec 2011/bugs/finding moments of distributions.R')
dat <- read.csv('dat.csv',header=T)
			input1 <-  list()
			input1$N <- nrow(dat)
			input1$I <- dat[,3]
			#input1$I2 <- dat[,4]
			input1$C <- c(dat[,2],rep(0,3)) # the rep is the landings for next 3 years
			input1$I[1:13]<-input1$I[1:13]/2.3 #remove the catchability 

#Add in Priors
			Ku<-max(input1$I)
 			rs <- find.moments(lo=0.1,up=1.1,l.perc=0.05,u.perc=0.95,dist='norm')
			ks <- find.moments(lo=1/(Ku),up=1/(Ku*10),l.perc=0.05,u.perc=0.95,dist='lnorm')
			Ks <<- find.moments(lo=Ku*0.5,up=(Ku*2.5),l.perc=0.05,u.perc=0.95,dist='lnorm')
			
			input1$r.a<-rs[[1]] ; input1$r.b <- rs[[2]]
			input1$k.a<-ks[[1]] ; input1$k.b <- ks[[2]]
			input1$q.a<-0.0001 ; input1$q.b <- 1.1
			#Process = a b observation=c d
			input1$a0<-0.000001;  input1$b0<-100; 
			input1$c0<-0.000001; input1$d0<-100;
			input1$nyrs=3 #for projections


		
		
spBUGS3 <- function(input=input1,inits=inits,n = 100000, burn = 20000, thin = 10, debug = F, wd="C:/Documents and Settings/cooka/Desktop/Scripts/silver hake/surplus production/"){
		require(R2WinBUGS)
			
	#	Initial values for WinBUGS one for each chain
		inits <- list(list(P=rep(0.75,times=input$N+3),
						r=0.55,
						k=0.003,
        				q1=1,
        				q2=1,
        				q3=1
   						     					),
				list(P=rep(0.8,times=input$N+3),
						r=0.4,
						k=0.005,
        				q1=1,
        				q2=1,
        				q3=1 
     					))
			
		# Parameters to be returned 
		parameters <- c('itau2','isigma2','Tau2','Sigma2','r','K','MSP','MSPc','BMSP','BMSPc','FMSP','FMSPc','B','Imean','q1','q2','q3','P.res')
					
		
		## Call to WinBUGS ##
		tmp <- bugs(data = input, inits, parameters.to.save = parameters, model.file = paste(wd,"sp3q3.bug",sep=""), 
		n.chains = 2, n.iter = n, n.burnin = burn, n.thin = thin, bugs.directory = "C:/Program Files/WinBUGS14/", debug = debug)
		return(tmp)
	} 

a<- spBUGS3(debug=T)
dput(a,'sp model Silverhake 3q.txt')

setwd('C:/Documents and Settings/cooka/Desktop/Scripts/silver hake/surplus production')
a <- dget('sp model Silverhake 3q.txt')

#2 qs starting in 1983

setwd('C:/Documents and Settings/cooka/Desktop/Scripts/silver hake/surplus production')
source('C:/Documents and Settings/cooka/Desktop/Scripts/Surplus Production dec 2011/bugs/finding moments of distributions.R')
dat <- read.csv('dat.csv',header=T)
dat <- dat[dat$Year>1982,]
			
			input1 <-  list()
			input1$N <- nrow(dat)
			input1$I <- dat[,3]
			#input1$I2 <- dat[,4]
			input1$C <- c(dat[,2],rep(0,3)) # the rep is the landings for next 3 years

#Add in Priors
			Ku<-max(input1$I)
 			rs <- find.moments(lo=0.1,up=1.1,l.perc=0.05,u.perc=0.95,dist='norm')
			ks <- find.moments(lo=1/(Ku),up=1/(Ku*10),l.perc=0.05,u.perc=0.95,dist='lnorm')
			Ks <<- find.moments(lo=Ku*0.5,up=(Ku*2.5),l.perc=0.05,u.perc=0.95,dist='lnorm')
			
			input1$r.a<-rs[[1]] ; input1$r.b <- rs[[2]]
			input1$k.a<-ks[[1]] ; input1$k.b <- ks[[2]]
			input1$q.a<-0.0001 ; input1$q.b <- 1.1
			#Process = a b observation=c d
			input1$a0<-0.000001;  input1$b0<-100; 
			input1$c0<-0.000001; input1$d0<-100;
			input1$nyrs=3 #for projections


		
		
spBUGS3 <- function(input=input1,inits=inits,n = 100000, burn = 20000, thin = 10, debug = F, wd="C:/Documents and Settings/cooka/Desktop/Scripts/silver hake/surplus production/"){
		require(R2WinBUGS)
			
	#	Initial values for WinBUGS one for each chain
		inits <- list(list(P=rep(0.75,times=input$N+3),
						r=0.55,
						k=0.003,
        				q1=1,
        				q2=1
   						     					),
				list(P=rep(0.8,times=input$N+3),
						r=0.4,
						k=0.005,
        				q1=1,
        				q2=1 
     					))
			
		# Parameters to be returned 
		parameters <- c('itau2','isigma2','Tau2','Sigma2','r','K','MSP','MSPc','BMSP','BMSPc','FMSP','FMSPc','B','Imean','q1','q2','P.res')
					
		
		## Call to WinBUGS ##
		tmp <- bugs(data = input, inits, parameters.to.save = parameters, model.file = paste(wd,"sp3q2.bug",sep=""), 
		n.chains = 2, n.iter = n, n.burnin = burn, n.thin = thin, bugs.directory = "C:/Program Files/WinBUGS14/", debug = debug)
		return(tmp)
	} 

a<- spBUGS3(debug=T)


#2 qs starting in 1983 and iTQ

setwd('C:/Documents and Settings/cooka/Desktop/Scripts/silver hake/surplus production')
source('C:/Documents and Settings/cooka/Desktop/Scripts/Surplus Production dec 2011/bugs/finding moments of distributions.R')
dat <- read.csv('dat.csv',header=T)
dat <- dat[dat$Year>1982,]
			
			input1 <-  list()
			input1$N <- nrow(dat)
			input1$I1 <- dat[,3]
			input1$I2 <- dat[,4]
			input1$C <- c(dat[,2],rep(0,3)) # the rep is the landings for next 3 years

#Add in Priors
			Ku<-max(input1$I)
 			rs <- find.moments(lo=0.1,up=1.1,l.perc=0.05,u.perc=0.95,dist='norm')
			ks <- find.moments(lo=1/(Ku),up=1/(Ku*10),l.perc=0.05,u.perc=0.95,dist='lnorm')
			Ks <<- find.moments(lo=Ku*0.5,up=(Ku*2.5),l.perc=0.05,u.perc=0.95,dist='lnorm')
			
			input1$r.a<-rs[[1]] ; input1$r.b <- rs[[2]]
			input1$k.a<-ks[[1]] ; input1$k.b <- ks[[2]]
			input1$q.a<-0.0001 ; input1$q.b <- 1.1
			#Process = a b observation=c d
			input1$a0<-0.000001;  input1$b0<-100; 
			input1$c0<-0.000001; input1$d0<-100;
			input1$nyrs=3 #for projections


		
		
spBUGS3 <- function(input=input1,inits=inits,n = 100000, burn = 20000, thin = 10, debug = F, wd="C:/Documents and Settings/cooka/Desktop/Scripts/silver hake/surplus production/"){
		require(R2WinBUGS)
			
	#	Initial values for WinBUGS one for each chain
		inits <- list(list(P=rep(0.75,times=input$N+3),
						r=0.55,
						k=0.003,
        				q1=1,
        				q2=1
   						     					),
				list(P=rep(0.8,times=input$N+3),
						r=0.4,
						k=0.005,
        				q1=1,
        				q2=1 
     					))
			
		# Parameters to be returned 
		parameters <- c('itau2','isigma2','Tau2','Sigma2','Gam2','r','K','MSP','MSPc','BMSP','BMSPc','FMSP','FMSPc','B','Imean','q1','q2','P.res')
					
		
		## Call to WinBUGS ##
		tmp <- bugs(data = input, inits, parameters.to.save = parameters, model.file = paste(wd,"sp3q22I.bug",sep=""), 
		n.chains = 2, n.iter = n, n.burnin = burn, n.thin = thin, bugs.directory = "C:/Program Files/WinBUGS14/", debug = debug)
		return(tmp)
	} 

a<- spBUGS3(debug=T)
dput(a,'sp model Silverhake 2q 2I.txt')


#1 q starting in 1992 and iTQ

setwd('C:/Documents and Settings/cooka/Desktop/Scripts/silver hake/surplus production')
source('C:/Documents and Settings/cooka/Desktop/Scripts/Surplus Production dec 2011/bugs/finding moments of distributions.R')
dat <- read.csv('dat.csv',header=T)
dat <- dat[dat$Year>1992,]
			
			input1 <-  list()
			input1$N <- nrow(dat)
			input1$I1 <- dat[,3]
			input1$I2 <- dat[,4]
			input1$C <- c(dat[,2],rep(0,3)) # the rep is the landings for next 3 years

#Add in Priors
			Ku<-max(input1$I1)
 			rs <- find.moments(lo=0.1,up=1.1,l.perc=0.05,u.perc=0.95,dist='norm')
			ks <- find.moments(lo=1/(Ku),up=1/(Ku*10),l.perc=0.05,u.perc=0.95,dist='lnorm')
			Ks <<- find.moments(lo=Ku*0.5,up=(Ku*2.5),l.perc=0.05,u.perc=0.95,dist='lnorm')
			
			input1$r.a<-rs[[1]] ; input1$r.b <- rs[[2]]
			input1$k.a<-ks[[1]] ; input1$k.b <- ks[[2]]
			input1$q.a<-0.0001 ; input1$q.b <- 1.1
			#Process = a b observation=c d
			input1$a0<-0.000001;  input1$b0<-100; 
			input1$c0<-0.000001; input1$d0<-100;
			input1$nyrs=3 #for projections


		
		
spBUGS3 <- function(input=input1,inits=inits,n = 100000, burn = 20000, thin = 10, debug = F, wd="C:/Documents and Settings/cooka/Desktop/Scripts/silver hake/surplus production/"){
		require(R2WinBUGS)
			
	#	Initial values for WinBUGS one for each chain
		inits <- list(list(P=rep(0.75,times=input$N+3),
						r=0.55,
						k=0.003,
        				q1=1
   						     					),
				list(P=rep(0.8,times=input$N+3),
						r=0.4,
						k=0.005,
        				q1=1
        				 
     					))
			
		# Parameters to be returned 
		parameters <- c('itau2','isigma2','Tau2','Sigma2','Gam2','r','K','MSP','MSPc','BMSP','BMSPc','FMSP','FMSPc','B','Imean','q1','P.res')
					
		
		## Call to WinBUGS ##
		tmp <- bugs(data = input, inits, parameters.to.save = parameters, model.file = paste(wd,"sp3q1I2.bug",sep=""), 
		n.chains = 2, n.iter = n, n.burnin = burn, n.thin = thin, bugs.directory = "C:/Program Files/WinBUGS14/", debug = debug)
		return(tmp)
	} 

a<- spBUGS3(debug=T)
dput(a,'sp model Silverhake 1q 2I 1993.txt')


#1 q starting in 1992 and iTQ no observation error

setwd('C:/Documents and Settings/cooka/Desktop/Scripts/silver hake/surplus production')
source('C:/Documents and Settings/cooka/Desktop/Scripts/Surplus Production dec 2011/bugs/finding moments of distributions.R')
dat <- read.csv('dat.csv',header=T)
dat <- dat[dat$Year>1992,]
			
			input1 <-  list()
			input1$N <- nrow(dat)
			input1$I1 <- dat[,3]
			input1$I2 <- dat[,4]
			input1$OE1 <- dat[,5]
			input1$OE2 <- dat[,6]
			input1$C <- c(dat[,2],rep(0,3)) # the rep is the landings for next 3 years

#Add in Priors
			Ku<-max(input1$I1)
 			rs <- find.moments(lo=0.1,up=1.1,l.perc=0.05,u.perc=0.95,dist='norm')
			ks <- find.moments(lo=1/(Ku),up=1/(Ku*10),l.perc=0.05,u.perc=0.95,dist='lnorm')
			Ks <<- find.moments(lo=Ku*0.5,up=(Ku*2.5),l.perc=0.05,u.perc=0.95,dist='lnorm')
			
			input1$r.a<-rs[[1]] ; input1$r.b <- rs[[2]]
			input1$k.a<-ks[[1]] ; input1$k.b <- ks[[2]]
			input1$q.a<-0.0001 ; input1$q.b <- 1.1
			#Process = a b observation=c d
			input1$a0<-0.000001;  input1$b0<-100; 
			input1$c0<-0.000001; input1$d0<-100;
			input1$nyrs=3 #for projections


		
		
spBUGS3 <- function(input=input1,inits=inits,n = 100000, burn = 20000, thin = 10, debug = F, wd="C:/Documents and Settings/cooka/Desktop/Scripts/silver hake/surplus production/"){
		require(R2WinBUGS)
			
	#	Initial values for WinBUGS one for each chain
		inits <- list(list(P=rep(0.75,times=input$N+3),
						r=0.55,
						k=0.003,
        				q1=1
   						     					),
				list(P=rep(0.8,times=input$N+3),
						r=0.4,
						k=0.005,
        				q1=1
        				 
     					))
			
		# Parameters to be returned 
		parameters <- c('Sigma2','r','K','MSP','MSPc','BMSP','BMSPc','FMSP','FMSPc','B','Imean','q1','P.res')
					
		
		## Call to WinBUGS ##
		tmp <- bugs(data = input, inits, parameters.to.save = parameters, model.file = paste(wd,"sp3q1I2nooe.bug",sep=""), 
		n.chains = 2, n.iter = n, n.burnin = burn, n.thin = thin, bugs.directory = "C:/Program Files/WinBUGS14/", debug = debug)
		return(tmp)
	} 

a<- spBUGS3(debug=T)
dput(a,'sp model Silverhake 1q 2I no oe.txt')


#1 q starting in 1992  no observation error

setwd('C:/Documents and Settings/cooka/Desktop/Scripts/silver hake/surplus production')
source('C:/Documents and Settings/cooka/Desktop/Scripts/Surplus Production dec 2011/bugs/finding moments of distributions.R')
dat <- read.csv('dat.csv',header=T)
dat <- dat[dat$Year>1992,]
			
			input1 <-  list()
			input1$N <- nrow(dat)
			input1$I1 <- dat[,3]
			input1$OE1 <- dat[,5]
			input1$C <- c(dat[,2],rep(0,3)) # the rep is the landings for next 3 years

#Add in Priors
			Ku<-max(input1$I1)
 			rs <- find.moments(lo=0.1,up=1.1,l.perc=0.05,u.perc=0.95,dist='norm')
			ks <- find.moments(lo=1/(Ku),up=1/(Ku*10),l.perc=0.05,u.perc=0.95,dist='lnorm')
			Ks <<- find.moments(lo=Ku*0.5,up=(Ku*2.5),l.perc=0.05,u.perc=0.95,dist='lnorm')
			
			input1$r.a<-rs[[1]] ; input1$r.b <- rs[[2]]
			input1$k.a<-ks[[1]] ; input1$k.b <- ks[[2]]
			input1$q.a<-0.0001 ; input1$q.b <- 1.1
			#Process = a b observation=c d
			input1$a0<-0.000001;  input1$b0<-100; 
			input1$c0<-0.000001; input1$d0<-100;
			input1$nyrs=3 #for projections


		
		
spBUGS3 <- function(input=input1,inits=inits,n = 100000, burn = 20000, thin = 10, debug = F, wd="C:/Documents and Settings/cooka/Desktop/Scripts/silver hake/surplus production/"){
		require(R2WinBUGS)
			
	#	Initial values for WinBUGS one for each chain
		inits <- list(list(P=rep(0.75,times=input$N+3),
						r=0.55,
						k=0.003,
        				q1=1
   						     					),
				list(P=rep(0.8,times=input$N+3),
						r=0.4,
						k=0.005,
        				q1=1
        				 
     					))
			
		# Parameters to be returned 
		parameters <- c('Sigma2','r','K','MSP','MSPc','BMSP','BMSPc','FMSP','FMSPc','B','Imean','q1','P.res')
					
		
		## Call to WinBUGS ##
		tmp <- bugs(data = input, inits, parameters.to.save = parameters, model.file = paste(wd,"sp3q1I1nooe.bug",sep=""), 
		n.chains = 2, n.iter = n, n.burnin = burn, n.thin = thin, bugs.directory = "C:/Program Files/WinBUGS14/", debug = debug)
		return(tmp)
	} 

a<- spBUGS3(debug=T)
dput(a,'sp model Silverhake 2q 2I.txt')


#1 q starting in 1992

setwd('C:/Documents and Settings/cooka/Desktop/Scripts/silver hake/surplus production')
source('C:/Documents and Settings/cooka/Desktop/Scripts/Surplus Production dec 2011/bugs/finding moments of distributions.R')
dat <- read.csv('dat.csv',header=T)
dat <- dat[dat$Year>1992,]
			
			input1 <-  list()
			input1$N <- nrow(dat)
			input1$I1 <- dat[,3]

			input1$C <- c(dat[,2],rep(8,3)) # the rep is the landings for next 3 years

#Add in Priors
			Ku<-max(input1$I1)
 			rs <- find.moments(lo=0.4,up=0.9,l.perc=0.05,u.perc=0.95,dist='norm')
			ks <- find.moments(lo=1/(Ku),up=1/(Ku*10),l.perc=0.05,u.perc=0.95,dist='lnorm')
			Ks <<- find.moments(lo=Ku*0.5,up=(Ku*2.5),l.perc=0.05,u.perc=0.95,dist='lnorm')
			
			input1$r.a<-rs[[1]] ; input1$r.b <- rs[[2]]
			input1$k.a<-ks[[1]] ; input1$k.b <- ks[[2]]
			input1$q.a<-0.0001 ; input1$q.b <- 1.1
			#Process = a b observation=c d
			input1$nyrs=3 #for projections


		
		
spBUGS3 <- function(input=input1,inits=inits,n = 100000, burn = 20000, thin = 10, debug = F, wd="C:/Documents and Settings/cooka/Desktop/Scripts/silver hake/surplus production/"){
		require(R2WinBUGS)
			
	#	Initial values for WinBUGS one for each chain
		inits <- list(list(P=rep(0.75,times=input$N+3),
						r=0.55,
						k=0.003,
        				q1=1
   						     					),
				list(P=rep(0.8,times=input$N+3),
						r=0.4,
						k=0.005,
        				q1=1
        				 
     					))
			
		# Parameters to be returned 
		parameters <- c('Tau2','Sigma2','r','K','MSP','MSPc','BMSP','BMSPc','FMSP','FMSPc','B','Imean','q1','P.res')
					
		
		## Call to WinBUGS ##
		tmp <- bugs(data = input, inits, parameters.to.save = parameters, model.file = paste(wd,"sp3q1I1.bug",sep=""), 
		n.chains = 2, n.iter = n, n.burnin = burn, n.thin = thin, bugs.directory = "C:/Program Files/WinBUGS14/", debug = debug)
		return(tmp)
	} 

a<- spBUGS3(debug=T)
dput(a,'sp model Silverhake 1q I1992.txt')




#2 

source('C:/Documents and Settings/cooka/Desktop/Scripts/Surplus Production dec 2011/bugs/finding moments of distributions.R')
dat <- read.csv('dat.csv',header=T)
dat <- dat[dat$Year>=1970,]
			input1 <-  list()
			input1$N <- nrow(dat)
			input1$I <- dat[,3]
			#input1$I2 <- dat[,4]
			input1$C <- c(dat[,2],rep(0,3)) # the rep is the landings for next 3 years
			

			
			

#Add in Priors
			Ku<-max(input1$I)
 			#rs <- find.moments(lo=0.1,up=0.8,l.perc=0.05,u.perc=0.95,dist='norm')
 			
			k1s <- find.moments(lo=1/Ku,up=1/(Ku*5),l.perc=0.05,u.perc=0.95,dist='lnorm')
			K1s <<- find.moments(lo=Ku,up=(Ku*5),l.perc=0.05,u.perc=0.95,dist='lnorm')

			
			input1$r.a<-8.941450203 ; input1$r.b <- 1/0.993347893 #informative prior from mcallister work
			input1$k1.a<-k1s[[1]] ; input1$k1.b <- k1s[[2]]
			input1$q.a<-0.01 ; input1$q.b <- 0.5
			#Process = a b observation=c d
			input1$a0<-0.000001;  input1$b0<-5; 
			input1$c0<-0.000001; input1$d0<-5;
			
			input1$nyrs=3 #for projections


		
		
spBUGS3 <- function(input=input1,inits=inits,n = 100000, burn = 20000, thin = 100, debug = F, wd="C:/Documents and Settings/cooka/Desktop/Scripts/silver hake/surplus production/"){
		require(R2WinBUGS)
			
	#	Initial values for WinBUGS one for each chain
		inits <- list(list(P=rep(0.75,times=input$N+3),
						r1=0.8,
						k1=0.003,
        				q1=0.4,
        							cy=10
        				
   						     					),
				list(P=rep(0.8,times=input$N+3),
						r1=0.9,
						k1=0.005,
        				q1=0.4,
        				cy=20
        				
     					))
			
		# Parameters to be returned 
		parameters <- c('itau2','isigma2','Tau2','Sigma2','r1','k1','k2','q1','q2','B','Imean','P.res','cy')
					
		
		## Call to WinBUGS ##
		tmp <- bugs(data = input, inits, parameters.to.save = parameters, model.file = paste(wd,"sp32K2r2qTshift.bug",sep=""), 
		n.chains = 2, n.iter = n, n.burnin = burn, n.thin = thin, bugs.directory = "C:/Program Files/WinBUGS14/", debug = debug)
		return(tmp)
	} 

a<- spBUGS3(debug=T)


#full two phase model

setwd('C:/Documents and Settings/cooka/Desktop/Scripts/silver hake/surplus production')
source('C:/Documents and Settings/cooka/Desktop/Scripts/Surplus Production dec 2011/bugs/finding moments of distributions.R')
dat <- read.csv('dat.csv',header=T)
dat <- dat[dat$Year>=1970,]
			input1 <-  list()
			input1$N <- nrow(dat)
			input1$I <- dat[,3]
			#input1$I2 <- dat[,4]
			input1$C <- c(dat[,2],rep(10,3)) # the rep is the landings for next 3 years
			

			
			
#Two phase K
#Add in Priors
			Ku<-max(input1$I)
 			#rs <- find.moments(lo=0.1,up=0.8,l.perc=0.05,u.perc=0.95,dist='norm')
 			
			k1s <- find.moments(lo=1/Ku,up=1/(Ku*5),l.perc=0.05,u.perc=0.95,dist='lnorm')
			k2s <- find.moments(lo=1/Ku,up=1/(Ku*5),l.perc=0.05,u.perc=0.95,dist='lnorm')
			K1s <<- find.moments(lo=Ku,up=(Ku*5),l.perc=0.05,u.perc=0.95,dist='lnorm')
			K2s <<- find.moments(lo=Ku,up=(Ku*5),l.perc=0.05,u.perc=0.95,dist='lnorm')
			
			input1$r.a<-8.941450203 ; input1$r.b <- 1/0.993347893 #informative prior from mcallister work
			input1$k1.a<-k1s[[1]] ; input1$k1.b <- k1s[[2]]
			input1$k2.a<-k2s[[1]] ; input1$k2.b <- k2s[[2]]
			input1$q.a<-0.05 ; input1$q.b <- 1
			#Process = a b observation=c d
			input1$a0<-0.000001;  input1$b0<-5; 
			input1$c0<-0.000001; input1$d0<-5;
			
			input1$nyrs=3 #for projections


		
		
spBUGS3 <- function(input=input1,inits=inits,n = 100000, burn = 20000, thin = 100, debug = F, wd="C:/Documents and Settings/cooka/Desktop/Scripts/silver hake/surplus production/"){
		require(R2WinBUGS)
			
	#	Initial values for WinBUGS one for each chain
		inits <- list(list(P=rep(0.75,times=input$N+3),
						r1=0.8,
						k1=0.003,
        				q1=0.4
   						     					),
				list(P=rep(0.8,times=input$N+3),
						r1=0.9,
						k1=0.005,
        				q1=0.4
        				
        				
     					))
			
		# Parameters to be returned 
		parameters <- c('Tau2','Sigma2','Tau21','Sigma21','r1','r2','K1','K2','q1','q2','B','Imean','P.res')
					
		
		## Call to WinBUGS ##
		tmp <- bugs(data = input, inits, parameters.to.save = parameters, model.file = paste(wd,"sp32K2r2q2tau2sig.bug",sep=""), 
		n.chains = 2, n.iter = n, n.burnin = burn, n.thin = thin, bugs.directory = "C:/Program Files/WinBUGS14/", debug = debug)
		return(tmp)
	} 
a<- spBUGS3(debug=T)
