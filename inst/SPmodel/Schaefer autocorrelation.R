		#Based on K. Holsmans MEPS paper from CAMEO work
		
		#NOTES
		#K is limited by top down factors (predation)
		#r is limited by bottom up factors (prey) which we are also assuming to be what is affected by Environmental factors December 16, 2011 02:24:03 PM (also Mueter and Megrey 2006)
		#Franz Mueter suggests that envt correlates are best modeled by multiplicative effects December 16, 2011 02:25:29 PM via email as well as mueter and megrey 2006
	
#source('C:/Documents and Settings/cooka/Desktop/Scripts/Surplus Production dec 2011/Schaefer autocorrelation.R')

Schaefer.env<-function(pars,data,env.data) {
		#Based on K. Holsmans MEPS paper from CAMEO work
				
		#PARAMETER SECTION
					logr = pars[1]
					logK = pars[2]
					sigma = pars[3]
					philogit = pars[4] #autocorrelation parameter
					gamma = pars[5] #environmental parameter

		#RECALCULATE PARAMETERS					
					r = exp(logr)
					K = exp(logK)
					sigma = sigma
					phi = (1-exp(philogit))/(1+exp(philogit))
					
		#SETUP VECTORS FOR FILLING IN
					n.data = nrow(data)
					NLL = numeric(n.data)
					ASP = data$ASP
					B = data$Biomass
					ASP.hat = numeric(n.data)
					eps = numeric(n.data)
					v = numeric(n.data)
					X = env.data
		
		#FIRST DATA POINT
					
					#ASP.hat[1] = (r*gamma*X[1])*B[1]*(1-B[1]/K)
					ASP.hat[1] = (r)*B[1]*(1-B[1]/K)
					eps[1] = ASP[1]-ASP.hat[1]
					sigma.2.use = sigma#/sqrt(1-phi^2)
				
					NLL[1] = -dnorm(eps[1],0,sigma.2.use,log=TRUE)
				
		#RECURSIVE FILL IN
					for (i in 2:n.data)
					{
						ASP.hat[i] = (r*gamma*X[i])*B[i]*(1-B[i]/K)+phi*eps[i-1]
						#ASP.hat[i] = (r)*B[i]*(1-B[i]/K)+phi
						eps[i] = ASP[i]-ASP.hat[i]		
						v[i] = eps[i] -phi*eps[i-1]
						NLL[i] = -dnorm(v[i],0,sigma,log=TRUE)
					}
					nll = sum(NLL)
		abc = list(ASP.hat=ASP.hat,nll=nll)
		return(abc)
		}


ASP.autocorr<-function(pars,data) {

	logr=pars[1]
	logK=pars[2]
	logsigma=pars[3]
	philogit=pars[4]
	
	r=exp(logr)
	K=exp(logK)
	sigma=exp(logsigma)
	phi=(1-exp(philogit))/(1+exp(philogit))
	
	n.data=nrow(data)

	NLL=rep(NA,n.data)
	# Set up data
	ASP=data[,2]
	B=data[,1]
	ASP.hat=rep(NA,n.data)
	ASP.bar=rep(NA,n.data)

	eps=rep(NA,n.data)
	v=rep(NA,n.data)
	# calculate NLL for first data point
	ASP.hat[1]=r*B[1]*(1-B[1]/K)
	eps[1]=ASP[1]-ASP.hat[1]
	sigma.2.use=sigma/sqrt(1-phi^2)

	NLL[1]=-dnorm(eps[1],0,sigma.2.use,log=TRUE)

	# Get rest of NLL
	for (i in 2:n.data)
	{
		ASP.hat[i]<-r*B[i]*(1-B[i]/K)+phi*eps[i-1]
		eps[i]<-ASP[i]-ASP.hat[i]		
		v[i]<-eps[i]-phi*eps[i-1]
		NLL[i]=-dnorm(v[i],0,sigma,log=TRUE)
	}
	sum(NLL)
}
