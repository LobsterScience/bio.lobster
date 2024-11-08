#script for scaefer autocorrelation.r
	
	setwd('C:/Adam/Surplus production modeling 2012')
	source('C:/Documents and Settings/cooka/Desktop/Scripts/Surplus Production dec 2011/data in/cameo data scripts.R')
	source('C:/Documents and Settings/cooka/Desktop/Scripts/Surplus Production dec 2011/Schaefer autocorrelation.R')

	options(stringsAsFactors=F)
	require(RODBC)
	channel <- odbcConnectAccess('CAMEO.mdb')

	ESS.species <- sqlQuery(channel,paste("select distinct b.species from best_available_biomass b, landings l where b.region='ESS' and l.species=b.species;"))
	WSS.species <- sqlQuery(channel,paste("select distinct b.species from best_available_biomass b, landings l where b.region='WSS' and l.species=b.species;"))
	
		
	dat <- all.dat.WSS(WSS.species[16,1])	

	
	ASP.models <-	function(dat, environmental.columns,lags=0:6) {
					#set up ASP
						dat$ASP <- 0
						for(i in 1:nrow(dat)-1) {
							dat[i,'ASP'] <- dat[i+1,'Biomass'] - dat[i,'Biomass'] + dat[i,'Landings'] 
						}
				plot.arr(dat$ASP,dat$Biomass)
				windows()
				with(dat,plot(Biomass,ASP))
				#set up environmental correlates		
						ec <- environmental.columns
					for(i in 1:length(ec)) {
					dat[,ec[i]] <- (dat[,ec[i]]-mean(dat[,ec[i]]))/sd(dat[,ec[i]])
					}
					
				#nls models w no lags
				#hoenig 1994 interms of MSY and BMSY schaefer parameterization
				
				#set up output table
				aa <- c('none',names(dat[,ec]))
				output <- data.frame(lags=rep(lags,each=length(aa)),Envt=t(t(rep(aa,times=length(lags)))),MSY=0,BMSY=0,Delta=0,AICc=0)
			 	output <- subset(output,!(Envt=='none' & lags>0))
			 	AICc <- function (npars,n,aic) { aic+(2*npars*(npars+1))/(n+npars+1)}
			 	h.start <- mean(dat$ASP)
			 	w.start <- mean(dat$Biomass)/0.8
b
			 	#no environmental correlates	
			 	 if(cor) {tr <- gnls(ASP ~ (2* h / w * Biomass - h / w ^2 * Biomass ^2) ,data=dat,start=c(h=20000,w=20000),correlation=corAR1(),control=list(nlsTol=.1,returnObject=T))}
					 	tr <- nls(ASP ~ (2* h / w * Biomass - h / w ^2 * Biomass ^2) ,data=dat,start=list(h=h.start,w=w.start))
						output[1,3] <- coef(tr)[1]
						output[1,4] <- coef(tr)[2]
						output[1,5] <- NA
						output[1,6] <- AICc(npars=2,n=nrow(dat),aic=AIC(tr))
					 aaa<-as.data.frame(cbind(dat$Biomass,predict(tr)))
					aaa<-aaa[order(aaa[,1]),]
					with(aaa,lines(aaa),col=1)

				#environmental correlates and lags
					mm <- 1
					for(i in 1:length(lags)) {
						if(lags[i]>0) {
						dat1 = as.data.frame(cbind(dat[-c(1:lags[i]),c(1:4,14)],dat[-c(nrow(dat):(nrow(dat)-lags[i]+1)),c(5:13)])) 	
						}
						else {dat1 = dat}
						
						for(j in 1:length(ec))	{
								mm <- mm+1	
								tr1 <- tryCatch(nls(ASP ~ (2* h / w * Biomass - h / w ^2 * Biomass ^2) * exp(g*dat[,ec[j]]),data=dat1,start=list(h=h.start,w=w.start,g=0)),error=function(e) NA)
								if(is.na(tr1)) {
								output[mm,3] <- NA
								output[mm,4] <- NA
								output[mm,5] <- NA
								output[mm,6] <- NA
								}
								else {
								output[mm,3] <- coef(tr1)[1]
								output[mm,4] <- coef(tr1)[2]
								output[mm,5] <- coef(tr1)[3]
								output[mm,6] <- AICc(npars=3,n=nrow(dat1),aic=AIC(tr1))
								}
						}
					}
				output$K <- output[,4]*2
				 output$r <- (output[,3]*4)/(2*output[,4])

		return(output)
		}
			dat <- all.dat.WSS(WSS.species[6,1])	
			dat <- all.dat.ESS(ESS.species[6,1])
		ASP.models(dat,5:13)

		
		

	
	#optim
	pars <- c(r = (0.3),logK = log(max(dat$Biomass)),sigma = log(1.1),philogit = log(1.1),gamma = log(1.1))
	fns <- function(pars) Schaefer.env(pars,dat,env.data=dat[,5])$nll
	
	o.res <- optim(fn=fns,pars,method='L-BFGS-B',lower=c(0.0001,log(max(dat$Biomass)/1.5),-100000,-1,-1000),upper=c(2,log(max(dat$Biomass)*4),100000,1,1000))
	
	Schaefer.env(o.res$par,dat,env.dat=dat[,5])
	

