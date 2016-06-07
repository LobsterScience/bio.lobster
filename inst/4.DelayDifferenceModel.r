# Script for running delay-difference population model for scallop stocks.
#	The following is an example of a few formulations using data from scallop 
#   stocks in Golden Bay, New Zealand and Georges Bank, Canada.
#	Script reads in data, sets up priors, call WinBUGS to fit the model and 
#   runs a few plotting functions in R 


	# Data Inputs (LFA.dat)
	#
	# C = Catch
	# I = Biomass index for commercial size from survey
	# IR = Biomass index for reruit size from survey
	# w.bar = average weight of commercial size scallop
	# w.k = average weight of recruit size scallop

	# Other potential inputs
	#
	# U = Commercial CPUE index
	# I.cv, IR.cv, U.cv = Coefficients of variation for biomass indices
	# CF = annual estimates of average condition factor for the stock
	# l.bar = average shell width of commercial size scallop
	# l.k = average shell width of recruit size scallop
	# N = total numbers of commercial size scallops from survey
	# NR = total numbers of recruit size scallops from survey
	# clappers = total numbers of dead (cluckers) commercial size scallops from survey
	# clappersR =  total numbers of dead (cluckers) recruit size scallops from survey


# FSRS data
		
	# this section is to deal with the fact that there are uneven binning going on for the different size categories
	# it creates a pseudo CL (the mid point of each size category)
	loadfunctions('lobster')
	lobster.db("fsrs")

	scd<-read.csv(file.path( project.datadirectory("lobster"), "data","inputs","FSRS_SIZE_CODES.csv"))
	mls<-read.csv(file.path( project.datadirectory("lobster"), "data","inputs","MinLegalSize.csv"))

	scd$LENGTH<-rowMeans(scd[c("MIN_S","MAX_S")])
	FSRS.dat<-merge(subset(fsrs,LFA<35),scd[c("SIZE_CD","LENGTH")])

	wa<-c(0.000608, 0.001413, 0.00482)
	wb<-c(3.058, 2.875, 2.638)

	FSRS.dat$WEIGHT<-NA
	for(i in 1:3){
		FSRS.dat$WEIGHT[FSRS.dat$SEX==i]<-FSRS.dat$LENGTH[FSRS.dat$SEX==i]^wb[i]*wa[i]
	}

	FSRS.dat$SYEAR<-FSRS.dat$HAUL_YEAR
	FSRS.dat$HAUL_DATE<-as.Date(FSRS.dat$HAUL_DATE)
	FSRS.dat$SYEAR[FSRS.dat$LFA%in%c("33","34")]<-as.numeric(substr(FSRS.dat$S_LABEL[FSRS.dat$LFA%in%c("33","34")],6,9))
	FSRS.dat<-subset(FSRS.dat,SOAK_DAYS<6)	# Remove soak days greater than 5

	lfas<-sort(unique(FSRS.dat$LFA))[-10]
	lc=ncol(mls)-1
	lr=nrow(mls)
	mls2<-with(mls,data.frame(Year=rep(Year,lc),LFA=sort(rep(lfas,lr)),MLS=c(LFA27, LFA28, LFA29, LFA30, LFA31a, LFA31b, LFA32, LFA33, LFA34)))

	LVBpar=list(linf=281,k=0.065,t0=0)

	yrs<- sort(unique(FSRS.dat$SYEAR))
	for(y in yrs){
		for(i in lfas){
		FSRS.dat$RS[FSRS.dat$SYEAR==y&FSRS.dat$LFA==i]<-age.back(mls2$MLS[mls2$Year==y&mls2$LFA==i],LVB=LVBpar,age.dif=-1)$suggested.bin
		}
	}

	FSRS.dat$RECRUITS<-0
	FSRS.dat$RECRUITS[FSRS.dat$SHORT==1&FSRS.dat$LENGTH>=FSRS.dat$RS]<-FSRS.dat$WEIGHT[FSRS.dat$SHORT==1&FSRS.dat$LENGTH>=FSRS.dat$RS]
	FSRS.dat$BIOMASS<-0
	FSRS.dat$BIOMASS[FSRS.dat$SHORT==0]<-FSRS.dat$WEIGHT[FSRS.dat$SHORT==0]
	FSRS.dat$WBAR<-FSRS.dat$BIOMASS
	FSRS.dat$WK<-FSRS.dat$RECRUITS

	LD1 <- aggregate(cbind(WBAR,WK) ~ LFA + SYEAR, data=FSRS.dat,mean, na.rm=T)
	LD2 <- aggregate(cbind(TRAP_NO,RECRUITS,BIOMASS) ~ LFA + SYEAR, data=FSRS.dat,sum, na.rm=T)
	LobsterData<-merge(LD1,LD2)
	LobsterData$R<-LobsterData$RECRUITS/LobsterData$TRAP_NO
	LobsterData$B<-LobsterData$BIOMASS/LobsterData$TRAP_NO


	LandingsUpdate<-read.csv(file.path( project.datadirectory("lobster"), "data","inputs","AnnualandSeasonalLandingsLFA27-38.LFS2015.csv"))
	LandingsUpdate$YEAR<-as.numeric(substr(LandingsUpdate$YEAR,1,4))
	Landat<-merge(subset(LandingsUpdate,TYPE=="Annual"&YEAR>1998,c("YEAR","LFA27", "LFA28", "LFA29", "LFA30", "LFA31A", "LFA31B", "LFA32")),
	subset(LandingsUpdate,TYPE=="Seasonal"&YEAR>1998,c("YEAR","LFA33", "LFA34")))
	Landings<-with(Landat,data.frame(LFA=sort(rep(lfas,nrow(Landat))),SYEAR=rep(YEAR,lc),C=c(LFA27, LFA28, LFA29, LFA30, LFA31A, LFA31B, LFA32, LFA33, LFA34)))

	LobsterData<-merge(LobsterData,Landings)

#LobsterData <- read.csv()
head(LobsterData)
#these data assume recruit sized scallops will recruit to the commercial size (90 mm) the following year
#the data have been scaled by sampled fraction, area swept by the dredge, and stratum area (but not dredge efficiency).
#instead, catchability q is estimated in the model.

###### Basic Version ######

		yrs<-2000:2014	# years of data
		LFA.dat<-with(subset(LobsterData, LFA == 34&SYEAR%in%yrs),list(C=C,I=B,IR=R,w.bar=WBAR,w.k=WK))
		NY<- length(yrs)
		LFA.dat$NY<-NY  # number of years


	## Growth

		# predict weight at age from LVB length at age

		Linf <- 281
		K <- 0.065
		t0 <- 0

		# back calculate 1 year to calculate recruit size 
		age.back(82.5,LVB=list(linf=Linf,k=K,t0=t0),age.dif=-1)




		LWa <- 0.000608
		LWb <- 3.058
		LWa * 100^LWb

		lvb <- Linf * (1 - exp(-K * (1:20 - t0)))
		waa.t <- lvb^LWb * LWa

		# fit weight at age model to calculate rho and alpha
		waa.tm1 <- c(NA, waa.t)
		waa.t <- c(waa.t, NA)
		waa.lm <- lm(waa.t ~ waa.tm1)
		alpha <- coef(waa.lm)[1]
		rho <- coef(waa.lm)[2]

		# Use rho and alpha to calculate Growth terms
		LFA.dat$g<-rho + alpha / LFA.dat$w.bar # biomass growth multiplier for commercial size
		LFA.dat$gR<-rho + alpha / LFA.dat$w.k # biomass growth multiplier for recruit size

		# Growth if you have CF, l.bar and l.k
		#LFA.dat$g<-getG(CF,l.bar,b=LWb,VB=c(144,0.4,0))$g
		#LFA.dat$gR<-getG(CF,l.k,b=LWb,VB=c(144,0.4,0))$g


		LFA.dat<-with(LFA.dat, list(C=C,I=I,IR=IR,g=g,gR=gR,NY=NY)) # it's important you don't give WinBUGS any information it doesn't need (w.bar, w.k)


	## Priors

		LFApriors=list(
			logK=			list(a=8,		b=8,		d="dnorm",		i1=7,	i2=5,	l=1		),		# scaler to total biomass
			r=				list(a=0, 		b=1,		d="dlnorm",		i1=0.2,	i2=0.9,	l=NY	),		# recruit index
			m=				list(a=-1.6, 	b=0.5,		d="dlnorm",		i1=0.2,	i2=0.3,	l=NY	),		# mortality commercial size
			mR=				list(a=-1.6, 	b=0.5,		d="dlnorm",		i1=0.2,	i2=0.3,	l=NY	),		# mortality recruits
			q=				list(a=1, 		b=1, 		d="dbeta",		i1=0.2,	i2=0.5,	l=1		),		# catchability for survey commercial size
			qR=				list(a=1, 		b=1, 		d="dbeta",		i1=0.2,	i2=0.5,	l=1		),		# catchability for survey  recruits
			sigma=			list(a=0, 		b=5,		d="dunif",		i1=2,	i2=3,	l=1		),		# process error (SD)
			itau2=			list(a=3, 		b=0.44629,	d="dgamma",		i1=15,	i2=30,	l=1		),		# measurement error for commercial size (precision)
			iepsilon2=		list(a=3, 		b=0.44629,	d="dgamma",		i1=15,	i2=30,	l=1		)		# measurement error for recruits (precision)
		)

		# Check priors by plotting the distributions in R
		#plot(seq(0,5,l=100),dlnorm(seq(0,5,l=100),-1.6,1.4),type='l') 		# m
		#plot(seq(0,1,l=100),dbeta(seq(0,1,l=100),40,40),type='l') 			# q
		#lines(seq(0,1,l=100),dbeta(seq(0,1,l=100),7,15),col="red") 		# qR

	## Runs WinBUGS
	LFA.out<-delayBUGS("DDLFA", LFA.dat, LFApriors, yrs, n = 60000, burn = 30000, thin = 10,debug=F,parameters=c(names(LFApriors),'K','P','B','R','mu','Iresid','IRresid','Presid'),sw='jags')

	## Saves Model Output
	save(list=c("LFA.out","LFA.dat"),file="LFA.Rdata")

	## Plotting model results

		# plot fits to abundance indices 
		fit.plt(LFA.out, CI=T,graphic='R')

		# plot the posterior distributions of the estimated parameters
		post.plt(LFA.out,LFApriors,years=yrs, graphic='R',nr=3,nc=3,wd=15,multi=F)
		#post.plt(LFA.out,LFApriors,years=yrs, graphic='R',nr=2,nc=3,wd=15,multi=T)

		# plot the biomass estimates for commercial and recruit size scallops
		biomass.plt(LFA.out,years=yrs, graphic='R')	

		# plot the expliotion rate and natural survival fraction
		exploit.plt(LFA.out, years=yrs, plt=c('f','m','mR'),graphic='R')

		# plot residuals for the fit and process (process residuals represent the difference between what the dynamics and the data say about biomass)
		# Note: with current version of the data there are some large process reiduals 
		diag.plt(LFA.out, yrs,graphic='R')

		# add projected biomass to the model output object for various catch senarios (C.p)
		#source("_Rfunctions/projections.r")
		#LFA.out<-projections(LFA.out,C.p=seq(20,300,20))

		# preform the prediction evaluation procedure:
		# runs model up to years in pe predicting biomass in the nnext year then compares that prediction 
		# to the estimate when the model is fit up to that year
		#source("_Rfunctions/peR.r")
		#peR("LFA", LFA.dat, LFApriors, yrs, pe=2012:2004,n = 60000, burn = 30000, thin = 10, plot=0,lab='NZGB1',debug=F,wd=MyDirectory) # models running
		#peR("LFA", LFA.dat, LFApriors, yrs, pe=2012:2004,run=F, plot=3,graphic='R',lab='NZGB1') # plots results
		

 