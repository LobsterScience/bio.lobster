
  
	p = bio.lobster::load.environment()
	la()
		
	assessment.year = 2018 ########### check the year ############### !!!!!!!!!!!


    p$syr = 2005
    p$yrs = p$syr:p$current.assessment.year

    figdir = file.path(project.datadirectory("bio.lobster"),"figures","LFA3438Framework2019")

    p$lfas = c("34", "35", "36", "38") # specify lfas for data summary
  


#############################################################################3
############ Do LAA Model

	AllSurveyData=list()
	bins = seq(0,195,5)
	
	i = 1
	for(s in 1:3){
		for (l in 1:length(bins)){

			AllSurveyData[[i]] = SurveyTowData(Size.range=c(bins[l],bins[l]+5),Sex = s, Years=1970:2018,redo=T,lab=i,by.sex=T)

			AllSurveyData[[i]]$survey[AllSurveyData[[i]]$survey=="Scallop"&month(AllSurveyData[[i]]$date)<9] <- "ScallopSummer"
			AllSurveyData[[i]]$survey[AllSurveyData[[i]]$survey=="Scallop"&month(AllSurveyData[[i]]$date)>8] <- "ScallopFall"

			AllSurveyData[[i]]$survey[AllSurveyData[[i]]$survey=="LobsterBalloon"&month(AllSurveyData[[i]]$date)<9] <- "LobsterBalloonSummer"
			AllSurveyData[[i]]$survey[AllSurveyData[[i]]$survey=="LobsterBalloon"&month(AllSurveyData[[i]]$date)>8] <- "LobsterBalloonFall"
			
			AllSurveyData[[i]]$sex = s
			AllSurveyData[[i]]$CL = bins[l]+5
			i = i + 1
		}
	}
	AllSurveyTows = do.call("rbind",AllSurveyData)

	write.csv(AllSurveyTows,file.path(project.datadirectory('bio.lobster'),"data","products","AllSurveyTows.csv"),row.names=F)
	AllSurveyTows=read.csv(file.path(project.datadirectory('bio.lobster'),"data","products","AllSurveyTows.csv"))
	

	x=list()
	
	bins = seq(0,195,5)
	for (i in 1:length(bins)){
		x[[i]]=aggregate(LobDen~year+survey+sex,subset(AllSurveyTows,CL==bins[i]+5&LFA==34),mean )
		x[[i]]$len = i
	}

	surveyData = do.call("rbind",x)



	x=aggregate(LobDen~year+survey,surveyData,sum)

	s=unique(x$survey)
	x11()
	par(mfrow=c(4,2))
	for(i in 1:length(s))plot(LobDen~year,subset(x,survey==s[i]),type='b',main=s[i])


	yr=1995:2018
	y=list()
	survs=unique(surveyData$survey)
	for(s in 1:3){
		for(i in 1:length(survs)){
			x = subset(surveyData,survey==survs[i]&year%in%yr&sex==s,c('year','LobDen','len'))
			CLF = reshape(x[order(x$len),],idvar='year',timevar='len',direction='wide',sep='')

			y[[i]] = merge(CLF,data.frame(year=yr),all=T)[,-1]
		}	
		names(y)=survs
		BubblePlotCLF(y,bins=seq(0,200,5),yrs=yr,filen=paste0('surveys',s),prop=F,LS=82.5,inch=0.2,bg=rgb(0,0,1,0.1),cex.lab=2,cex.axis=1.5)
	}

######################################



	AllSurveyTows=read.csv(file.path(project.datadirectory('bio.lobster'),"data","products","AllSurveyTows.csv"))
	CatchComp=constructCatchComp()


	dat=buildDatObj(AllSurveyTows,CatchComp,yrs=1982:2018,bins = seq(0,195,5))

		

	dat$srmode <- 0
	dat$fcormode <- 2

	dat$keyF <- c(0,1,2,3,4,5,6,7,7)

	dat$keyQ <- rbind(c(NA,NA,NA,NA,NA,NA,NA,NA,NA),
	                  c(NA, 0, 1, 2, 3, 4, 5, 5,NA),
	                  c( 6, 7, 8, 9,10,10,NA,NA,NA))

	dat$keySd <- rbind(c( 0, 0, 0, 0, 0, 0, 0, 0, 0),
	                   c(NA, 1, 1, 1, 1, 1, 1, 1,NA),
	                   c( 2, 2, 2, 2, 2, 2,NA,NA,NA))

	dat$covType <- c(0,1,1) # 0=ID, 1=IGAR, 2=US ...

	                    #  0-1 1-2 2-3 3-4 4-5 5-6 6-7 7-8 
	dat$keyIGAR <- rbind(c( -1, -1, -1, -1, -1, -1, -1, -1),
	                     c( NA, 0,  1,  2,  3,  4,  5, NA),
	                     c( 6,  7,  8,  9,  9, NA, NA, NA))

	dat$noParUS <- sapply(1:length(dat$fleetTypes),
	                      function(f){
	                        A<-sum(!is.na(dat$keySd[f,]))
	                        ifelse(dat$covType[f]==2, (A*A-A)/2, 0)
	                      })

	par <- list()
	par$logsdR <-0
	par$logsdS <- 0
	par$logsdF <- numeric(max(dat$keyF)+1)
	par$rickerpar <- if(dat$srmode==1){c(1,1)}else{numeric(0)}
	par$transRhoF <- if(dat$fcormode==0){numeric(0)}else{0.1}
	par$bhpar <- if(dat$srmode==2){c(1,1)}else{numeric(0)}
	par$logQ <- numeric(max(dat$keyQ, na.rm=TRUE)+1)
	par$logsd <- numeric(max(dat$keySd, na.rm=TRUE)+1)
	par$logIGARdist <- numeric(max(dat$keyIGAR, na.rm=TRUE)+1)
	par$parUS <- numeric(sum(dat$noParUS))
	par$logN <- matrix(0, nrow=length(dat$year), ncol=length(dat$age))
	par$logF <- matrix(0, nrow=length(dat$year), ncol=max(dat$keyF)+1)
	par$missing <- numeric(sum(is.na(dat$obs)))

	obj <- MakeADFun(dat, par, random=c("logN", "logF", "missing"), DLL="babysam", map=list(logsdF=as.factor(rep(0,length(par$logsdF)))))

	fit <- nlminb(obj$par, obj$fn, obj$gr, control=list(eval.max=1000, iter.max=1000))


