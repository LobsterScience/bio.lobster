    
  
	p = bio.lobster::load.environment()

    p$lfas = c("27", "28", "29", "30", "31.1", "31.2", "32", "33", "34") # specify lfas for data summary
    p$syr = 2005
    p$yrs = p$syr:p$current.assessment.year

   

    ## Carapace Length Frequency Plots

    # Lobster Survey
	CarapaceLengthFrequencies(LFAs='34', DS='LobsterSurvey', fn='v1')
	CarapaceLengthFrequencies(LFAs='34', DS='LobsterSurvey', Yrs=2010:2015, rel=T, bins=seq(0,200,5), pdf=F)

	# Scallop Survey
	CarapaceLengthFrequencies(LFAs='34', DS='ScallopSurvey')
	CarapaceLengthFrequencies(LFAs='34', DS='ScallopSurvey', Yrs=2010:2015, rel=F, ymax=1, bins=seq(0,200,1), fn='v2')
	CarapaceLengthFrequencies(LFAs=c("35","36","38") , DS='ScallopSurvey', Yrs=2010:2015, rel=F, ymax=1.3, fn='')
	CarapaceLengthFrequencies(LFAs=c("35","36","38") , DS='ScallopSurvey', Yrs=2010:2015, rel=T, fn='Rel')
	
	
	# at Sea Sampling
	CarapaceLengthFrequencies(LFAs= p$lfas, DS='atSea', by="LFA", fn='byLFA')
	CarapaceLengthFrequencies(LFAs= '34', DS='atSea', by=c("Q","SEX"), fn='34')
	
	
	# Port sampling
	CarapaceLengthFrequencies(LFAs= c("27", "28", "29", "30", "31", "32", "33", "34"), DS='port', Yrs=2007:2015, by="LFA", fn='byLFA')
	CarapaceLengthFrequencies(LFAs='34', DS='port', Yrs=2010:2015, by="SEX", bins=seq(0,200,1), fn='34')
	
	# FSRS recruitment traps
	CarapaceLengthFrequencies(LFAs= p$lfas, DS='fsrs', by="LFA", bins=seq(0,140,10))
	CarapaceLengthFrequencies(LFAs= p$lfas, DS='fsrs', by="LFA", bins=c(seq(0,70,10),75,seq(80,200,10)))


    ## Scallop Survey Trend
    SCALSURVsublegals.dat<-ScallopSurveyProcess(Yrs=2005:2015,size.range=c(0,82.5),bin.size=5)
    lobDenScal38 <- with(subset(SCALSURVsublegals.dat,LFA==38),tapply(LobDen,YEAR,mean))
    stdts.plt(data.frame(Year=as.numeric(names(lobDenScal38)),LobsterDensity=lobDenScal38),ylim=c(0,2),ylab="sub-legal Lobster Density (#/1000 m2)",graphic='pdf',fn='LFA35scalSurvLobDen')

	LOGcpue.dat<-read.csv(file.path( project.datadirectory("lobster"), "data","products","CommercialCPUE.csv"))
    stdts.plt(subset(LOGcpue.dat,lfa==38,c('year','cpue')),ylim=c(0,2.5),ylab="CPUE (kg/TH)",graphic='pdf',fn='LFA35CPUE')



    ## CPUE
    logsInSeason<-lobster.db('process.logs')
    CPUEplot(logsInSeason,lfa=c('35','36','38'),yrs=2006:2015,graphic='pdf')

    logsInSeason<-lobster.db('process.logs')
    CPUE.lst = list()
    yrs = sort(unique(year(logsInSeason$DATE_FISHED)))
    weeks = 1:53

    for (y in 1:length(yrs)){

    	CPUE.lst[[y]] = list()

    	for(i in weeks){
			
			catch = with(subset(logsInSeason,year(DATE_FISHED)==yrs[y]&week(DATE_FISHED)==i),tapply(WEIGHT_KG,GRID_NUM,sum))
			effort = with(subset(logsInSeason,year(DATE_FISHED)==yrs[y]&week(DATE_FISHED)==i),tapply(NUM_OF_TRAPS,GRID_NUM,sum))
			CPUE.lst[[y]][[i]] = catch / effort

    	}
    	names(CPUE.lst[[y]]) = weeks
    }
    names(CPUE.lst) = yrs

    save(CPUE.lst,file="CPUElist.Rdata")


