require(bio.lobster)
require(bio.utilities)
  
	p = bio.lobster::load.environment()
	la()
		
	assessment.year = 2018 ########### check the year ############### !!!!!!!!!!!


    p$syr = 1989
    p$yrs = p$syr:(p$current.assessment.year-1)

    figdir = file.path(project.datadirectory("bio.lobster"),"figures","LFA3438Framework2019")

    p$lfas = c("34", "35", "36", "38") # specify lfas for data summary
    p$subareas = c("34", "35", "36", "38") # specify lfas for data summary

    logsInSeason<-lobster.db('process.logs.redo')
    logsInSeason<-lobster.db('process.logs')
    logsInSeasonH<-lobster.db('historic.cpue')
	logsInSeasonV<-lobster.db('process.vlogs')
	
	lS = subset(logsInSeason,LFA %in% p$lfas)
 


    cpueLFA.dat = CPUEplot(logsInSeason,lfa= p$lfas,yrs=2002:2018,graphic='R',export=T)
    cpueLFA.dat = CPUEplot(logsInSeason,lfa= p$lfas,yrs=2006:2018,graphic='pdf',path=figdir)
    cpueSubArea.dat = CPUEplot(logsInSeason,subarea= p$subareas,yrs=2006:2018,graphic='R')
