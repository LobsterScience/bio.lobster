####### Update 27-33 2017:
p = bio.lobster::load.environment()
la()

#lobster.db('logs.redo',p=p)
lobster.db(DS='process.logs.redo', p=p)
logsInSeason<-lobster.db('process.logs')

p$lfas = c("27", "28", "29", "30", "31A", "31B", "32", "33","34") # specify lfas for data summary
p$subareas = c("27N","27S", "28", "29", "30", "31A", "31B", "32", "33E", "33W","34") # specify lfas for data summary


	TempModelling = TempModel( annual.by.area=F)
	CPUE.data<-CPUEModelData(p,redo=T,TempModelling)
	CPUE.data<-CPUEModelData(p,redo=F)


    cpueRyan.dat = CPUEplot(CPUE.data,lfa= c("27","32","34"),yrs=2001:2016,graphic='R')
 	manlogs = cpueSubAreaMan.dat$annual.dat
