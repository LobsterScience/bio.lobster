#effort

require(bio.lobster)

	p = bio.lobster::load.environment()
	la()


    p$syr = 2005
    p$yrs = p$syr:p$current.assessment.year

    figdir = file.path(project.datadirectory("bio.lobster"),"figures","LFA2733Framework2018")

    p$lfas = c("27", "28", "29", "30", "31A", "31B", "32", "33") # specify lfas for data summary
  
    p$subareas = c("27N","27S", "28", "29", "30", "31A", "31B", "32", "33E", "33W") # specify lfas for data summary

  p$lfas = c("27", "28", "29", "30", "31A", "31B", "32", "33") # specify lfas for data summary
    p$subareas = c("27N","27S", "28", "29", "30", "31A", "31B", "32", "33E", "33W") # specify lfas for data summary


 #   logsInSeason<-lobster.db('process.logs.redo')
    logsInSeason<-lobster.db('process.logs')

#From LFA2733Framework2017.R
	 CPUE.data<-CPUEModelData(p,redo=F)

	 cpueData2=    CPUEplot(CPUE.data,lfa= p$lfas,yrs=2008:2016,graphic='R')$annual.data
	 cD = subset(cpueData2, YEAR>2007,select=c('LFA','YEAR','CPUE',"CATCH","EFFORT"))
		


	 cD = rename.df(cD,'YEAR','YR')
	 d = lobster.db('seasonal.landings')
	g = lobster.db('annual.landings')

	g = subset(g,YR %in% 2008:2016,select=c('YR','LFA27',"LFA28",'LFA29','LFA30','LFA31A','LFA31B','LFA32'))
	d$YR = substr(d$SYEAR,6,9) 
	d = subset(d,YR %in% 2008:2016, select=c('YR','LFA33'))
	d$LFA = 33
	names(d)[2] = 'Landings'
	g = reshape(g,idvar="YR",times=substr(names(g)[-1],4,6),timevar="LFA",varying=list(names(g)[-1]),direction='long')
	names(g)[3] = 'Landings' 

	land = as.data.frame(rbind(g,d))


	fd = merge(land,cD,all=T)
	graphics.off()
	fd$Effort = fd$Landings / fd$CPUE
l = unique(fd$LFA)
for(i in 1:length(l)){
	li = subset(fd,LFA==l[i])
	plot(li$YR,li$Effort,xlab='Year',ylab='Effort (x1000 TH)', type='b',main=paste('LFA',l[i]),col='blue',lwd=3)
savePlot(file.path(figdir,paste('FisheryEffortLFA',l[i],'.png',sep='')),type='png')
}
