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


    logsInSeason<-lobster.db('process.logs.redo')
    logsInSeason<-lobster.db('process.logs')

#From LFA2733Framework2017.R
	 CPUE.data<-CPUEModelData(p,redo=F)
	 cDD = subset(CPUE.data,SYEAR>1989,select=c('SYEAR','WEIGHT_KG','NUM_OF_TRAPS','LFA','type'))
	 cDD = rename.df(cDD,c('SYEAR','WEIGHT_KG','NUM_OF_TRAPS'),c('time','catch','effort'))

	 out = list()
	 m=0
	 lf = unique(cDD$LFA)
	 for(i in 1:length(lf)){
	 	u = subset(cDD,LFA==lf[i])
	 	y = unique(u$time) 
	 	for(j in 1:length(y)){
	 		uu = subset(u,time==y[j])
	 		mn = unique(uu$type)
	 		for(k in 1:length(mn)){
	 			uuu = subset(uu,type==mn[k])
	 			m=m+1
	 			oo = jackknife(uuu,err='both')
	 			oo$lfa = lf[i]
	 			oo$yr = y[j]
	 			oo$type = mn[k]
	 			out[[m]] = oo
	 		}

	 	}

	 }

	 



	 cpueData2=    CPUEplot(CPUE.data,lfa= p$lfas,yrs=1981:2016,graphic='R')$annual.data
	 cD = subset(cpueData2, YEAR>1989,select=c('LFA','YEAR','CPUE',"CATCH","EFFORT"))
		


	 cD = rename.df(cD,'YEAR','YR')
	 d = lobster.db('seasonal.landings')
	g = lobster.db('annual.landings')

	g = subset(g,YR %in% 1990:2016,select=c('YR','LFA27',"LFA28",'LFA29','LFA30','LFA31A','LFA31B','LFA32'))
	d$YR = substr(d$SYEAR,6,9) 
	d = subset(d,YR %in% 1990:2016, select=c('YR','LFA33'))
	d$LFA = 33
	names(d)[2] = 'Landings'
	g = reshape(g,idvar="YR",times=substr(names(g)[-1],4,6),timevar="LFA",varying=list(names(g)[-1]),direction='long')
	names(g)[3] = 'Landings' 

	land = as.data.frame(rbind(g,d))


	fd = merge(land,cD,all=T)

	fd$Effort = fd$Landings / fd$CPUE

for(i in 1:length(p$lfas)){
	li = subset(fd,LFA==p$lfas[i])
	plot(li$)
}
