#' @export
buildDatObj = function(AllSurveyTows,CatchComp,yrs=1982:2018,bins = seq(0,195,5),lfa='34'){

	
	x=list()
	
	for (i in 1:length(bins)){
		x[[i]]=aggregate(LobDen~year+survey+sex,subset(AllSurveyTows,CL==bins[i]+5&LFA==34),mean )
		x[[i]]$len = i
	}

	surveyData = do.call("rbind",x)

	CatchComp$len = as.numeric(as.factor(CatchComp$CL))
	CatchComp$fleet = 1
	surveyData$fleet = as.numeric(as.factor(surveyData$survey))+1
	surveyData$obs = surveyData$LobDen

	dat1 = rbind(CatchComp[c('year','fleet','sex','len','obs')],surveyData[c('year','fleet','sex','len','obs')])
	dat1 = subset(dat1,year%in%yrs)
	dat1 = with(dat1,dat1[order(year,fleet,sex,len),])


	dat=list()

	dat$obs = dat1$obs
	dat$aux = dat1[,-which(names(dat1)=='obs')]

	# index vector for referencing observations
	 # idx<-which(dat$aux[,2]==f)
	fleets = sort(unique(dat1$fleet))
	sexes = sort(unique(dat1$sex))
	idx1=idx2=array(NA,c(length(fleets),length(yrs),length(sexes)))

	for(i in 1:length(fleets)){
		for(j in 1:length(yrs)){
			for(k in 1:length(sexes)){
				idx1[i,j,k]= min(which(dat1$fleet==fleets[i]&dat1$year==yrs[j]&dat1$sex==sexes[k]))
				idx2[i,j,k]= max(which(dat1$fleet==fleets[i]&dat1$year==yrs[j]&dat1$sex==sexes[k]))
			}
		}
	}
	idx1[!is.finite(idx1)]=NA
	idx2[!is.finite(idx2)]=NA

	dat$idx1 = idx1
	dat$idx2 = idx2

	# fleetTypes: 0 = fishery, 1 = summer survey, 2 = fall survey
	mths=with(AllSurveyTows,tapply(month(date),survey,median))
	dat$fleetTypes=c(Fishery=0,ifelse(mths<9,1,2))
	

	# 
	dat$minYear = min(yrs)
	dat$minLen = 1
	dat$minSex = 1

	dat$year = yrs
	dat$len = unique(dat$aux$len)
	dat$sex = 1:3


	# natural mortality
	dat$M = round(matrix(getM(CL=bins+2.5),length(yrs),length(bins),byrow=T),2)

	# weight at length
	dat$WgtM = round(matrix(lobLW(CL=bins+2.5,sex=1),length(yrs),length(bins),byrow=T)/1000,4)
	dat$WgtF = round(matrix(lobLW(CL=bins+2.5,sex=2),length(yrs),length(bins),byrow=T)/1000,4)

	# maturity oogive
	dat$MO = round(matrix(pMat(p=list(Area=lfa),cl=bins+2.5),length(yrs),length(bins),byrow=T),2)

	# Growth Transition Matrix from Simulation
	load(file=file.path(project.datadirectory("bio.lobster"),"outputs","sim","sim34ResultsBase.rdata"))

	transMats = getTransMatrix(rlist,lfa=lfa,Plot=F)
	dat$transMatM = Reduce('%*%',transMats$males)
	dat$transMatF = Reduce('%*%',transMats$females)






	return(dat)

}