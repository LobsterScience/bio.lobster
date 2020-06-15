
########## ILTS


	#abundance

	surveyLobsters<-LobsterSurveyProcess(lfa="34", yrs=1996:2018, mths=c("Aug","Jul","Jun"), bin.size=2.5, Net='NEST',size.range=c(70,82.5))

	R1.ab = SpatialGamLobsterSurvey(surveyLobsters,Years = 1996:2018,lab="R1")


	surveyLobsters<-LobsterSurveyProcess(lfa="34", yrs=1996:2018, mths=c("Aug","Jul","Jun"), bin.size=2.5, Net='NEST',size.range=c(82.5,95))

	R0.ab = SpatialGamLobsterSurvey(surveyLobsters,Years = 1996:2018,lab="R0")


	surveyLobsters<-LobsterSurveyProcess(lfa="34", yrs=1996:2018, mths=c("Aug","Jul","Jun"), bin.size=2.5, Net='NEST',size.range=c(0,200))

	total.ab = SpatialGamLobsterSurvey(surveyLobsters,Years = 1996:2018,lab="tot")


	surveyLobsters<-LobsterSurveyProcess(lfa="34", yrs=1996:2018, mths=c("Aug","Jul","Jun"), bin.size=2.5, Net='NEST',size.range=c(82.5,200))

	com.ab = SpatialGamLobsterSurvey(surveyLobsters,Years = 1996:2018,lab="com")


	surveyLobsters<-LobsterSurveyProcess(lfa="34", yrs=1996:2018, mths=c("Aug","Jul","Jun"), bin.size=2.5, Net='NEST',size.range=c(0,82.5))

	sub.ab = SpatialGamLobsterSurvey(surveyLobsters,Years = 1996:2018,lab="sub")


		#males

		surveyLobsters<-LobsterSurveyProcess(lfa="34", yrs=1996:2018, mths=c("Aug","Jul","Jun"), bin.size=2.5, Net='NEST',size.range=c(0,200),sex=1)

		totalM.ab = SpatialGamLobsterSurvey(surveyLobsters,Years = 1996:2018,lab="totM")


		surveyLobsters<-LobsterSurveyProcess(lfa="34", yrs=1996:2018, mths=c("Aug","Jul","Jun"), bin.size=2.5, Net='NEST',size.range=c(82.5,200),sex=1)

		comM.ab = SpatialGamLobsterSurvey(surveyLobsters,Years = 1996:2018,lab="comM")


		surveyLobsters<-LobsterSurveyProcess(lfa="34", yrs=1996:2018, mths=c("Aug","Jul","Jun"), bin.size=2.5, Net='NEST',size.range=c(0,82.5),sex=1)

		subM.ab = SpatialGamLobsterSurvey(surveyLobsters,Years = 1996:2018,lab="subM")


		#females

		surveyLobsters<-LobsterSurveyProcess(lfa="34", yrs=1996:2018, mths=c("Aug","Jul","Jun"), bin.size=2.5, Net='NEST',size.range=c(0,200),sex=2:3)

		totalF.ab = SpatialGamLobsterSurvey(surveyLobsters,Years = 1996:2018,lab="totF")


		surveyLobsters<-LobsterSurveyProcess(lfa="34", yrs=1996:2018, mths=c("Aug","Jul","Jun"), bin.size=2.5, Net='NEST',size.range=c(82.5,200),sex=2:3)

		comF.ab = SpatialGamLobsterSurvey(surveyLobsters,Years = 1996:2018,lab="comF")


		surveyLobstersM<-LobsterSurveyProcess(lfa="34", yrs=1996:2018, mths=c("Aug","Jul","Jun"), bin.size=2.5, Net='NEST',size.range=c(0,82.5),sex=1)
		surveyLobsters<-LobsterSurveyProcess(lfa="34", yrs=1996:2018, mths=c("Aug","Jul","Jun"), bin.size=2.5, Net='NEST',size.range=c(0,82.5),sex=2:3)

		x1=with(surveyLobsters,tapply(LobDen,YEAR,mean))/(with(surveyLobstersM,tapply(LobDen,YEAR,mean))	+ with(surveyLobsters,tapply(LobDen,YEAR,mean)))
		x2=subF.ab/(subM.ab+subF.ab)
		surveyLobsters$SR = surveyLobsters$LobDen /(surveyLobstersM$LobDen+surveyLobsters$LobDen)
		x3=with(surveyLobsters,tapply(SR,YEAR,mean))


		subF.ab = SpatialGamLobsterSurvey(surveyLobsters,Years = 1996:2018,lab="subF")


		#males

		surveyLobsters<-LobsterSurveyProcess(lfa="34", yrs=1996:2018, mths=c("Aug","Jul","Jun"), bin.size=2.5, Net='NEST',size.range=c(0,200),sex=3)

		totalB.ab = SpatialGamLobsterSurvey(surveyLobsters,Years = 1996:2018,lab="totB")

	#biomasss

	surveyLobsters<-LobsterSurveyProcess(lfa="34", yrs=1996:2018, mths=c("Aug","Jul","Jun"), bin.size=2.5, Net='NEST',size.range=c(70,82.5),biomass=T)

	R1.bm = SpatialGamLobsterSurvey(surveyLobsters,Years = 1996:2018,lab="R1bm",ylab="Lobster (kt)")


	surveyLobsters<-LobsterSurveyProcess(lfa="34", yrs=1996:2018, mths=c("Aug","Jul","Jun"), bin.size=2.5, Net='NEST',size.range=c(82.5,95),biomass=T)

	R0.bm = SpatialGamLobsterSurvey(surveyLobsters,Years = 1996:2018,lab="R0bm",ylab="Lobster (kt)")


	surveyLobsters<-LobsterSurveyProcess(lfa="34", yrs=1996:2018, mths=c("Aug","Jul","Jun"), bin.size=2.5, Net='NEST',size.range=c(0,200),biomass=T)

	total.bm = SpatialGamLobsterSurvey(surveyLobsters,Years = 1996:2018,lab="totbm",ylab="Lobster (kt)")


	surveyLobsters<-LobsterSurveyProcess(lfa="34", yrs=1996:2018, mths=c("Aug","Jul","Jun"), bin.size=2.5, Net='NEST',size.range=c(82.5,200),biomass=T)

	com.bm = SpatialGamLobsterSurvey(surveyLobsters,Years = 1996:2018,lab="combm",ylab="Lobster (kt)")

	
	surveyLobsters<-LobsterSurveyProcess(lfa="34", yrs=1996:2018, mths=c("Aug","Jul","Jun"), bin.size=2.5, Net='NEST',size.range=c(0,82.5),biomass=T)

	sub.bm = SpatialGamLobsterSurvey(surveyLobsters,Years = 1996:2018,lab="sub",ylab="Lobster (kt)")





	surveyLobsters<-LobsterSurveyProcess(lfa="34", yrs=2005:2018, mths=c("Aug","Jul","Jun"), sex=c(2,3),bin.size=2.5, Net='NEST',size.range=c(120,200))

	BigF.ab = SpatialGamLobsterSurvey(surveyLobsters,Years = 2005:2018,lab="BigF")

	BigF.ab = c(rep(NA,9),BigF.ab)



	LobsterSurveyIndicators = data.frame(cbind(R1.ab,R0.ab,com.ab,sub.ab,total.ab,R1.bm,R0.bm,com.bm,sub.bm,total.bm,comM.ab,subM.ab,totalM.ab,comF.ab,subF.ab,totalF.ab,totalB.ab,BigF.ab))


	save(LobsterSurveyIndicators,file=file.path(project.datadirectory("bio.lobster"),"outputs","LobsterSurveyIndicators.rdata"))
	load(file=file.path(project.datadirectory("bio.lobster"),"outputs","LobsterSurveyIndicators.rdata"))



############# Scallop Survey	


	scalSurv<-ScallopSurveyProcess(size.range=c(0,200),bin.size=5)

 	#stupid little map
 	LobsterMap('34-38',title="Scallop Survey Tows by Month")
 	colors = brewer.pal(5,"Dark2")[c(rep(NA,4),1:5,NA,NA)]
	points(lat~lon,scalSurv,pch=16,col=colors[month(TOW_DATE)])
	legend('bottomright',legend=5:9,col=colors[5:9],pch=16,)


	# R1

	scalSurv<-ScallopSurveyProcess(size.range=c(70,82.5),bin.size=2.5)

	R1.ab.s34=with(subset(scalSurv,month(TOW_DATE)<8&LFA==34&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))
	R1.ab.f34=with(subset(scalSurv,month(TOW_DATE)>7&LFA==34&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))
	R1.ab.35=with(subset(scalSurv,LFA==35&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))
	R1.ab.36=with(subset(scalSurv,LFA%in%c(36,37)&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))
	R1.ab.38=with(subset(scalSurv,LFA==38&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))


	# R0

	scalSurv<-ScallopSurveyProcess(size.range=c(82.5,90),bin.size=2.5)

	R0.ab.s34=with(subset(scalSurv,month(TOW_DATE)<8&LFA==34&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))
	R0.ab.f34=with(subset(scalSurv,month(TOW_DATE)>7&LFA==34&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))
	R0.ab.35=with(subset(scalSurv,LFA==35&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))
	R0.ab.36=with(subset(scalSurv,LFA%in%c(36,37)&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))
	R0.ab.38=with(subset(scalSurv,LFA==38&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))


	# total

	scalSurv<-ScallopSurveyProcess(size.range=c(0,200),bin.size=5)

	tot.ab.s34=with(subset(scalSurv,month(TOW_DATE)<8&LFA==34&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))
	tot.ab.f34=with(subset(scalSurv,month(TOW_DATE)>7&LFA==34&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))
	tot.ab.35=with(subset(scalSurv,LFA==35&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))
	tot.ab.36=with(subset(scalSurv,LFA%in%c(36,37)&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))
	tot.ab.38=with(subset(scalSurv,LFA==38&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))


	# com

	scalSurv<-ScallopSurveyProcess(size.range=c(82.5,200),bin.size=2.5)

	com.ab.s34=with(subset(scalSurv,month(TOW_DATE)<8&LFA==34&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))
	com.ab.f34=with(subset(scalSurv,month(TOW_DATE)>7&LFA==34&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))
	com.ab.35=with(subset(scalSurv,LFA==35&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))
	com.ab.36=with(subset(scalSurv,LFA%in%c(36,37)&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))
	com.ab.38=with(subset(scalSurv,LFA==38&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))


	# sub

	scalSurv<-ScallopSurveyProcess(size.range=c(0,82.5),bin.size=2.5)

	sub.ab.s34=with(subset(scalSurv,month(TOW_DATE)<8&LFA==34&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))
	sub.ab.f34=with(subset(scalSurv,month(TOW_DATE)>7&LFA==34&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))
	sub.ab.35=with(subset(scalSurv,LFA==35&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))
	sub.ab.36=with(subset(scalSurv,LFA%in%c(36,37)&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))
	sub.ab.38=with(subset(scalSurv,LFA==38&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))



	# total males

	scalSurv<-ScallopSurveyProcess(size.range=c(0,200),bin.size=5,sex=1)

	totM.ab.s34=with(subset(scalSurv,month(TOW_DATE)<8&LFA==34&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))
	totM.ab.f34=with(subset(scalSurv,month(TOW_DATE)>7&LFA==34&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))
	totM.ab.35=with(subset(scalSurv,LFA==35&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))
	totM.ab.36=with(subset(scalSurv,LFA%in%c(36,37)&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))
	totM.ab.38=with(subset(scalSurv,LFA==38&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))


	# com males

	scalSurv<-ScallopSurveyProcess(size.range=c(82.5,200),bin.size=2.5,sex=1)

	comM.ab.s34=with(subset(scalSurv,month(TOW_DATE)<8&LFA==34&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))
	comM.ab.f34=with(subset(scalSurv,month(TOW_DATE)>7&LFA==34&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))
	comM.ab.35=with(subset(scalSurv,LFA==35&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))
	comM.ab.36=with(subset(scalSurv,LFA%in%c(36,37)&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))
	comM.ab.38=with(subset(scalSurv,LFA==38&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))


	# sub males

	scalSurv<-ScallopSurveyProcess(size.range=c(0,82.5),bin.size=2.5,sex=1)

	subM.ab.s34=with(subset(scalSurv,month(TOW_DATE)<8&LFA==34&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))
	subM.ab.f34=with(subset(scalSurv,month(TOW_DATE)>7&LFA==34&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))
	subM.ab.35=with(subset(scalSurv,LFA==35&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))
	subM.ab.36=with(subset(scalSurv,LFA%in%c(36,37)&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))
	subM.ab.38=with(subset(scalSurv,LFA==38&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))



	# total females

	scalSurv<-ScallopSurveyProcess(size.range=c(0,200),bin.size=5,sex=2:3)

	totF.ab.s34=with(subset(scalSurv,month(TOW_DATE)<8&LFA==34&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))
	totF.ab.f34=with(subset(scalSurv,month(TOW_DATE)>7&LFA==34&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))
	totF.ab.35=with(subset(scalSurv,LFA==35&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))
	totF.ab.36=with(subset(scalSurv,LFA%in%c(36,37)&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))
	totF.ab.38=with(subset(scalSurv,LFA==38&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))


	# com females

	scalSurv<-ScallopSurveyProcess(size.range=c(82.5,200),bin.size=2.5,sex=2:3)

	comF.ab.s34=with(subset(scalSurv,month(TOW_DATE)<8&LFA==34&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))
	comF.ab.f34=with(subset(scalSurv,month(TOW_DATE)>7&LFA==34&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))
	comF.ab.35=with(subset(scalSurv,LFA==35&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))
	comF.ab.36=with(subset(scalSurv,LFA%in%c(36,37)&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))
	comF.ab.38=with(subset(scalSurv,LFA==38&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))

	# big females

	scalSurv<-ScallopSurveyProcess(size.range=c(120,200),bin.size=2.5,sex=2:3)

	bigF.ab.s34=with(subset(scalSurv,month(TOW_DATE)<8&LFA==34&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))
	bigF.ab.f34=with(subset(scalSurv,month(TOW_DATE)>7&LFA==34&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))
	bigF.ab.35=with(subset(scalSurv,LFA==35&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))
	bigF.ab.36=with(subset(scalSurv,LFA%in%c(36,37)&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))
	bigF.ab.38=with(subset(scalSurv,LFA==38&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))

	# sub females

	scalSurv<-ScallopSurveyProcess(size.range=c(0,82.5),bin.size=2.5,sex=2:3)

	subF.ab.s34=with(subset(scalSurv,month(TOW_DATE)<8&LFA==34&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))
	subF.ab.f34=with(subset(scalSurv,month(TOW_DATE)>7&LFA==34&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))
	subF.ab.35=with(subset(scalSurv,LFA==35&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))
	subF.ab.36=with(subset(scalSurv,LFA%in%c(36,37)&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))
	subF.ab.38=with(subset(scalSurv,LFA==38&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))


	# total unidentified

	scalSurv<-ScallopSurveyProcess(size.range=c(0,200),bin.size=5,sex=0)

	totU.ab.s34=with(subset(scalSurv,month(TOW_DATE)<8&LFA==34&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))
	totU.ab.f34=with(subset(scalSurv,month(TOW_DATE)>7&LFA==34&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))
	totU.ab.35=with(subset(scalSurv,LFA==35&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))
	totU.ab.36=with(subset(scalSurv,LFA%in%c(36,37)&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))
	totU.ab.38=with(subset(scalSurv,LFA==38&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))


	# com unidentified

	scalSurv<-ScallopSurveyProcess(size.range=c(82.5,200),bin.size=2.5,sex=0)

	comU.ab.s34=with(subset(scalSurv,month(TOW_DATE)<8&LFA==34&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))
	comU.ab.f34=with(subset(scalSurv,month(TOW_DATE)>7&LFA==34&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))
	comU.ab.35=with(subset(scalSurv,LFA==35&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))
	comU.ab.36=with(subset(scalSurv,LFA%in%c(36,37)&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))
	comU.ab.38=with(subset(scalSurv,LFA==38&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))


	# sub unidentified

	scalSurv<-ScallopSurveyProcess(size.range=c(0,82.5),bin.size=2.5,sex=0)

	subU.ab.s34=with(subset(scalSurv,month(TOW_DATE)<8&LFA==34&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))
	subU.ab.f34=with(subset(scalSurv,month(TOW_DATE)>7&LFA==34&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))
	subU.ab.35=with(subset(scalSurv,LFA==35&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))
	subU.ab.36=with(subset(scalSurv,LFA%in%c(36,37)&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))
	subU.ab.38=with(subset(scalSurv,LFA==38&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))


	totM.ab.s34=totM.ab.s34+totU.ab.s34*0.5
	totM.ab.f34=totM.ab.f34+totU.ab.f34*0.5
	totM.ab.35 =totM.ab.35 +totU.ab.35 *0.5
	totM.ab.36 =totM.ab.36 +totU.ab.36 *0.5
	totM.ab.38 =totM.ab.38 +totU.ab.38 *0.5

	comM.ab.s34=comM.ab.s34+comU.ab.s34*0.5
	comM.ab.f34=comM.ab.f34+comU.ab.f34*0.5
	comM.ab.35 =comM.ab.35 +comU.ab.35 *0.5
	comM.ab.36 =comM.ab.36 +comU.ab.36 *0.5
	comM.ab.38 =comM.ab.38 +comU.ab.38 *0.5

	subM.ab.s34=subM.ab.s34+subU.ab.s34*0.5
	subM.ab.f34=subM.ab.f34+subU.ab.f34*0.5
	subM.ab.35 =subM.ab.35 +subU.ab.35 *0.5
	subM.ab.36 =subM.ab.36 +subU.ab.36 *0.5
	subM.ab.38 =subM.ab.38 +subU.ab.38 *0.5

	totF.ab.s34=totF.ab.s34+totU.ab.s34*0.5
	totF.ab.f34=totF.ab.f34+totU.ab.f34*0.5
	totF.ab.35 =totF.ab.35 +totU.ab.35 *0.5
	totF.ab.36 =totF.ab.36 +totU.ab.36 *0.5
	totF.ab.38 =totF.ab.38 +totU.ab.38 *0.5

	comF.ab.s34=comF.ab.s34+comU.ab.s34*0.5
	comF.ab.f34=comF.ab.f34+comU.ab.f34*0.5
	comF.ab.35 =comF.ab.35 +comU.ab.35 *0.5
	comF.ab.36 =comF.ab.36 +comU.ab.36 *0.5
	comF.ab.38 =comF.ab.38 +comU.ab.38 *0.5

	subF.ab.s34=subF.ab.s34+subU.ab.s34*0.5
	subF.ab.f34=subF.ab.f34+subU.ab.f34*0.5
	subF.ab.35 =subF.ab.35 +subU.ab.35 *0.5
	subF.ab.36 =subF.ab.36 +subU.ab.36 *0.5
	subF.ab.38 =subF.ab.38 +subU.ab.38 *0.5


	# total berried

	scalSurv<-ScallopSurveyProcess(size.range=c(0,200),bin.size=5,sex=3)

	totB.ab.s34=with(subset(scalSurv,month(TOW_DATE)<8&LFA==34&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))
	totB.ab.f34=with(subset(scalSurv,month(TOW_DATE)>7&LFA==34&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))
	totB.ab.35=with(subset(scalSurv,LFA==35&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))
	totB.ab.36=with(subset(scalSurv,LFA%in%c(36,37)&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))
	totB.ab.38=with(subset(scalSurv,LFA==38&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))




	# R1 biomass

	scalSurv<-ScallopSurveyProcess(size.range=c(70,82.5),bin.size=2.5,biomass=T)

	R1.bm.s34=with(subset(scalSurv,month(TOW_DATE)<8&LFA==34&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))
	R1.bm.f34=with(subset(scalSurv,month(TOW_DATE)>7&LFA==34&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))
	R1.bm.35=with(subset(scalSurv,LFA==35&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))
	R1.bm.36=with(subset(scalSurv,LFA%in%c(36,37)&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))
	R1.bm.38=with(subset(scalSurv,LFA==38&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))


	# R0 biomass

	scalSurv<-ScallopSurveyProcess(size.range=c(82.5,90),bin.size=2.5,biomass=T)

	R0.bm.s34=with(subset(scalSurv,month(TOW_DATE)<8&LFA==34&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))
	R0.bm.f34=with(subset(scalSurv,month(TOW_DATE)>7&LFA==34&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))
	R0.bm.35=with(subset(scalSurv,LFA==35&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))
	R0.bm.36=with(subset(scalSurv,LFA%in%c(36,37)&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))
	R0.bm.38=with(subset(scalSurv,LFA==38&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))


	# total biomass

	scalSurv<-ScallopSurveyProcess(size.range=c(0,200),bin.size=5,biomass=T)

	tot.bm.s34=with(subset(scalSurv,month(TOW_DATE)<8&LFA==34&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))
	tot.bm.f34=with(subset(scalSurv,month(TOW_DATE)>7&LFA==34&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))
	tot.bm.35=with(subset(scalSurv,LFA==35&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))
	tot.bm.36=with(subset(scalSurv,LFA%in%c(36,37)&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))
	tot.bm.38=with(subset(scalSurv,LFA==38&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))


	# com biomass

	scalSurv<-ScallopSurveyProcess(size.range=c(82.5,200),bin.size=2.5,biomass=T)

	com.bm.s34=with(subset(scalSurv,month(TOW_DATE)<8&LFA==34&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))
	com.bm.f34=with(subset(scalSurv,month(TOW_DATE)>7&LFA==34&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))
	com.bm.35=with(subset(scalSurv,LFA==35&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))
	com.bm.36=with(subset(scalSurv,LFA%in%c(36,37)&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))
	com.bm.38=with(subset(scalSurv,LFA==38&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))


	# sub biomass

	scalSurv<-ScallopSurveyProcess(size.range=c(0,82.5),bin.size=2.5,biomass=T)

	sub.bm.s34=with(subset(scalSurv,month(TOW_DATE)<8&LFA==34&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))
	sub.bm.f34=with(subset(scalSurv,month(TOW_DATE)>7&LFA==34&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))
	sub.bm.35=with(subset(scalSurv,LFA==35&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))
	sub.bm.36=with(subset(scalSurv,LFA%in%c(36,37)&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))
	sub.bm.38=with(subset(scalSurv,LFA==38&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))



	# total berried

	scalSurv<-ScallopSurveyProcess(size.range=c(0,200),bin.size=5,sex=3)

	totB.ab.s34=with(subset(scalSurv,month(TOW_DATE)<8&LFA==34&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))
	totB.ab.f34=with(subset(scalSurv,month(TOW_DATE)>7&LFA==34&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))
	totB.ab.35=with(subset(scalSurv,LFA==35&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))
	totB.ab.36=with(subset(scalSurv,LFA%in%c(36,37)&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))
	totB.ab.38=with(subset(scalSurv,LFA==38&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))



	ScallopSurveyIndicatorsS34 = data.frame(cbind(R1.ab.s34,R0.ab.s34,com.ab.s34,sub.ab.s34,tot.ab.s34,R1.bm.s34,R0.bm.s34,com.bm.s34,sub.bm.s34,tot.bm.s34,comM.ab.s34,subM.ab.s34,totM.ab.s34,comF.ab.s34,subF.ab.s34,totF.ab.s34,totB.ab.s34,bigF.ab.s34))
	ScallopSurveyIndicatorsF34 = data.frame(cbind(R1.ab.f34,R0.ab.f34,com.ab.f34,sub.ab.f34,tot.ab.f34,R1.bm.f34,R0.bm.f34,com.bm.f34,sub.bm.f34,tot.bm.f34,comM.ab.f34,subM.ab.f34,totM.ab.f34,comF.ab.f34,subF.ab.f34,totF.ab.f34,totB.ab.f34,bigF.ab.f34))
	ScallopSurveyIndicators35 = data.frame(cbind(R1.ab.35,R0.ab.35,com.ab.35,sub.ab.35,tot.ab.35,R1.bm.35,R0.bm.35,com.bm.35,sub.bm.35,tot.bm.35,comM.ab.35,subM.ab.35,totM.ab.35,comF.ab.35,subF.ab.35,totF.ab.35,totB.ab.35,bigF.ab.35))
	ScallopSurveyIndicators36 = data.frame(cbind(R1.ab.36,R0.ab.36,com.ab.36,sub.ab.36,tot.ab.36,R1.bm.36,R0.bm.36,com.bm.36,sub.bm.36,tot.bm.36,comM.ab.36,subM.ab.36,totM.ab.36,comF.ab.36,subF.ab.36,totF.ab.36,totB.ab.36,bigF.ab.36))
	ScallopSurveyIndicators38 = data.frame(cbind(R1.ab.38,R0.ab.38,com.ab.38,sub.ab.38,tot.ab.38,R1.bm.38,R0.bm.38,com.bm.38,sub.bm.38,tot.bm.38,comM.ab.38,subM.ab.38,totM.ab.38,comF.ab.38,subF.ab.38,totF.ab.38,totB.ab.38,bigF.ab.38))


	save(list=c("ScallopSurveyIndicatorsS34","ScallopSurveyIndicatorsF34","ScallopSurveyIndicators35","ScallopSurveyIndicators36","ScallopSurveyIndicators38"),file=file.path(project.datadirectory("bio.lobster"),"outputs","ScallopSurveyIndicators.rdata"))
	
	load(file=file.path(project.datadirectory("bio.lobster"),"outputs","LobsterSurveyIndicators.rdata"))

