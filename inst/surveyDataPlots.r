
	p = bio.lobster::load.environment()
	la()


	surveyLobsters34 = LobsterSurveyProcess(lfa="34",yrs=1996:2017,mths=c("Aug","Jul","Jun"),bin.size=5,Net='NEST')
	surveyLobsters34m = LobsterSurveyProcess(lfa="34",yrs=1996:2017,mths=c("Aug","Jul","Jun"),bin.size=5,Net='NEST',sex=1)
	surveyLobsters34f = LobsterSurveyProcess(lfa="34",yrs=1996:2017,mths=c("Aug","Jul","Jun"),bin.size=5,Net='NEST',sex=2)
	surveyLobsters34b = LobsterSurveyProcess(lfa="34",yrs=1996:2017,mths=c("Aug","Jul","Jun"),bin.size=5,Net='NEST',sex=3)


	LobsterMap('34')



	data = subset(surveyLobsters34,YEAR==2017,c("SET_LONG","SET_LAT","LobDen","MEAN_LENGTH"))
	datam = subset(surveyLobsters34m,YEAR==2017,c("SET_LONG","SET_LAT","LobDen","MEAN_LENGTH"))
	dataf = subset(surveyLobsters34f,YEAR==2017,c("SET_LONG","SET_LAT","LobDen","MEAN_LENGTH"))
	datab = subset(surveyLobsters34b,YEAR==2017,c("SET_LONG","SET_LAT","LobDen","MEAN_LENGTH"))
	names(data) = c("x","y","den","ml")
	data$denm = datam$LobDen
	data$denf = dataf$LobDen
	data$denb = datab$LobDen
	
	zeros = subset(data,den==0)
	data = subset(data,den>0)

	data$line.cols=1

	ramp <- colorRamp(c("red", "white"))

	data$fill.cols=rgb( ramp((data$ml-50)/100),alpha=255*0.7, max = 255)

	# map

	LobsterMap(ylim=c(42.8,44.6), xlim=c(-67.15,-65.2),mapRes="UR",title="LFA 34 Lobster Survey",isobath=seq(50,500,50),bathcol=rgb(0,0,1,0.2),bathy.source='bathy')
	points(y~x,zeros,pch=4)
	surveyBubbles(data,scaler=0.5)

	# map
	LobsterMap(ylim=c(42.8,44.6), xlim=c(-67.25,-65.2),mapRes="UR",title="LFA 34 Lobster Survey",isobath=seq(50,500,50),bathcol=rgb(0,0,1,0.2),bathy.source='bathy')
	points(y~x,zeros,pch=4)
	surveyBubbles(data,scaler=0.1,pie=T)







