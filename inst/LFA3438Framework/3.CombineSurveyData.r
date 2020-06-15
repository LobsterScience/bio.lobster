  
	p = bio.lobster::load.environment()
	la()

    figdir = file.path(project.datadirectory("bio.lobster"),"figures","LFA3438Framework2019")


#####################################################
######## Plot Data

# R1 = sub-legal sized lobsters expected to recruit to fishery after on more molt
AllSurveyDataR1 = SurveyTowData(Size.range=c(70,82.5),Sex = c(1,2,3), Years=1970:2018,redo=T,lab="R1")
#AllSurveyDataR1 =assignArea(AllSurveyDataR1,coords=c("X","Y"))

	surveyplotdata = AllSurveyDataR1

	surveyplotdata$fill.cols=NA
	surveyplotdata$fill.cols[surveyplotdata$survey=="LobsterNest"] = rgb(1,0,0,0.7)
	surveyplotdata$fill.cols[surveyplotdata$survey=="LobsterBalloon"] = rgb(1,0,0,0.7)
	surveyplotdata$fill.cols[surveyplotdata$survey=="Scallop"] = rgb(0,0,1,0.7)
	surveyplotdata$fill.cols[surveyplotdata$survey=="DFOsummer"] = rgb(0,1,0,0.7)
	surveyplotdata$fill.cols[surveyplotdata$survey=="NEFSCfall"] = rgb(0,1,1,0.7)
	surveyplotdata$fill.cols[surveyplotdata$survey=="NEFSCspring"] = rgb(0,1,1,0.7)


	for(i in 1970:2018){

		data = with(subset(surveyplotdata,year==i),data.frame(den=LobDen,x=X,y=Y,line.cols='black',fill.cols=fill.cols))
		zeros = subset(data,den==0)
		data = subset(data,den>0)
		if(nrow(data)==0) next

		# map
		pdf(file.path( figdir,paste0("SurveyBubblesR1",i,".pdf")),8,8)
		LobsterMap("34-38",title=paste("Surveys R1",i),isobath=seq(50,500,50),bathcol=rgb(0,0,1,0.2),bathy.source='bathy')
		points(y~x,zeros,pch=4,col=zeros$fill.cols)
		surveyBubbles(data,scaler=0.5,pie=F)
		dev.off()
	}


	pdf(file.path( figdir,paste0("SurveyBubblesR1.pdf")),8,8)
	for(i in 1970:2018){

		data = with(subset(surveyplotdata,year==i),data.frame(den=LobDen,x=X,y=Y,line.cols='black',fill.cols=fill.cols))
		zeros = subset(data,den==0)
		data = subset(data,den>0)
		if(nrow(data)==0) next

		# map
		LobsterMap("34-38",title=paste("Surveys R1",i),isobath=seq(50,500,50),bathcol=rgb(0,0,1,0.2),bathy.source='bathy')
		points(y~x,zeros,pch=4,col=zeros$fill.cols)
		surveyBubbles(data,scaler=0.5,pie=F)
	}
		dev.off()


# R0 = legal sized lobsters that have recruited to fishery after their last molt
AllSurveyDataR0 = SurveyTowData(Size.range=c(82.5,95),Sex = c(1,2,3), Years=1970:2018,redo=T,lab="R0")

	surveyplotdata = AllSurveyDataR0

	surveyplotdata$fill.cols=NA
	surveyplotdata$fill.cols[surveyplotdata$survey=="LobsterNest"] = rgb(1,0,0,0.7)
	surveyplotdata$fill.cols[surveyplotdata$survey=="LobsterBalloon"] = rgb(1,0,0,0.7)
	surveyplotdata$fill.cols[surveyplotdata$survey=="Scallop"] = rgb(0,0,1,0.7)
	surveyplotdata$fill.cols[surveyplotdata$survey=="DFOsummer"] = rgb(0,1,0,0.7)
	surveyplotdata$fill.cols[surveyplotdata$survey=="NEFSCfall"] = rgb(0,1,1,0.7)
	surveyplotdata$fill.cols[surveyplotdata$survey=="NEFSCspring"] = rgb(0,1,1,0.7)

	pdf(file.path( figdir,paste0("SurveyBubblesR0.pdf")),8,8)

	for(i in 1970:2018){

		data = with(subset(surveyplotdata,year==i),data.frame(den=LobDen,x=X,y=Y,line.cols='black',fill.cols=fill.cols))
		zeros = subset(data,den==0)
		data = subset(data,den>0)
		if(nrow(data)==0) next

		# map
		LobsterMap("34-38",title=paste("Surveys R0",i),isobath=seq(50,500,50),bathcol=rgb(0,0,1,0.2),bathy.source='bathy')
		points(y~x,zeros,pch=4,col=zeros$fill.cols)
		surveyBubbles(data,scaler=0.5,pie=F)
	}
	dev.off()



	for(i in 1970:2018){

		data = with(subset(surveyplotdata,year==i),data.frame(den=LobDen,x=X,y=Y,line.cols='black',fill.cols=fill.cols))
		zeros = subset(data,den==0)
		data = subset(data,den>0)
		if(nrow(data)==0) next

		# map
		pdf(file.path( figdir,paste0("SurveyBubblesR0",i,".pdf")),8,8)
		LobsterMap("34-38",title=paste("Surveys R0",i),isobath=seq(50,500,50),bathcol=rgb(0,0,1,0.2),bathy.source='bathy')
		points(y~x,zeros,pch=4,col=zeros$fill.cols)
		surveyBubbles(data,scaler=0.5,pie=F)
		dev.off()
	}

















### Lobster Survey data: Looking at the proportion of tows where lobster were measured

> sapply(1996:2016,function(i){nrow(subset(surveyLobsters34,YEAR==i&!is.na(NUM_MEASURED)&NUM_CAUGHT>0))/nrow(subset(surveyLobsters34,YEAR==i&NUM_CAUGHT>0))})->x
> names(x)=1996:2016
> x
     1996      1997      1998      1999      2000      2001      2002      2003      2004      2005      2006      2007      2008      2009      2010      2011      2012      2013 
0.0000000 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000 0.3243243 0.6190476 0.6562500 0.5000000 0.6216216 0.6666667 0.5652174 1.0000000 0.9750000 
     2014      2015      2016 
1.0000000 1.0000000 1.0000000 

LobsterMap('34')
points(SET_LAT~SET_LONG,subset(surveyLobsters34,YEAR==2006&!is.na(NUM_MEASURED)&NUM_CAUGHT>0))
points(SET_LAT~SET_LONG,subset(surveyLobsters34,YEAR==2006&is.na(NUM_MEASURED)&NUM_CAUGHT>0),pch=16)


