
require(SpatialHub)
SurveyStations = read.csv(file.path(project.datadirectory('bio.lobster'),'data','SurveyStations.csv'))

names(SurveyStations)[6:9] = c("Y1","X1","Y2","X2") 

SurveyStations$EID = 1:nrow(SurveyStations)
SurveyStations$X1 = SurveyStations$X1 * -1
SurveyStations$X2 = SurveyStations$X2 * -1
SurveyStations$Y2[is.na(SurveyStations$Y2)] = SurveyStations$Y1[is.na(SurveyStations$Y2)]
SurveyStations$X2[is.na(SurveyStations$X2)] = SurveyStations$X1[is.na(SurveyStations$X2)]

	pdf("SurveyStations2.pdf")
		SurveyStations2<-assignStation(SurveyStations,lines=T,map='lfa34')

		dev.off()
		



