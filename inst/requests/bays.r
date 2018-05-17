

p = bio.lobster::load.environment()


bays = read.csv(file.path(project.datadirectory('bio.lobster'),"data",'bays.csv'))
portlocs =  read.csv(file.path(project.datadirectory('bio.lobster'),"data",'portslocs.csv'))
vesselsbyport =  read.csv(file.path(project.datadirectory('bio.lobster'),"data",'vesselsbyport.csv'))

LobsterMap(ylim=c(42.5,45),xlim=c(-65.8,-61.5))

points(Y~X,bays,pch=16,col='red')

baysG = assignArea(bays,coords=c("X","Y"))


logsInSeason=lobster.db("process.logs")

	daysFished<-aggregate(DATE_FISHED ~ SYEAR + LFA + GRID_NUM + LICENCE_ID, data=logsInSeason,FUN= function(x) length(unique(x)))	

	daysFished$LICENCE<-1
	licencegrids = lobGridPlot(subset(daysFished, LFA%in%p$lfas&SYEAR==2017, c("LFA", "GRID_NUM", "LICENCE")), FUN=sum, lvls= licenceLevels)


licencesperbay = merge(baysG, licencegrids$pdata,all.x=T)[,c(4:8,10)]

names(licencesperbay)[6] = "LICENCES"


write.csv(licencesperbay,"licencesperbay.csv",row.names=F)

