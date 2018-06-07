	p = bio.lobster::load.environment()
	la()

pdf("LFA32map.pdf")
LobsterMap("32",labels='grid',title="LFA 32")
dev.off()

	yrs = 2011:2017

	potgrids.lst = list()
	daysFished<-aggregate(DATE_FISHED ~ SYEAR + LFA + GRID_NUM + LICENCE_ID, data=logsInSeason,FUN= function(x) length(unique(x)))	
	daysFished$LICENCE<-1

	for(i in 1:length(yrs)){
	potgrids = lobGridPlot(subset(logsInSeason,LFA==32&SYEAR == yrs[i],c("LFA","GRID_NUM","NUM_OF_TRAPS")),FUN=sum) 
	licencegrids = lobGridPlot(subset(daysFished, LFA==32&SYEAR==yrs[i], c("LFA", "GRID_NUM", "LICENCE")), FUN=sum)

	potgrids.lst[[i]] = potgrids$pdata	
	potgrids.lst[[i]]$year = yrs[i]
	potgrids.lst[[i]]$licences = licencegrids$pdata$Z
	}

	gridEffortData32 = do.call("rbind",potgrids.lst)
	gridEffortData32 = gridEffortData32[,c("PID","SID","year","Z","licences")]
	names(gridEffortData32) = c("LFA","grid","year","traphauls","licences")

	write.csv(gridEffortData32,"gridEffortData32.csv",row.names=F)



LFAs<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolys.csv"))
attr(LFAs,"projection")="LL"

calcArea(LFAs,1)
