#maps

#Fishing Area
		logs41   = lobster.db('logs41')
		logs41$YEAR = year(logs41$FV_FISHED_DATETIME)
  		logs41 = makePBS(logs41,polygon=F)
        logs41 = logs41[-which(is.na(logs41[,c('X','Y','EID')])),]
		yr = unique(logs41$YEAR)

		#prune to polygons
				LFA41 = read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","LFA41Offareas.csv"))
				LFA41 = joinPolys(as.PolySet(LFA41),operation='UNION')
				LFA41 = subset(LFA41,SID==1)
				attr(LFA41,'projection') <- 'LL'

				l = findPolys(logs41,LFA41)$EID
			logs41 = subset(logs41,EID %in% l)

			LFA41<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","LFA41Offareas.csv"))
				y = c(2002,2007,2013)
			for(i in y) {
					pdf(file=file.path(project.figuredirectory('bio.lobster'),paste('lfa41Logs',i,i+5,'.pdf',sep='.')))
					LobsterMap('41')
					addPoints(subset(logs41,YEAR %in% c(i:(i+5))),pch='.',col='green')
					addPolys(LFA41,border='red')		
					dev.off()
				}


#Summer Survey Map
LobsterMap(xlim=c(-69,-56.8),ylim=c(41.2,47.5),boundaries='LFAs',addSummerStrata=T,output='bio.lobster',fname = 'summerstratamap.pdf',save=T,labcex =0.8,labels=F)

LobsterMap('41',boundaries='LFAs',addSummerStrata=T,output='bio.lobster',fname='summerstratamap41.pdf',save=F,labcex =0.8,labels=F)