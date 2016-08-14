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


#Survey Maps
		LFA41 = read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","LFA41Offareas.csv"))
				LFA41 = joinPolys(as.PolySet(LFA41),operation='UNION')
				LFA41 = subset(LFA41,SID==1)
				attr(LFA41,'projection') <- 'LL'


LobsterMap(xlim=c(-69,-56.8),ylim=c(41.2,47.5),boundaries='LFAs',addSummerStrata=T,output='bio.lobster',fname = 'summerstratamap.pdf',save=T,labcex =0.8,labels=F)


pdf(file=file.path(project.figuredirectory('bio.lobster'),paste('summerstratamap41.pdf',sep='.')))
		LobsterMap('41',boundaries='LFAs',addSummerStrata=T,output='bio.lobster',fname='summerstratamap41.pdf',save=F,labcex =0.8,labels=F,subsetSummerStrata=c(472,473,477,478,481,482,483,484,485))
		addPolys(LFA41,border='blue',lwd=2)
	 dev.off()

pdf(file=file.path(project.figuredirectory('bio.lobster'),paste('georgesmap41.pdf',sep='.')))
LobsterMap(xlim=c(-70.5,-63.5),ylim=c(38.5,45),boundaries='LFAs',addGeorgesStrata=T,output='bio.lobster',fname='georgesmap41.pdf',save=F,labcex =0.8,labels=T)
	addPolys(LFA41,border='blue',lwd=2)
	b = file.path(project.datadirectory('bio.polygons'),'data','Science','PED','GeorgesBankStrata.rdata')
	 load(b)
	 addPolys(subset(out,PID %in% c(1,2)),lty=1,border='green')
	dev.off()


pdf(file=file.path(project.figuredirectory('bio.lobster'),paste('americanmap41full.pdf',sep='.')))
LobsterMap(xlim=c(-71,-63.5),ylim=c(38,45),boundaries='LFAs',addAmericanStrata=T,output='bio.lobster',fname='americanmapfull41.pdf',save=F,labcex =0.8,labels=T)
a = importShapefile(find.bio.gis('BTS_Strata'),readDBF=T) 
                          l = attributes(a)$PolyData[,c('PID','STRATA')]
                          a = merge(a,l,by='PID',all.x=T)
    addPolys(a,border='red')                      
    addPolys(subset(a,STRATA %in% c(1160, 1170, 1180, 1190, 1200, 1210, 1220, 1290, 1300, 1310, 1340, 1360)),lty=1,border='red',col=adjustcolor('white',alpha.f=1))
			
         
	addPolys(LFA41,border='blue',lwd=2)



	dev.off()

