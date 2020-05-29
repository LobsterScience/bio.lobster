#Generic Maps

require(bio.lobster)
require(bio.polygons)
require(bio.utilities)
require(PBSmapping)


LobsterMap(xlim=c(-69,-57.5),ylim=c(41.5,47.2),boundaries='LFAs',labcex=.8,fname='OverallLFAs.pdf',save=T,output='bio.lobster')
LobsterMap('41',boundaries='LFAs',labcex=.8,fname='LFA41.pdf',save=T,output='bio.lobster')


LobsterMap(xlim=c(-69,-56.8),ylim=c(41.2,47.5),boundaries='LFAs',addSummerStrata=T,output='bio.lobster','summerstratamap.pdf',save=F,labcex =0.8)


	FA41 = read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","LFA41Offareas.csv"))
	LFA41 = joinPolys(as.PolySet(LFA41),operation='UNION')
	LFA41 = subset(LFA41,PID==1)
	attr(LFA41,'projection') <- 'LL'

	addPolys(LFA41,border='red')
