#Generic Maps

require(bio.lobster)
require(bio.polygons)
require(bio.utilities)
require(PBSmapping)


LobsterMap(xlim=c(-69,-57.5),ylim=c(41.5,47.2),boundaries='LFAs',labcex=.8,fname='OverallLFAs.pdf',save=T,output='bio.lobster')
LobsterMap('41',boundaries='LFAs',labcex=.8,fname='LFA41.pdf',save=T,output='bio.lobster')



