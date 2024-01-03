require(bio.lobster)
require(bio.utilities)
require(PBSmapping)
library("ggplot2")
library("sf")

#Map LFA 41 with boundaries and NAFO
g=ggLobsterMap(area ='all', addLFALabels = T)

nafo.xy<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","nafo.csv"))
nafo<-unique(nafo.xy$label)
nafo.sel<-subset(nafo.xy,label%in%nafo)
nafo.dat<-merge(calcCentroid(nafo.sel),nafo.sel[c("PID","label")])[!duplicated(nafo.sel[c("PID","label")]),]
nafo.dat$label[nafo.dat$label=="5ZC"]<-"5ZEM"

addPolys(nafo.xy,border='grey',col=NULL)
addLabels(nafo.dat,col=rgb(0.5,0.5,0.5,0.5),cex=2)
