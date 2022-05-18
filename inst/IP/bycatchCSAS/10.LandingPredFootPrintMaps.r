require(bio.lobster)
require(bio.utilities)
require(lubridate)
require(devtools)
options(stringAsFactors=F)
la()
load_all('C:/Users/Cooka/Documents/git/bio.utilities')


wd = ('C:\\Users\\Cooka\\OneDrive - DFO-MPO\\BycatchLobster')
setwd(wd)
p = bio.lobster::load.environment()


ef = readRDS('results/LandingsPredictedActual.rds')
ef = bio.utilities::rename.df(ef,c('PID','SID'),c('LFA','GRID_NO'))
ef$Z = ef$Rediduals/1000
x =  subset(ef,LFA==33)
q = quantile(x$Z,probs=.99,na.rm=T)
x$Z[which(x$Z>q)] <- q
ux = c(min(x$Z),max(x$Z))
x$WOS = x$WOS-6

png('Figures/ModelOutput/LobsterPredictionsvLogs331-12.png',width=10,height=12,units='in',res=300)
ggLobsterMap('33',bathy=T,attrData = subset(x,LFA==33& WOS %in% 1:12),fw='WOS',legLab='Landings Difference (t)',addLFALabels = F,scaleTrans = 'identity' ,brks=ux)
dev.off()

png('Figures/ModelOutput/LobsterPredictionsvLogs13-21.png',width=10,height=12,units='in',res=300)
ggLobsterMap('33',bathy=T,attrData = subset(x,LFA==33& WOS %in% 13:24),fw='WOS',legLab='Landings Difference (t)',addLFALabels = F,scaleTrans = 'identity' ,brks=ux)
dev.off()


x =  subset(ef,LFA==34)
q = quantile(x$Z,probs=.99,na.rm=T)
x$Z[which(x$Z>q)] <- q
ux = c(min(x$Z),max(x$Z))
x$WOS = x$WOS-6
png('Figures/ModelOutput/LobsterPredictionsvLogs34-1-12.png',width=10,height=12,units='in',res=300)
ggLobsterMap('34',bathy=T,attrData = subset(x,LFA==34& WOS %in% 1:12),fw='WOS',legLab='Landings Difference (t)',addLFALabels = F,scaleTrans = 'identity' ,brks=ux)
dev.off()

png('Figures/ModelOutput/LobsterPredictionsvLogs34-13-21.png',width=10,height=12,units='in',res=300)
ggLobsterMap('34',bathy=T,attrData = subset(x,LFA==34& WOS %in% 13:24),fw='WOS',legLab='Landings Difference (t)',addLFALabels = F,scaleTrans = 'identity' ,brks=ux)
dev.off()

x =  subset(ef,LFA==35)
q = quantile(x$Z,probs=.99,na.rm=T)
x$Z[which(x$Z>q)] <- q
ux = c(min(x$Z),max(x$Z))
png('Figures/ModelOutput/LobsterPredictionsvLogs35-1-10.png',width=10,height=12,units='in',res=300)
ggLobsterMap('35',bathy=T,attrData = subset(x,LFA==35 & WOS %in% c(1:10)),fw='WOS',legLab='Landings Difference (t)',addLFALabels = F,scaleTrans ='identity',brks=ux)
dev.off()
png('Figures/ModelOutput/LobsterPredictionsvLogs35-22-31.png',width=10,height=12,units='in',res=300)
ggLobsterMap('35',bathy=T,attrData = subset(x,LFA==35 & WOS %in% c(22:31)),fw='WOS',legLab='Landings Difference (t)',addLFALabels = F,scaleTrans ='identity',brks=ux)
dev.off()
png('Figures/ModelOutput/LobsterPredictionsvLogs35-32-40.png',width=10,height=12,units='in',res=300)
ggLobsterMap('35',bathy=T,attrData = subset(x,LFA==35 & WOS %in% c(32:42)),fw='WOS',legLab='Landings Difference (t)',addLFALabels = F,scaleTrans ='identity',brks=ux)
dev.off()                          


