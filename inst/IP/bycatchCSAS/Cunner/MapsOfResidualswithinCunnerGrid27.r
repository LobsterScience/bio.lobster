#residuals by grid
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

g = readRDS('results/ModelOutput/cunnerFullPred27.rds')
g$res = g$pred - g$CunnerWt
gP = aggregate(res~LFA+GRIDNUM+WOS,data=g,FUN=mean)
gP = bio.utilities::rename.df(gP,'GRIDNUM','GRID_NO')

x =  subset(gP,LFA==27)
x$Z = x$res
ux = c(min(x$Z),max(x$Z))

png('Figures/ModelOutput/CunnerResiduals27-1-9.png',width=10,height=12,units='in',res=300)
ggLobsterMap('27',bathy=T,attrData =x,fw='WOS',legLab='Mean Residual',addLFALabels = F,scaleTrans = 'identity' ,brks=ux)
dev.off()



g = readRDS('results/LandingsPredictedActual27.rds')

x =  subset(g,PID==27)
x$Z = x$Rediduals/1000
ux = c(min(x$Z),max(x$Z))
x$LFA = x$PID
x$GRID_NO = x$SID
png('Figures/ModelOutput/LobsterLandingsDiff27-1-9.png',width=10,height=12,units='in',res=300)
ggLobsterMap('27',bathy=T,attrData = subset(x,PID==27& WOS %in% 1:9),fw='WOS',legLab='Landings Difference (t)',addLFALabels = F,scaleTrans = 'identity' ,brks=ux)
dev.off()


