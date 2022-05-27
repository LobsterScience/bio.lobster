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

g = readRDS('results/JonahFullPred31ab.rds')
g$res = g$pred - g$JonahWt
i = which(g$GRIDNUM==9)
g$GRIDNUM[i] = 337

i = which(g$GRIDNUM==15)
g$GRIDNUM[i] = 339

i = which(g$GRIDNUM==335336)
g$GRIDNUM[i] = 335

gP = aggregate(res~LFA+GRIDNUM,data=g,FUN=mean)
gP = bio.utilities::rename.df(gP,'GRIDNUM','GRID_NO')

gP$LFA = ifelse(gP$LFA=='31A',311,312)
x =  gP
x$Z = x$res
ux = c(min(x$Z),max(x$Z))




png('Figures/ModelOutput/JonahResiduals31ab.png',width=10,height=12,units='in',res=300)
ggLobsterMap('ENS',bathy=T,attrData = x ,legLab='Mean Residual',addLFALabels = F,scaleTrans = 'identity' ,brks=ux)
dev.off()



g = readRDS('results/ModelOutput/LandingsPredictedActual31ab.rds')
x=g
x$Z = x$Rediduals/1000
ux = c(min(x$Z),max(x$Z))
x$LFA = x$PID
x$GRID_NO = x$SID
x$LFA = ifelse(x$LFA=='31A',311,312)

png('Figures/ModelOutput/LobsterLandingsDiff31ab.png',width=10,height=12,units='in',res=300)
ggLobsterMap('ENS',bathy=T,attrData = x,fw='SYEAR',legLab='Landings Difference (t)',addLFALabels = F,scaleTrans = 'identity' ,brks=ux)
dev.off()


