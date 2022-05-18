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

g = readRDS('JonahsdmTMBFull.rds')
gg = g[[2]]
gg$res = g[[1]]$family$linkinv(g[[2]]$est) - g[[2]]$JonahWt
gP = aggregate(res~LFA+GRIDNUM+WOS,data=gg,FUN=mean)
gP = bio.utilities::rename.df(gP,'GRIDNUM','GRID_NO')

x =  subset(gP,LFA==33)
x$Z = x$res
ux = c(min(x$Z),max(x$Z))
x$WOS = x$WOS-6

png('Figures/ModelOutput/JonahResiduals33-1-12.png',width=10,height=12,units='in',res=300)
ggLobsterMap('33',bathy=T,attrData = subset(x,LFA==33& WOS %in% 1:12),fw='WOS',legLab='Mean Residual',addLFALabels = F,scaleTrans = 'identity' ,brks=ux)
dev.off()

png('Figures/ModelOutput/JonahResiduals33-13-24.png',width=10,height=12,units='in',res=300)
ggLobsterMap('33',bathy=T,attrData = subset(x,LFA==33& WOS %in% 13:24),fw='WOS',legLab='Mean Residual',addLFALabels = F,scaleTrans = 'identity' ,brks=ux)
dev.off()


x =  subset(gP,LFA==34)
x$Z = x$res
ux = c(min(x$Z),max(x$Z))
x$WOS = x$WOS-6

png('Figures/ModelOutput/JonahResiduals34-1-12.png',width=10,height=12,units='in',res=300)
ggLobsterMap('34',bathy=T,attrData = subset(x,LFA==34& WOS %in% 1:12),fw='WOS',legLab='Mean Residual',addLFALabels = F,scaleTrans = 'identity' ,brks=ux)
dev.off()

png('Figures/ModelOutput/JonahResiduals34-13-24.png',width=10,height=12,units='in',res=300)
ggLobsterMap('34',bathy=T,attrData = subset(x,LFA==34& WOS %in% 13:24),fw='WOS',legLab='Mean Residual',addLFALabels = F,scaleTrans = 'identity' ,brks=ux)
dev.off()



x =  subset(gP,LFA==35)
x$Z = x$res
ux = c(min(x$Z),max(x$Z))


png('Figures/ModelOutput/JonahResiduals35-1-10.png',width=10,height=12,units='in',res=300)
ggLobsterMap('35',bathy=T,attrData = subset(x,LFA==35& WOS %in% 1:10),fw='WOS',legLab='Mean Residual',addLFALabels = F,scaleTrans = 'identity' ,brks=ux)
dev.off()

png('Figures/ModelOutput/JonahResiduals35-21-31.png',width=10,height=12,units='in',res=300)
ggLobsterMap('35',bathy=T,attrData = subset(x,LFA==35& WOS %in% 21:31),fw='WOS',legLab='Mean Residual',addLFALabels = F,scaleTrans = 'identity' ,brks=ux)
dev.off()

png('Figures/ModelOutput/JonahResiduals35-32-40.png',width=10,height=12,units='in',res=300)
ggLobsterMap('35',bathy=T,attrData = subset(x,LFA==35& WOS %in% 32:40),fw='WOS',legLab='Mean Residual',addLFALabels = F,scaleTrans = 'identity' ,brks=ux)
dev.off()
