require(bio.lobster)
require(bio.utilities)
require(lubridate)
require(devtools)
options(stringAsFactors=F)
la()
load_all('C:/Users/Cooka/Documents/git/bio.utilities')


wd = ('C:\\Users\\cooka/OneDrive - DFO-MPO/Projects/BycatchLobster/')
setwd(wd)
p = bio.lobster::load.environment()


ef = readRDS('results/BumpedUpEffortByGridNum.rds')
ef = aggregate(BTTH~LFA+WOS+GRID_NUM,data=ef,FUN=function(x) sum(x)/3)
eL = split(ef,f=list(ef$LFA,ef$WOS))
eL = rm.from.list(eL)
eLm = aggregate(BTTH~LFA,data=ef,FUN=function(x) quantile(x, seq(0.01,0.99,length.out = 7)))

gr = list()

for(i in 1:length(eL)){
  u = unique(eL[[i]]$LFA)
  k = subset(eLm,LFA==u)
  lv = c(0,round(unlist(k[2:length(k)])/10)*10)
  w = lobGridPlot(eL[[i]][,c('LFA','GRID_NUM','BTTH')],FUN=max,lvls=lv,cuts=T)$pdata
  w$WOS = unique(eL[[i]]$WOS)
  gr[[i]] = w
  }

o = do.call(rbind,gr)
oi = unique(o$PID)

for(i in 1:length(oi)){
  k = which(o$PID==oi[i])
  l = max(o$cuts[k])
  o$cuts[intersect(which(o$cuts==l),k)] = max(o$Z[k])
 }

names(o)[1:2] <- c('LFA','GRID_NO')

x =  subset(o,LFA==27)
ux = c(min(x$Z),max(x$Z))

png('Figures/ModelOutput/LobsterTRapHaulsByWeek27.png',width=10,height=12,units='in',res=300)
ggLobsterMap('27',bathy=T,attrData = subset(o,LFA==27),fw='WOS',legLab='TrapHauls',addLFALabels = F,brks=ux)
dev.off()

x =  subset(o,LFA==311)
ux = c(min(x$Z),max(x$Z))
png('Figures/ModelOutput/LobsterTRapHaulsByWeek31A.png',width=10,height=12,units='in',res=300)
ggLobsterMap('31A',bathy=T,attrData = subset(o,LFA==311),fw='WOS',legLab='TrapHauls',addLFALabels = F,brks=ux)
dev.off()

x =  subset(o,LFA==312)
ux = c(min(x$Z),max(x$Z))
png('Figures/ModelOutput/LobsterTRapHaulsByWeek31B.png',width=10,height=12,units='in',res=300)
ggLobsterMap('31B',bathy=T,attrData = subset(o,LFA==312),fw='WOS',legLab='TrapHauls',addLFALabels = F,brks=ux)
dev.off()

x =  subset(o,LFA==33)
ux = c(min(x$Z),max(x$Z))
png('Figures/ModelOutput/LobsterTRapHaulsByWeek331-12.png',width=10,height=12,units='in',res=300)
ggLobsterMap('33',bathy=T,attrData = subset(o,LFA==33 & WOS %in% 1:12),fw='WOS',legLab='TrapHauls',addLFALabels = F,scaleTrans='sqrt',brks=ux)
dev.off()

png('Figures/ModelOutput/LobsterTRapHaulsByWeek3313-24.png',width=10,height=12,units='in',res=300)
ggLobsterMap('33',bathy=T,attrData = subset(o,LFA==33 & WOS %in% 13:24),fw='WOS',legLab='TrapHauls',addLFALabels = F,scaleTrans='sqrt',brks=ux)
dev.off()


x =  subset(o,LFA==34)
ux = c(min(x$Z),max(x$Z))
png('Figures/ModelOutput/LobsterTRapHaulsByWeek341-12.png',width=10,height=12,units='in',res=300)
ggLobsterMap('34',bathy=T,attrData = subset(o,LFA==34 & WOS %in% 1:12),fw='WOS',legLab='TrapHauls',addLFALabels = F,scaleTrans='sqrt',brks=ux)
dev.off()

png('Figures/ModelOutput/LobsterTRapHaulsByWeek3413-24.png',width=10,height=12,units='in',res=300)
ggLobsterMap('34',bathy=T,attrData = subset(o,LFA==34 & WOS %in% 13:24),fw='WOS',legLab='TrapHauls',addLFALabels = F,scaleTrans='sqrt',brks=ux)
dev.off()


x =  subset(o,LFA==35)
ux = c(min(x$Z),max(x$Z))
png('Figures/ModelOutput/LobsterTRapHaulsByWeek351-10.png',width=10,height=12,units='in',res=300)
ggLobsterMap('35',bathy=T,attrData = subset(o,LFA==35 & WOS %in% c(1:10)),fw='WOS',legLab='TrapHauls',addLFALabels = F,scaleTrans ='sqrt',brks=ux)
dev.off()
png('Figures/ModelOutput/LobsterTRapHaulsByWeek3522-31.png',width=10,height=12,units='in',res=300)
ggLobsterMap('35',bathy=T,attrData = subset(o,LFA==35 & WOS %in% c(22:31)),fw='WOS',legLab='TrapHauls',addLFALabels = F,scaleTrans ='sqrt',brks=ux)
dev.off()
png('Figures/ModelOutput/LobsterTRapHaulsByWeek3332-42.png',width=10,height=12,units='in',res=300)

ggLobsterMap('35',bathy=T,attrData = subset(o,LFA==35 & WOS %in% c(32:42)),fw='WOS',legLab='TrapHauls',addLFALabels = F,scaleTrans ='sqrt',brks=ux)
                          
dev.off()


