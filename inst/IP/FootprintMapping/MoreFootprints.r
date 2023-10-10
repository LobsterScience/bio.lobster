#fisheries footprint mapping and analyses of spatial fisheries data
wd = ('C:\\Users\\Cooka\\OneDrive - DFO-MPO\\CanUsCollabEffortMapping')

ef = readRDS('DiscretizedData/TrapHaulsWithinGridandWeek.rds')
eL = split(ef,f=list(ef$LFA,ef$WOS,ef$SYEAR))
eL = rm.from.list(eL)
eLm = aggregate(BTTH~LFA+SYEAR,data=ef,FUN=function(x) quantile(x, seq(0.01,0.99,length.out = 7)))

gr = list()

for(i in 1:length(eL)){
  u = unique(eL[[i]]$LFA)
  y = unique(eL[[i]]$SYEAR)
  k = subset(eLm,LFA==u & SYEAR==y)
  lv = c(0,round(unlist(k[2:length(k)])/10)*10)
  w = lobGridPlot(eL[[i]][,c('LFA','GRID_NUM','BTTH')],FUN=max,lvls=lv,cuts=T)$pdata
  w$WOS = unique(eL[[i]]$WOS)
  w$SYEAR = y
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

x =  subset(o,LFA==33)
ux = c(min(x$Z),max(x$Z))

ggLobsterMap('33',bathy=T,attrData = subset(o,LFA==x),fw='WOS',legLab='TrapHauls',addLFALabels = F,brks=ux)
