#Eastern Shore Islands request Nick Jeffery Nov 2017

require(bio.lobster)
require(reshape)
a = lobster.db('process.logs')

grids = 324:331

b = subset(a,GRID_NUM %in% grids & SYEAR > 2006)

g  = aggregate(LICENCE_ID~GRID_NUM+SYEAR,data=b,FUN=function(x) length(unique(x)))
g1 = aggregate(WEIGHT_KG ~GRID_NUM+SYEAR,data=b,FUN=sum)
g2 = aggregate(NUM_OF_TRAPS~GRID_NUM+SYEAR,data=b,FUN=sum)



lobster.db('fsrs')
b = subset(fsrs,LFA_GRID %in% grids & TEMP>-99 & HAUL_YEAR >2006)
b = aggregate(TEMP~LFA_GRID+HAUL_YEAR,data=b,FUN=mean)
names(b) = c('GRID_NUM','SYEAR','Mean.Temp.Fishing.Season')
Data = Reduce(function(...) merge(...,all=T), list(g,g1,g2,b))
names(Data)[3:5] =c('Number.Licenses','Total.Log.Landings','Total.Trap.Hauls')

save(Data,file='~/tmp/EasternShoreLobsterData.rdata')
