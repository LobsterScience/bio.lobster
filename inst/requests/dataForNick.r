p = bio.lobster::load.environment()

LobsterMap("32",labels='grid')

grids = 324:331

lobster.db("logs.redo")
lobster.db("process.logs.redo")
logsInSeason=lobster.db("process.logs")

LobsterData = data.frame(sapply(grids,function(x){with(subset(logsInSeason,GRID_NUM==x),tapply(TOTAL_WEIGHT_KG,SYEAR,sum,na.rm=T))}))

names(LobsterData) = grids

LobsterData

