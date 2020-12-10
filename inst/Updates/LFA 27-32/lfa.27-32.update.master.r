p = bio.lobster::load.environment()
require(SpatialHub)
require(lubridate)

la()

assessment.year = p$current.assessment.year 
p$current.assessment.year = p$current.assessment.year - 1 



p$lfas = c("27", "28", "29", "30", "31A", "31B", "32") # specify lfas for data summary
p$subareas = c("27N","27S", "28", "29", "30", "31A", "31B", "32") # specify lfas for data summary

# update data through ROracle
NewDataPull =F
if(NewDataPull){
  lobster.db('fsrs.redo')
  lobster.db('logs.redo')
  lobster.db('annual.landings.redo')
  lobster.db('vlog.redo')
  logs=lobster.db('process.logs.redo')
}


#	 png(filename="MapLFA2732.png",width=6.5, height=6.5, units = "in", res = 800)
# LobsterMap('27-32')
#dev.off()
```
