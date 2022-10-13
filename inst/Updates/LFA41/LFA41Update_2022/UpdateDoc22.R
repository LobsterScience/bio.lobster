
require(RODBC)
require(rgdal)
require(devtools)
require(roxygen2)
require(geosphere)
require(SpatialHub)
require(lubridate)
require(bio.utilities)
require(bio.lobster)
require(rstanarm)
require(rlang)
require(glue)
require(PBSmapping)
require(bio.survey)
require(bio.lobster)
require(dplyr)

p = bio.lobster::load.environment()
la()
assessment.year = 2022 ##Check Year
p$syr = 1989
p$yrs = p$syr:assessment.year

figdir = file.path("C:/Users/HowseVJ/Documents/GitHub/bio.lobster/inst/Updates/LFA41/LFA41Update_2022")

p$lfas = c("41") # specify lfa

## from Oracle
lobster.db('logs.redo') 

#groundfish.db('odbc.redo',datayrs = 1970:2022)


#Not Oracle		
logs=lobster.db('process.logs.redo')
logs=lobster.db("process.logs")

land=lobster.db('annual.landings.redo')
land = lobster.db('annual.landings')
#land = lobster.db('seasonal.landings')
#land=lobster.db('seasonal.landings.redo')

#groundfish.db('gscat.odbc.redo')
#groundfish.db('gsinf.odbc.redo')
#groundfish.db('gsdet.odbc.redo')

#check what's in there
a = groundfish.db('gsinf.odbc')
summary(a)



## map 
#x11(width=5, height=5)
#LobsterMap(ylim=c(41.1,44),xlim=c(-68,-63.5))

LFAs<-read.csv(file.path(project.datadirectory("bio.lobster"), "data","maps","LFAPolys.csv"))
LFAgrid<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","GridPolys.csv"))

png(filename=file.path(figdir, "MapLFA41.png"),width=8, height=6, units = "in", res = 800)
LobsterMap( ylim=c(41,44),xlim=c(-68,-63),nafo ="all",addGrids=F)

dev.off() 


#########LANDINGS

##Landings barplot
ll<- annual.landings
ll <- ll[order(ll$YR),]
par(mar=c(3,5,3,3))
cols<-c("steelblue4","steelblue4", "steelblue4", "steelblue4", "steelblue4", "steelblue4", "steelblue4", "steelblue4", "steelblue4",
        "steelblue4", "steelblue4", "steelblue4", "steelblue4", "steelblue4", "steelblue4", "steelblue4","steelblue4","steelblue4","steelblue4", "firebrick3")
bardens<-c(500,500,500,500,500,500,500,500,500,500,500,500,500,500,500,500,500,500,500,30)

lands<- barplot(ll$LFA41[ll$YR > 2001], ylim =c(0,1000), col = cols, density = bardens, names.arg = 2002:2022, cex.names= 0.8, space =c(0,0), ylab = "Landings (t)" )
box()
abline(h=720, col ='red', lwd = 2)


####### 

##Trips Since 2018  Aggregated  and bumped up by effort

#.	LOBSTER.ISTRAPS - trap details
#GEAR_COND:
#  0	TRAP/HOOK EMPTY - NO BAIT
#1	BAIT PRESENT - NO CATCH
#2	CATCH PRESENT
#3	TRAP - MAJOR DAMAGE
#4	TRAP - MINOR DAMAGE/OPEN

#.	LOBSTER.ISSETSMV - catch details
#.	MARFISSCI.LOBSTER_MS_SLIP - slip weights from logs

#not all trips are observed soo select
#SOURCE = 0 in ISSETS_MV



#### FROM SQL

#LOBSTER.ISTRAPS - trap details
SELECT *
  FROM
lobster.istraps 
WHERE comarea_id = 'L41'

#LOBSTER.ISSETSMV - catch details
select * from lobster.issets_mv
where comarea_id = 'L41'
and source = 0

#	MARFISSCI.LOBSTER_MD_SLIP - slip weights 

select to_char(landing_date_time, 'yyyy') yr, sum(slip_weight_lbs) slip_weight_lbs
from marfissci.lobster_md_slip
where licence_id like '14%'
group by to_char(landing_date_time, 'yyyy')
order by to_char(landing_date_time, 'yyyy')

##effort from logs
select to_char(fv_fished_datetime, 'yyyy') yr, sum(num_of_traps) num_of_traps
from marfissci.lobster_md_log
where licence_id like '14%'
group by to_char(fv_fished_datetime, 'yyyy')
order by to_char(fv_fished_datetime, 'yyyy')


