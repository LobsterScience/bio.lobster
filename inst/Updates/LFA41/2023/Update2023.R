
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
assessment.year = 2023 ##Check Year
p$syr = 1989
p$yrs = p$syr:assessment.year

figdir = file.path("C:/Users/HowseVJ/Documents/GitHub/bio.lobster/inst/Updates/LFA41/LFA41Update_2022")

p$lfas = c("41") # specify lfa

## from Oracle
#lobster.db('logs.redo') 

#groundfish.db('odbc.redo',datayrs = 1970:2023)


#Not Oracle		
#logs=lobster.db('process.logs.redo')
logs=lobster.db("process.logs")

#land=lobster.db('annual.landings.redo')
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
ll<- land
ll <- ll[order(ll$YR),]
par(mar=c(3,5,3,3))
cols<-c("steelblue4","steelblue4", "steelblue4", "steelblue4", "steelblue4", "steelblue4", "steelblue4", "steelblue4", "steelblue4",
        "steelblue4", "steelblue4", "steelblue4", "steelblue4", "steelblue4", "steelblue4", "steelblue4","steelblue4","steelblue4","steelblue4","steelblue4","steelblue4", "firebrick3")
bardens<-c(500,500,500,500,500,500,500,500,500,500,500,500,500,500,500,500,500,500,500,500,500,30)

lands<- barplot(ll$LFA41[ll$YR > 2001], ylim =c(0,1000), col = cols, density = bardens, names.arg = 2002:2023, cex.names= 0.8, space =c(0,0), ylab = "Landings (t)" )
box()
abline(h=720, col ='red', lwd = 2)


####### 
##Landings Table 

ll 

### Observed Trips Table

#From SQL Pull 
#select startdate, yr, tripno, trapno, speciescode, species, calwt, nvl(discardwt,0) discardwt from (
#  select startdate, to_char(startdate, 'yyyy') yr, tripno, trapno, speciescode, species, calwt,
#  case
#  when speciescode = 2550 and carlength < 82 then calwt
#  when speciescode = 2550 and carlength = 82 then calwt/2
#  when speciescode = 2550 and carlength >=150 then calwt
#  when speciescode = 2550 and vnotch is not null and sex = 2 then calwt
#  when speciescode = 2550 and sex = 3 then calwt
#  when speciescode = 2550 and shell in (1,2,3) then calwt
#  when speciescode ! = 2550 then calwt
#  else null
#  end discardwt
#  from lobster.lobster_atsea_vw
#  where lfa = '41'
#  and startdate > '2018-10-15')

Obstraps<-read.csv("C:/Users/HowseVJ/Documents/GitHub/bio.lobster/inst/Updates/LFA41/lobster_atsea_v_2.csv")

##Calculate the number of unique observed Trips each year

##Observed Traps
#make unique ID with Trap and Trip
Obstraps$obstrapID<-paste(Obstraps$TRIPNO, Obstraps$TRAPNO)
num_obtrap<-aggregate(obstrapID ~ YR, data=Obstraps, function(x) length(unique(x)))



##Calculate the number of unique Fishing Trips Each Year 

## Maybe just from Logs41?

## from Oracle
lobster.db('logs.redo') 




#MARFISSCI.LOBSTER_MD_LOG - is the string by string estimated catch and effort from logs.
#select to_char(fv_fished_datetime, 'yyyy') yr, sum(num_of_traps) num_of_traps
#from marfissci.lobster_md_log
#where MON_DOC_DEFN_ID = 19 
#group by to_char(fv_fished_datetime, 'yyyy')
#order by to_char(fv_fished_datetime, 'yyyy')


#ISTRAPS is the data collected by the observer during observed trips on the gear success form (attached).

#ISSETSMV is the data collected by the observer on the catch per set (string). It is an estimation based on the observed traps. Like the catch card we fill out on the ILTS, but bumped up to the entire string of traps.

##FOR Observered Traps:
##


#loginfo is the number of traps  = effort - total fished traps
loginfo<-read.csv("C:/Users/HowseVJ/Documents/GitHub/bio.lobster/inst/Updates/LFA41/Lobster_Md_log_2.csv")
loginfo<-loginfo[loginfo$YR>2017,]
colnames(loginfo)<-c("YEAR","TOTALTRAPS")
