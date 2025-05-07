
require(RODBC)
require(devtools)
require(roxygen2)
require(geosphere)
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
require(ggplot2)
require(sf)
require(tidyr)


p=list()
p$assessment.year = 2024 ##Check Year
p$syr = 1989

p$lfa=41
p$yrs = 1947:p$assessment.year

figdir = file.path("C:/Users/HowseVJ/OneDrive - DFO-MPO/LFA 41 Update")

p$lfas = c("41") # specify lfa

#Not Oracle		
logs=lobster.db('process.logs.redo')
logs=lobster.db("process.logs")

land=lobster.db('annual.landings.redo')
land = lobster.db('annual.landings')

#groundfish.db('odbc.redo',datayrs = 1970:2024)
#groundfish.db('gscat.odbc.redo')
#groundfish.db('gsinf.odbc.redo')
#groundfish.db('gsdet.odbc.redo')

#check what's in there
a = groundfish.db('gsinf.odbc')
summary(a)
## FOR NEFSC Survey
nefsc.db(DS='odbc.dump.redo')
#inf = nefsc.db( DS = 'usinf.redo.odbc',fn.root = NULL,p=p)
#ca = nefsc.db( DS = 'uscat.redo.odbc',fn.root = NULL,p=p)
#de = nefsc.db( DS = 'usdet.redo.odbc',fn.root = NULL,p=p)

#inf = nefsc.db( DS = 'usinf.clean.redo',fn.root = NULL,p=p)
#de = nefsc.db( DS = 'usdet.clean.redo',fn.root = NULL,p=p)
#ca = nefsc.db( DS = 'uscat.clean.redo',fn.root = NULL,p=p)

inf = nefsc.db( DS = 'usinf.clean',fn.root = NULL,p=p)
de = nefsc.db( DS = 'usdet.clean',fn.root = NULL,p=p)
ca = nefsc.db( DS = 'uscat.clean',fn.root = NULL,p=p)
 
#nefsc.db(DS = 'usstrata.area.redo.odbc')
nefsc.db(DS = 'usstrata.area')

#########LANDINGS
land41 <- land[, c("YR", "LFA41")]
land41<- land41[land41$YR > 2001 & land41$YR <= 2024,]
land41 <- land41[order(land41$YR), ]


##Landings barplot
land41 <-land41 %>%
  mutate(
    fill_color = ifelse(YR == 2024, "#96ACB7", "#202C59"),
  )

# Create the barplot
ggplot(land41, aes(x = factor(YR), y = LFA41, fill = fill_color)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = c("#202C59" = "#202C59", "#96ACB7" = "#96ACB7")) +
  theme_minimal() +
  labs(x = "Year", y = "Landings (t)") +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.border = element_rect(color = "black", fill = NA),  # Add plot borders
    axis.line = element_line(color = "black")  # Add axis lines
  ) +
  scale_x_discrete(breaks = seq(2002, 2024, by = 2)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 900)) +  # Set y-axis limit to 900
  geom_bar(data = land41 %>% filter(YR == 2024), aes(x = factor(YR), y = LFA41), stat = "identity", fill = NA, color = "#96ACB7", linetype = "dashed") +
  geom_hline(yintercept = 720, color = "red")


##Landings Table 
landings41<-land41[, c("YR", "LFA41")]

write.csv(land41, "C:/Users/HowseVJ/OneDrive - DFO-MPO/LFA 41 Update/landingstable.csv")

#########Observer Coverage #############

lobster.db("observer41.redo")
lobster.db("observer41")
lobster.db('logs41.redo') 
lobster.db('logs41')


##Add SYEAR to 41 log data
L41log<-logs41
L41log$dates<-as.POSIXct(L41log$FV_FISHED_DATETIME, format='%m/%d/%Y %H:%M:%S')
L41log$dates = as.Date(L41log$dates,format='%m-%d-%Y')
L41log$MONTH = month(L41log$dates)
L41log$SYEAR= year(L41log$dates)
L41log$DAY=day(L41log$dates)

L41log$DDLON<-L41log$DDLON*-1
L41log<-L41log[L41log$SYEAR > 2010  & L41log$SYEAR < 2025, ]


##Observer Data
Obs41<-observer41
##Need SYear -- pull it out of date
Obs41$dates<-as.POSIXct(Obs41$STARTDATE, format='%Y-%m-%d %H:%M:%S')
Obs41$dates = as.Date(Obs41$dates,format='%m-%d-%Y')
Obs41$MONTH = month(Obs41$dates)
Obs41$SYEAR= year(Obs41$dates)
Obs41$DAY=day(Obs41$dates)

Obs41<-Obs41[Obs41$SYEAR > 2010  & Obs41$SYEAR < 2025, ]


##Number of observed Trips
Obs_trips <- aggregate(TRIPNO ~ SYEAR, data = Obs41, FUN = function(x) length(unique(x)))
names(Obs_trips)[2] <- "ObservedTrips"

##Number of Fishing Trips - from Logs
Fish_trips <- aggregate(TRIP_ID ~ SYEAR, data = L41log, FUN = function(x) length(unique(x)))
names(Fish_trips)[2] <- "TotalLogTrips"


##Number of Fishing Trips - From Slips
slip41$dates<-as.POSIXct(slip41$LANDING_DATE_TIME, format='%Y-%m-%d %H:%M:%S')
slip41$dates = as.Date(slip41$dates,format='%m-%d-%Y')
slip41$MONTH = month(slip41$dates)
slip41$SYEAR= year(slip41$dates)

slip41<-slip41[slip41$SYEAR > 2010  & slip41$SYEAR < 2025, ]

Slip_trips <- aggregate(LANDING_DATE_TIME ~ SYEAR, data = slip41, FUN = function(x) length(unique(x)))
names(Slip_trips)[2] <- "TotalSlipTrips"


Obs_Fish <- merge(Obs_trips, Fish_trips, by = "SYEAR")
ObsTable <- merge(Obs_Fish, Slip_trips, by = "SYEAR")

ObsTable$PercentObserved_logs<-(ObsTable$ObservedTrips/ObsTable$TotalLogTrips)*100

write.csv(ObsTable,"C:/Users/HowseVJ/OneDrive - DFO-MPO/LFA 41 Update/ObsTable.csv")

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

Obstraps<-read.csv("C:/Users/HowseVJ/OneDrive - DFO-MPO/LFA 41 Update/lobster_atsea_v_2.csv")

##Calculate the number of unique observed Trips each year

##Observed Traps
#make unique ID with Trap and Trip
Obstraps$obstrapID<-paste(Obstraps$TRIPNO, Obstraps$TRAPNO)
num_obtrap<-aggregate(obstrapID ~ YR, data=Obstraps, function(x) length(unique(x)))
colnames(num_obtrap)<-c("YEAR","OBSTOTALTRAPS")


##Calculate the number of unique Fishing Trips Each Year 

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
loginfo<-read.csv("C:/Users/HowseVJ/OneDrive - DFO-MPO/LFA 41 Update/Lobster_Md_log_2.csv")
loginfo<-loginfo[loginfo$YR>2017,]
colnames(loginfo)<-c("YEAR","TOTALTRAPS")



loginfo<-loginfo[loginfo$YEAR<2025,]
loginfo<-na.omit(loginfo)

percentObs_traps<-merge(loginfo,num_obtrap, by="YEAR")
percentObs_traps$PerObserved<-(percentObs_traps$OBSTOTALTRAPS)/(percentObs_traps$TOTALTRAPS)
percentObs_traps$PerObserved<-(percentObs_traps$PerObserved*100)

write.csv(percentObs_traps,"C:/Users/HowseVJ/Documents/GitHub/bio.lobster/inst/Updates/LFA41/LFA41_2024/PercentTrapsObs.csv")





######## Try it in R for the same answer


###
require(bio.lobster)
#at sea

lobster.db('atSea')
a = subset(atSea,LFA %in% c(41))
a$YR=year(a$STARTDATE)
a<-a[a$YR >2010,]

## Maybe just from Logs41?

## from Oracle
lobster.db('logs.redo') 
lobster.db('logs')
