
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
p$assessment.year = 2026 ##Check Year
p$syr = 1989

p$lfa=41
p$yrs = 1947:p$assessment.year

figdir = file.path("C:/Users/cooka/Documents/tmp")

p$lfas = c("41") # specify lfa

#Not Oracle		

land=lobster.db('annual.landings.redo')
land = lobster.db('annual.landings')

groundfish.db('odbc.redo',datayrs = 1970:2026)
groundfish.db('gscat.odbc.redo')
groundfish.db('gsinf.odbc.redo')
groundfish.db('gsdet.odbc.redo')
groundfish.db('gs_trawl_conversions_redo')


## FOR NEFSC Survey 
inf = nefsc.db( DS = 'usinf.redo.odbc',fn.root = NULL,p=p)
ca = nefsc.db( DS = 'uscat.redo.odbc',fn.root = NULL,p=p)
de = nefsc.db( DS = 'usdet.redo.odbc',fn.root = NULL,p=p)

inf = nefsc.db( DS = 'usinf.clean.redo',fn.root = NULL,p=p)
de = nefsc.db( DS = 'usdet.clean.redo',fn.root = NULL,p=p)
ca = nefsc.db( DS = 'uscat.clean.redo',fn.root = NULL,p=p)

nefsc.db(DS = 'usstrata.area.redo.odbc')
nefsc.db(DS = 'usstrata.area')



#########LANDINGS
land41 <- land[, c("YR", "LFA41")]
land41<- land41[land41$YR > 2001 & land41$YR <= 2026,]
land41 <- land41[order(land41$YR), ]


##Landings barplot
land41 <-land41 %>%
  mutate(
    fill_color = ifelse(YR == 2026, "#96ACB7", "#202C59"),
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
  scale_x_discrete(breaks = seq(2002, 2025, by = 2)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 900)) +  # Set y-axis limit to 900
  geom_bar(data = land41 %>% filter(YR == 2026), aes(x = factor(YR), y = LFA41), stat = "identity", fill = NA, color = "#96ACB7", linetype = "dashed") +
  geom_hline(yintercept = 720, color = "red")
ggsave(file.path(figdir,'Landings41.pnd'))

##Landings Table 
landings41<-land41[, c("YR", "LFA41")]

write.csv(land41, file.path(figdir,"landingstable.csv"))




#########Observer Coverage #############

lobster.db("atSea.redo")
lobster.db("atSea")
Obs41 = subset(atSea,LFA==41)
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
L41log<-L41log[L41log$SYEAR > 2010  & L41log$SYEAR < 2027, ]


##Observer Data
##Need SYear -- pull it out of date
Obs41$dates<-as.POSIXct(Obs41$STARTDATE, format='%Y-%m-%d %H:%M:%S')
Obs41$dates = as.Date(Obs41$dates,format='%m-%d-%Y')
Obs41$MONTH = month(Obs41$dates)
Obs41$SYEAR= year(Obs41$dates)
Obs41$DAY=day(Obs41$dates)

Obs41<-Obs41[Obs41$SYEAR > 2010  & Obs41$SYEAR < 2027, ]


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

slip41<-slip41[slip41$SYEAR > 2010  & slip41$SYEAR < 2027, ]

Slip_trips <- aggregate(LANDING_DATE_TIME ~ SYEAR, data = slip41, FUN = function(x) length(unique(x)))
names(Slip_trips)[2] <- "TotalSlipTrips"


Obs_Fish <- merge(Obs_trips, Fish_trips, by = "SYEAR")
ObsTable <- merge(Obs_Fish, Slip_trips, by = "SYEAR")

ObsTable$PercentObserved_logs<-(ObsTable$ObservedTrips/ObsTable$TotalLogTrips)*100

write.csv(ObsTable,file.path(figdir,"ObsTable25.csv"))

Obs41$obstrapID<-paste(Obs41$TRIPNO, Obs41$TRAPNO,Obs41$STRINGNO)
num_obtrap<-aggregate(obstrapID ~ SYEAR, data=Obs41, function(x) length(unique(x)))
colnames(num_obtrap)<-c("YEAR","OBSTOTALTRAPS")



loginfo = aggregate(NUM_OF_TRAPS~SYEAR,data=L41log,FUN=sum)

percentObs_traps<-merge(loginfo,num_obtrap, by.x="SYEAR",by.y='YEAR')
percentObs_traps$PerObserved<-(percentObs_traps$OBSTOTALTRAPS)/(percentObs_traps$NUM_OF_TRAPS)
percentObs_traps$PerObserved<-(percentObs_traps$PerObserved*100)

write.csv(percentObs_traps,file.path(figdir,"PercentTrapsObs.csv"))
