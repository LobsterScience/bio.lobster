###Mapping LFA 41

bio.datadirectory <- "C:/Users/HowseVJ/Documents/bio.data"
git.repo <- "c:/Users/HowseVJ/Documents/GitHub"

require(bio.lobster)
require(bio.utilities)
require(devtools)
require(PBSmapping)
require(lubridate)
require(ggplot2)
require(dplyr)
require(RODBC)



p = bio.lobster::load.environment()
p$libs = NULL
fp = file.path(project.datadirectory('bio.lobster'),"analysis")
la()
assessment.year = 2024
p$yrs = 1947:p$current.assessment.year
p$lfas = c("41") # specify lfa


#figdir = file.path("C:/Users/HowseVJ/Documents/Scripts")

lobster.db( DS="observer41.redo")
lobster.db( DS="observer41")
lobster.db( DS = 'logs41.redo', p=p) 
lobster.db( DS = 'logs41', p=p)


##Add SYEAR to 41 data
L41log<-logs41
L41log$dates<-as.POSIXct(L41log$FV_FISHED_DATETIME, format='%m/%d/%Y %H:%M:%S')
L41log$dates = as.Date(L41log$dates,format='%m-%d-%Y')
L41log$MONTH = month(L41log$dates)
L41log$SYEAR= year(L41log$dates)
L41log$DAY=day(L41log$dates)



##Make Label position df-- need to shift them

Offarea41<-read.csv("C:/Users/HowseVJ/Documents/bio.data/bio.lobster/data/maps/LFA41Offareas.csv")
Offarealab<-Offarea41[!duplicated(Offarea41$OFFAREA),]
names(Offarealab)[5]<- "label"
Offarealab$X<-c(-67.42, -66.1, -66.4, -64.83, -66.85)
Offarealab$Y<-c(43.0, 41.5,42.2,42.35, 42.6)
Offarealab$Area<-c("CROWELL","GBANK","GBASIN","SE.Browns", "SW.Browns")


##add points of Observer Coverage on top or side by side heatmap of the observer coverage?- each year by SYEAR
Obs41<-observer41
Obs41$LONGITUDE<-Obs41$LONGITUDE*-1

##Need SYear -- pull it out of date
Obs41$dates<-as.POSIXct(Obs41$STARTDATE, format='%Y-%m-%d %H:%M:%S')
Obs41$dates = as.Date(Obs41$dates,format='%m-%d-%Y')
Obs41$MONTH = month(Obs41$dates)
Obs41$SYEAR= year(Obs41$dates)
Obs41$DAY=day(Obs41$dates)

###Clip Observer Coverage for maps

Obs41<-Obs41[Obs41$SYEAR > 2017, ]

## Number of Trips
Trips_logs<-aggregate(MON_DOC_ID ~SYEAR, data = L41log, FUN = function(x) length(unique(x)))


##Trap Hauls by Year in LOGS

Agg_Logs<- aggregate(NUM_OF_TRAPS ~SYEAR, data = L41log, sum, na.rm=TRUE)
Agg_Logs<-Agg_Logs[Agg_Logs$SYEAR >2017,]
##Trap Hauls by Year in Observed
##Observed Traps
#make unique ID with Trap and Trip
Obs41$obstrapID<-paste(Obs41$TRIPNO, Obs41$TRAPNO)
num_obtrap<-aggregate(obstrapID ~ SYEAR, data=Obs41, function(x) length(unique(x)))
colnames(num_obtrap)<-c("SYEAR","OBSTOTALTRAPS")

Percentobs<-merge(Agg_Logs,num_obtrap, by = "SYEAR")
Percentobs$PercentObserved<-(Percentobs$OBSTOTALTRAPS)/(Percentobs$NUM_OF_TRAPS)
Percentobs$PercentObserved<-(Percentobs$PercentObserved*100)



###Summarized observed trips

filtered_data <- Obs41 %>%
  filter(SYEAR %in% c(2022, 2023, 2024))

unique_trips <- filtered_data %>%
  select(TRIPNO, MONTH, SYEAR) %>%
  distinct()
