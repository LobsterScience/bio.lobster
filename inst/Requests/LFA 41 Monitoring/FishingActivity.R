require(bio.lobster)
require(bio.utilities)
require(devtools)
require(PBSmapping)
require(lubridate)
require(ggplot2)
require(dplyr)
require(RODBC)
require(rgdal)


p = bio.lobster::load.environment()
p$libs = NULL
la()
assessment.year = 2024
p$yrs = 1947:p$current.assessment.year
p$lfas = c("41") # specify lfa

la()
wd = ('E:/Nova Scotia/Lobster Job/Stock Assessment/LFA 41/LFA 41 Evaluation/Fishing Activity')
setwd(wd)


lobster.db( DS="observer41.redo")
lobster.db( DS="observer41")

lobster.db( DS = 'logs41.redo', p=p) 
lobster.db( DS = 'logs41', p=p)



#Break out dates
logs41$dates <-as.POSIXct(logs41$FV_FISHED_DATETIME, format='%Y-%m-%d %H:%M:%S')
logs41$dates = as.Date(logs41$dates,format='%m-%d-%Y')
logs41$MONTH = month(logs41$dates)
logs41$SYEAR= year(logs41$dates)
logs41$DAY=day(logs41$dates)

# Subset slip41 and logs41 by RANDELL DOMINAUX in Last 5 years

logs41<-logs41[logs41$SYEAR > 2017  & logs41$SYEAR < 2025, ]


#number of Trips per year
Trips_log<-aggregate(MON_DOC_ID ~SYEAR, data = logs41, FUN = function(x) length(unique(x)))

#number of Trips per month by Year
Trips_mon_yr<-logs41%>%
  group_by(MONTH,SYEAR)%>%
  summarize(Trips_summary=n_distinct(MON_DOC_ID))
Trips_mon_yr<-as.data.frame(Trips_mon_yr)

##plot the num Trips each month, with the Syear as a different colour line
Trips_mon_yr<- na.omit(Trips_mon_yr)
ggplot(Trips_mon_yr, aes(x = MONTH, y = Trips_summary, color = factor(SYEAR))) +
  geom_line() +
  geom_point() +
  labs(x = "Month", y = "Number of Trips", title = "Number of Trips by Month and Year") +
  scale_x_discrete(limits = month.name) +
  scale_color_discrete(name = "Year")

##in October of 2023, 2022, and 2018, Trips were really low - Check CPUE


#number of traps per trip - range of traps per trip
triptraps<-logs41 %>%
  group_by(MON_DOC_ID, MONTH, SYEAR)%>%
    summarize(traps_per_trip =sum(NUM_OF_TRAPS))
triptraps<-as.data.frame(triptraps)

#Avg Traps by month and Year
Avg_traps_trips<-triptraps%>%
  group_by(MONTH, SYEAR)%>%
  summarize(avg_traps =mean(traps_per_trip))
Avg_traps_trips<-as.data.frame(Avg_traps_trips)

##plot the Avg Traps for each month, with the Syear as a different colour line
Avg_traps_trips<- na.omit(Avg_traps_trips)
ggplot(Avg_traps_trips, aes(x = MONTH, y = avg_traps, color = factor(SYEAR))) +
  geom_line() +
  geom_point() +
  labs(x = "Month", y = "Average Traps", title = "Average Traps per Trip by Month and Year") +
  scale_x_discrete(limits = month.name) +
  scale_color_discrete(name = "Year")

#####What's going in October of 2022 and 2023?



##CPUE - To investigate October low trips





# average catch per trap - range of catch per trap
# Average soak days - number of soak-days
# CPUE each month
# Total Landings each month
#
