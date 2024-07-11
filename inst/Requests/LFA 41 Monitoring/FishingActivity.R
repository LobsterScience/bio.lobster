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


#lobster.db( DS="observer41.redo")
lobster.db( DS="observer41")

#lobster.db( DS = 'logs41.redo', p=p) 
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


#number of traps per trip 
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
LogLandings<-logs41

yr_cpue=aggregate(cbind(NUM_OF_TRAPS,EST_WEIGHT_LOG_LBS)~SYEAR,data=LogLandings,FUN=sum)
yr_cpue$CPUE=yr_cpue$EST_WEIGHT_LOG_LBS/yr_cpue$NUM_OF_TRAPS



moyr_cpue<-LogLandings%>%
  group_by(MONTH,SYEAR)%>%
  summarise(total_traps = sum(NUM_OF_TRAPS),
            total_lbs = sum(EST_WEIGHT_LOG_LBS))
moyr_cpue <-as.data.frame(moyr_cpue)
moyr_cpue<-na.omit(moyr_cpue)

moyr_cpue$cpue<-moyr_cpue$total_lbs/moyr_cpue$total_traps


ggplot(moyr_cpue, aes(x = MONTH, y = cpue, color = factor(SYEAR))) +
  geom_line() +
  geom_point() +
  labs(x = "Month", y = "Catch (lbs/tHs)", color = "Year") +
  theme_classic()+
  scale_x_discrete(limits = month.name)

# average catch per trap - range of catch per trap
# Average soak days - number of soak-days
# CPUE each month
# Total Landings each month
#


###Looking for issues all situations where Traps are partial 

CheckTraps<-logs41[logs41$NUM_OF_TRAPS<10,]

# Remove rows where all columns are NA
  CheckTraps<- CheckTraps[rowSums(is.na(CheckTraps)) != ncol(CheckTraps), ]
write.csv(CheckTraps, 'E:/Nova Scotia/Lobster Job/Stock Assessment/LFA 41/LFA 41 Evaluation/Fishing Activity/CheckTraps.csv')


##Trips with unusually low Traps per trip

Oct2022<-logs41[logs41$MONTH == 10 & logs41$SYEAR == 2022, ]
#September 2020
Oct2023<-logs41[logs41$MONTH == 10 & logs41$SYEAR == 2023, ]


##Trips where Fishing occurred inside LFA 40
#September 2019
Sept2019<-logs41[logs41$MONTH == 9 & logs41$SYEAR == 2019, ]
#September 2020
Sept2020<-logs41[logs41$MONTH == 9 & logs41$SYEAR == 2020, ]
#September 2023
Sept2023<-logs41[logs41$MONTH == 9 & logs41$SYEAR == 2023, ]

##Atypical Soak Days 

CheckSoak<-logs41[logs41$SOAK_DAYS<10,]
CheckSoak<- CheckSoak[rowSums(is.na(CheckSoak)) != ncol(CheckSoak), ]

write.csv(CheckSoak, 'E:/Nova Scotia/Lobster Job/Stock Assessment/LFA 41/LFA 41 Evaluation/Fishing Activity/CheckSoak.csv')




####CHECK ACTIVITY IN LFA 40
require(bio.survey)
require(bio.lobster)
p = bio.lobster::load.environment()
p$libs = NULL

require(bio.lobster)
require(bio.utilities)
require(sf)
la()

lobster.db('logs41') #make sure to do a database recapture through logs41.redo before moving on

logs41 = rename.df(logs41,c('FV_FISHED_DATETIME'),c('DATE_FISHED'))
rL = readRDS(file.path("C:/Users/HowseVJ/Documents/bio.data/bio.lobster/data/maps/LFAPolysSF.rds"))


logs41$yr = year(logs41$DATE_FISHED) #2002 to present
ziff41$yr = year(ziff41$DATE_FISHED) #1995 to 2001
ziff41$DDLON = ziff41$DDLON * -1
off41$yr  = year(off41$DATE_FISHED) #1981 to 1994

logs41$OFFAREA = NULL   

a41 = logs41
a41$fishingYear = a41$yr
#a41 = makePBS(a41,polygon=F)
a41$ADJCATCH = a41$ADJCATCH / 2.2 #convert to kgs then to t

a41 = subset(a41,!is.na(DDLON))
a41$DDLON = a41$DDLON*-1
aS = st_as_sf(a41,coords=c('DDLON','DDLAT'),crs=4326)

aSS=st_join(aS,rL,join=st_intersects)
##aSS will have LFA column now

checkClosure<-aSS[aSS$LFA == 40,]
checkClosure<-checkClosure[!is.na(checkClosure$ADJCATCH),]
checkClosure<-checkClosure[checkClosure$fishingYear>2017,]

write.csv(checkClosure, 'E:/Nova Scotia/Lobster Job/Stock Assessment/LFA 41/LFA 41 Evaluation/Fishing Activity/CheckClosure.csv')

