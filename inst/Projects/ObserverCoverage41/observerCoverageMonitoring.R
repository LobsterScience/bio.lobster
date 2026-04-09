##Observer Coverage

require(bio.lobster)
require(bio.utilities)
require(devtools)
require(lubridate)
require(ggplot2)
require(dplyr)
require(wesanderson)
require(viridis)
require(viridis)



p=list()
assessment.year = 2026 ##Check Yea

p$lfas = c("41") # specify lfa


lobster.db("observer41.redo")
lobster.db("observer41")
obs<-observer41


##Add SYEAR to 41 data

obs$dates<-as.POSIXct(obs$STARTDATE,format="%Y-%m-%d")

obs$Year <- year(obs$STARTDATE)
obs$Month <- month(obs$STARTDATE)
obs$Day <- day(obs$STARTDATE)

obs_check <- subset(obs, Year > 2024)


n_unique_trips <- dplyr::n_distinct(obs_check$TRIPNO)
n_unique_trips

library(dplyr)

unique_trips <- obs_check %>%
  group_by(TRIPNO) %>%
  summarise(CompletionDate = max(dates, na.rm = TRUE)) %>%
  arrange(CompletionDate)

