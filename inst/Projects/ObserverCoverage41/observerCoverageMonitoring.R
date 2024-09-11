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
assessment.year = 2024 ##Check Year
p$syr = 2022
p$yrs = p$syr:assessment.year
p$lfas = c("41") # specify lfa


#lobster.db("observer41.redo")
lobster.db("observer41")
obs<-observer41


##Add SYEAR to 41 data

obs$dates<-as.POSIXct(obs$STARTDATE,format="%Y-%m-%d")

obs$Year <- year(obs$STARTDATE)
obs$Month <- month(obs$STARTDATE)
obs$Day <- day(obs$STARTDATE)

obs_2024 <- subset(obs, Year == 2024)

