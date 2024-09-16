###Current Dive survey
require(dplyr)
require(devtools)
require(sf)
require(lubridate)
library(ggplot2)
require(ggspatial)
require(bio.lobster)
require(bio.utilities)


setwd("C:/Users/HowseVJ/OneDrive - DFO-MPO/LFA 35-38 Framework Resources/Figures")

dive = read.csv("C:/Users/HowseVJ/Documents/bio.data/bio.lobster/data/divesurvey/CurrentDiveTransects.csv")


##Separate out date ( for Month, Day, Year)
dive$dates<-as.POSIXct(dive$date, format="%m/%d/%Y")
dive$month = month(dive$dates)
dive$year= year(dive$dates)

## Add a column  (TRANSECT ID) that provides a unique identifier for transects (Transect No + Year)
dive$tid = paste(dive$transect_id,dive$year,sep="-")
length(unique(dive$tid))


#Lobster encountered on transect is "lobster' column - sometimes encountered but not measured
dive$lobster[is.na(dive$lobster)] = 0

#Checks lobster encountered by unique transect
datsum<-dive%>%
  group_by(tid, year, location)%>%
  summarise(lobster=sum(lobster))
datsum=as.data.frame(datsum)


### Fill in NAs in secondary substrate to be 0s?
dive$substrate_2[is.na(dive$substrate_2)]<-0

##CONVERT LAT LONGS

dive$LATITUDE_S<-convert.dd.dddd(dive$start_lat, format = "dec.deg")
dive$LONGITUDE_S<-convert.dd.dddd(-dive$start_long, format = "dec.deg")

dive$LATITUDE_E<-convert.dd.dddd(dive$end_lat, format = "dec.deg")
dive$LONGITUDE_E<-convert.dd.dddd(-dive$end_long, format = "dec.deg")

### CHECK LOCATIONS

diveNB<-dive %>% filter(region == "New Brunswick")

cents = readRDS(file.path(project.datadirectory("bio.lobster"), "data","maps","LFALabelsSF.rds"))
cents$geometry[cents$label == "36"] <- st_sfc(st_point(c(-66, 45.15)))
cents$geometry[cents$label == "35"] <- st_sfc(st_point(c(-64.85, 45.22)))

ns_coast = readRDS(file.path(project.datadirectory("bio.lobster"), "data","maps","CoastSF.rds"))


p = ggLobsterMap(area='38',addGrids=F,
                  fill.colours = 'grey',bathy=F,return.object = T,colourLFA = F)
p = p + geom_point(data = diveNB, aes(x = LONGITUDE_S, y = LATITUDE_S), color = "red")
p


## Run the data through fillSubstrate()Function in bio.lobster that 'fills in the blanks' for when substrate type is recorded - done by the unique transect ID (TID)
script_path <- "C:/Users/HowseVJ/Documents/GitHub/bio.lobster/R/fillSubstrate_vh.R"
source(script_path)

x=diveNB
### Reformat character columns to numeric
cols_to_convert <- c("transect_no", "transect_width","transect_length", "max_depth","avg_depth","bottom_temp","month","year","sex")
x[cols_to_convert] <- lapply(x[cols_to_convert], as.numeric)


###Fill substrate with missing information to build full habitat coverage
xs = split(x,f=x$tid)
out = list()
for(i in 1:length(xs)){
  print(unique(xs[i]$tid))
  out[[i]] <- fillSubstrate_vh(xs[[i]])
}
final = do.call(rbind,out)

### Reformat character columns to numeric
cols_to_convert <- c("transect_no", "transect_width","transect_length", "max_depth","avg_depth","bottom_temp","month","year","sex")
final[cols_to_convert] <- lapply(final[cols_to_convert], as.numeric)


diveTrans<-final

write.csv(diveTrans,"C:/Users/HowseVJ/Documents/bio.data/bio.lobster/data/divesurvey/BOF_DiveFormatted.csv")

