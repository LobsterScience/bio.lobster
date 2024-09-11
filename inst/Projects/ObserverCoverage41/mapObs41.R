###Mapping LFA 41

bio.datadirectory <- "C:/Users/HowseVJ/Documents/bio.data"
git.repo <- "c:/Users/HowseVJ/Documents/GitHub"

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
p$syr = 1989
p$yrs = p$syr:assessment.year
p$lfas = c("41") # specify lfa


figdir = file.path("C:/Users/HowseVJ/Documents/Scripts")

lobster.db( DS="observer41.redo")
lobster.db( DS="observer41")
#lobster.db( DS = 'logs41.redo', p=p) 
lobster.db( DS = 'logs41', p=p)

Offarea41<-read.csv("C:/Users/HowseVJ/Documents/bio.data/bio.lobster/data/maps/LFA41Offareas.csv")


##Add SYEAR to 41 data
L41log<-logs41
L41log$dates<-as.POSIXct(L41log$FV_FISHED_DATETIME, format='%m/%d/%Y %H:%M:%S')
L41log$dates = as.Date(L41log$dates,format='%m-%d-%Y')
L41log$MONTH = month(L41log$dates)
L41log$SYEAR= year(L41log$dates)
L41log$DAY=day(L41log$dates)

L41log$DDLON<-L41log$DDLON*-1


##Make Label position df-- need to shift them
Offarealab<-Offarea41[!duplicated(Offarea41$OFFAREA),]
names(Offarealab)[5]<- "label"
Offarealab$X<-c(-67.42, -66.1, -66.4, -64.83, -66.85)
Offarealab$Y<-c(43.0, 41.5,42.2,42.35, 42.6)
Offarealab$Area<-c("CROWELL","GBANK","GBASIN","SE.Browns", "SW.Browns")


Obs41<-observer41


##Need SYear -- pull it out of date
Obs41$dates<-as.POSIXct(Obs41$STARTDATE, format='%Y-%m-%d %H:%M:%S')
Obs41$dates = as.Date(Obs41$dates,format='%m-%d-%Y')
Obs41$MONTH = month(Obs41$dates)
Obs41$SYEAR= year(Obs41$dates)
Obs41$DAY=day(Obs41$dates)

###Clip Observer Coverage for maps

Obs41<-Obs41[Obs41$SYEAR > 2018  & Obs41$SYEAR < 2024, ]


#Identify what dates were observed
sort(unique(Obs41$dates))
# "2019-05-10" "2019-06-27" "2019-07-29" "2019-11-22" "2020-03-23" "2020-08-08" "2020-11-23" "2021-03-19" "2022-03-22" "2022-12-03"
# "2023-03-16"



###################################### 2021 ###################################
off21<-L41log[L41log$MONTH == 3 & L41log$SYEAR == 2021,]
off21<-subset(off21, select=c(DDLAT, DDLON, NUM_OF_TRAPS,EST_WEIGHT_LOG_LBS, ADJCATCH, OFFAREA, MONTH,SYEAR,DAY))


Ob21<-Obs41[Obs41$MONTH == 3 & Obs41$SYEAR ==2021,]
Ob21<-subset(Ob21, select=c(LATITUDE,LONGITUDE,PORTNAME, MONTH,SYEAR,DAY))

LobsterMap(ylim=c(40.7,44),xlim=c(-68,-63.5),boundaries='LFAs', mapRes="UR", addGrids = F)
addPolys(Offarea41)
addLabels(Offarealab, cex = 0.75)
with(off21,points(x=DDLON,y=DDLAT,pch = 19, cex=0.75, col='black'))
with(Ob21, points(x=LONGITUDE,y=LATITUDE,pch=8,cex=0.75,col='red'))
title(main="Mar 2021")


###################################### 2022 ###################################
off22<-L41log[L41log$MONTH == 12 & L41log$SYEAR == 2022,]
off22<-subset(off22, select=c(DDLAT, DDLON, NUM_OF_TRAPS,EST_WEIGHT_LOG_LBS, ADJCATCH, OFFAREA, MONTH,SYEAR,DAY))


Ob22<-Obs41[Obs41$MONTH == 12 & Obs41$SYEAR ==2022,]
Ob22<-subset(Ob22, select=c(LATITUDE,LONGITUDE,PORTNAME, MONTH,SYEAR,DAY))

LobsterMap(ylim=c(40.7,44),xlim=c(-68,-63.5),boundaries='LFAs', mapRes="UR", addGrids = F)
addPolys(Offarea41)
addLabels(Offarealab, cex = 0.75)
with(off22,points(x=DDLON,y=DDLAT,pch = 19, cex=0.75, col='black'))
with(Ob22, points(x=LONGITUDE,y=LATITUDE,pch=8,cex=0.75,col='red'))
title(main="Dec 2022")



###################################### 2023###################################
off23<-L41log[L41log$MONTH == 6 & L41log$SYEAR == 2023,]
off23<-subset(off23, select=c(DDLAT, DDLON, NUM_OF_TRAPS,EST_WEIGHT_LOG_LBS, ADJCATCH, OFFAREA, MONTH,SYEAR,DAY))


Ob23<-Obs41[Obs41$MONTH == 6 & Obs41$SYEAR ==2023,]
Ob23<-subset(Ob23, select=c(LATITUDE,LONGITUDE,PORTNAME, MONTH,SYEAR,DAY))

LobsterMap(ylim=c(40.7,44),xlim=c(-68,-63.5),boundaries='LFAs', mapRes="UR", addGrids = F)
addPolys(Offarea41)
addLabels(Offarealab, cex = 0.75)
with(off23,points(x=DDLON,y=DDLAT,pch = 19, cex=0.75, col='black'))
with(Ob23, points(x=LONGITUDE,y=LATITUDE,pch=8,cex=0.75,col='red'))
title(main="June 2023")
