### Check Bycatch Uploads


###
require(bio.utilities)
require(bio.lobster)
require(dplyr)
#at sea
##lobster.db('atSea.redo')
lobster.db('atSea')
a = subset(atSea,LFA %in% c(33,34,35) & DESCRIPTION %in% c('SWLSS') 
           & !is.na(SPECIESCODE))
a$YR=year(a$STARTDATE)


##Check Trips uploaded
tripcheck <- a %>%
filter(YR >2020) %>%
  distinct(TRIPNO)

tripcheck<-as.data.frame(tripcheck)


##Check trips in data base against list of trips to be uploaded

##Double check the trips and dates in SQL