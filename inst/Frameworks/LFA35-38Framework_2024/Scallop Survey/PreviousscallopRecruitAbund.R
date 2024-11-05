##Scallop Recruitment Index 

require(bio.lobster)
require(bio.utilities)
require(Mar.utils)
require(devtools)
require(ggplot2)

require(roxygen2)
require(geosphere)
require(lubridate)

require(rstanarm)
require(rlang)
require(glue)
require(PBSmapping)
require(sf)
require(splancs)
require(raptr)
require(lwgeom)


p=list()
assessment.year = 2024##Check Year
p$syr = 1989
p$yrs = p$syr:assessment.year

lobster.db('scallop.redo')
lobster.db('scallop')

#scallop survey reworked in 2023 

############################################################SHOULD THE LINE CONNECT THROUGH 2020 where no data was collected? ##################

##################  LFA 35  ################## 


sc = scallopSurveyIndex(redo=F,size_range = c(70,82), lfa=35)
sc = sc[[1]]

scalRecruit35<- ggplot(data = sc, aes(x = YEAR,y = as.numeric(Index))) +
  geom_point(size=2, colour = '#003f5c')+
  geom_line(data=sc,aes(x=YEAR,y=Rmed),colour='#f95d6a',lwd=1.25)+
  labs(x = "Year", y = "Scallop Survey Recruit Abundance (Mean # per tow)") +
  theme_test()

scalRecruit35FR <- ggplot(data = sc, aes(x = YEAR,y = as.numeric(Index))) +
  geom_point(size=2, colour = '#003f5c')+
  geom_line(data=sc,aes(x=YEAR,y=Rmed),colour='#f95d6a',lwd=1.25)+
  labs(x = "Année", y = "Abondance des recrutés du relevé des pétoncles") +
  theme_test()

##################  LFA 36  ################## 
sc = scallopSurveyIndex(redo=F,size_range = c(70,82), lfa=36)
sc = sc[[1]]


scalRecruit36 <- ggplot(data = sc, aes(x = YEAR,y = as.numeric(Index))) +
  geom_point(size=2, colour = '#003f5c')+
  geom_line(data=sc,aes(x=YEAR,y=Rmed),colour='#f95d6a',lwd=1.25)+
  labs(x = "Year", y = "Scallop Survey Recruit Abundance (Mean # per tow)") +
  theme_test() 


scalRecruit36FR<- ggplot(data = sc, aes(x = YEAR,y = as.numeric(Index))) +
  geom_point(size=2, colour = '#003f5c')+
  geom_line(data=sc,aes(x=YEAR,y=Rmed),colour='#f95d6a',lwd=1.25)+
  labs(x = "Année", y = "Abondance des recrutés du relevé des pétoncles") +
  theme_test()


#LFA 38
sc = scallopSurveyIndex(redo=F,size_range = c(70,82), lfa=38)
sc = sc[[1]]


scalRecruit38<- ggplot(data = sc, aes(x = YEAR,y = as.numeric(Index))) +
  geom_point(size=2, colour = '#003f5c')+
  scale_y_continuous(limits=c(0,4))+
  geom_line(data=sc,aes(x=YEAR,y=Rmed),colour='#f95d6a',lwd=1.25)+
  labs(x = "Year", y = "Scallop Survey Recruit Abundance (Mean # per tow)") +
  theme_test()

scalRecruit38FR <- ggplot(data = sc, aes(x = YEAR,y = as.numeric(Index))) +
  geom_point(size=2, colour = '#003f5c')+
  geom_line(data=sc,aes(x=YEAR,y=Rmed),colour='#f95d6a',lwd=1.25)+
  labs(x = "Année", y = "Abondance des recrutés du relevé des pétoncles") +
  theme_test() 
