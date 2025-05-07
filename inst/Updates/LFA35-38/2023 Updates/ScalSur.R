##Scallop Recruitment 

require(bio.lobster)
require(bio.utilities)
require(devtools)
require(rgdal)
require(ggplot2)

require(roxygen2)
require(geosphere)
require(SpatialHub)
require(lubridate)

require(rstanarm)
require(rlang)
require(glue)
require(PBSmapping)
require(sf)


p = bio.lobster::load.environment()
la()
assessment.year = 2024 ##Check Year
p$syr = 1989
p$yrs = p$syr:assessment.year

figdir = file.path("2023 Updates")
p$lfas = c("36") # specify lfa


#scallop survey reworked in 2023
lobster.db('scallop.redo')
sc = scallopSurveyIndex(redo=T,size_range = c(70,82), lfa=36)
sc = sc[[1]]



g2a <- ggplot(data = sc, aes(x = YEAR,y = as.numeric(Index))) +
  geom_point(size=2)+
  geom_line(data=sc,aes(x=YEAR,y=Rmed),colour='red',lwd=1.25)+
  labs(x = "Year", y = "Scallop Survey Recruit Abundance (Mean # per tow)") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))



g2FR <- ggplot(data = sc, aes(x = YEAR,y = as.numeric(Index))) +
  geom_point(size=2)+
  geom_line(data=sc,aes(x=YEAR,y=Rmed),colour='red',lwd=1.25)+
  labs(x = "Année", y = "Abondance des recrutés du relevé des pétoncles") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))



#LFA 38
lobster.db('scallop.redo')
sc = scallopSurveyIndex(redo=T,size_range = c(70,82), lfa=38)
sc = sc[[1]]



g2a <- ggplot(data = sc, aes(x = YEAR,y = as.numeric(Index))) +
  geom_point(size=2)+
  scale_y_continuous(limits=c(0,4))+
  geom_line(data=sc,aes(x=YEAR,y=Rmed),colour='red',lwd=1.25)+
  labs(x = "Year", y = "Scallop Survey Recruit Abundance (Mean # per tow)") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


g2FR <- ggplot(data = sc, aes(x = YEAR,y = as.numeric(Index))) +
  geom_point(size=2)+
  geom_line(data=sc,aes(x=YEAR,y=Rmed),colour='red',lwd=1.25)+
  labs(x = "Année", y = "Abondance des recrutés du relevé des pétoncles") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

