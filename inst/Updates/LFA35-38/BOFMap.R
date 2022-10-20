setwd("C:/Users/HowseVJ/Documents/GitHub/bio.lobster/inst/Updates/LFA35-38")

require(rgdal)
require(devtools)
require(geosphere)
require(SpatialHub)
require(bio.utilities)
require(bio.lobster)
require(PBSmapping)
require(bio.survey)


p = bio.lobster::load.environment()
la()
assessment.year = 2022 ##Check Year
p$syr = 1989
p$yrs = p$syr:assessment.year


x11(width=5, height=5)
LobsterMap(ylim=c(43.3,46),	xlim=c(-67.8,-63.2))
