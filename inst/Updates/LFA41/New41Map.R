require(bio.lobster)
require(bio.utilities)
require(PBSmapping)
require(dplyr)
library("ggplot2")
library("sf")
p = bio.lobster::load.environment()
la()

#Map LFA 41 with boundaries and NAFO

ggLobsterMap('41_full',addNAFO = T, nafo=c('5Y','4X','5Z'), addLFALabels = T, addGrids = F)
