require(ggplot2)
require(terra)
require(tidyterra)
require(sf)
require(bio.lobster)

v = readRDS('C:\\Users\\Cooka\\Documents\\bio_data\\bio.lobster\\data\\OFI_lobster_covariates/ofi_pointData500m.rds')
vs = st_as_sf(v,crs=32620)
vs$z = vs$depth *-1

l = readRDS(file.path(git.repo,'bio.lobster.data/mapping_data/LFAPolysSF.rds'))
lp = st_transform(l,crs=32620)
pip <- st_join(vs, lp)
pip = subset(pip,!is.na(LFA))

ggplot(subset(pip,LFA==38),aes(fill=log(z),colour=log(z)))+geom_sf()
ggplot(subset(pip,LFA==38),aes(fill=slope,colour=slope))+geom_sf()
ggplot(subset(pip,LFA==38),aes(fill=rie,colour=rie))+geom_sf() #The local standard deviation of the residual topography surface, where the residual topography surface is calculated as the DTM minus the focal mean of the DTM

#scallop sets
a = lobster.db('scallop')
lfa = 38            ###### PICK THE LFA  

scallop.tows=a[[1]]
scallopSurv = a[[2]]
scallop.tows$Y = convert.dd.dddd(scallop.tows$START_LAT)
scallop.tows$X = convert.dd.dddd(scallop.tows$START_LONG)
scT = subset(scallop.tows,select=c('TOW_SEQ','TOW_DATE','STRATA_ID','X','Y'))
totS = st_as_sf(scT,coords = c('X','Y'),crs=st_crs(4326))


ss = st_join(totS,v, join=st_within)
xx = subset(ss,LFA==lfa)

