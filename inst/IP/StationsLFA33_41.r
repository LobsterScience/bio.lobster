require(ggplot2)
require(terra)
require(tidyterra)
require(sf)
require(bio.lobster)
require(bio.utilities)
require(geosphere)
v = readRDS('C:\\Users\\Cooka\\Documents\\bio_data\\bio.lobster\\data\\OFI_lobster_covariates/ofi_pointData500m.rds')
vs = st_as_sf(v,crs=32620)
vs$z = vs$depth *-1

l = readRDS(file.path(git.repo,'bio.lobster.data/mapping_data/LFAPolysSF.rds'))
lp = st_transform(l,crs=32620)
pip <- st_join(vs, lp)
pip = subset(pip,!is.na(LFA))
pic = st_coordinates(pip)
dp = distm(pic)
ggplot(subset(pip,LFA %in% c(33,34,40,41)&z<300),aes(fill=(z),colour=(z)))+geom_sf()
ggplot(subset(pip,LFA %in% c(33,34,40)&z<300),aes(fill=slope,colour=slope))+geom_sf()
ggplot(subset(pip,LFA %in% c(33,34,40,41)&3<500),aes(fill=rie,colour=rie))+geom_sf() #The local standard deviation of the residual topography surface, where the residual topography surface is calculated as the DTM minus the focal mean of the DTM

#scallop sets
a = lobster.db('scallop')
lfa = 38            ###### PICK THE LFA  

scallop.tows=a[[1]]
scallopSurv = a[[2]]
scallop.tows$Y = convert.dd.dddd(scallop.tows$START_LAT)
scallop.tows$X = convert.dd.dddd(scallop.tows$START_LONG)
scT = subset(scallop.tows,select=c('TOW_SEQ','TOW_DATE','STRATA_ID','X','Y'))
totS = st_as_sf(scT,coords = c('X','Y'),crs=st_crs(4326))
sc = st_transform(totS,crs=32620)

ss = st_join(sc,lp)
ss = subset(ss,!is.na(LFA))

ggplot()+
  geom_sf(data=subset(pip,LFA==38),aes(fill=slope,colour=slope)) +
  scale_fill_gradient(low = "white", high = "red")+
  scale_colour_gradient(low = "white", high = "red")+
  geom_sf(data=subset(ss,LFA==38),size=.4)+
  theme_test_adam()


###rv

x = RV_sets()
x = subset(x,select=c(mission,setno,LONGITUDE,LATITUDE,DATE))
xs = st_as_sf(x,coords=c('LONGITUDE','LATITUDE'),crs=4326)
xs = st_transform(xs,crs = 32620)
xs = st_join(xs,lp)
xs = subset(xs,!is.na(LFA))
xs$yr = lubridate::year(xs$DATE)
xs$mn = lubridate::month(xs$DATE)

y = ILTS_ITQ_All_Data(redo_base_data = F,size=c(82,300),aggregate=T,species=2550,biomass = T)
y = subset(y,select=c(TRIP_ID,SET_NO, SET_LONG,SET_LAT,SET_DATE))    
ys = st_as_sf(y,coords=c('SET_LONG','SET_LAT'),crs=4326)
ys = st_transform(ys,crs = 32620)
ys = st_join(ys,lp)
ys = subset(ys,!is.na(LFA))
ys$yr = lubridate::year(ys$SET_DATE)


ggplot()+
  #geom_sf(data=subset(pip,LFA%in% c(33,34,40,41)),aes(fill=slope,colour=slope)) +
  #scale_fill_gradient(low = "white", high = "red")+
  #scale_colour_gradient(low = "white", high = "red")+
  geom_sf(data=subset(ss,LFA%in% c(33,34,40,41)),size=.01)+
  geom_sf(data=subset(xs,LFA%in% c(33,34,40,41) & yr==2024 & mn %in% c(7,8)),size=.8,colour='blue')+
  geom_sf(data=subset(ys,LFA%in% c(33,34,40,41) & yr==2024),size=.8,colour='green')+
  geom_sf(data=subset(lp,LFA %in% c(33,34,40,41)),fill=NA)+
  theme_test_adam()


###getting xx from git/FrameworkLFA35_38/inst/Scallop/3.scallopRecruitAbunance within 38 and all sizes of lobster
xxs = st_transform(xx,crs=32620)


ggplot()+
  geom_sf(data=subset(pip,LFA==38),aes(fill=slope,colour=slope)) +
  scale_fill_gradient(low = "white", high = "red")+
  scale_colour_gradient(low = "white", high = "red")+
  geom_sf(data=subset(xxs,LFA==38),size=.01)+
  geom_sf(data=subset(xxs,LFA==38 & ABUNDANCE_STD>0),size=.01,col='purple')+
  
  geom_sf(data=subset(xs,LFA==38& yr>2018),size=.8,colour='blue')+
  geom_sf(data=subset(ys,LFA==38 & yr>2018),size=.8,colour='green')+
  theme_test_adam()

