require(bio.lobster)
require(sf)
require(ggplot2)
sf_use_s2(FALSE) #needed for cropping
require(lubridate)

survey = readRDS(file=file.path(project.datadirectory('bio.lobster'),'data','BaseDataForClimateModel.rds'))


g = subset(survey, month(DATE) %in% c(5,6,7))
g = subset(g,YEAR>2015)
g = st_as_sf(g)
ns_coast =readRDS(file.path( project.datadirectory("bio.lobster"), "data","maps","CoastSF.rds"))
st_crs(ns_coast) <- 4326 # 'WGS84'; necessary on some installs
crs_utm20 <- 32620
ns_coast <- suppressWarnings(suppressMessages(
  st_crop(ns_coast,
          c(xmin = -68, ymin = 41, xmax = -57, ymax = 47.5))))
ns_coast = st_transform(ns_coast,crs=crs_utm20)
st_geometry(ns_coast) = st_geometry(ns_coast)/1000
st_crs(ns_coast) = crs_utm20

ggplot(subset(g, Berried==0))+
  geom_sf(pch=3,size=.5)+
  geom_sf(data=subset(g,Berried>0), pch=21,fill = alpha("red", 0.6))+
  facet_wrap(~YEAR)+
  geom_sf(data=ns_coast,fill='wheat')+
  coord_sf(xlim = c(st_bbox(g)$xmin,st_bbox(g)$xmax),
           ylim = c(st_bbox(g)$ymin,st_bbox(g)$ymax),
           expand = TRUE)+
  scale_x_continuous(breaks = c(round(seq(st_bbox(g)$xmin,st_bbox(g)$xmax,length.out=2),2)))+
  scale_y_continuous(breaks = c(round(seq(st_bbox(g)$ymin,st_bbox(g)$ymax,length.out=2),2)))
