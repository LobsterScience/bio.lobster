require(bio.lobster)
la()

ns_coast =readRDS(file.path( project.datadirectory("bio.lobster"), "data","maps","CoastSF.rds"))
st_crs(ns_coast) <- 4326 # 'WGS84'; necessary on some installs
crs_utm20 <- 32620
ns_coast <- suppressWarnings(suppressMessages(
  st_crop(ns_coast,
          c(xmin = -68, ymin = 41, xmax = -64, ymax = 47.5))))


v = scallopSurveyIndex(return_sets_object = T,lfa=35,redo=T) #lfa does not matter here
v = subset(v,YEAR>2018)

ggplot(subset(v, ABUNDANCE_STD_PRU>0))+
  geom_sf(pch=21, aes(size = ABUNDANCE_STD_PRU),fill = alpha("red", 0.6))+
  geom_sf(data=subset(v,ABUNDANCE_STD_PRU==0), pch=3,size=.5)+
  facet_wrap(~YEAR)+
  geom_sf(data=ns_coast,fill='wheat')+
  coord_sf(xlim = c(st_bbox(v)$xmin,st_bbox(v)$xmax),
           ylim = c(st_bbox(v)$ymin,st_bbox(v)$ymax),
           expand = TRUE)+
  scale_x_continuous(breaks = c(round(seq(st_bbox(v)$xmin,st_bbox(v)$xmax,length.out=2),2)))+
  scale_y_continuous(breaks = c(round(seq(st_bbox(v)$ymin,st_bbox(v)$ymax,length.out=2),2)))
