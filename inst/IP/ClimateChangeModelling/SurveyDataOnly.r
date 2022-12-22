
require(sdmTMB)
require(bio.lobster)
require(bio.utilities)
require(lubridate)
require(devtools)
require(dplyr)
require(ggplot2)
require(INLA)
options(stringAsFactors=F)
require(PBSmapping)
require(SpatialHub)
require(sf)
la()
fd=file.path(project.datadirectory('bio.lobster'),'analysis','ClimateModelling')
dir.create(fd,showWarnings=F)
setwd(fd)
survey = readRDS(file=file.path(project.datadirectory('bio.lobster'),'data','BaseDataForClimateModel.rds'))
sf_use_s2(FALSE) #needed for cropping

# Project our survey data coordinates:

survey$lZ = log(survey$z)
survey = subset(survey,!is.na(lZ))

survey = subset(survey, SOURCE %in% c("NEFSC_RV"  ,"Snow crab survey"  ,"DFO_RV" ,"ILTS_ITQ" ))

survey$LO = log(survey$OFFSET)
survey = subset(survey,OFFSET>0.00001 & OFFSET< 0.12)
survey$BT = survey$GlT
survey = subset(survey,!is.na(BT))
survey$pa = ifelse(survey$Berried>0,1,0)
ns_coast =readRDS(file.path( project.datadirectory("bio.lobster"), "data","maps","CoastSF.rds"))
st_crs(ns_coast) <- 4326 # 'WGS84'; necessary on some installs
crs_utm20 <- 32620
ns_coast <- suppressWarnings(suppressMessages(
  st_crop(ns_coast,
          c(xmin = -68, ymin = 41, xmax = -56.5, ymax = 47.5))))

ns_coast <- st_transform(ns_coast, crs_utm20)

st_crs(ns_coast) <- 4326 # 'WGS84'; necessary on some installs
crs_utm20 <- 32620

survey <- survey %>%   
  st_as_sf()

survey = subset(survey,YEAR>2017)
surv_utm_coords <- st_coordinates(survey)

survey$X1000 <- surv_utm_coords[,1] 
survey$Y1000 <- surv_utm_coords[,2] 

spde <- make_mesh(as_tibble(survey), xy_cols = c("X1000", "Y1000"),
                   n_knots=400,type = "cutoff_search")
plot(spde)

# Add on the barrier mesh component:
bspde <- add_barrier_mesh(
  spde, ns_coast, range_fraction = 0.1,
  proj_scaling = 1000, plot = TRUE
)


#issues with fitting on a biweekly moving to quarters
survey$m = month(survey$DATE) 
survey$Q = ifelse(survey$m %in% c(10,11,12),1,ifelse(survey$m %in% c(1,2,3),2,ifelse(survey$m %in% c(4,5,6),3,4)))
survey$Time = survey$YEAR+survey$Q/4


fitsur = sdmTMB(WEIGHT_KG~
               s(lZ,k=3)+s(BT,k=4)+Q,
             data=as_tibble(survey),
            offset = 'LO',
             mesh=bspde,
             time='YEAR',
                          family=tweedie(link='log'),
             spatial='on',
             spatiotemporal='ar1')

saveRDS(fitsur,'sdmTMBsurveyonly2018-2022byQ.rds')


Glsur = readRDS('GlorysPredictSurface.rds')
x = Glsur


plot_smooth(fitsur,select=2)


x = bio.utilities::rename.df(x,c('bottomT','yr'),c('BT','YEAR'))
x = subset(x,z>0)
x$lZ = log(x$z)
x$X1000 = st_coordinates(x)[,1]
x$Y1000 = st_coordinates(x)[,2]
x = subset(x,exp(lZ)<400 & x$YEAR>2017)

x = as_tibble(subset(x,select=c(Q,YEAR,BT,X1000,Y1000,lZ)))
x$geometry=NULL

g = predict(fitsur,newdata=x)

  g$pred = fitsur$family$linkinv(g$est)

  gsf = st_as_sf(g,coords = c("X1000","Y1000"),crs=32620,remove=F)


rL = readRDS(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolysSF.rds"))
rL = st_as_sf(rL)
st_crs(rL) <- 4326
rL = st_transform(rL,32620) 
st_geometry(rL) <- st_geometry(st_as_sf(rL$geometry/1000)) 
st_crs(rL) <- 32620


ff = st_join(gsf,rL,join=st_within)
gsf = subset(ff,!is.na(LFA))

#Maps
mm = c(0.001,max(gsf$pred))
ggplot(subset(gsf,Q==3 &YEAR %in% 2019)) +
  geom_sf(aes(fill=pred,color=pred,size=1.3)) +
  facet_wrap(~YEAR)+ 
  scale_fill_viridis_c(trans=scales::boxcox_trans(0.3)) +
  scale_color_viridis_c(trans=scales::boxcox_trans(0.3)) +
 #  geom_sf(data=rL,size=1,colour='black',fill=NA)+
  theme( axis.ticks.x = element_blank(),
         axis.text.x = element_blank(),
         axis.title.x = element_blank(),
         axis.ticks.y = element_blank(),
         axis.text.y = element_blank(),
         axis.title.y = element_blank()
  ) +
  coord_sf()xlim=c(0,800),ylim=c(4300,5302))
savePlot('surKGQ32019.png') 

xx = st_as_sf(survey)

ggplot()+geom_sf(data=xx)

saveRDS(list(data=survey,grid=bspde,model=fitpa),file='PABerried600kDec22022.rds')





