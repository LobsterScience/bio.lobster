
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

p = bio.lobster::load.environment()
p = spatial_parameters(type='canada.east')

a = compileAbundPresAbs(redo=F,size=F)
a = subset(a, YEAR>2016)
attr(a,'projection') = "LL"
aA = lonlat2planar(a,input_names=c('LONGITUDE','LATITUDE'),proj.type =  "lambert.conic.canada.east")

ba = lobster.db('bathymetry')
locsmap = match( 
  array_map( "xy->1", aA[,c("plon","plat")], gridparams=p$gridparams ), 
  array_map( "xy->1", ba[,c("plon","plat")], gridparams=p$gridparams ) )

baXY = planar2lonlat(ba,proj.type=p$internal.projection)


aA$Depth = ba$z[locsmap]
i = which(aA$Depth<0)
aA = aA[-i,] 
aT = as_tibble(aA)
sf_use_s2(FALSE) #needed for cropping

rL = readRDS(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolysSF.rds"))
ns_coast =readRDS(file.path( project.datadirectory("bio.lobster"), "data","maps","CoastSF.rds"))
st_crs(ns_coast) <- 4326 # 'WGS84'; necessary on some installs
st_crs(rL) <- 4326
crs_utm20 <- 32620
ns_coast <- suppressWarnings(suppressMessages(
  st_crop(ns_coast,
          c(xmin = -68, ymin = 41, xmax = -56.5, ymax = 47.5))))

ns_coast <- st_transform(ns_coast, crs_utm20)


st_crs(ns_coast) <- 4326 # 'WGS84'; necessary on some installs
st_crs(rL) <- 4326
crs_utm20 <- 32620
rL = rL[-which(!(st_is_valid(rL))),]

rL <- st_transform(rL, crs_utm20)

baX = st_as_sf(as_tibble(baXY), coords=c('lon','lat'),crs=4326)
baX = st_transform(baX, crs_utm20)

baXr = st_filter(baX, rL)
baXrr = subset(baXr,z<400 )

# Project our survey data coordinates:
survey <- aT %>%   
  st_as_sf(crs = 4326, coords = c("LONGITUDE", "LATITUDE")) %>% 
  st_crop( c(xmin = -68, ymin = 41, xmax = -56.5, ymax = 47.5))%>% 
  st_transform(crs_utm20) 

# Plot our coast and survey data:
    ggplot(data=rL) +
      geom_sf(size=1) +
      geom_sf(data = survey, size = 0.5)
    
surv_utm_coords <- st_coordinates(survey)

survey$X1000 <- surv_utm_coords[,1] / 1000
survey$Y1000 <- surv_utm_coords[,2] / 1000

spde <- make_mesh(as_tibble(survey), xy_cols = c("X1000", "Y1000"),
                  n_knots = 400, type = "kmeans")
plot(spde)

# Add on the barrier mesh component:
bspde <- add_barrier_mesh(
  spde, ns_coast, range_fraction = 0.1,
  proj_scaling = 1000, plot = TRUE
)


mesh_df_water <- bspde$mesh_sf[bspde$normal_triangles, ]
mesh_df_land <- bspde$mesh_sf[bspde$barrier_triangles, ]
ggplot(rL) +
  geom_sf() +
  geom_sf(data = mesh_df_water, size = 1, colour = "blue") +
  geom_sf(data = mesh_df_land, size = 1, colour = "green")

survey$lZ = log(survey$Depth)
survey$W = round(survey$DYEAR*365 / 14) # 2 week intervals

#what is the right offset for gear
#km2 for tows is estimated from sensors
#km2 for traps from Watson et al 2009 NJZ MFR 43 1 -- home radius of 17m, bait radius of 11m == 28m 'attraction zone'
# pi*(.014^2) # assuming traps are independent

i = which(survey$OFFSET_METRIC == 'Number of traps')
survey$OFFSET[i] = survey$OFFSET[i] * pi*(.014^2)
survey$LO = log(survey$OFFSET)
fit = sdmTMB(WEIGHT_KG~
               s(lZ,k=5),
             data=as_tibble(survey),
             offset = 'LO',
             time='W', 
             mesh=bspde,
             extra_time = 17,
             family=tweedie(link='log'),
             spatial='on',
             spatiotemporal='ar1')

go =predict(fit) 
go$pred = fit$family$linkinv(go$est)



rL = readRDS(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolysSF.rds"))
st_crs(rL) <- 4326
crs_utm20 <- 32620
#rL = rL[-which(!(st_is_valid(rL))),]
rL <- suppressWarnings(suppressMessages(
  st_crop(rL,
          c(xmin = -68, ymin = 41, xmax = -56.5, ymax = 47.5))))
rL <- st_transform(rL, crs_utm20)
rL = st_union(rL)

baT <- baXY %>% st_as_sf(crs = 4326, coords = c("lon", "lat")) %>%
  st_crop(c(xmin = -68, ymin = 42, xmax = -57.5, ymax = 47)) %>%		
  st_transform(crs_utm20)				

baT = subset(baT,z>0 & z<400)
  #x
b = st_coordinates(baT)
baT$X1000 = b[,1]/1000
baT$Y1000 = b[,2]/1000
baT$X = b[,1]
baT$Y = b[,2]
baT$Depth = baT$z

ba = baT[,c('X','Y','Depth','X1000','Y1000')]
ba$geometry <- NULL
be = as.data.frame(sapply(ba,rep.int,27))
be$W = rep(0:26,each=dim(ba)[1])
be$geometry = NULL
be$lZ = log(be$Depth)
g = predict(fit,newdata=be)

g1 = fit$family$linkinv(g)

be$pred = apply(g1,1,median)
be$sd = apply(g1,1,sd)
be$lQ = apply(g1,1,quantile,0.25)
be$uQ = apply(g1,1,quantile,0.75)



gsf = st_as_sf(be,coords = c("X","Y"),crs=32620,remove=F)



#Maps
png('Figures/ModelOutput/lobstersdmTMBwk1-12.png', width = 10, height = 12,units='in',pointsize=12, res=300,type='cairo')
mm = c(0.001,max(gsf$pred))
ggplot(subset(gsf,W %in% 1)) +
  geom_sf(aes(fill=pred,color=pred)) + 
  scale_fill_viridis_c(trans='log',limits=mm) +
  scale_color_viridis_c(trans='log',limits=mm) +
  facet_wrap(~W) +
  geom_sf(data=rL,size=1,colour='black',fill=NA)+
  theme( axis.ticks.x = element_blank(),
         axis.text.x = element_blank(),
         axis.title.x = element_blank(),
         axis.ticks.y = element_blank(),
         axis.text.y = element_blank(),
         axis.title.y = element_blank()
  ) +
  coord_sf()
dev.off()



saveRDS(list(data=aT,grid=bspde,preds=be),file='results/dataForLFA33-35.rds')

