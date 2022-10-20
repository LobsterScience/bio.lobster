
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
aT = readRDS(file=file.path(project.datadirectory('bio.lobster'),'data','BaseDataForClimateModel.rds'))
sf_use_s2(FALSE) #needed for cropping

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

# Project our survey data coordinates:
survey <- aT %>%   
  st_as_sf()
    
surv_utm_coords <- st_coordinates(survey)

survey$X1000 <- surv_utm_coords[,1] / 1000
survey$Y1000 <- surv_utm_coords[,2] / 1000

spde <- make_mesh(as_tibble(survey), xy_cols = c("X1000", "Y1000"),
                   n_knots=600,type = "cutoff_search")
plot(spde)

# Add on the barrier mesh component:
bspde <- add_barrier_mesh(
  spde, ns_coast, range_fraction = 0.1,
  proj_scaling = 1000, plot = TRUE
)


survey$lZ = log(survey$z)
survey$W = round(survey$DYEAR*365 / 14) # 2 week intervals

#what is the right offset for gear
#km2 for tows is estimated from sensors
#km2 for traps from Watson et al 2009 NJZ MFR 43 1 -- home radius of 17m, bait radius of 11m == 28m 'attraction zone'
# pi*(.014^2) # assuming traps are independent

i = which(survey$OFFSET_METRIC == 'Number of traps')
survey$OFFSET[i] = survey$OFFSET[i] * pi*(.014^2)
survey$LO = log(survey$OFFSET)
fit = sdmTMB(WEIGHT_KG~
               s(lZ,k=5)+s(BT),
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



d = dir(file.path(bio.datadirectory,'bio.lobster','Temperature Data/QuinnBT-2022/'),full.names = T)
d = d[grep('Can',d)]
x = read.table(d[7],header=T)
x$Date = as.Date(as.character(x$DateYYYYMMDD), format='%Y%m%d')
x = subset(x,Depth_m<400,select=c(Date,Time,Longitude,Latitude,BottomTemp,Depth_m))
 
x = subset(x,Time==1) 
x = subset(x,Depth_m<400)
x = x %>% st_as_sf(coords = c("Longitude",'Latitude'),crs=4326) %>% st_transform(crs_utm20)

x_utm_coords <- st_coordinates(x)

x$X1000 <- x_utm_coords[,1] / 1000
x$Y1000 <- x_utm_coords[,2] / 1000

x = bio.utilities::rename.df(x,c('BottomTemp','Depth_m'),c('BT','z'))
x$lZ = log(x$z)

W = seq(1,26,by=2)
x = as_tibble(subset(x,select=c(BT,X1000,Y1000,lZ)))
x$geometry=NULL
be = as.data.frame(sapply(x,rep.int,27))
be$W = rep(0:26,each=dim(x)[1])

be= as_tibble(be)

g = predict(fit,newdata=(be))

  g$pred = fit$family$linkinv(g$est)

  gsf = st_as_sf(g,coords = c("X1000","Y1000"),crs=32620,remove=F)



#Maps
mm = c(0.001,max(gsf$pred))
ggplot(subset(gsf,W %in% 0:26)) +
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
savePlot('wtWeek600k.png') 
saveRDS(list(data=survey,grid=bspde,preds=be,model=fit),file='AllwtTw600k.rds')

