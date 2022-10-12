require(bio.lobster)
require(bio.utilities)
require(lubridate)
require(ggplot2)
require(sf)
d = dir(file.path(bio.datadirectory,'bio.lobster','Temperature Data/QuinnBT-2022/'),full.names = T)

x = read.table(d[27],header=T)
x = subset(x,Time==1) 

x = st_as_sf(x,coords = c("Longitude",'Latitude'))
st_crs(x) <- 4326
ns_coast =readRDS(file.path( project.datadirectory("bio.lobster"), "data","maps","CoastSF.rds"))
st_crs(ns_coast) <- 4326 # 'WGS84'; necessary on some installs
crs_utm20 <- 32620
ns_coast <- suppressWarnings(suppressMessages(
  st_crop(ns_coast,
          c(xmin = -68, ymin = 41, xmax = -56.5, ymax = 47.5))))
ns_coast <- st_transform(ns_coast, crs_utm20)

# Project our survey data coordinates:
xs <- x %>%   
  st_transform(crs_utm20) 

# Plot our coast and survey data:
ggplot(data=ns_coast) +
  geom_sf()+
  geom_sf(data=xs, aes(fill=Depth_m, color=Depth_m) )+
  scale_fill_viridis_c(trans='log') +
  scale_color_viridis_c(trans='log') 
  
  
x_utm_coords <- st_coordinates(xs)

x$X1000 <- x_utm_coords[,1] / 1000
x$Y1000 <- x_utm_coords[,2] / 1000

spde <- make_mesh(as_tibble(x), xy_cols = c("X1000", "Y1000"),
                  n_knots = 400, type = "kmeans")
plot(spde)

# Add on the barrier mesh component:
bspde <- add_barrier_mesh(
  spde, ns_coast, range_fraction = 0.1,
  proj_scaling = 1000, plot = TRUE
)


mesh_df_water <- bspde$mesh_sf[bspde$normal_triangles, ]
mesh_df_land <- bspde$mesh_sf[bspde$barrier_triangles, ]
ggplot(ns_coast) +
  geom_sf() +
  geom_sf(data = mesh_df_water, size = 1, colour = "blue") +
  geom_sf(data = mesh_df_land, size = 1, colour = "green")

fit = sdmTMB(Depth_m~Time,
              data=as_tibble(x),
             mesh=bspde,
             family=tweedie(link='log'),
             spatial='on'
             )

go =predict(fit) 
go$pred = fit$family$linkinv(go$est)




  ggplot(subset(x, Time %in% seq(1,24,by=3))) +
  geom_sf(aes(fill=BottomTemp,color=BottomTemp)) + 
  scale_fill_viridis_c(trans='sqrt') +
  scale_color_viridis_c(trans='sqrt') +
    facet_wrap(~DateYYYYMMDD) +
    theme( axis.ticks.x = element_blank(),
           axis.text.x = element_blank(),
           axis.title.x = element_blank(),
           axis.ticks.y = element_blank(),
           axis.text.y = element_blank(),
           axis.title.y = element_blank()
    ) +
    coord_sf()
  

  x1 = read.table(d[27],header=T)
  x0 = read.table(d[7],header=T)
  
  x1$Date = as.Date(as.character(x1$DateYYYYMMDD), format='%Y%m%d')
  x0$Date = as.Date(as.character(x0$DateYYYYMMDD), format='%Y%m%d')
  
  x0$DOY = yday(x0$Date)
  x1$DOY = yday(x1$Date)
  
  x1 = rename.df(x1,'BottomTemp','BottomTemp2035')
  
  x2 = merge(x1[,c('Longitude','Latitude','DOY','BottomTemp2035')],x0[,c('Longitude','Latitude','DOY','BottomTemp')])
  x2$Diff = x2$BottomTemp2035-x2$BottomTemp
  
  x2 = st_as_sf(x2,coords = c("Longitude",'Latitude'))
  st_crs(x2) <- 4326
  
  u = unique(x2$DOY)
  u = u[order(u)]
  ggplot(subset(x2, DOY %in% u[seq(1,24,by=3)])) +
    geom_sf(aes(fill=Diff,color=Diff)) + 
    scale_fill_viridis_c() +
    scale_color_viridis_c() +
    facet_wrap(~DOY) +
    theme( axis.ticks.x = element_blank(),
           axis.text.x = element_blank(),
           axis.title.x = element_blank(),
           axis.ticks.y = element_blank(),
           axis.text.y = element_blank(),
           axis.title.y = element_blank()
    ) +
    coord_sf()
  
#above 20
  x2$a20f = ifelse(x2$BottomTemp2035>=20,1,0)
  x2$a20n = ifelse(x2$BottomTemp>=20,1,0)
  
  x3 = st_as_sf(as.data.frame(st_coordinates(x2)) %>% distinct(X,Y), coords=1:2, crs=st_crs(x2)) 
  x3$ID = 1:nrow(x3)
  x4 = st_join(x2,x3)
  x5=aggregate(cbind(a20f,a20n)~ID,data=x4,FUN=sum)
  
xx5 =  merge(x5,x3)
xx5$a20fP = xx5$a20f/27
xx5$a20nP = xx5$a20n/27
xx5$pInc = xx5$a20fP-xx5$a20nP

ggplot(st_as_sf(xx5)) + 
  geom_sf(aes(fill=pInc, color=pInc))+
  scale_fill_viridis_c() +
  scale_color_viridis_c() 
  



  