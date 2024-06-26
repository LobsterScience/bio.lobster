# getting prediction surfaces from temp
require(bio.lobster)
require(bio.utilities)
require(lubridate)
require(ggplot2)
require(sf)
require(forecast)
d = dir(file.path(bio.datadirectory,'bio.lobster','Temperature Data/QuinnBT-2022/'),full.names = T)
fd=file.path(project.datadirectory('bio.lobster'),'analysis','ClimateModelling')
dir.create(fd,showWarnings=F)
setwd(fd)

##Can
dd = d[grep('Can',d)]
crs_utm20 <- 32620
ou = list()

for(i in 1:length(dd)){
  x1 = read.table(dd[i],header=T)
  x1$Date = as.Date(as.character(x1$DateYYYYMMDD), format='%Y%m%d')
  ou[[i]] = st_as_sf(x1,coords = c("Longitude",'Latitude'),crs=4326) %>% 
       st_transform(crs_utm20)
 }

cb = do.call(rbind,ou)
cb = st_as_sf(cb,crs=32620)
cb$X1000 = st_coordinates(cb)[,1]/1000
cb$Y1000 = st_coordinates(cb)[,2]/1000
cb$YR = year(cb$Date)
cb$rYR = cb$YR
cb$rYR = ifelse(cb$rYR %in% 2030:2035, 2032,cb$rYR)
cb$rYR = ifelse(cb$rYR %in% 2050:2055, 2052,cb$rYR)
cb$rYR = ifelse(cb$rYR %in% 2095:2099, 2097,cb$rYR)
cb = subset(cb,rYR>2029 &Depth_m<400,select=c(rYR,Time,Date,X1000,Y1000,Depth_m,BottomTemp))
cb$lZ = log(cb$Depth_m)
cb$W = cb$Time
st_geometry(cb) <- NULL
cb = as_tibble(cb)
cbr = aggregate(BottomTemp~X1000+Y1000+rYR+lZ+W,data=cb,FUN=mean)
saveRDS(cbr,'CanProjectionSurfaces.rds')

g=readRDS('CanProjectionSurfaces.rds')
g = st_as_sf(g,coords = c('X1000','Y1000'),crs=32620)
st_crs(g) <- 32620


rL = readRDS(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolysSF.rds"))
rL = st_as_sf(rL)
st_crs(rL) <- 4326
rL = st_transform(rL,32620) 
st_geometry(rL) <- st_geometry(st_as_sf(rL$geometry/1000)) 
st_crs(rL) <- 32620

rL1 = subset(rL,LFA<41)



ggplot(subset(g,W==20 &rYR ==2032)) +
  geom_sf(aes(fill=BottomTemp,color=BottomTemp),size=2) + 
  scale_fill_viridis_c() +
  scale_color_viridis_c() +
   geom_sf(data=rL1,linewidth=1.32,colour='black',fill=NA)+
  theme( axis.ticks.x = element_blank(),
         axis.text.x = element_blank(),
         axis.title.x = element_blank(),
         axis.ticks.y = element_blank(),
         axis.text.y = element_blank(),
         axis.title.y = element_blank()
  ) +
  coord_sf()+
  theme_void()


