require(bio.lobster)
require(bio.utilities)
require(lubridate)
require(ggplot2)
require(sf)
d = dir(file.path(bio.datadirectory,'bio.lobster','Temperature Data/QuinnBT-2022/'),full.names = T)

x = read.table(d[27],header=T)

x = st_as_sf(x,coords = c("Longitude",'Latitude'))
st_crs(x) <- 4326
x1 = subset(x,Time==1)


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
  