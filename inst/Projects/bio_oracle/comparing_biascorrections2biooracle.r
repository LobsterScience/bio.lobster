#biooracle
require(bio.lobster)
require(devtools)
la()
require(ggplot2)
require(sf)
require(biooracler)
require(terra)
dataset_id <- "thetao_baseline_2000_2019_depthmean"
biooracler::info_layer(dataset_id )

time = c('2001-01-01T00:00:00Z', '2010-01-01T00:00:00Z')
latitude = c(40, 50)
longitude = c(-70, -57)

constraints = list(time, latitude, longitude)
names(constraints) = c("time", "latitude", "longitude")
variables = 'thetao_mean'
lay = download_layers(dataset_id , variables ,  constraints )
plot(lay$thetao_mean_2)

mean_rast <- app(lay, mean, na.rm = TRUE)

bio_or <- st_as_sf(as.data.frame(mean_rast, xy = TRUE),
                   coords = c("x", "y"),
                   crs = "OGC:CRS84")

bio_or = st_transform(bio_or,crs=32620)

temp=readRDS(file.path(bio.lobster::project.datadirectory('bio.lobster.glorys'),'Glorys2000-2025wBiasCorrColumn_doy_june15.rds'))
temp$temperature = temp$Glor + temp$pred
require(data.table)
dt = as.data.table(subset(temp,yr %in% 2000:2019))

result <- dt[, .(mean_temp = mean(temperature, na.rm = TRUE), mean_glor =mean(Glor, na.rm = TRUE), mean_bc = mean(pred,na.rm=T) ), 
             by = .(X1000, Y1000)]

result[, X_m := X1000 * 1000]
result[, Y_m := Y1000 * 1000]

library(sf)

res <- st_as_sf(result,
                   coords = c("X_m", "Y_m"),
                   crs = 32620)
ggplot(res,aes(fill=mean_glor,colour=mean_glor))+geom_sf()+scale_colour_viridis_c()+scale_fill_viridis_c()
  ggplot(res,aes(fill=mean_bc,colour=mean_bc))+geom_sf()+scale_colour_viridis_c()+scale_fill_viridis_c()


idx = st_nearest_feature(res,bio_or)

library(dplyr)

idx <- st_nearest_feature(sf1, sf2)

result <- res %>%
  mutate(nearest_id = idx) %>%
  bind_cols(bio_or[idx, ] %>% st_drop_geometry())


result$g2b = result$mean_glor - result$mean_temp
ggplot(result,aes(fill=g2b,colour=g2b))+geom_sf()+scale_colour_viridis_c()+scale_fill_viridis_c()
