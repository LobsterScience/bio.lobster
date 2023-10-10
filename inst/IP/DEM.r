require(ggplot2)
require(terra)
require(tidyterra)
require(sf)
require(bio.lobster)
layerDir=file.path(project.datadirectory("bio.lobster"), "data","maps")

v = terra::rast(x='C:\\Users\\Cooka\\Documents\\bio_data\\bio.lobster\\bathymetry\\dem\\w001001.adf')
v1 = terra::as.points(v)


saveRDS(v1,file='C:\\Users\\Cooka\\Documents\\bio_data\\bio.lobster\\bathymetry\\SpatvectorDEM.rds')
v1 = readRDS(file='C:\\Users\\Cooka\\Documents\\bio_data\\bio.lobster\\bathymetry\\SpatvectorDEM.rds')


l = readRDS(file.path( layerDir,"LFAPolysSF.rds"))
l = st_transform(l,crs=32620)
l = subset(l,LFA<40)
cropped_vector <- crop(v1, l)


#creating a contour
contour_depth <- 100  # Adjust the desired depth

contour_lines <- contourLines(x = cropped_vector$x, y = cropped_vector$y, z = cropped_vector$z, levels = contour_depth)
contour_sf <- st_sf(geometry = contour_lines[[1]])
v2 = st_as_sf(v1,as.points=TRUE,merge=FALSE)

ggplot() +
  geom_spatraster(data = v) +
  scale_fill_hypso_c()