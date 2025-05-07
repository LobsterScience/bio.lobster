##Convert kmz
library(sf)
library(tidyverse)
library(zip)

require(bio.lobster)
require(bio.utilities)
require(devtools)


# Unzip the KMZ file
unzip("C:/Users/HowseVJ/Documents/bio.data/bio.lobster/data/maps/Lobster Fishing Areas.kml.kmz", exdir = "C:/Users/HowseVJ/Documents/bio.data/bio.lobster/data/maps/lobsterareasunzip")

# Read the KML file
kml_file <- list.files("C:/Users/HowseVJ/Documents/bio.data/bio.lobster/data/maps/lobsterareasunzip", pattern = "\\.kml$", full.names = TRUE)
layers <- st_layers(kml_file)
print(layers)

kml_data <- st_read(kml_file, layer = "Area 38B")

geom_type <- unique(st_geometry_type(kml_data))
print(geom_type)



# Separate the geometries
multipolygon_data <- kml_data[st_geometry_type(kml_data) == "MULTIPOLYGON", ]
point_data <- kml_data[st_geometry_type(kml_data) == "POINT", ]

# Extract coordinates for each type
multipolygon_coords <- st_coordinates(multipolygon_data)
point_coords <- st_coordinates(point_data)

# Extract attributes
multipolygon_attributes <- st_drop_geometry(multipolygon_data)
point_attributes <- st_drop_geometry(point_data)


# Add missing columns to point_coords
point_coords<-as.data.frame(point_coords)
point_coords$L1 <- NA
point_coords$L2 <- NA
point_coords$L3 <- NA

# Ensure column names match
colnames(point_coords) <- colnames(multipolygon_coords)

# Combine the coordinates and attributes
combined_coords <- rbind(
  cbind(multipolygon_attributes, multipolygon_coords),
  cbind(point_attributes, point_coords)
)

# View the combined data
print(combined_coords)



### CHECK THAT IT WORKED

area38b_sf <- st_as_sf(combined_coords, coords = c("X" ,"Y"), crs = 4326)
polygon_38b <- st_coordinates(area38b_sf)
polygon_38b <- rbind(polygon_38b, polygon_38b[1, ])# Ensure the polygon is closed by repeating the first point at the end
polygon_38b <- st_polygon(list(polygon_38b))# Create a POLYGON object
sf_38b <- st_sfc(polygon_38b, crs = 4326)# Convert to an sf object


p = ggLobsterMap(area="BoF",addGrids=F,
                 fill.colours = 'grey',bathy=F,return.object = T,lwd=10,colourLFA = T)+
geom_sf(data =sf_38b , color = "#0d0887", fill="#0d0887", alpha = 0.5)
  
######

###OTHER OPTION



# Unzip the KMZ file
unzip("C:/Users/HowseVJ/Documents/bio.data/bio.lobster/data/maps/38B Grey Zone & Corridors.kmz", exdir = "C:/Users/HowseVJ/Documents/bio.data/bio.lobster/data/maps/lobsterareasunzip")

# Read the KML file
kml_file <- list.files("C:/Users/HowseVJ/Documents/bio.data/bio.lobster/data/maps/lobsterareasunzip", pattern = "\\.kml$", full.names = TRUE)
layers <- st_layers(kml_file)
print(layers)

# Read a specific layer (replace 'layer_name' with the actual layer name)
kml_data <- st_read(kml_file, layer = "38B Grey Zone & Corridors")

# Separate the geometries
multipolygon_data <- kml_data[st_geometry_type(kml_data) == "MULTIPOLYGON", ]
point_data <- kml_data[st_geometry_type(kml_data) == "POINT", ]

# Extract coordinates for each type
multipolygon_coords <- st_coordinates(multipolygon_data)
point_coords <- st_coordinates(point_data)

# Extract attributes
multipolygon_attributes <- st_drop_geometry(multipolygon_data)
point_attributes <- st_drop_geometry(point_data)

# Convert point_coords to a data frame if it's not already
point_coords <- as.data.frame(point_coords)

# Add missing columns to point_coords
point_coords$L1 <- NA
point_coords$L2 <- NA
point_coords$L3 <- NA

# Ensure column names match
colnames(point_coords) <- colnames(multipolygon_coords)

# Combine the coordinates and attributes
combined_coords <- rbind(
  cbind(multipolygon_attributes, multipolygon_coords),
  cbind(point_attributes, point_coords)
)

# View the combined data
print(combined_coords)
write.csv(combined_coords,"C:/Users/HowseVJ/Documents/bio.data/bio.lobster/data/maps/lfaRework/shape38b.csv")

### CHECK THAT IT WORKED

area38b_sf <- st_as_sf(combined_coords, coords = c("X" ,"Y"), crs = 4326)
polygon_38b <- st_coordinates(area38b_sf)
polygon_38b <- rbind(polygon_38b, polygon_38b[1, ])# Ensure the polygon is closed by repeating the first point at the end
polygon_38b <- st_polygon(list(polygon_38b))# Create a POLYGON object
sf_38b <- st_sfc(polygon_38b, crs = 4326)# Convert to an sf object


p = ggLobsterMap(area="BoF",addGrids=F,
                 fill.colours = 'grey',bathy=F,return.object = T,lwd=10,colourLFA = T)+
  geom_sf(data =sf_38b , color = "#0d0887", fill="#0d0887", alpha = 0.5)

p





### Mike's Version of converting


process_kmz_pts <- function(kmz_file=NULL, makeShp = TRUE, output_folder=NULL) {
  output_folder_ <- ifelse(is.null(output_folder),getwd(),output_folder)
  unzip(kmz_file, exdir = tempdir())
  kml_file <- list.files(tempdir(), pattern = "*.kml", full.names = TRUE)[1]
  layers <- sf::st_layers(kml_file)
  for(i in seq_along(layers$name)) {
    layer <- sf::st_read(kml_file, layer = layers$name[i])
    layer_points <- layer[sf::st_geometry_type(layer) == "POINT", ]
    layer_points <- sfheaders::sf_point(sf::st_coordinates(layer_points)[,1:2])
    if(nrow(layer_points) > 0) {
      layer_points <- sf::st_set_crs(layer_points, 4326) #since kml, we're assuming WGS84
      layer_name_cln<- gsub(" ", "_", layers$name[i])
      assign(x = layer_name_cln, value = layer_points,envir = .GlobalEnv)
      if (makeShp) sf::st_write(layer_points, append = F, paste0(output_folder_, "/", layer_name_cln, ".shp"))
    }
  }
  file.remove(kml_file)
}
and then you call it via
process_kmz_pts(kmz_file = "c:/Users/McMahonM/OneDrive - DFO-MPO/Support/Individuals/HowseV/LFA/38B Grey Zone & Corridors.kmz",output_folder = "C:/dfo-mpo/")


