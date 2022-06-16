

#########################################
###########testing map without fine coastline

map_data <- rnaturalearth::ne_countries(
  scale = "medium",
  returnclass = "sf", country = "canada")

# Crop the polygon for plotting and efficiency:
st_bbox(map_data)
ns_coast <- suppressWarnings(suppressMessages(
  st_crop(map_data,
          c(xmin = -62.2, ymin = 44, xmax = -60, ymax = 45.5))))
crs_utm20 <- 2961

st_crs(ns_coast) <- 4326 # 'WGS84'; necessary on some installs
ns_coast <- st_transform(ns_coast, crs_utm20)
ggplot(data=ns_coast)+geom_sf


rL=readRDS(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolysNoLand.rds"))
st_crs(v) <- 4326

st_crs(rL) <- 4326
crs_utm20 <- 32620
ns_coast <- suppressWarnings(suppressMessages(
  st_crop(ns_coast,
          c(xmin = -62.2, ymin = 44., xmax = -59.8, ymax = 45.6))))

rL <- suppressWarnings(suppressMessages(
  st_crop(rL,
          c(xmin = -62.2, ymin = 44., xmax = -59.8, ymax = 45.6))))

ns_coast <- st_transform(ns_coast, crs_utm20)
rL <- st_transform(rL, crs_utm20)
# Project our survey data coordinates:
survey <- aT %>%   st_as_sf(crs = 4326, coords = c("X", "Y")) %>%
  st_transform(crs_utm20)

# Plot our coast and survey data:
ggplot(data=rL) +
  geom_sf(size=1) +
  geom_sf(data = survey, size = 0.5,col='red')

