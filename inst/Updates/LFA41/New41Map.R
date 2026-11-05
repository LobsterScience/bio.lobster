require(bio.lobster)
require(bio.utilities)
require(dplyr)
library(ggplot2)
library(sf)
require(tidyr)
require(dplyr)
require(dbscan)
require(viridis)
require(devtools)

la()

#Map LFA 41 with boundaries and NAFO
setwd("C:/Users/HowseVJ/OneDrive - DFO-MPO/LFA 41 Update/2026/Figs")
offareanames = read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","LFA41Offareas.csv"))
NAFO<-readRDS("C:/Users/HowseVJ/Documents/GitHub/bio.lobster.data/mapping_data/NAFO_sf.rds")
NAFOshp<-st_read("C:/Users/HowseVJ/OneDrive - DFO-MPO/FW Preparation 33,34,41 - Vic Folder/mapping/LFA41Offareas_polygons.shp")

offarea<-st_read("C:/Users/HowseVJ/OneDrive - DFO-MPO/FW Preparation 33,34,41 - Vic Folder/mapping/LFA41Offareas_polygons.shp")

points_sf <- st_as_sf(offareanames, coords = c("X", "Y"), crs = 4326)
polys <- points_sf %>%
  group_by(PID, OFFAREA) %>%
  summarise(geometry = st_combine(geometry), .groups = "drop") %>%
  st_cast("POLYGON")
labels <- st_centroid(polys) %>%
  mutate(lon = st_coordinates(.)[,1],
         lat = st_coordinates(.)[,2]) %>%
  select(OFFAREA, lon, lat)



data <- NAFO |>
  filter(NAFO_1 %in% c("5Z", "4X")) |>
  group_by(NAFO_1) |>
  summarise()

#dissolve the (now irrelevant) lines that showed the subdivisions
data <- data |>
  dplyr::group_by(NAFO_1) |>
  dplyr::summarise()




bbox_41 <- st_bbox(
  c(
  xmin = -69,
  xmax = -63,
  ymin = 40.51,
  ymax = 44
  ),
  crs = st_crs(data)
)

bbox_41_sf <- st_as_sfc(bbox_41)

ggplot2::ggplot() +
  ggplot2::geom_sf(
    data = data,
    fill=NA,
    colour = "red"
  ) +
  ggplot2::geom_sf_text(
    data = data,
    ggplot2::aes(label = NAFO_1)
  ) +
  ggplot2::coord_sf(
    xlim = c(-68, -58),
    ylim = c(41, 46),
    expand = FALSE
  )


nafo_sub <- st_crop(
  data,
  xmin = -68.5,
  xmax = -63,
  ymin = 40.45,
  ymax = 44
)


nafo_sub_lines <- data |>
  st_boundary() |>
  st_intersection(bbox_41_sf)


ggLobsterMap(
  ylim = c(40.5,44),
  xlim = c(-68,-63),
  addGrids = FALSE,
  fill.colours = "grey",
  bathy = TRUE,
  return.object = TRUE,
  addLFALabels = TRUE,
  addLFAlines = TRUE,
  addNAFO = FALSE
) +
  geom_sf(
    data = nafo_sub_lines,
    colour = "orange",
    linewidth = 1.15,
    linetype = "solid",
    inherit.aes = FALSE
  ) +
  geom_sf(
    data = offarea,
    fill = NA,
    colour = "blue",
    linewidth = 1,
    show.legend = FALSE
  ) +
  geom_sf_text(
    data = labels,
    aes(label = OFFAREA),
    size = 3,
    fontface = "italic",
    colour = "blue"
  ) +
  coord_sf(
    xlim = c(-68.5, -63),
    ylim = c(40.5, 44),
    expand = FALSE
  )+
  annotate("text", y = 41, x = -67.6,
           label = "5Ze",
           fontface = "bold",
           size = 4,
           colour = "orange") +
  annotate("text", y = 41.5, x = -64.5,
           label = "4X",
           fontface = "bold",
           size = 4,
           colour = "orange")+
  annotate("text", y=43.25, x=-68,
           label = "5Y",family='sans',
           fontface="bold",size=4,col="orange")


ggsave(
  "LFA41NAFO_Map.png",
  width = 10,
  height = 8,
  units = "in",
  dpi = 600
)



###########FRENCH ############

#Map LFA 41 with boundaries and NAFO
setwd("C:/Users/HowseVJ/OneDrive - DFO-MPO/LFA 41 Update/2026/Figs")
offareanames = read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","LFA41Offareas.csv"))

offareanames <- offareanames %>%
  mutate(
    OFFAREA = recode(
      OFFAREA,
      "Crowell.Basin" = "Bassin de Crowell",
      "Georges.Bank"  = "Banc de Georges",
      "Georges.Basin" = "Bassin de Georges",
      "SE.Browns"     = "Sud-est Browns",
      "SW.Browns"     = "Sud-ouest Browns"
    )
  )


NAFO<-readRDS("C:/Users/HowseVJ/Documents/GitHub/bio.lobster.data/mapping_data/NAFO_sf.rds")
NAFOshp<-st_read("C:/Users/HowseVJ/OneDrive - DFO-MPO/FW Preparation 33,34,41 - Vic Folder/mapping/LFA41Offareas_polygons.shp")

offarea<-st_read("C:/Users/HowseVJ/OneDrive - DFO-MPO/FW Preparation 33,34,41 - Vic Folder/mapping/LFA41Offareas_polygons.shp")

points_sf <- st_as_sf(offareanames, coords = c("X", "Y"), crs = 4326)
polys <- points_sf %>%
  group_by(PID, OFFAREA) %>%
  summarise(geometry = st_combine(geometry), .groups = "drop") %>%
  st_cast("POLYGON")
labels <- st_centroid(polys) %>%
  mutate(lon = st_coordinates(.)[,1],
         lat = st_coordinates(.)[,2]) %>%
  select(OFFAREA, lon, lat)



data <- NAFO |>
  filter(NAFO_1 %in% c("5Z", "4X")) |>
  group_by(NAFO_1) |>
  summarise()

#dissolve the (now irrelevant) lines that showed the subdivisions
data <- data |>
  dplyr::group_by(NAFO_1) |>
  dplyr::summarise()




bbox_41 <- st_bbox(
  c(
    xmin = -69,
    xmax = -63,
    ymin = 40.51,
    ymax = 44
  ),
  crs = st_crs(data)
)

bbox_41_sf <- st_as_sfc(bbox_41)

ggplot2::ggplot() +
  ggplot2::geom_sf(
    data = data,
    fill=NA,
    colour = "red"
  ) +
  ggplot2::geom_sf_text(
    data = data,
    ggplot2::aes(label = NAFO_1)
  ) +
  ggplot2::coord_sf(
    xlim = c(-68, -58),
    ylim = c(41, 46),
    expand = FALSE
  )


nafo_sub <- st_crop(
  data,
  xmin = -68.5,
  xmax = -63,
  ymin = 40.45,
  ymax = 44
)


nafo_sub_lines <- data |>
  st_boundary() |>
  st_intersection(bbox_41_sf)


ggLobsterMap(
  ylim = c(40.5,44),
  xlim = c(-68,-63),
  addGrids = FALSE,
  fill.colours = "grey",
  bathy = TRUE,
  return.object = TRUE,
  addLFALabels = TRUE,
  addLFAlines = TRUE,
  addNAFO = FALSE
) +
  geom_sf(
    data = nafo_sub_lines,
    colour = "orange",
    linewidth = 1.15,
    linetype = "solid",
    inherit.aes = FALSE
  ) +
  geom_sf(
    data = offarea,
    fill = NA,
    colour = "blue",
    linewidth = 1,
    show.legend = FALSE
  ) +
  geom_sf_text(
    data = labels,
    aes(label = OFFAREA),
    size = 3,
    fontface = "italic",
    colour = "blue"
  ) +
  coord_sf(
    xlim = c(-68.5, -63),
    ylim = c(40.5, 44),
    expand = FALSE
  )+
  annotate("text", y = 41, x = -67.6,
           label = "5Ze",
           fontface = "bold",
           size = 4,
           colour = "orange") +
  annotate("text", y = 41.5, x = -64.5,
           label = "4X",
           fontface = "bold",
           size = 4,
           colour = "orange")+
  annotate("text", y=43.25, x=-68,
           label = "5Y",family='sans',
           fontface="bold",size=4,col="orange")


ggsave(
  "LFA41NAFO_Map_FRENCH.png",
  width = 10,
  height = 8,
  units = "in",
  dpi = 600
)
  