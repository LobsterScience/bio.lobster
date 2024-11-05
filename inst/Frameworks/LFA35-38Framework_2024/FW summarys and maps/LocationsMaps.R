require(ggplot2)
require(sf)
require(bio.lobster)
require(bio.utilities)
require(tidyr)
require(dplyr)
require(dbscan)
require(viridis)
require(devtools)


###Maritimes Region with  NAFO
cents = readRDS(file.path(project.datadirectory("bio.lobster"), "data","maps","LFALabelsSF.rds"))
cents$geometry[cents$label == "36"] <- st_sfc(st_point(c(-66, 45.15)))
cents$geometry[cents$label == "35"] <- st_sfc(st_point(c(-64.85, 45.22)))

p = ggLobsterMap(ylim=c(40,48), xlim=c(-68,-54.7),addGrids=F,fill.colours = 'lightgrey',bathy=T,return.object = T,lwd=8,
                 colourLFA = T, addNAFO = T, addNAFOLabels = F, nafo=c('5Y','4X','5Z'), addLFALabels = F)+
  geom_sf_text(data=cents, aes(label=label),family='sans',fontface="bold", size=3,col="black")+
  annotate("text", y=42.5, x=-63,label = "41",family='sans',fontface="bold",size=3,col="black")+
  annotate("text", y=44, x=-67.75,label = "5Y",family='sans',fontface="bold",size=3.25,col="orange")+
  annotate("text", y=41, x=-67.6,label = "5Z",family='sans',fontface="bold",size=3.25,col="orange")+
  annotate("text", y=41.5, x=-64.5,label = "4X",family='sans',fontface="bold",size=3.25,col="orange")
  
ggsave(filename = "MartimesRegion.png", plot = p, path = "C:/Users/HowseVJ/OneDrive - DFO-MPO/LFA 35-38 Framework Resources/Figures", width = 10, height = 8, units = "in", dpi = 300)


##########  km2 area covered of LFAs 35-38 ###########

lobarea = readRDS(file.path(project.datadirectory("bio.lobster"), "data","maps","LFAPolysSF.rds"))
coastal = readRDS(file.path(project.datadirectory("bio.lobster"), "data","maps","CoastSF.rds"))

##Check CRS is same
coastal <- st_transform(coastal, crs = st_crs(lobarea))
##Determine spatial difference
lobarea_clean <- st_difference(lobarea, st_union(coastal))

#calc area
lobarea_clean1 <- lobarea_clean %>%
  mutate(area_km2 = st_area(.) / 10^6)

area_summary1 <- lobarea_clean1 %>%
  group_by(LFA) %>%
  summarize(total_area_km2 = sum(area_km2))
area_summary1<-as.data.frame(area_summary1)

###Other Way to check

lobarea_clean$area_km2 <- as.numeric(st_area(lobarea_clean)) / 10^6

# Summarize the area for each unique LFA
unique_lfas <- unique(lobarea_clean$LFA)
total_area_km2 <- sapply(unique_lfas, function(lfa) {
  sum(lobarea_clean$area_km2[lobarea_clean$LFA == lfa])
})

area_summary2 <- data.frame(LFA = unique_lfas, total_area_km2 = total_area_km2)

write.csv(area_summary2, "C:/Users/HowseVJ/OneDrive - DFO-MPO/LFA 35-38 Framework Resources/Figures/LFAareacover.csv")


####Locations mentioned in FW Document - diving and collector sites
color_palette <- viridis::viridis(10, option = "inferno")
bof_labs=read.csv(file.path(project.datadirectory("bio.lobster"), "data","maps","BOF_FWLabels.csv"))

BOFlab = readRDS(file.path(project.datadirectory("bio.lobster"), "data","maps","LFALabelsSF.rds"))
BOFlab<- BOFlab[BOFlab$label %in% c("34","35", "36", "37", "38"),]
BOFlab$geometry[BOFlab$label == "34"] <- st_sfc(st_point(c(-66.1, 44.7)))
BOFlab$geometry[BOFlab$label == "35"] <- st_sfc(st_point(c(-65.5, 44.95)))
BOFlab$geometry[BOFlab$label == "36"] <- st_sfc(st_point(c(-65.9, 45.1)))
BOFlab$geometry[BOFlab$label == "38"] <- st_sfc(st_point(c(-66.8, 44.55)))

b = ggLobsterMap( ylim=c(44.5,45.5),xlim=c(-67.42,-65),addGrids=F,
                 fill.colours = 'grey',bathy=F,return.object = T,colourLFA = T,addLFAlines=T)
b = b + geom_sf_text(data=BOFlab, aes(label=label),family='sans',fontface="bold", size=4,col="black")
  b = b + geom_point(data = bof_labs, aes(x = longitude, y = latitude, color = label), shape = 17, size = 3) 
  b = b +  scale_color_manual(values = color_palette) +
    guides(color = guide_legend(title = NULL))
b

ggsave(filename = "diveandcollectors.png", plot = b, path = "C:/Users/HowseVJ/OneDrive - DFO-MPO/LFA 35-38 Framework Resources/Figures", width = 10, height = 8, units = "in", dpi = 300)


##### With Mike's rework of LFAs 

##LFAs
lobarea = st_read(file.path(project.datadirectory("bio.lobster"), "data","maps","lfaRework","LFA_rework_mmm.shp"))
# Define the bounding box
bbox <- st_bbox(c(xmin = -67.8, xmax = -63.2, ymin = 43.75, ymax = 46), crs = st_crs(lobarea))
# Clip the lobarea data
lobarea_clipped <- st_crop(lobarea, bbox)


##Area 38b
L38b<-read.csv("C:/Users/HowseVJ/Documents/bio.data/bio.lobster/data/maps/lfaRework/shape38b.csv")
sf38<-st_as_sf(L38b, coords=c("X","Y"),crs=4326)
polygon_38b <- st_coordinates(sf38)
polygon_38b <- rbind(polygon_38b, polygon_38b[1, ])# Ensure the polygon is closed by repeating the first point at the end
polygon_38b <- st_polygon(list(polygon_38b))# Create a POLYGON object
sf_38b <- st_sfc(polygon_38b, crs = 4326)# Convert to an sf object


## Add Grids and clip
grids<-readRDS("C:/Users/HowseVJ/Documents/bio.data/bio.lobster/data/maps/GridPolysSF.rds")
gbox <- st_bbox(lobarea_clipped)
grids <- st_crop(grids, gbox)
##Now Clip to coast
coastal = readRDS(file.path(project.datadirectory("bio.lobster"), "data","maps","CoastSF.rds"))
coastal <- st_transform(coastal, crs = st_crs(grids))
grid <- st_difference(grids, st_union(coastal))
grid_clipped <- st_intersection(grid, lobarea_clipped)

#Labels
BOFlab = readRDS(file.path(project.datadirectory("bio.lobster"), "data","maps","LFALabelsSF.rds"))
BOFlab<- BOFlab[BOFlab$label %in% c("34","35", "36", "37", "38"),]
BOFlab$geometry[BOFlab$label == "34"] <- st_sfc(st_point(c(-66.58, 43.91)))
BOFlab$geometry[BOFlab$label == "35"] <- st_sfc(st_point(c(-65.58, 44.95)))
BOFlab$geometry[BOFlab$label == "37"] <- st_sfc(st_point(c(-66.41, 44.9)))
BOFlab$geometry[BOFlab$label == "36"] <- st_sfc(st_point(c(-65.915, 45.1)))
BOFlab$geometry[BOFlab$label == "38"] <- st_sfc(st_point(c(-66.75, 44.44)))

p = ggLobsterMap(area="custom", ylim=c(43.75,46),xlim=c(-67.8,-63.2), addGrids=F,
                 fill.colours = 'grey',bathy=T,color=NA,colourLFA = F,addLFAlines=F)+
  geom_sf(data=lobarea_clipped, fill=NA,color="black",linewidth=0.75)+
   geom_sf(data =sf_38b,color="black",linewidth=0.7)+
  geom_sf(data=grid_clipped, fill=NA, color="#333438", linewidth=0.4)+
geom_sf_text(data=BOFlab, aes(label=label),family='sans',size=4.5,col="black")+
  annotate("text",y=44.42, x=-67.09,label = "B",family='sans',size=4.5,col="black")
p 

ggsave(filename = "BoFLfasgrid.png", plot = p, path = "C:/Users/HowseVJ/OneDrive - DFO-MPO/LFA 35-38 Framework Resources/Figures", width = 10, height = 8, units = "in", dpi = 300)
