require(ggplot2)
require(sf)
require(bio.lobster)
require(tidyr)
require(dplyr)
require(dbscan)
require(viridis)

########## I need to know the km2 area covered of LFAs 35-38, and the KMs of coastline from NS to NB of LFAs 35-38 ########## 


cents = readRDS(file.path(project.datadirectory("bio.lobster"), "data","maps","LFALabelsSF.rds"))
cents$geometry[cents$label == "36"] <- st_sfc(st_point(c(-66, 45.15)))
cents$geometry[cents$label == "35"] <- st_sfc(st_point(c(-64.85, 45.22)))

ns_coast = readRDS(file.path(project.datadirectory("bio.lobster"), "data","maps","CoastSF.rds"))
us_lobareas=st_read(file.path(project.datadirectory("bio.lobster"), "data","maps","americanlobster","Lobster_Management_Areas.shp"))
##Adding Area38B
area38b= read.csv(file.path(project.datadirectory("bio.lobster"), "data","maps","area38B.csv"))
area38b<-area38b%>% drop_na()
area38b_sf <- st_as_sf(area38b, coords = c("Longitude", "Latitude"), crs = 4326)
polygon_38b <- st_coordinates(area38b_sf)
polygon_38b <- rbind(polygon_38b, polygon_38b[1, ])# Ensure the polygon is closed by repeating the first point at the end
polygon_38b <- st_polygon(list(polygon_38b))# Create a POLYGON object
sf_38b <- st_sfc(polygon_38b, crs = 4326)# Convert to an sf object



p = ggLobsterMap(ylim=c(40,48), xlim=c(-68,-54.7),addGrids=F,
                 fill.colours = 'grey',bathy=T,return.object = T,lwd=10,colourLFA = T)
p = p + geom_sf_text(data=cents, aes(label=label),family='sans',fontface="bold", size=3.5,col="#0d0887")##LFA Labels
p= p+geom_sf(data=us_lobareas, fill="#ed7953", alpha=0.3)+coord_sf(ylim=c(40,48), xlim=c(-68,-54.7)) ## adds american boundaries
p=p+geom_sf(data =sf_38b , color = "#0d0887", fill="#0d0887", alpha = 0.5)+coord_sf(ylim=c(40,48), xlim=c(-68,-54.7)) + ## adds Area 38B
    annotate("text",y=44.4, x=-67.15,label = "B",family='sans',fontface="bold",size=3,col="white")+
    annotate("text", y=42.5, x=-63.57,label = "41",family='sans',fontface="bold",size=3.5,col="#0d0887")+
    annotate("text", x = -68, y = 43.95, label = "Down East", size = 3, family = 'sans',color = "#0d0887")
p
ggsave(filename = "LFAs.png", plot = p, path = "C:/Users/HowseVJ/OneDrive - DFO-MPO/LFA 35-38 Framework Resources/Figures", width = 10, height = 8, units = "in", dpi = 300)

##Issues: 
## Why is the land in America not grey too?
## Area 38B doesn't have the right coordinates - It doesn't line up with the American/Canadian border?
# Why is Grand Manan and Digby Neck, campbello, deer island etc not grey?



##Locations mentioned in FW Document

# color_palette <- c("Maces Bay" = "#f98e09", 
#                    "Beaver Harbour" = "#bc3754",  
#                    "Flagg Cove" = "#e45a31",  
#                    "Deadman's to Seeley's Head" = "#8a226a",
#                    "West Isles"="#210c4a",
#                    "Letang Estuary"="#57106e",
#                     "Passamaquoddy Bay"="#fcffa4",
#                      "Whale Cove"="#f9cb35",
#                      "Annapolis Basin"="#000004") 

color_palette <- viridis::viridis(9, option = "inferno")
bof_labs=read.csv(file.path(project.datadirectory("bio.lobster"), "data","maps","BOF_FWLabels.csv"))

BOFlab = readRDS(file.path(project.datadirectory("bio.lobster"), "data","maps","LFALabelsSF.rds"))
BOFlab<- BOFlab[BOFlab$label %in% c("34","35", "36", "37", "38"),]
BOFlab$geometry[BOFlab$label == "34"] <- st_sfc(st_point(c(-66.1, 44.7)))
BOFlab$geometry[BOFlab$label == "35"] <- st_sfc(st_point(c(-65.5, 44.95)))
BOFlab$geometry[BOFlab$label == "36"] <- st_sfc(st_point(c(-65.9, 45.1)))
BOFlab$geometry[BOFlab$label == "38"] <- st_sfc(st_point(c(-66.8, 44.55)))

b = ggLobsterMap( ylim=c(44.5,45.5),xlim=c(-67.42,-65),addGrids=F,
                 fill.colours = 'grey',bathy=F,return.object = T,colourLFA = T)
b = b + geom_sf_text(data=BOFlab, aes(label=label),family='sans',fontface="bold", size=4,col="black")
  b = b + geom_point(data = bof_labs, aes(x = longitude, y = latitude, color = label), shape = 17, size = 3) 
  b = b +  scale_color_manual(values = color_palette) +
    guides(color = guide_legend(title = NULL))
b

##Issues: - Why is Grand Manan and Digby Neck, campbello, deer island etc not grey?

ggsave(filename = "diveandcollectors.png", plot = b, path = "C:/Users/HowseVJ/OneDrive - DFO-MPO/LFA 35-38 Framework Resources/Figures", width = 10, height = 8, units = "in", dpi = 300)





