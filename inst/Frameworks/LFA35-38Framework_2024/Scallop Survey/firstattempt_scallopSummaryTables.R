##Scallop Summary Tables

require(bio.lobster)
require(bio.utilities)
require(Mar.utils)
require(devtools)
require(ggplot2)
require(lubridate)
require(rstanarm)
require(sf)
require(splancs)
require(raptr)
require(lwgeom)
require(dplyr)

p=list()
assessment.year = 2024 ##Check Year
p$syr = 1989
p$yrs = p$syr:assessment.year
lfa=c(34:41)
layerDir=file.path(project.datadirectory("bio.lobster"), "data","maps")
return_sets_object=F

#lobster.db('scallop.redo')
lobster.db('scallop')

scallopStratDefs$X = scallopStratDefs$LONGITUDE
scallopStratDefs$Y = scallopStratDefs$LATITUDE
scallopStratDefs$label=as.character(scallopStratDefs$STRATA_ID)
scallopStratDefs$PID=scallopStratDefs$STRATA_ID
scallopStratDefs$POS=scallopStratDefs$ORDINAL

sdef=Mar.utils::df_to_sf(scallopStratDefs) ## Make Dataframe into SF object

names(sdef)[1]='STRATA_ID'
sdef$total_area = st_area(sdef) #total area of strata

#polygons LFA
rL = read.csv(file.path(layerDir,"Polygons_LFA.csv"))
rL$label=rL$LFA

rdef=Mar.utils::df_to_sf(rL,lat.field = "Y",lon.field = "X") 
rdef<-merge(rdef,unique(rL[,c("PID", "LFA")]), all.x=T, by="PID")

#intersect LFA and strata

lf = subset(rdef,LFA %in% lfa)
v = st_intersection(sdef,lf)
v$total_area_r = st_area(v) #area within LFA
j = which(v$STRATA_ID ==49)
if(length(j)>0) v$total_area_r[j] = v$total_area[j] = 239000000
#
scallop.tows$Y = convert.dd.dddd(scallop.tows$START_LAT)
scallop.tows$X = convert.dd.dddd(scallop.tows$START_LONG)
scT = subset(scallop.tows,select=c('TOW_SEQ','TOW_DATE','STRATA_ID','X','Y','BOTTOM_TEMP'))

scC = subset(scallopSurv,select=c("TOW_SEQ",  "ABUNDANCE_STD","MEAS_VAL", "SEX_ID"))

scC = merge(scT,scC,all.x=T)
scC$YEAR = lubridate::year(scC$TOW_DATE)
scC$BOTTOM_TEMP = ifelse(is.na(scC$BOTTOM_TEMP),-99,scC$BOTTOM_TEMP)
#year subset
s = subset(scC,YEAR>=1999)


scalmeas = st_as_sf(s,coords = c('X','Y'),crs=st_crs(4326)) ## Convert to sf object
measlocs = st_join(scalmeas,v, join=st_within)

############################## Map the NAs #############################

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


# Filter the sf object to include only rows where LFA is NA
na_lfa <- subset(measlocs, is.na(LFA))

p = ggLobsterMap(area="BoF",addGrids=F,
                 fill.colours = 'grey',bathy=T,return.object = T,lwd=10,colourLFA = T)
p = p +  geom_sf(data = na_lfa, aes(geometry = geometry), color = "red", size = 2) +
  theme_minimal() 




scallocs <- st_drop_geometry(measlocs)



# Create Decade column with custom breaks, including 2019-2024
sumS <- scallocs %>%
  mutate(Decade = cut(YEAR, breaks = c(1999, 2010, 2019, 2025), right = FALSE, labels = c("1999-2009", "2010-2018", "2019-2024")))

# Check for missing values and data types
sumS <- sumS %>%
  filter(!is.na(MEAS_VAL) & !is.na(SEX_ID) & !is.na(TOW_SEQ)) %>%
  mutate(MEAS_VAL = as.numeric(MEAS_VAL), SEX_ID = as.numeric(SEX_ID), TOW_SEQ = as.character(TOW_SEQ))

# Summarize data by decade
summary_scal <- sumS %>%
  group_by(Decade,LFA) %>%
  summarize(
    total_animals = n(),
    recruits = sum(MEAS_VAL >= 70 & MEAS_VAL < 82), 
    males = sum(SEX_ID == 1),
    females = sum(SEX_ID == 2 | SEX_ID == 3),
    number_of_tows = n_distinct(TOW_SEQ)  ## is TOW_SEQ the unique TOW ID?
  )

summary_scal<-as.data.frame(summary_scal)

#### Issues: tows without lobster are filtered out - so I'm not getting the total tows - vs "those with lobster"
###Map the NAs--- they are in the LFAS?
