library(sf)
library(ggplot2)
library(dplyr)

df = ILTS_ITQ_All_Data(return_tow_tracks = T)
df$yr = lubridate::year(df$GPSDATE)
df = subset(df,lubridate::year(GPSDATE)==2021, select=c(TRIP_ID,SET_NO,X,Y))
df = df[!duplicated(df),]
# Create a LINESTRING for each group

# Create LINESTRING geometries for each group
lines_list <- df %>%
  group_by(TRIP_ID, SET_NO) %>%
  group_split() %>%
  lapply(function(group) {
    st_linestring(as.matrix(group[, c("X", "Y")]))
  })

# Create sf object
lines_sf <- st_sf(
  TRIP_ID = sapply(df %>% group_by(TRIP_ID, SET_NO) %>% group_keys(), `[[`, 1),
  SET_NO = sapply(df %>% group_by(TRIP_ID, SET_NO) %>% group_keys(), `[[`, 2),
  geometry = st_sfc(lines_list, crs = 4326)
)

# Plot with ggplot2
r = ggLobsterMap(xlim=c(-67.5,-64),ylim=c(42.8,45.55),return.object = T,addGrids = F,addLFAlines = T)
lines_sf$di = st_length(lines_sf)
r+geom_sf(data=subset(lines_sf,as.numeric(di)<5000) , size = 1,colour='red') +
  theme_minimal() 

d = (surveyMeasurements)

d = subset(d,lubridate::year(HAUL_DATE) %in% 2023:2024 & lubridate::month(HAUL_DATE)>8)

d$Hard = ifelse(d$SHELL %in% c(4:6,7),'Hard','Soft')
d$Dead = ifelse(d$DAMAGE %in% 3:4, 'Dead', 'Alive')


d = subset(d,!is.na(Dead) & !is.na(Hard))
bp = table(d$Dead,d$Hard)
df = as.data.frame(bp)
names(df)=c('Alive_Dead','Hard_Soft','Freq')


ggplot(df)+geom_mosaic(aes(weight=Freq,x=product(Alive_Dead,Hard_Soft),fill=Alive_Dead))+theme_minimal(base_size = 14)+labs(x="",y="")

