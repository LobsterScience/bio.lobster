require(lubridate)
require(devtools)
require(ggplot2)
require(dplyr)
require(devtools)
require(geosphere)
require(lubridate)
require(bio.utilities)
require(bio.lobster)
require(bio.survey)
require(sf)

##RV data for 2024 not completed loaded yet 
p=list()
p$libs = NULL
fp = file.path(project.datadirectory('bio.lobster'),"analysis")
p$yrs = 1970:2024
p$area="LFA35-38"


x = RV_sets()
cr = sum(x$Legal_wt,na.rm=T)/sum(x$WEIGHT_KG,na.rm=T)
i = which(is.na(x$Legal))
x$Legal_wt[i] = x$WEIGHT_KG[i] * cr
x = subset(x,month(DATE) %in% 6:8)
x = subset(x,select=c(mission,setno,Lobster,WEIGHT_KG,Legal, Legal_wt, Berried,EMPTY,LONGITUDE,LATITUDE,DATE, YEAR))
x$Survey = 'RV'

x$sid = paste(x$mission,x$setno,sep="-")

### CHECK POSITION OF DATA
p = ggLobsterMap(area='BoF',addGrids=F,
                 fill.colours = 'grey',bathy=F,return.object = T,colourLFA = F)
p = p + geom_point(data =bof_RV, aes(x = LONGITUDE, y = LATITUDE), color = "red")
p

####polygons LFA
layerDir=file.path(project.datadirectory("bio.lobster"), "data","maps")

rL = read.csv(file.path(layerDir,"Polygons_LFA.csv"))
rL$label=rL$LFA

rdef=Mar.utils::df_to_sf(rL,lat.field = "Y",lon.field = "X") 
rdef<-merge(rdef,unique(rL[,c("PID", "LFA")]), all.x=T, by="PID")
##Convert dataframe to Sf
all_tows_sf <- st_as_sf(x , coords = c("LONGITUDE", "LATITUDE"), crs = 4326)
# Check same coordinate system
rdef <- st_transform(rdef, crs = st_crs(all_tows_sf))

## Join them spatially
all_tows_with_LFA <- st_join(all_tows_sf, rdef, join = st_within)
##BOF only
bof_RV <- all_tows_with_LFA  %>%
  filter(LFA %in% c(35, 36, 37, 38))

##convert back to DF
bof_RVdf <- as.data.frame(bof_RV)

rvLobs <-bof_RVdf %>%
  mutate(Decade = cut(YEAR, 
                      breaks = c(1970, 1981, 1991, 1999, 2010, 2025), 
                      right = FALSE, 
                      labels = c("1970-1980", "1981-1990", "1991-1998", "1999-2009", "2010-2024")))

summary_RV <-rvLobs %>%
  group_by(Decade) %>%
  summarize(
    TotalSets = n_distinct(sid),
    LobsterEncountered = sum(Lobster, na.rm = TRUE),
    SetsWithoutLobster = n_distinct(sid[EMPTY == 1]),
    BerriedEncountered = sum(Berried, na.rm = TRUE)
  )

summary_RV<- as.data.frame(summary_RV)

#No Dplyr
unique_sid <- tapply(rvLobs$sid, rvLobs$Decade, function(x) length(unique(x)))
lobster_count <- tapply(rvLobs$Lobster, rvLobs$Decade, sum, na.rm = TRUE)
unique_sid_empty <- tapply(rvLobs$sid[rvLobs$EMPTY == 1], rvLobs$Decade[rvLobs$EMPTY == 1], function(x) length(unique(x)))
sum_berried <- tapply(rvLobs$Berried, rvLobs$Decade, sum, na.rm = TRUE)

summary_df <- data.frame(
  decade = names(unique_sid),
  unique_sid = unique_sid,
  lobster_count = lobster_count,
  unique_sid_empty = unique_sid_empty,
  sum_berried = sum_berried
)
summary_df<-as.data.frame(summary_df)
print(summary_df)
write.csv(summary_df, "C:/Users/HowseVJ/OneDrive - DFO-MPO/LFA 35-38 Framework Resources/Figures/RVsummarytable.csv")

###ISSUES: Getting a lot higher numbers on all fronts for the sets, lobs etc.