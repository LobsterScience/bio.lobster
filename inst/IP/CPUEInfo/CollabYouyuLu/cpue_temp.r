require(bio.lobster)
require(bio.utilities)
require(devtools)
require(dplyr)

la()

gg = read.csv(file.path(project.datadirectory('bio.lobster'),'data','maps','GridGroups.csv'))

gg = subset(gg,LFA %in% c(33,34))
lg = lobster.db('process.logs')
lg = subset(lg,LFA %in% c(33,34) & SYEAR>2007 & SYEAR<2024)

g1 = merge(lg,gg)
ga = aggregate(cbind(NUM_OF_TRAPS,WEIGHT_KG)~DATE_FISHED+LFA+GridGrouping+SYEAR,data=g1,FUN=sum)
ga$CPUE = ga$WEIGHT_KG / ga$NUM_OF_TRAPS

write.csv(ga,file='LFA33_34CPUE_GRIDGROUP_DATE.csv')


#observed BTs
lobster.db('fsrs')
fsrs = merge(fsrs,gg,by.x=c('LFA_GRID','LFA'),by.y=c('GRID_NUM','LFA'))
fsrs=subset(fsrs,!is.na(TEMP))

fs = fsrs %>% select(c(HAUL_DATE,GridGrouping,LFA,LAT_DD,LONG_DD,TEMP)) %>% distinct()
fs = fs[order(fs$HAUL_DATE,fs$GridGrouping,fs$LFA),]
write.csv(fs,file='LFA33_34_Observed_FSRS_Temperature_GRIDGROUP_DATE.csv')



gg = st_read(file.path(project.datadirectory('bio.lobster'),'data','maps','GIS','LFA 34 Grid Grouping polygons_NAD83_region.shp'))
st_crs(gg) <- 4326

g2 = st_read(file.path(project.datadirectory('bio.lobster'),'data','maps','GIS','LFA 33 Grid Grouping polygons_NAD83_region.shp'))
st_crs(g2) <- 4326

td = lobster.db('temperature.data')
td = st_as_sf(td,coords=c('LON_DD','LAT_DD'),crs=4326)
sf_use_s2(FALSE)
td1 = st_within(td,gg)