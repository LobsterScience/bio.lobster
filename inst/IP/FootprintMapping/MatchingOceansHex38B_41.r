##review of fishery atlas
require(bio.lobster)
require(devtools)
require(sf)
la()

g = lobster.db('annual.landings')
g1 = subset(g, YR %in% 2014:2018)
g2 = subset(g, YR %in% 2019:2023)

#####spatial logs

a = lobster.db('greyzone_logs')
a$Date = lubridate::ymd(a$DATE_SAILED)
a$yr = lubridate::year(a$Date)


a$DDLAT = round((((a$ENT_LATITUDE /100/100-trunc(a$ENT_LATITUDE/100/100))*100)/60)+trunc(a$ENT_LATITUDE/100/100),4)
a$DDLON = round((((a$ENT_LONGITUDE/100/100-trunc(a$ENT_LONGITUDE/100/100))*100)/60)+trunc(a$ENT_LONGITUDE/100/100),4)*-1
a = subset(a,!is.na(DDLON))
aR = st_as_sf(a,coords=c('DDLON','DDLAT'),crs=4326)



canada_albers <- st_crs("+proj=aea +lat_1=50 +lat_2=70 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")
if(redo.hex){
bbox <- st_as_sf(st_sfc(st_polygon(list(rbind(
  c(-68.5, 40), c(-55, 40), c(-55, 48), c(-68.5, 48), c(-68.5, 40)
))), crs = 4326))  # WGS84


bbox_proj <- st_transform(bbox,crs= canada_albers)


hex_area <- 10000000
hex_side <- sqrt(3) * sqrt((2 * target_area) / (3 * sqrt(3)))

hex_grid <- st_make_grid(bbox_proj, cellsize = hex_side, square = FALSE)
hex_sf <- st_sf(geometry = hex_grid)

##eez
ezc = readRDS(file= file.path(git.repo,'bio.lobster.data','mapping_data','eez_mar_sf.rds'))
ezc = st_transform(ezc,crs=canada_albers)
pp = readRDS(file.path(git.repo,'bio.lobster.data','mapping_data','DisputedZone_38B.rds'))
pp = st_transform(pp,crs=canada_albers)
pp = st_make_valid(pp)
ezcB = st_union(ezc,pp)

he = st_intersects(hex_sf,ezcB,sparse = F)

he = hex_sf[he,]

saveRDS(he,file.path(git.repo,'bio.lobster.data','mapping_data','eez_hexagons_mar_sf_incl38B.rds'))
}

he = readRDS(file.path(git.repo,'bio.lobster.data','mapping_data','eez_hexagons_mar_sf_incl38B.rds'))
he$pid = 1:nrow(he)
aR = st_transform(aR,canada_albers)
ah = st_join(aR,he,join=st_within)

ahr = subset(ah,!is.na(pid))
ahr$NUM_OF_TRAPS = as.numeric(ahr$NUM_OF_TRAPS)
ahrr = subset(ahr,yr %in% 2019:2023 & !is.na(EST_WEIGHT_LOG_LBS) & NUM_OF_TRAPS<564)
ahrr$triID = paste(ahrr$VR_NUMBER, ahrr$DATE_SAILED,sep="_")
aa =aggregate(cbind(NUM_OF_TRAPS,EST_WEIGHT_LOG_LBS)~pid,data=ahrr, FUN=sum)
ab = aggregate(triID~pid,data=ahrr, FUN=function(x) length(unique(x)))

aba =merge(aa,ab)
abac = st_as_sf(merge(aba,he))

pp = readRDS(file.path(git.repo,'bio.lobster.data','mapping_data','DisputedZone_38B.rds'))
pp = st_transform(pp,crs=canada_albers)
pp = st_make_valid(pp)

app = st_intersects(abac, pp,sparse = F)

abacS = abac[app,]

require(patchwork)

v = ggLobsterMap('38',colourLFA = FALSE,return.object = T, addLFAlines =F,addGrids = F)

#continuous
p0t = v + geom_sf(data=subset(abacS,triID>4),aes( fill = NUM_OF_TRAPS))+
  scale_fill_distiller(palette = "YlOrRd",direction = 1,trans='sqrt')+geom_sf(data=pp,colour='blue',fill=NA, linewidth=1.)+labs(title='ContinuousTraps')+theme_test_adam()+theme(legend.position = 'none')

#quantile breaks
br = quantile(abacS$NUM_OF_TRAPS, probs=seq(0,1,length.out=6),na.rm=T)
rbr = round(br)
labs1 =  paste0("[", head(rbr, -1), ", ", tail(rbr, -1), "]")

abacS$qEstNtr = cut(abacS$NUM_OF_TRAPS, breaks=br, include.lowest = T, labels = labs1)

p1t = v +
  geom_sf(data=subset(abacS,triID>4),aes( fill = qEstNtr))+
  scale_fill_brewer(palette = "YlOrRd")+geom_sf(data=pp,colour='blue',fill=NA, linewidth=1)+labs(title='Quantile Break Traps')+theme_test_adam()+theme(legend.position = 'none')

gridExtra::grid.arrange(p1,p0,ncol=2)

###wt
#continuous
p0 = v + geom_sf(data=subset(abacS,triID>4),aes( fill = EST_WEIGHT_LOG_LBS))+
  scale_fill_distiller(palette = "YlOrRd",direction = 1,trans='sqrt')+geom_sf(data=pp,colour='blue',fill=NA, linewidth=1.)+labs(title='Continuous')+theme_test_adam()+theme(legend.position = 'none')

#quantile breaks
br = quantile(abacS$EST_WEIGHT_LOG_LBS, probs=seq(0,1,length.out=6),na.rm=T)
rbr = round(br)
labs1 =  paste0("[", head(rbr, -1), ", ", tail(rbr, -1), "]")

abacS$qEstWt = cut(abacS$EST_WEIGHT_LOG_LBS, breaks=br, include.lowest = T, labels = labs1)

p1 = v +
  geom_sf(data=subset(abacS,triID>4),aes( fill = qEstWt))+
  scale_fill_brewer(palette = "YlOrRd")+geom_sf(data=pp,colour='blue',fill=NA, linewidth=1)+labs(title='Quantile Break')+theme_test_adam()+
  theme(legend.position = 'none')

gridExtra::grid.arrange(p1,p0,p1t, p0t,ncol=2)


###lfa 41
rL = readRDS(file.path(git.repo,'bio.lobster.data','mapping_data',"LFAPolys37Split.rds"))
r4 = subset(rL, PID==41)
r4 = st_transform(r4, canada_albers)
he = readRDS(file.path(git.repo,'bio.lobster.data','mapping_data','eez_hexagons_mar_sf_incl38B.rds'))

rh = st_intersects(he,r4, sparse = F)
he4 = he[rh,]
he4$pid = 1:nrow(he4)
lobster.db('logs41')
logs41$DDLON = logs41$DDLON*-1
a4 = subset(logs41, !is.na(DDLON)  )
a4 = st_as_sf(a4,coords=c('DDLON','DDLAT'),crs=4326)
a4 = st_transform(a4,canada_albers)
a4 = st_join(a4,he4)
a4r = subset(a4,!is.na(pid))
a4r$yr = lubridate::year(a4r$FV_FISHED_DATETIME)
a4r$NUM_OF_TRAPS = as.numeric(a4r$NUM_OF_TRAPS)
a4rr = subset(a4r,yr %in% 2019:2023 & !is.na(EST_WEIGHT_LOG_LBS) )
a4 =aggregate(cbind(NUM_OF_TRAPS,EST_WEIGHT_LOG_LBS)~pid,data=a4rr, FUN=sum)
a44 = st_as_sf(merge(a4,he4))


require(patchwork)

v = ggLobsterMap('41',colourLFA = FALSE,return.object = T, addLFAlines =T,addGrids = F)

#continuous
p0t = v + geom_sf(data=a44,aes( fill = NUM_OF_TRAPS))+
  scale_fill_distiller(palette = "YlOrRd",direction = 1,trans='sqrt')+labs(title='ContinuousTraps')+theme_test_adam()+theme(legend.position = 'none')

#quantile breaks
br = quantile(a44$NUM_OF_TRAPS, probs=seq(0,1,length.out=6),na.rm=T)
rbr = round(br)
labs1 =  paste0("[", head(rbr, -1), ", ", tail(rbr, -1), "]")

a44$qEstNtr = cut(a44$NUM_OF_TRAPS, breaks=br, include.lowest = T, labels = labs1)

p1t = v +
  geom_sf(data=subset(a44),aes( fill = qEstNtr))+
  scale_fill_brewer(palette = "YlOrRd")+labs(title='Quantile Break Traps')+theme_test_adam()+theme(legend.position = 'none')

gridExtra::grid.arrange(p1,p0,ncol=2)

###wt
#continuous
p0 = v + geom_sf(data=subset(a44),aes( fill = EST_WEIGHT_LOG_LBS))+
  scale_fill_distiller(palette = "YlOrRd",direction = 1,trans='sqrt')+labs(title='Continuous')+theme_test_adam()+theme(legend.position = 'none')

#quantile breaks
br = quantile(a44$EST_WEIGHT_LOG_LBS, probs=seq(0,1,length.out=6),na.rm=T)
rbr = round(br)
labs1 =  paste0("[", head(rbr, -1), ", ", tail(rbr, -1), "]")

a44$qEstWt = cut(a44$EST_WEIGHT_LOG_LBS, breaks=br, include.lowest = T, labels = labs1)

p1 = v +
  geom_sf(data=subset(a44),aes( fill = qEstWt))+
  scale_fill_brewer(palette = "YlOrRd")+labs(title='Quantile Break')+theme_test_adam()+
  theme(legend.position = 'none')

gridExtra::grid.arrange(p1,p0,p1t, p0t,ncol=2)
