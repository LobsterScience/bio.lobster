require(sf)
require(bio.lobster)
require(devtools)
la()
r = readRDS(file.path(git.repo,'bio.lobster.data','mapping_data','NAFO_sf.rds'))
g = lobster.db('process.logs41.unfiltered')
g$DDLON = g$DDLON*-1
g$yr= lubridate::year(g$FV_FISHED_DATETIME)
r = subset(r,NAFO %in% c('5ZEJC','5ZEMC'))
gs = st_as_sf(subset(g,!is.na(DDLON)),coords=c('DDLON','DDLAT'),crs=4326)

grs = st_join(gs,r,join=st_within)

to = aggregate(ADJCATCH_KG~yr,data=grs,FUN=sum)
fz = aggregate(ADJCATCH_KG~yr,data=subset(grs,!is.na(NAFO)),FUN=sum)
names(fz)[2] = 'AdjCatch5z'

tf = merge(to,fz)
tf$Prop5z = tf$AdjCatch5z / tf$ADJCATCH_KG
aggregate(ADJCATCH_KG~yr,data=g,FUN=sum)
write.csv(tf,file= ('C:\\Users\\Cooka\\OneDrive - DFO-MPO\\Requests/Sr Management/LFA41/LFA41Landingsin5z.csv'))
g4 = ggLobsterMap('41',return.object = T,colourLFA = F)

g4+geom_sf(data=subset(gs,OFFAREA != 'UNKNOWN' & yr>2014 & !is.na(yr)))+facet_wrap(~yr)




#####
#combined footprints 33-38 & 41
require(devtools)
require(bio.lobster)
require(bio.utilities)
require(sf)
load_all('C:/Users/Zissersonb/Documents/git/bio.utilities')


la()
wd = ('C:\\Users\\Cooka\\OneDrive - DFO-MPO\\CanUsCollabEffortMapping')
setwd(wd)


layerDir=file.path(project.datadirectory("bio.lobster"), "data","maps")

##from inst/IP/FootprintMapping/1.EstimatingTrapHaulsFromSlipsandSplit2Grids.r

Tot = readRDS('DiscretizedData/PrivacyScreened_TrapHauls_Landings_Trips_Gridand.rds')
Tot = subset(Tot,LFA %in% c(33,34,35,36,37,38))

#making plots of Tot

GrMap = readRDS(file.path(project.datadirectory('bio.lobster'),'data','maps','LFA27-38GridsPrunedtoDepth-sf.rds'))
coa = st_as_sf(readRDS(file.path( project.datadirectory("bio.lobster"), "data","maps","CoastlineSF_NY_NL.rds")))
GrMap = subset(GrMap,LFA %in% c(33,34,35,36,37,38))


GrMap1 = GrMap
GrMap1$area = st_area(GrMap1)/1000000
GrMap1$V2 = paste(GrMap1$LFA, GrMap1$GRID_NO,sep="-")
st_geometry(GrMap1)<- NULL
gg = aggregate(area~LFA+GRID_NO,data=GrMap1,FUN=function(x) abs(sum(x)))

GrMap2 =merge(GrMap,gg)

gTot = merge(GrMap2,Tot,by.x=c('LFA','GRID_NO'),by.y=c('LFA','Grid'),all.x=T)


r<-readRDS(file.path( layerDir,"GridPolysSF.rds"))
b=subset(r,LFA %in% c(33:38))

g27p = subset(gTot, LFA%in% 33:38 & FishingYear%in%2016:2025)


##LFA 41
logs41p = lobster.db('process.logs41')
logs41p$DDLON = logs41p$DDLON*-1
logs41p = subset(logs41p, !is.na(DDLON) )
lo41 = st_as_sf(logs41p,coords = c('DDLON','DDLAT'),crs=4326)

l4 = readRDS(file.path(project.datadirectory('bio.lobster'),'data','maps','LFAPolysSF.rds'))
l4 = subset(l4,LFA==41, select=LFA)
bbox <- st_bbox(l4)
grid <- st_make_grid(bbox, cellsize = c(0.17, 0.17), what = "polygons")
grid_polygons <- st_as_sf(grid)

grid_in_polygon <- st_intersection(grid_polygons, l4)
grid_in_polygon$ID = 1:nrow(grid_in_polygon)
grid_in_polygon$LFA <- 41
ggplot(l4)+geom_sf()+geom_sf(data=grid_in_polygon,col='red')

#joined logbooks and grids for 41

jj = st_join(lo41,grid_in_polygon)
jj$yr = year(jj$FV_FISHED_DATETIME)

jja = aggregate(cbind(NUM_OF_TRAPS,ADJCATCH_KG)~ID+yr,data=jj,FUN=sum)
jja = merge(jja,grid_in_polygon)

jja = st_as_sf(jja)

##two separate colour scales 
require(ggnewscale)
#####

names(jja)[1:4]=c('GRID_NO','FishingYear','TrapHauls','Landings')

ggplot()+
  geom_sf(data=subset(g27p,FishingYear==2021),aes(fill=Landings/1000)) +
  scale_fill_continuous_diverging(palette='Purple-Green',name='LFA 33 & 34') +
  new_scale_fill() +
  geom_sf(data=subset(jja,FishingYear==2021),aes(fill=Landings/1000))+
  scale_fill_continuous_diverging(palette='Blue-Red',name='LFA 41') +
  geom_sf(data=coa,fill='grey')+
  geom_sf(data=GrMap,fill=NA)+
  geom_sf(data=grid_in_polygon,fill=NA)+
  coord_sf(xlim = c(st_bbox(g27p)$xmin,st_bbox(g27p)$xmax),
           ylim = c(st_bbox(jja)$ymin,st_bbox(g27p)$ymax),
           expand = FALSE)+ggtitle('Landings Inshore 2020-2021 FY; Offshore 2021 Calendar Year')


###33 34 adjacent grids
grids = c(118,119,149,132,148,167,185,186,204,205,206,222,223,224,207:214,485:496)
l3 = g27p
l3 = subset(l3,LFA %in% c(33,34))
l3$Adj = 0 
l3$Adj[which(l3$GRID_NO %in% grids)] <- 1

l3a = aggregate(cbind(Landings,Landings*Adj)~LFA+FishingYear,data=subset(l3,FishingYear>2004 &FishingYear<2026),FUN=sum)
l3a$PropAdj = l3a$V2/l3a$Landings*100
names(l3a)[4] <- 'PropLandingsAdj240_41'
write.csv(l3a,file= ('C:\\Users\\Cooka\\OneDrive - DFO-MPO\\Requests/Sr Management/LFA41/LFA41Landingsin33_34_adjacent_40_41.csv'))


ggplot()+
  geom_sf(data=subset(subset(g27p,GRID_NO %in% grids),FishingYear==2020),aes(fill=Landings/1000)) +
  scale_fill_continuous_diverging(palette='Purple-Green',name='LFA 33 & 34') +
  new_scale_fill() +
  geom_sf(data=subset(jja,FishingYear==2020),aes(fill=Landings/1000))+
  scale_fill_continuous_diverging(palette='Blue-Red',name='LFA 41') +
  geom_sf(data=coa,fill='grey')+
  geom_sf(data=GrMap,fill=NA)+
  geom_sf(data=grid_in_polygon,fill=NA)+
  coord_sf(xlim = c(st_bbox(g27p)$xmin,st_bbox(g27p)$xmax),
           ylim = c(st_bbox(jja)$ymin,st_bbox(g27p)$ymax),
           expand = FALSE)+ggtitle('Landings Inshore 2019-2020 FY; Offshore 2020 Calendar Year')




ggplot()+
  geom_sf(data=subset(subset(g27p,GRID_NO %in% grids),FishingYear==2021),aes(fill=Landings/1000)) +
  scale_fill_continuous_diverging(palette='Purple-Green',name='LFA 33 & 34') +
  new_scale_fill() +
  geom_sf(data=subset(jja,FishingYear==2021),aes(fill=Landings/1000))+
  scale_fill_continuous_diverging(palette='Blue-Red',name='LFA 41') +
  geom_sf(data=coa,fill='grey')+
  geom_sf(data=GrMap,fill=NA)+
  geom_sf(data=grid_in_polygon,fill=NA)+
  coord_sf(xlim = c(st_bbox(g27p)$xmin,st_bbox(g27p)$xmax),
           ylim = c(st_bbox(jja)$ymin,st_bbox(g27p)$ymax),
           expand = FALSE)+ggtitle('Landings Inshore 2020-2021 FY; Offshore 2021 Calendar Year')



ggplot()+
  geom_sf(data=subset(subset(g27p,GRID_NO %in% grids),FishingYear==2022),aes(fill=Landings/1000)) +
  scale_fill_continuous_diverging(palette='Purple-Green',name='LFA 33 & 34') +
  new_scale_fill() +
  geom_sf(data=subset(jja,FishingYear==2022),aes(fill=Landings/1000))+
  scale_fill_continuous_diverging(palette='Blue-Red',name='LFA 41') +
  geom_sf(data=coa,fill='grey')+
  geom_sf(data=GrMap,fill=NA)+
  geom_sf(data=grid_in_polygon,fill=NA)+
  coord_sf(xlim = c(st_bbox(g27p)$xmin,st_bbox(g27p)$xmax),
           ylim = c(st_bbox(jja)$ymin,st_bbox(g27p)$ymax),
           expand = FALSE)+ggtitle('Landings Inshore 2021-2022 FY; Offshore 2022 Calendar Year')



ggplot()+
  geom_sf(data=subset(subset(g27p,GRID_NO %in% grids),FishingYear==2023),aes(fill=Landings/1000)) +
  scale_fill_continuous_diverging(palette='Purple-Green',name='LFA 33 & 34') +
  new_scale_fill() +
  geom_sf(data=subset(jja,FishingYear==2023),aes(fill=Landings/1000))+
  scale_fill_continuous_diverging(palette='Blue-Red',name='LFA 41') +
  geom_sf(data=coa,fill='grey')+
  geom_sf(data=GrMap,fill=NA)+
  geom_sf(data=grid_in_polygon,fill=NA)+
  coord_sf(xlim = c(st_bbox(g27p)$xmin,st_bbox(g27p)$xmax),
           ylim = c(st_bbox(jja)$ymin,st_bbox(g27p)$ymax),
           expand = FALSE)+ggtitle('Landings Inshore 2022-2023 FY; Offshore 2023 Calendar Year')



ggplot()+
  geom_sf(data=subset(subset(g27p,GRID_NO %in% grids),FishingYear==2024),aes(fill=Landings/1000)) +
  scale_fill_continuous_diverging(palette='Purple-Green',name='LFA 33 & 34') +
  new_scale_fill() +
  geom_sf(data=subset(jja,FishingYear==2024),aes(fill=Landings/1000))+
  scale_fill_continuous_diverging(palette='Blue-Red',name='LFA 41') +
  geom_sf(data=coa,fill='grey')+
  geom_sf(data=GrMap,fill=NA)+
  geom_sf(data=grid_in_polygon,fill=NA)+
  coord_sf(xlim = c(st_bbox(g27p)$xmin,st_bbox(g27p)$xmax),
           ylim = c(st_bbox(jja)$ymin,st_bbox(g27p)$ymax),
           expand = FALSE)+ggtitle('Landings Inshore 2023-2024 FY; Offshore 2024 Calendar Year')


ggplot()+
  geom_sf(data=subset(subset(g27p,GRID_NO %in% grids),FishingYear==2025),aes(fill=Landings/1000)) +
  scale_fill_continuous_diverging(palette='Purple-Green',name='LFA 33 & 34') +
  new_scale_fill() +
  geom_sf(data=subset(jja,FishingYear==2025),aes(fill=Landings/1000))+
  scale_fill_continuous_diverging(palette='Blue-Red',name='LFA 41') +
  geom_sf(data=coa,fill='grey')+
  geom_sf(data=GrMap,fill=NA)+
  geom_sf(data=grid_in_polygon,fill=NA)+
  coord_sf(xlim = c(st_bbox(g27p)$xmin,st_bbox(g27p)$xmax),
           ylim = c(st_bbox(jja)$ymin,st_bbox(g27p)$ymax),
           expand = FALSE)+ggtitle('Landings Inshore 2024-2025 FY; Offshore 2025 Calendar Year')

