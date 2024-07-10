#combined footprints 33-38 & 41
require(devtools)
require(bio.lobster)
require(bio.utilities)
require(sf)
load_all('C:/Users/Cooka/Documents/git/bio.utilities')


la()
wd = ('C:\\Users\\Cooka\\OneDrive - DFO-MPO\\CanUsCollabEffortMapping')
setwd(wd)


layerDir=file.path(project.datadirectory("bio.lobster"), "data","maps")

##from inst/IP/FootprintMapping/1.EstimatingTrapHaulsFromSlipsandSplit2Grids.r

Tot = readRDS('DiscretizedData/PrivacyScreened_TrapHauls_Landings_Trips_Gridand.rds')
Tot$LFA = ifelse(Tot$LFA=='31B',312,Tot$LFA)
Tot$LFA = ifelse(Tot$LFA=='31A',311,Tot$LFA)


#making plots of Tot

GrMap = readRDS(file.path(project.datadirectory('bio.lobster'),'data','maps','LFA27-38GridsPrunedtoDepth-sf.rds'))
coa = st_as_sf(readRDS(file.path( project.datadirectory("bio.lobster"), "data","maps","CoastlineSF_NY_NL.rds")))


GrMap1 = GrMap
GrMap1$area = st_area(GrMap1)/1000000
GrMap1$V2 = paste(GrMap1$LFA, GrMap1$GRID_NO,sep="-")
st_geometry(GrMap1)<- NULL
gg = aggregate(area~LFA+GRID_NO,data=GrMap1,FUN=function(x) abs(sum(x)))

GrMap2 =merge(GrMap,gg)

gTot = merge(GrMap2,Tot,by.x=c('LFA','GRID_NO'),by.y=c('LFA','Grid'),all.x=T)


r<-readRDS(file.path( layerDir,"GridPolysSF.rds"))
b=subset(r,LFA %in% c(33:38))

o=subset(GrMap,LFA %in% c(33:38))

ggplot(b)+
  geom_sf()+
  geom_sf(data=coa,fill='grey')+
  geom_sf(data=o,fill='red')+
  coord_sf(xlim = c(st_bbox(b)$xmin,st_bbox(b)$xmax),
           ylim = c(st_bbox(b)$ymin,st_bbox(b)$ymax),
           expand = FALSE)


gTot$CPUE = gTot$Landings/gTot$TrapHauls
g27p = subset(gTot, LFA%in% 33:38 & FishingYear%in%2016:2023)

ok1 = ggplot(g27p,aes(fill=CPUE))+
  geom_sf() +
  scale_fill_distiller(trans='identity',palette='Spectral') +
  facet_wrap(~FishingYear)+
  #  geom_sf(data=g27n,fill='white')+  
  geom_sf(data=coa,fill='grey')+
  geom_sf(data=GrMap,fill=NA)+
  coord_sf(xlim = c(st_bbox(g27p)$xmin,st_bbox(g27p)$xmax),
           ylim = c(st_bbox(g27p)$ymin,st_bbox(g27p)$ymax),
           expand = FALSE)+
  scale_x_continuous(breaks = c(round(seq(st_bbox(g27p)$xmin,st_bbox(g27p)$xmax,length.out=2),2)))+
  scale_y_continuous(breaks = c(round(seq(st_bbox(g27p)$ymin,st_bbox(g27p)$ymax,length.out=2),2)))

gl = subset(g27p,FishingYear==2016)
gp = subset(g27p,FishingYear==2023)

gl$geometry<- NULL

gg = merge(gp,gl[,c('LFA','GRID_NO','CPUE')],by=c('LFA','GRID_NO'))


percent_diff <- function(row) {
  row$geometry<- NULL
  
  abs_diff <- (as.numeric(row[1]) - as.numeric(row[2]))
  mean_val <- mean(as.numeric(row))
  percent_diff <- (abs_diff / mean_val) * 100
  return(percent_diff)
}

gg$percentChange =  apply(gg[,c('CPUE.x','CPUE.y')],1,percent_diff)


require(colorspace)
ggplot(subset(gg,PrivacyScreen==1),aes(fill=percentChange))+
  geom_sf() +
  scale_fill_continuous_diverging(palette='Purple-Green') +
  #facet_wrap(~FishingYear)+
  #  geom_sf(data=g27n,fill='white')+  
  geom_sf(data=coa,fill='grey')+
  geom_sf(data=GrMap,fill=NA)+
  coord_sf(xlim = c(st_bbox(g27p)$xmin,st_bbox(g27p)$xmax),
           ylim = c(st_bbox(g27p)$ymin,st_bbox(g27p)$ymax),
           expand = FALSE)+
  scale_x_continuous(breaks = c(round(seq(st_bbox(g27p)$xmin,st_bbox(g27p)$xmax,length.out=2),2)))+
  scale_y_continuous(breaks = c(round(seq(st_bbox(g27p)$ymin,st_bbox(g27p)$ymax,length.out=2),2)))



##LFA 41
lobster.db('process.logs41')
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
grid_in_polygon$LFA <- NULL
ggplot(l4)+geom_sf()+geom_sf(data=grid_in_polygon,col='red')


#joined logbooks and grids for 41

jj = st_join(lo41,grid_in_polygon)
jj$yr = year(jj$FV_FISHED_DATETIME)

jja = aggregate(cbind(NUM_OF_TRAPS,ADJCATCH_KG)~ID+yr,data=jj,FUN=sum)
jja = merge(jja,grid_in_polygon)

jja = st_as_sf(jja)

j23 = subset(jja,yr==2023)

names(j23)[1:4]=c('GRID_NO','FishingYear','TrapHauls','Landings')

j23$CPUE = j23$Landings/j23$TrapHauls

#####
ggplot(subset(gg),aes(fill=Landings))+
  geom_sf() +
  geom_sf(data=j23,aes(fill=Landings))+
  scale_fill_continuous_diverging(palette='Purple-Green') +
  #facet_wrap(~FishingYear)+
  #  geom_sf(data=g27n,fill='white')+  
  geom_sf(data=coa,fill='grey')+
  geom_sf(data=GrMap,fill=NA)+
 geom_sf(data=grid_in_polygon,fill=NA)+
  coord_sf(xlim = c(st_bbox(gg)$xmin,st_bbox(gg)$xmax),
           ylim = c(st_bbox(j23)$ymin,st_bbox(gg)$ymax),
           expand = FALSE)+ggtitle('Landings Inshore 2022-2023 FY; Offshore 2023 Calendar Year')
  
ggplot(subset(gg),aes(fill=TrapHauls))+
  geom_sf() +
  geom_sf(data=j23,aes(fill=TrapHauls))+
  scale_fill_continuous_diverging(palette='Purple-Green') +
  #facet_wrap(~FishingYear)+
  #  geom_sf(data=g27n,fill='white')+  
  geom_sf(data=coa,fill='grey')+
  geom_sf(data=GrMap,fill=NA)+
  geom_sf(data=grid_in_polygon,fill=NA)+
  coord_sf(xlim = c(st_bbox(gg)$xmin,st_bbox(gg)$xmax),
           ylim = c(st_bbox(j23)$ymin,st_bbox(gg)$ymax),
           expand = FALSE)+ggtitle('Traphauls Inshore 2022-2023 FY; Offshore 2023 Calendar Year')

ggplot(subset(gg),aes(fill=CPUE.x))+
  geom_sf() +
  geom_sf(data=j23,aes(fill=CPUE))+
  scale_fill_continuous_diverging(palette='Purple-Green') +
  #facet_wrap(~FishingYear)+
  #  geom_sf(data=g27n,fill='white')+  
  geom_sf(data=coa,fill='grey')+
  geom_sf(data=GrMap,fill=NA)+
  geom_sf(data=grid_in_polygon,fill=NA)+
  coord_sf(xlim = c(st_bbox(gg)$xmin,st_bbox(gg)$xmax),
           ylim = c(st_bbox(j23)$ymin,st_bbox(gg)$ymax),
           expand = FALSE)+ggtitle('CPUE Inshore 2022-2023 FY; Offshore 2023 Calendar Year')

