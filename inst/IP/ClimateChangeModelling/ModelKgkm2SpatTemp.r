
require(sdmTMB)
require(bio.lobster)
require(bio.utilities)
require(lubridate)
require(devtools)
require(dplyr)
require(ggplot2)
require(INLA)
options(stringAsFactors=F)
require(PBSmapping)
require(SpatialHub)
require(sf)
la()
fd=file.path(project.datadirectory('bio.lobster'),'analysis','ClimateModelling')
dir.create(fd,showWarnings=F)
setwd(fd)
aT = readRDS(file=file.path(project.datadirectory('bio.lobster'),'data','BaseDataForClimateModel.rds'))
sf_use_s2(FALSE) #needed for cropping

ns_coast =readRDS(file.path( project.datadirectory("bio.lobster"), "data","maps","CoastSF.rds"))
st_crs(ns_coast) <- 4326 # 'WGS84'; necessary on some installs
crs_utm20 <- 32620
ns_coast <- suppressWarnings(suppressMessages(
  st_crop(ns_coast,
          c(xmin = -68, ymin = 41, xmax = -56.5, ymax = 47.5))))

ns_coast <- st_transform(ns_coast, crs_utm20)

st_crs(ns_coast) <- 4326 # 'WGS84'; necessary on some installs
crs_utm20 <- 32620

# Project our survey data coordinates:
survey <- aT %>%   
  st_as_sf()
    
surv_utm_coords <- st_coordinates(survey)

survey$X1000 <- surv_utm_coords[,1] 
survey$Y1000 <- surv_utm_coords[,2] 

spde <- make_mesh(as_tibble(survey), xy_cols = c("X1000", "Y1000"),
                   n_knots=600,type = "cutoff_search")
plot(spde)

# Add on the barrier mesh component:
bspde <- add_barrier_mesh(
  spde, ns_coast, range_fraction = 0.1,
  proj_scaling = 1000, plot = TRUE
)


survey$lZ = log(survey$CanZ)

#what is the right offset for gear
#km2 for tows is estimated from sensors
#km2 for traps from Watson et al 2009 NJZ MFR 43 1 -- home radius of 17m, bait radius of 11m == 28m 'attraction zone'
# pi*(.014^2) # assuming traps are independent
survey$W = ceiling(yday(survey$DATE)/366*25)

i = which(survey$OFFSET_METRIC == 'Number of traps')
survey$OFFSET[i] = survey$OFFSET[i] * pi*(.014^2)
survey$LO = log(survey$OFFSET)
survey$BT = survey$HadBT
fit = sdmTMB(WEIGHT_KG~
               s(lZ,k=5)+s(BT),
             data=as_tibble(survey),
            offset = 'LO',
             time='W', 
             mesh=bspde,
             family=tweedie(link='log'),
             spatial='on',
             spatiotemporal='ar1')

go =predict(fit) 
go$pred = fit$family$linkinv(go$est)



d = dir(file.path(bio.datadirectory,'bio.lobster','Temperature Data/QuinnBT-2022/'),full.names = T)
d = d[grep('Can',d)]
x = read.table(d[7],header=T)
x$Date = as.Date(as.character(x$DateYYYYMMDD), format='%Y%m%d')
x = subset(x,Depth_m<400,select=c(Date,Time,Longitude,Latitude,BottomTemp,Depth_m))
 
x = subset(x,Time==1) 
x = subset(x,Depth_m<400)
x = x %>% st_as_sf(coords = c("Longitude",'Latitude'),crs=4326) %>% st_transform(crs_utm20)

x_utm_coords <- st_coordinates(x)

x$X1000 <- x_utm_coords[,1] / 1000
x$Y1000 <- x_utm_coords[,2] / 1000

x = bio.utilities::rename.df(x,c('BottomTemp','Depth_m'),c('BT','z'))
x$lZ = log(x$z)

x = as_tibble(subset(x,select=c(BT,X1000,Y1000,lZ)))
x$geometry=NULL
be = as.data.frame(sapply(x,rep.int,27))
be$W = rep(0:26,each=dim(x)[1])

be= as_tibble(be)

g = predict(fit,newdata=(be))

  g$pred = fit$family$linkinv(g$est)

  gsf = st_as_sf(g,coords = c("X1000","Y1000"),crs=32620,remove=F)

rL = readRDS(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolysSF.rds"))
rL = st_as_sf(rL)
st_crs(rL) <- 4326
rL = st_transform(rL,32620) 
st_geometry(rL) <- st_geometry(st_as_sf(rL$geometry/1000)) 
st_crs(rL) <- 32620




#Maps
mm = c(0.001,max(gsf$pred))
ggplot(subset(gsf,W %in% 0:26)) +
  geom_sf(aes(fill=pred,color=pred)) + 
  scale_fill_viridis_c(trans='log',limits=mm) +
  scale_color_viridis_c(trans='log',limits=mm) +
  facet_wrap(~W) +
  geom_sf(data=rL,size=1,colour='black',fill=NA)+
  theme( axis.ticks.x = element_blank(),
         axis.text.x = element_blank(),
         axis.title.x = element_blank(),
         axis.ticks.y = element_blank(),
         axis.text.y = element_blank(),
         axis.title.y = element_blank()
  ) +
  coord_sf()
savePlot('wtWeek600k.png') 









saveRDS(list(data=survey,grid=bspde,preds=gsf,model=fit),file='AllwtTw600k.rds')

a=readRDS('AllwtTw600k.rds')
survey=a[[1]]
bspde=a[[2]]
gsf=st_as_sf(a[[3]])
fit=a[[4]]

gsf$X = gsf$X1000*1000
gsf$Y = gsf$Y1000*1000
st_geometry(gsf) <-NULL
gsf = st_as_sf(gsf,coords=c('X','Y'))
st_crs(gsf) <- 32620

rL = readRDS(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolysSF.rds"))
st_crs(rL) <- 4326
rL <- st_transform(rL, 32620)
rl = st_coordinates(rL)
rl[,1] = rl[,1]/1000
rl[,2] = rl[,2]/1000

rl = st_as_sf(rl,)

i = st_intersects(gsf,rL,sparse=F)
    gsf$i = unlist(apply(i,1,function(x) ifelse(any(x), which(x),NA))) #this generates the index for LFAs, without this points outside LFAs dont get counted and make a mess
gsf$LFA = rL$LFA[gsf$i]
gsf1 = gsf

st_geometry(gsf) <-NULL

ggplot(subset(gsf,!is.na(LFA)),aes(log(pred)))+
  geom_histogram(aes(y=..density..),position='identity')+
  facet_wrap(~LFA)+
  geom_vline(xintercept=0)
  savePlot('DistributionOfPredictedDensity.png') 


###predictions
Can = readRDS('CanProjectionSurfaces.rds')
  x = bio.utilities::rename.df(Can,c('BottomTemp'),c('BT'))

x= as_tibble(x)
x1 = subset(x,W==1)
x2 = subset(x,W==25)
x1$W=0
x2$W=26
x = rbind(x,x1)
x = rbind(x,x2)

#####2035
xr = subset(x,rYR==2032)

g = predict(fit,newdata=(xr))

  g$pred = fit$family$linkinv(g$est)

  gsf35 = st_as_sf(g,coords = c("X1000","Y1000"),crs=32620,remove=F)

rL = readRDS(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolysSF.rds"))
rL = st_as_sf(rL)
st_crs(rL) <- 4326
rL = st_transform(rL,32620) 
st_geometry(rL) <- st_geometry(st_as_sf(rL$geometry/1000)) 
st_crs(rL) <- 32620


i = st_intersects(gsf35,rL,sparse=F)
    gsf35$i = unlist(apply(i,1,function(x) ifelse(any(x), which(x),NA))) #this generates the index for LFAs, without this points outside LFAs dont get counted and make a mess
gsf35$LFA = rL$LFA[gsf35$i]
gsf35a = gsf35
st_geometry(gsf35) <-NULL

ggplot(subset(gsf35,!is.na(LFA)),aes(log(pred)))+
  geom_histogram(aes(y=..density..),position='identity')+
  facet_wrap(~LFA)+
  geom_vline(xintercept=0)
  savePlot('DistributionOfPredictedDensity35Y.png') 

mm = c(0.001,max(gsf35$pred))
ggplot(subset(gsf35a)) +
  geom_sf(aes(fill=pred,color=pred)) + 
  scale_fill_viridis_c(trans='log',limits=mm) +
  scale_color_viridis_c(trans='log',limits=mm) +
  facet_wrap(~W) +
  #geom_sf(data=rL,size=1,colour='black',fill=NA)+
  theme( axis.ticks.x = element_blank(),
         axis.text.x = element_blank(),
         axis.title.x = element_blank(),
         axis.ticks.y = element_blank(),
         axis.text.y = element_blank(),
         axis.title.y = element_blank()
  ) +
  coord_sf()
savePlot('wt600k2032.png') 

oo = list()
w = unique(gsf1$W)
for(i in 1:length(w)){
oo[[i]] = st_join(subset(gsf1,W==w[i]),subset(gsf35a,W=w[i],select=pred),join=st_nearest_feature,left=TRUE)
}

gg = do.call(rbind,oo)
gg$diff = gg$pred.y - gg$pred.x
gg$negative = abs(gg$diff)
gg$positive = gg$diff
mm = c(0.01,max(gg$positive))
require(ggnewscale)
ggplot(subset(gg,W %in% seq(0,26,4) & diff>0)) +
  geom_sf(aes(fill=positive,color=positive)) + 
  scale_fill_gradient(low = "white", high = "red", na.value = NA,trans='log',limits=mm)+
  scale_color_gradient(trans='log',low = "white", high = "red", na.value = NA,limits=mm) +
new_scale_color()+
new_scale_fill()+
  geom_sf(data=subset(gg,W %in% seq(0,26,4) & diff<0))+
  aes(fill=negative,color=negative) + 
  scale_fill_gradient(low = "white", high = "blue", na.value = NA,trans='log',limits=mm)+
  scale_color_gradient(trans='log',low = "white", high = "blue", na.value = NA,limits=mm) +
  facet_wrap(~W) +
  
  #geom_sf(data=rL,size=1,colour='black',fill=NA)+
  theme( axis.ticks.x = element_blank(),
         axis.text.x = element_blank(),
         axis.title.x = element_blank(),
         axis.ticks.y = element_blank(),
         axis.text.y = element_blank(),
         axis.title.y = element_blank()
  ) +
  coord_sf()
savePlot('wt600kdiff2032-2022.png') 


########2055



xr = subset(x,rYR==2052)

g = predict(fit,newdata=(xr))

  g$pred = fit$family$linkinv(g$est)

  gsf55 = st_as_sf(g,coords = c("X1000","Y1000"),crs=32620,remove=F)

rL = readRDS(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolysSF.rds"))
rL = st_as_sf(rL)
st_crs(rL) <- 4326
rL = st_transform(rL,32620) 
st_geometry(rL) <- st_geometry(st_as_sf(rL$geometry/1000)) 
st_crs(rL) <- 32620


i = st_intersects(gsf55,rL,sparse=F)
    gsf55$i = unlist(apply(i,1,function(x) ifelse(any(x), which(x),NA))) #this generates the index for LFAs, without this points outside LFAs dont get counted and make a mess
gsf55$LFA = rL$LFA[gsf55$i]
gsf55a = gsf55
st_geometry(gsf55) <-NULL

ggplot(subset(gsf55,!is.na(LFA)),aes(log(pred)))+
  geom_histogram(aes(y=..density..),position='identity')+
  facet_wrap(~LFA)+
  geom_vline(xintercept=0)
  savePlot('DistributionOfPredictedDensity55Y.png') 

mm = c(0.001,max(gsf55$pred))
ggplot(subset(gsf55a)) +
  geom_sf(aes(fill=pred,color=pred)) + 
  scale_fill_viridis_c(trans='log',limits=mm) +
  scale_color_viridis_c(trans='log',limits=mm) +
  facet_wrap(~W) +
  #geom_sf(data=rL,size=1,colour='black',fill=NA)+
  theme( axis.ticks.x = element_blank(),
         axis.text.x = element_blank(),
         axis.title.x = element_blank(),
         axis.ticks.y = element_blank(),
         axis.text.y = element_blank(),
         axis.title.y = element_blank()
  ) +
  coord_sf()
savePlot('wt600k2052.png') 

oo = list()
w = unique(gsf1$W)
for(i in 1:length(w)){
oo[[i]] = st_join(subset(gsf1,W==w[i]),subset(gsf55a,W=w[i],select=pred),join=st_nearest_feature,left=TRUE)
}

gg = do.call(rbind,oo)
gg$diff = gg$pred.y - gg$pred.x
gg$negative = abs(gg$diff)
gg$positive = gg$diff
mm = c(0.01,max(gg$positive))
require(ggnewscale)
ggplot(subset(gg,W %in% seq(0,26,4) & diff>0)) +
  geom_sf(aes(fill=positive,color=positive)) + 
  scale_fill_gradient(low = "white", high = "red", na.value = NA,trans='log',limits=mm)+
  scale_color_gradient(trans='log',low = "white", high = "red", na.value = NA,limits=mm) +
new_scale_color()+
new_scale_fill()+
  geom_sf(data=subset(gg,W %in% seq(0,26,4) & diff<0))+
  aes(fill=negative,color=negative) + 
  scale_fill_gradient(low = "white", high = "blue", na.value = NA,trans='log',limits=mm)+
  scale_color_gradient(trans='log',low = "white", high = "blue", na.value = NA,limits=mm) +
  facet_wrap(~W) +
  
  #geom_sf(data=rL,size=1,colour='black',fill=NA)+
  theme( axis.ticks.x = element_blank(),
         axis.text.x = element_blank(),
         axis.title.x = element_blank(),
         axis.ticks.y = element_blank(),
         axis.text.y = element_blank(),
         axis.title.y = element_blank()
  ) +
  coord_sf()
savePlot('wt600kdiff2052-2022.png') 


#2097



xr = subset(x,rYR==2097)

g = predict(fit,newdata=(xr))

  g$pred = fit$family$linkinv(g$est)

  gsf97 = st_as_sf(g,coords = c("X1000","Y1000"),crs=32620,remove=F)

rL = readRDS(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolysSF.rds"))
rL = st_as_sf(rL)
st_crs(rL) <- 4326
rL = st_transform(rL,32620) 
st_geometry(rL) <- st_geometry(st_as_sf(rL$geometry/1000)) 
st_crs(rL) <- 32620


i = st_intersects(gsf97,rL,sparse=F)
    gsf97$i = unlist(apply(i,1,function(x) ifelse(any(x), which(x),NA))) #this generates the index for LFAs, without this points outside LFAs dont get counted and make a mess
gsf97$LFA = rL$LFA[gsf97$i]
gsf97a = gsf97
st_geometry(gsf97) <-NULL

ggplot(subset(gsf97,!is.na(LFA)),aes(log(pred)))+
  geom_histogram(aes(y=..density..),position='identity')+
  facet_wrap(~LFA)+
  geom_vline(xintercept=0)
  savePlot('DistributionOfPredictedDensity97Y.png') 

mm = c(0.001,max(gsf97$pred))
ggplot(subset(gsf97a)) +
  geom_sf(aes(fill=pred,color=pred)) + 
  scale_fill_viridis_c(trans='log',limits=mm) +
  scale_color_viridis_c(trans='log',limits=mm) +
  facet_wrap(~W) +
  #geom_sf(data=rL,size=1,colour='black',fill=NA)+
  theme( axis.ticks.x = element_blank(),
         axis.text.x = element_blank(),
         axis.title.x = element_blank(),
         axis.ticks.y = element_blank(),
         axis.text.y = element_blank(),
         axis.title.y = element_blank()
  ) +
  coord_sf()
savePlot('wt600k2097.png') 

oo = list()
w = unique(gsf1$W)
for(i in 1:length(w)){
oo[[i]] = st_join(subset(gsf1,W==w[i]),subset(gsf97a,W=w[i],select=pred),join=st_nearest_feature,left=TRUE)
}

gg97 = do.call(rbind,oo)
gg97$diff = gg97$pred.y - gg97$pred.x
gg97$negative = abs(gg97$diff)
gg97$positive = gg97$diff
mm = c(0.01,max(gg97$positive))
require(ggnewscale)
ggplot(subset(gg97,W %in% seq(0,26,4) & diff>0)) +
  geom_sf(aes(fill=positive,color=positive)) + 
  scale_fill_gradient(low = "white", high = "red", na.value = NA,trans='log',limits=mm)+
  scale_color_gradient(trans='log',low = "white", high = "red", na.value = NA,limits=mm) +
new_scale_color()+
new_scale_fill()+
  geom_sf(data=subset(gg97,W %in% seq(0,26,4) & diff<0))+
  aes(fill=negative,color=negative) + 
  scale_fill_gradient(low = "white", high = "blue", na.value = NA,trans='log',limits=mm)+
  scale_color_gradient(trans='log',low = "white", high = "blue", na.value = NA,limits=mm) +
  facet_wrap(~W) +
  
  #geom_sf(data=rL,size=1,colour='black',fill=NA)+
  theme( axis.ticks.x = element_blank(),
         axis.text.x = element_blank(),
         axis.title.x = element_blank(),
         axis.ticks.y = element_blank(),
         axis.text.y = element_blank(),
         axis.title.y = element_blank()
  ) +
  coord_sf()
savePlot('wt600kdiff2097-2022.png') 



oo = list()
w = unique(gsf1$W)
for(i in 1:length(w)){
oo[[i]] = st_join(subset(gsf55a,W==w[i]),subset(gsf97a,W=w[i],select=pred),join=st_nearest_feature,left=TRUE)
}

gg5597 = do.call(rbind,oo)
gg5597$diff = gg5597$pred.y - gg5597$pred.x
gg5597$negative = abs(gg5597$diff)
gg97$positive = gg97$diff
