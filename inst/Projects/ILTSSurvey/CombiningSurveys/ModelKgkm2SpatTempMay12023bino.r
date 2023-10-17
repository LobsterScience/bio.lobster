
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
fd=file.path(project.datadirectory('bio.lobster'),'analysis','CombiningSurveys')
dir.create(fd,showWarnings=F)
setwd(fd)
survey = readRDS(file=file.path(project.datadirectory('bio.lobster'),'data','BaseDataForClimateModel.rds'))
sf_use_s2(FALSE) #needed for cropping

# Project our survey data coordinates:

survey$lZ = log(survey$z)
survey = subset(survey,!is.na(lZ))
survey = subset(survey,!is.na(Lobster))

#what is the right offset for gear
#km2 for tows is estimated from sensors
#km2 for traps from Watson et al 2009 NJZ MFR 43 1 -- home radius of 17m, bait radius of 11m == 28m 'attraction zone'
# pi*(.014^2) # assuming traps are independent
survey$W = ceiling(yday(survey$DATE)/366*25)


mi= readRDS(file=file.path(project.datadirectory('bio.lobster'),'analysis','ClimateModelling','tempCatchability.rds'))


mi = mi[,c('temp','res')]
names(mi) = c('GlT','OFFSETcorr')
mi$GlT = round(mi$GlT,1)
mi = aggregate(OFFSETcorr~GlT,data=mi,FUN=mean)
survey$GlT = round(survey$GlT,1)
survey = dplyr::full_join(survey,mi)
survey$OFFSETcorr[which(survey$GlT< -.7)] <- 0
survey$OFFSETcorr[which(survey$GlT> 17)] <- 1

i = which(survey$OFFSET_METRIC == 'Number of traps')
survey$OFFSET[i] = survey$OFFSET[i] * pi*(.014^2) * survey$OFFSETcorr[i]

survey$LO = log(survey$OFFSET)
survey = subset(survey,OFFSET>0.00001 & OFFSET< 0.12)
survey$BT = survey$GlT
i = which(survey$Lobster>50 & survey$SOURCE =='AT_SEA_SAMPLES')
survey = survey[-i,]
i = which(survey$Lobster>5000)

ns_coast =readRDS(file.path( project.datadirectory("bio.lobster"), "data","maps","CoastSF.rds"))
st_crs(ns_coast) <- 4326 # 'WGS84'; necessary on some installs
crs_utm20 <- 32620
ns_coast <- suppressWarnings(suppressMessages(
  st_crop(ns_coast,
          c(xmin = -68, ymin = 41, xmax = -56.5, ymax = 47.5))))

ns_coast <- st_transform(ns_coast, crs_utm20)

st_crs(ns_coast) <- 4326 # 'WGS84'; necessary on some installs
crs_utm20 <- 32620

survey <- survey %>%   
  st_as_sf()
    
survey = subset(survey, !is.na(BT) & survey$z<400)
surv_utm_coords <- st_coordinates(survey)

survey$X1000 <- surv_utm_coords[,1] 
survey$Y1000 <- surv_utm_coords[,2] 

spde <- make_mesh(as_tibble(survey), xy_cols = c("X1000", "Y1000"),
                   n_knots=500,type = "cutoff_search")
plot(spde)

# Add on the barrier mesh component:
bspde <- add_barrier_mesh(
  spde, ns_coast, range_fraction = 0.1,
  proj_scaling = 1000, plot = TRUE
)
 mesh_df_water <- bspde$mesh_sf[bspde$normal_triangles, ]
     mesh_df_land <- bspde$mesh_sf[bspde$barrier_triangles, ]
     ggplot(ns_coast) +
       geom_sf() +
       geom_sf(data = mesh_df_water, size = 1, colour = "blue") +
       geom_sf(data = mesh_df_land, size = 1, colour = "green")+
       coord_sf()
  

survey$m = month(survey$DATE) 
survey$Q = ifelse(survey$m %in% c(10,11,12),1,ifelse(survey$m %in% c(1,2,3),2,ifelse(survey$m %in% c(4,5,6),3,4)))
survey$Time = survey$YEAR+survey$Q/4

#rerun feb4 2023
survey$pa = ifelse(survey$Lobster>0,1,0)
ss = as_tibble(survey)
fit = sdmTMB(pa~
               s(lZ,k=4)+s(BT,k=3)+Q,
             data=ss,
            offset = 'LO',
             time='YEAR', 
             mesh=bspde,
             family=binomial(link='logit'),
             spatial='on',
             spatiotemporal='ar1')


saveRDS(fit,'sdmTMBbyQBin0May22023.rds')
fit<-readRDS('sdmTMBbyQBin0May22023.rds')

x = predict(fit)
x$preds = fit$family$linkinv(x$est)
x$rawRes = x$Lobster - x$preds

Glsur = readRDS('GlorysPredictSurface.rds')
x = Glsur


#plot_smooth(fit,select=2)


x = bio.utilities::rename.df(x,c('bottomT','yr'),c('BT','YEAR'))
x = subset(x,z>0)
x$lZ = log(x$z)
x$X1000 = st_coordinates(x)[,1]
x$Y1000 = st_coordinates(x)[,2]
x = subset(x,exp(lZ)<400)

  x = as_tibble(subset(x,select=c(Q,YEAR,BT,X1000,Y1000,lZ)))
  x$geometry=NULL

  g = predict(fit,newdata=x)

  g$preds = fit$family$linkinv(g$est)


  gsf = st_as_sf(g,coords = c("X1000","Y1000"),crs=32620,remove=F)


rL = readRDS(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolysSF.rds"))
rL = st_as_sf(rL)
st_crs(rL) <- 4326
rL = st_transform(rL,32620) 
st_geometry(rL) <- st_geometry(st_as_sf(rL$geometry/1000)) 
st_crs(rL) <- 32620


ff = st_join(gsf,rL,join=st_within)
gsf = subset(ff,!is.na(LFA))

saveRDS(list(fit,gsf),'preds_sdmTMBbyQbinoMay32023.rds')
xx=readRDS('preds_sdmTMBbyQbinoMay32023.rds')
fit=xx[[1]]
gsf=xx[[2]]

#Maps
mm = c(0.0000001,max(quantile(gsf$pred,0.9999)))
ggplot(subset(gsf,Q==3 & YEAR %in% 2004:2022)) +
  geom_sf(aes(fill=preds,color=preds)) + 
  scale_fill_viridis_c(limits=mm) +
  scale_color_viridis_c(limits=mm) +
  facet_wrap(~YEAR) +
  #geom_sf(data=rL,size=1,colour='black',fill=NA)+
  theme( axis.ticks.x = element_blank(),
         axis.text.x = element_blank(),
         axis.title.x = element_blank(),
         axis.ticks.y = element_blank(),
         axis.text.y = element_blank(),
         axis.title.y = element_blank()
  ) +
  coord_sf()


savePlot('wtQ600.png') 


saveRDS(list(data=survey,grid=bspde,model=fitpa),file='PAwtw600kNov102022.rds')







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
