#modelling proportions to extend time series

require(tidyr)
require(sdmTMB)
require(sdmTMBextra)

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
fd=file.path(project.datadirectory('Update_LFA41'),'outputs','SURVEYS')
setwd(fd)

crs_utm20 <- 32620

#coastline
ns_coast =readRDS(file.path( project.datadirectory("bio.lobster"), "data","maps","CoastSF.rds"))
st_crs(ns_coast) <- 4326 # 'WGS84'; necessary on some installs
ns_coast <- suppressWarnings(suppressMessages(
  st_crop(ns_coast,
          c(xmin = -68, ymin = 41, xmax = -64, ymax = 46.5))))
ns_coast <- st_transform(ns_coast, crs_utm20)
st_geometry(ns_coast) = st_geometry(ns_coast)
st_crs(ns_coast) <- crs_utm20
nsc=ns_coast
st_geometry(nsc) = st_geometry(nsc)/1000
st_crs(nsc) <- crs_utm20

###########################################################################################################################################
#commercial sizes N
x = readRDS('proportions82_300.rds')
x$Proportion_Commercial = x$Legal/x$Lobster
x = subset(x,!is.na(Proportion_Commercial))
x = subset(x,Proportion_Commercial<=1)

#jitter for graph
xji = x
xji$Xj = jitter(xji$X)
xji$Yj = jitter(xji$Y)
xji = st_as_sf(xji,coords=c('Xj','Yj'),crs=4326)
xji <- st_transform(xji, crs_utm20)
st_geometry(xji) = st_geometry(xji)/1000
st_crs(xji) <- crs_utm20


pr =ggplot(xji)+geom_sf(aes(fill=Proportion_Commercial ,color=Proportion_Commercial ),size=2) +
  scale_fill_viridis_c() +
  scale_color_viridis_c() +
  geom_sf(data=nsc,fill='black')+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), 
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),        
        axis.text.x=element_blank(),
        axis.text.y=element_blank())



x = st_as_sf(x,coords=c('X','Y'),crs=4326)
x = st_transform(x,crs_utm20)
x_utm_coords = st_coordinates(x)
x$X1000 <- x_utm_coords[,1] /1000
x$Y1000 <- x_utm_coords[,2] /1000
st_geometry(x) =  st_geometry(x)/1000
st_crs(x) <- crs_utm20


##bathy
ba = readRDS('~/git/bio.lobster.data/mapping_data/bathymetrySF.rds')
ba = ba %>% st_as_sf() 
st_geometry(ba) = st_geometry(ba)/1000
st_crs(ba) = crs_utm20

ss = st_nearest_feature(x,ba)
ds = st_distance(x,ba[ss,],by_element=T)
st_geometry(ba) = NULL
x$z = ba$z[ss]
x$z_dist = as.numeric(ds)
x$lz = log(x$z)
ss = as_tibble(x)


wts = rescale0_1(ss$Nt)*100
ss$rN = round(ss$Legal)
ss$rT = round(ss$Lobster)

ss$nC =ss$rT-ss$rN

spde <- make_mesh(ss, xy_cols = c("X1000", "Y1000"),
                  n_knots=120,type = "cutoff_search")

bspde <- add_barrier_mesh(
  spde, ns_coast, range_fraction = 0.1,
  proj_scaling = 1000, plot = TRUE
)

fit_bino3 = sdmTMB(cbind(rN,nC)~s(lz)
                   ,
                   data=ss,
                   mesh=bspde,
                   family=binomial(link='logit'),
                   spatial='on'
                   )



fit_bino4 = sdmTMB(cbind(rN,nC)~1
                   ,
                   data=ss,
                   mesh=bspde,
                   family=binomial(link='logit'),
                   spatial='on'
)

s <- simulate(fit_bino3, nsim = 100, type = "mle-mvn")
r <- dharma_residuals(s, fit_bino3, plot =T)
plot(r$expected, r$observed)
abline(0, 1)

ss$dres = apply(s,1,median)
ss$Residuals = ss$dres/ss$Lobster - (ss$Legal/ss$Lobster)

sa = st_as_sf(ss)

pp=ggplot(sa)+geom_sf(aes(fill=Residuals ,color=Residuals ),size=2) +
scale_fill_viridis_c() +
scale_color_viridis_c() +
  geom_sf(data=nsc,fill='black')+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), 
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),        
        axis.text.x=element_blank(),
        axis.text.y=element_blank())


ff = readRDS(file.path( project.datadirectory("bio.lobster"), "data","maps","bathy_LFAPolysSF.rds"))
ff = subset(ff,z>10&z<max(ss$z) & LFA %in% 34:38) 
ff$X1000 = st_coordinates(ff)[,1]
ff$Y1000 = st_coordinates(ff)[,2]
ff$lz=log(ff$z)
fa = as_tibble(ff)
g = predict(fit_bino3,newdata = fa)
ff$Modelled_Proportion = fit_bino3$family$linkinv(g$est)




ggplot(ff) +
  geom_sf(aes(fill=Modelled_Proportion ,color=Modelled_Proportion ),size=2.1) +
  scale_fill_viridis_c() +
  scale_color_viridis_c() +
  geom_sf(data=nsc,fill='black')+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), 
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),        
        axis.text.x=element_blank(),
        axis.text.y=element_blank())

ff = subset(ff,select=c(LFA,Modelled_Proportion))
st_geometry(ff) = st_geometry(ff)*1000 
st_crs(ff) <- crs_utm20
ff = st_transform(ff,crs=4326)
saveRDS(ff,'modelled_Commercial_Proportions_34-38.rds')
#################################################################################################################################################@#
#commercial sizes W
x = readRDS('proportions_wt_82_300.rds')
x$Proportion_Comm_wt = x$Legal_wt/x$WEIGHT_KG
x = subset(x,!is.na(Proportion_Comm_wt))
#jitter for graph
xji = x
xji$Xj = jitter(xji$X)
xji$Yj = jitter(xji$Y)
xji = st_as_sf(xji,coords=c('Xj','Yj'),crs=4326)
xji <- st_transform(xji, crs_utm20)
st_geometry(xji) = st_geometry(xji)/1000
st_crs(xji) <- crs_utm20


pr =ggplot(xji)+geom_sf(aes(fill=Proportion_Comm_wt ,color=Proportion_Comm_wt ),size=2) +
  scale_fill_viridis_c() +
  scale_color_viridis_c() +
  geom_sf(data=nsc,fill='black')+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), 
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),        
        axis.text.x=element_blank(),
        axis.text.y=element_blank())



x = st_as_sf(x,coords=c('X','Y'),crs=4326)
x = st_transform(x,crs_utm20)
x_utm_coords = st_coordinates(x)
x$X1000 <- x_utm_coords[,1] /1000
x$Y1000 <- x_utm_coords[,2] /1000
st_geometry(x) =  st_geometry(x)/1000
st_crs(x) <- crs_utm20


##bathy
ba = readRDS('~/git/bio.lobster.data/mapping_data/bathymetrySF.rds')
ba = ba %>% st_as_sf() 
st_geometry(ba) = st_geometry(ba)/1000
st_crs(ba) = crs_utm20

ss = st_nearest_feature(x,ba)
ds = st_distance(x,ba[ss,],by_element=T)
st_geometry(ba) = NULL
x$z = ba$z[ss]
x$z_dist = as.numeric(ds)

ss = as_tibble(x)

spde <- make_mesh(ss, xy_cols = c("X1000", "Y1000"),
                  n_knots=150,type = "cutoff_search")

bspde <- add_barrier_mesh(
  spde, ns_coast, range_fraction = 0.1,
  proj_scaling = 1000, plot = TRUE
)

wts = rescale0_1(ss$Nt)*100
ss$rN = round(ss$Legal_wt)
ss$rT = round(ss$WEIGHT_KG)
ss$nC = ss$rT-ss$rN
ss$lz = log(ss$z)
fit_bino3 = sdmTMB(cbind(rN,nC)~s(lz)
                   ,
                   data=ss,
                   mesh=bspde,
                   family=binomial(link='logit'),
                   spatial='on'
)



fit_bino4 = sdmTMB(cbind(rN,nC)~1
                   ,
                   data=ss,
                   mesh=bspde,
                   family=binomial(link='logit'),
                   spatial='on'
)

s <- simulate(fit_bino3, nsim = 100, type = "mle-mvn")
r <- dharma_residuals(s, fit_bino3, plot =T)
plot(r$expected, r$observed)
abline(0, 1)

ss$dres = apply(s,1,median)
ss$Residuals = (ss$dres/ss$rT) -(ss$rN/ss$rT)
ss = subset(ss,!is.na(Residuals))

sa = st_as_sf(ss)

pp=ggplot(sa)+geom_sf(aes(fill=Residuals ,color=Residuals ),size=2) +
  scale_fill_viridis_c() +
  scale_color_viridis_c() +
  geom_sf(data=nsc,fill='black')+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), 
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),        
        axis.text.x=element_blank(),
        axis.text.y=element_blank())


ff = readRDS(file.path( project.datadirectory("bio.lobster"), "data","maps","bathy_LFAPolysSF.rds"))
ff = subset(ff,z>10&z<max(ss$z) & LFA %in% 34:38) 
ff$X1000 = st_coordinates(ff)[,1]
ff$Y1000 = st_coordinates(ff)[,2]
ff$lz=log(ff$z)
fa = as_tibble(ff)
g = predict(fit_bino3,newdata = fa)
ff$Modelled_Proportion = fit_bino$family$linkinv(g$est)




ggplot(ff) +
  geom_sf(aes(fill=Modelled_Proportion ,color=Modelled_Proportion ),size=2.1) +
  scale_fill_viridis_c() +
  scale_color_viridis_c() +
  geom_sf(data=nsc,fill='black')+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), 
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),        
        axis.text.x=element_blank(),
        axis.text.y=element_blank())

ff = subset(ff,select=c(LFA,Modelled_Proportion))
st_geometry(ff) = st_geometry(ff)*1000 
st_crs(ff) <- crs_utm20
ff = st_transform(ff,crs=4326)
saveRDS(ff,'modelled_Commercial_Proportions_wt_34-38.rds')





#########################################################################################################################################################

#recruit sizes
x = readRDS('proportions70_82.rds')
x$Proportion_Recruit = x$Recruits/x$Lobster
x = subset(x,!is.na(Proportion_Recruit))

#jitter for graph
xji = x
xji$Xj = jitter(xji$X)
xji$Yj = jitter(xji$Y)
xji = st_as_sf(xji,coords=c('Xj','Yj'),crs=4326)
xji <- st_transform(xji, crs_utm20)
st_geometry(xji) = st_geometry(xji)/1000
st_crs(xji) <- crs_utm20


pr =ggplot(xji)+geom_sf(aes(fill=Proportion_Recruit ,color=Proportion_Recruit ),size=2) +
  scale_fill_viridis_c() +
  scale_color_viridis_c() +
  geom_sf(data=nsc,fill='black')+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), 
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),        
        axis.text.x=element_blank(),
        axis.text.y=element_blank())



x = st_as_sf(x,coords=c('X','Y'),crs=4326)
x = st_transform(x,crs_utm20)
x_utm_coords = st_coordinates(x)
x$X1000 <- x_utm_coords[,1] /1000
x$Y1000 <- x_utm_coords[,2] /1000
st_geometry(x) =  st_geometry(x)/1000
st_crs(x) <- crs_utm20


##bathy
ba = readRDS('~/git/bio.lobster.data/mapping_data/bathymetrySF.rds')
ba = ba %>% st_as_sf() 
st_geometry(ba) = st_geometry(ba)/1000
st_crs(ba) = crs_utm20

ss = st_nearest_feature(x,ba)
ds = st_distance(x,ba[ss,],by_element=T)
st_geometry(ba) = NULL
x$z = ba$z[ss]
x$z_dist = as.numeric(ds)

ss = as_tibble(x)
ss = subset(ss,Recruits<quantile(Recruits,.90))
spde <- make_mesh(ss, xy_cols = c("X1000", "Y1000"),
                  n_knots=170,type = "cutoff_search")

bspde <- add_barrier_mesh(
  spde, ns_coast, range_fraction = 0.1,
  proj_scaling = 1000, plot = TRUE
)
ss$lz=log(ss$z)

ss$rN = round(ss$Recruits)
ss$rT = round(ss$Lobster)
ss$nC = ss$rT-ss$rN

fit_bino3 = sdmTMB(cbind(rN,nC)~s(lz)
                   ,
                   data=ss,
                   mesh=bspde,
                   family=binomial(link='logit'),
                   spatial='on'
)



fit_bino4 = sdmTMB(cbind(rN,nC)~1
                   ,
                   data=ss,
                   mesh=bspde,
                   family=binomial(link='logit'),
                   spatial='on'
)




s <- simulate(fit_bino4, nsim = 100, type = "mle-mvn")
r <- dharma_residuals(s, fit_bino3, plot =T)
plot(r$expected, r$observed)
abline(0, 1)

ss$dres = apply(s,1,median)
ss$Residuals = (ss$dres/ss$rT) -(ss$Recruits/ss$Lobster)

sa = st_as_sf(ss)

pp=ggplot(sa)+geom_sf(aes(fill=Residuals ,color=Residuals ),size=2) +
  scale_fill_viridis_c() +
  scale_color_viridis_c() +
  geom_sf(data=nsc,fill='black')+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), 
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),        
        axis.text.x=element_blank(),
        axis.text.y=element_blank())


ff = readRDS(file.path( project.datadirectory("bio.lobster"), "data","maps","bathy_LFAPolysSF.rds"))
ff = subset(ff,z>10&z<max(ss$z) & LFA %in% 34:38) 
ff$X1000 = st_coordinates(ff)[,1]
ff$Y1000 = st_coordinates(ff)[,2]
ff$lz=log(ff$z)
fa = as_tibble(ff)
g = predict(fit_bino3,newdata = fa)
ff$Modelled_Proportion = fit_bino$family$linkinv(g$est)




ggplot(ff) +
  geom_sf(aes(fill=Modelled_Proportion ,color=Modelled_Proportion ),size=2.1) +
  scale_fill_viridis_c() +
  scale_color_viridis_c() +
  geom_sf(data=nsc,fill='black')+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), 
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),        
        axis.text.x=element_blank(),
        axis.text.y=element_blank())

ff = subset(ff,select=c(LFA,Modelled_Proportion))
st_geometry(ff) = st_geometry(ff)*1000 
st_crs(ff) <- crs_utm20
ff = st_transform(ff,crs=4326)
saveRDS(ff,'modelled_recruit_Proportions_34-38.rds')


##################################################################################################################################################################
#recuits and commercial so 70:300

x = readRDS('proportions70_300.rds')
x$Proportion_RC = x$RecruitsComm/x$Lobster
x = subset(x,!is.na(Proportion_RC))

x = st_as_sf(x,coords=c('X','Y'),crs=4326)
x = st_transform(x,crs_utm20)
x_utm_coords = st_coordinates(x)
x$X1000 <- x_utm_coords[,1] /1000
x$Y1000 <- x_utm_coords[,2] /1000
st_geometry(x) =  st_geometry(x)/1000
st_crs(x) <- crs_utm20


##bathy
ba = readRDS('~/git/bio.lobster.data/mapping_data/bathymetrySF.rds')
ba = ba %>% st_as_sf() 
st_geometry(ba) = st_geometry(ba)/1000
st_crs(ba) = crs_utm20

ss = st_nearest_feature(x,ba)
ds = st_distance(x,ba[ss,],by_element=T)
st_geometry(ba) = NULL
x$z = ba$z[ss]
x$z_dist = as.numeric(ds)

ss = as_tibble(x)
ss$lz=log(ss$z)

ss$rN = round(ss$RecruitsComm)
ss$rT = round(ss$Lobster)
ss$nC = ss$rT-ss$rN


spde <- make_mesh(ss, xy_cols = c("X1000", "Y1000"),
                  n_knots=150,type = "cutoff_search")

bspde <- add_barrier_mesh(
  spde, ns_coast, range_fraction = 0.1,
  proj_scaling = 1000, plot = TRUE
)

fit_bino3 = sdmTMB(cbind(rN,nC)~s(lz)
                   ,
                   data=ss,
                   mesh=bspde,
                   family=binomial(link='logit'),
                   spatial='on'
)



s <- simulate(fit_bino3, nsim = 100, type = "mle-mvn")
r <- dharma_residuals(s, fit_bino3, plot =T)
plot(r$expected, r$observed)
abline(0, 1)
ss$prop = ss$rN/ss$rT
ss$dres = apply(s,1,median)
ss$Residuals = ss$dres/ss$rT -ss$prop

sa = st_as_sf(ss)

pp=ggplot(sa)+geom_sf(aes(fill=Residuals ,color=Residuals ),size=2) +
  scale_fill_viridis_c() +
  scale_color_viridis_c() +
  geom_sf(data=nsc,fill='black')+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), 
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),        
        axis.text.x=element_blank(),
        axis.text.y=element_blank())


ff = readRDS(file.path( project.datadirectory("bio.lobster"), "data","maps","bathy_LFAPolysSF.rds"))
ff = subset(ff,z>10&z<max(ss$z) & LFA %in% 34:38) 
ff$X1000 = st_coordinates(ff)[,1]
ff$Y1000 = st_coordinates(ff)[,2]
ff$lz=log(ff$z)
fa = as_tibble(ff)
g = predict(fit_bino3,newdata = fa)
ff$Modelled_Proportion = fit_bino$family$linkinv(g$est)




ggplot(ff) +
  geom_sf(aes(fill=Modelled_Proportion ,color=Modelled_Proportion ),size=2.1) +
  scale_fill_viridis_c() +
  scale_color_viridis_c() +
  geom_sf(data=nsc,fill='black')+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), 
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),        
        axis.text.x=element_blank(),
        axis.text.y=element_blank())

ff = subset(ff,select=c(LFA,Modelled_Proportion))
st_geometry(ff) = st_geometry(ff)*1000 
st_crs(ff) <- crs_utm20
ff = st_transform(ff,crs=4326)
saveRDS(ff,'modelled_commercial_and_recruit_Proportions_34-38.rds')
