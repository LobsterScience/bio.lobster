require(bio.lobster)
require(devtools)
require(bio.utilities)
require(ggplot2)
require(sf)
#rerun LFA34 updates to get survey figs


##temperature
tp = lobster.db('temperature.data')
require(sf)
tp1 = st_as_sf(tp,coords=c('LON_DD','LAT_DD'),crs=4326)
layerDir=file.path(project.datadirectory("bio.lobster"), "data","maps")

r<-readRDS(file.path( layerDir,"GridPolysSF.rds"))
rL = readRDS(file.path(layerDir,"LFAPolysSF.rds"))
ns_coast =readRDS(file.path( layerDir,"CoastSF.rds"))

ns_coast =readRDS(file.path( project.datadirectory("bio.lobster"), "data","maps","CoastSF.rds"))
st_crs(ns_coast) <- 4326 # 'WGS84'; necessary on some installs
sf_use_s2(FALSE) #needed for cropping
ns_coast <- suppressWarnings(suppressMessages(
  st_crop(ns_coast,
          c(xmin = -68, ymin = 41, xmax = -56.5, ymax = 47.5))))


tp2 = st_join(tp1,rL,join=st_within)


tp2$mn = lubridate::month(tp2$T_DATE)
tp2$yr = lubridate::year(tp2$T_DATE)

rLs = subset(rL,LFA %in% 33:38)
ggplot(rLs)+
  geom_sf()+
  geom_sf(data=subset(tp2,yr %in% 2023& LFA %in% 33:38))+
  facet_wrap(~mn)+
  coord_sf(xlim = c(st_bbox(rLs)$xmin,st_bbox(rLs)$xmax),
         ylim = c(st_bbox(rLs)$ymin,st_bbox(rLs)$ymax),
         expand = FALSE)

bof = subset(tp2, LFA %in% 35:38 & mn==9)
bof = st_transform(bof, 32620)
bof$X = st_coordinates(bof)[,1]/1000
bof$Y = st_coordinates(bof)[,2]/1000
st_geometry(bof) <- NULL

ns_coast = st_transform(ns_coast, 32620)

require(sdmTMB)
require(sdmTMBextra)

bof$md = lubridate::mday(bof$T_DATE)

bof = subset(bof, !is.na(DEPTH_M) & TEMP>0)
bof$fyr = as.factor(bof$yr)
mesh <- make_mesh(bof, xy_cols = c('X','Y'),  cutoff = 3)
bspde <- sdmTMBextra::add_barrier_mesh(
  mesh, ns_coast, range_fraction = 1,
  proj_scaling = 1000, plot = TRUE
)
bof$fLFA = as.factor(bof$LFA)
fit_sp = sdmTMB(TEMP~DEPTH_M+s(md)+fyr,
                data = bof,
                mesh = bspde,
                family=lognormal(link='log'),
                spatial = 'on'
                )


require(ggeffects)

#these are marginal effects year 
mydf <- ggpredict(fit_sp, terms = c('fyr [all]'))
plot(mydf)

mydf <- ggpredict(fit_sp, terms = c('fyr [all]'),condition=c('LFA36'))

require(ggplot2)
ggplot(mydf, aes(x = x, y = predicted)) +
  geom_point() +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high))+
  xlab('Year')+
  ylab('Temperature')+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#landings
a = lobster.db('process.logs.unfiltered')
a$mn = month(a$DATE_FISHED)
a = subset(a,LFA %in% c(35,36,38) & mn %in% c(10,11,12))

aa = aggregate(cbind(WEIGHT_KG,NUM_OF_TRAPS)~SYEAR,data=a,FUN=sum)
aa$yr = aa$SYEAR-1
aa$CPUE=aa$WEIGHT_KG/aa$NUM_OF_TRAPS

mm = as.data.frame(mydf)

am = merge(aa,mm,by.x='yr',by.y='x')
plot(am$predicted,am$CPUE,xlab='Temperature',ylab='CPUE in Oct/Nov/Dec',type='b',pch=".")
text(am$predicted,am$CPUE,labels=am$yr)

am$dCPUE = c(diff(am$CPUE),NA)

plot(am$predicted,am$dCPUE,xlab='Temperature',ylab='deltaCPUE',type='b',pch=".")
text(am$predicted,am$dCPUE,labels=am$yr)

##################################################

ns_coast =readRDS(file.path( project.datadirectory("bio.lobster"), "data","maps","CoastSF.rds"))
st_crs(ns_coast) <- 4326 # 'WGS84'; necessary on some installs
sf_use_s2(FALSE) #needed for cropping
ns_coast <- suppressWarnings(suppressMessages(
  st_crop(ns_coast,
          c(xmin = -68, ymin = 41, xmax = -56.5, ymax = 47.5))))


tp2 = st_join(tp1,rL,join=st_within)


tp2$mn = lubridate::month(tp2$T_DATE)
tp2$yr = lubridate::year(tp2$T_DATE)

rLs = subset(rL,LFA %in% 34)
ggplot(rLs)+
  geom_sf()+
  geom_sf(data=subset(tp2,yr %in% 2012& LFA %in% 33:34))+
  facet_wrap(~mn)+
  coord_sf(xlim = c(st_bbox(rLs)$xmin,st_bbox(rLs)$xmax),
           ylim = c(st_bbox(rLs)$ymin,st_bbox(rLs)$ymax),
           expand = FALSE)

bof = subset(tp2, LFA %in% 34 & mn==7)
bof = st_transform(bof, 32620)
bof$X = st_coordinates(bof)[,1]/1000
bof$Y = st_coordinates(bof)[,2]/1000
st_geometry(bof) <- NULL

ns_coast = st_transform(ns_coast, 32620)

require(sdmTMB)
require(sdmTMBextra)

bof$md = lubridate::mday(bof$T_DATE)

bof = subset(bof, !is.na(DEPTH_M) & TEMP>0)
bof$fyr = as.factor(bof$yr)
mesh <- make_mesh(bof, xy_cols = c('X','Y'),  cutoff = 3)
bspde <- sdmTMBextra::add_barrier_mesh(
  mesh, ns_coast, range_fraction = 1,
  proj_scaling = 1000, plot = TRUE
)
bof$fLFA = as.factor(bof$LFA)
fit_sp = sdmTMB(TEMP~DEPTH_M+s(md)+fyr,
                data = bof,
                mesh = bspde,
                family=lognormal(link='log'),
                spatial = 'on'
)


require(ggeffects)

#these are marginal effects year 
mydf <- ggpredict(fit_sp, terms = c('fyr [all]'))
plot(mydf)

da = aggregate(TEMP~fyr,data=bof,FUN=mean)
require(ggplot2)
ggplot(mydf, aes(x = x, y = predicted)) +
  geom_point() +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high))+
  geom_point(data=da,aes(x=fyr,y=TEMP),color='red')+
  xlab('Year')+
  ylab('Temperature')+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


#just ILTS station

h = ILTS_ITQ_All_Data(species = 2550, redo_base_data = F)
h = h %>% subset(!is.na(TEMPC)) %>% select(c(STATION, TEMPC,YEAR,SET_DATE)) %>% distinct()
ha = aggregate(YEAR~STATION, data=h,FUN=function(x) length(unique(x)))
ha = subset(ha,YEAR>6)

#landings
a = lobster.db('process.logs.unfiltered')
a$mn = month(a$DATE_FISHED)
a = subset(a,LFA %in% c(34) & mn %in% c(10,11,12))

aa = aggregate(cbind(WEIGHT_KG,NUM_OF_TRAPS)~SYEAR,data=a,FUN=sum)
aa$yr = aa$SYEAR-1
aa$CPUE=aa$WEIGHT_KG/aa$NUM_OF_TRAPS

mm = as.data.frame(mydf)

am = merge(aa,mm,by.x='yr',by.y='x')
plot(am$predicted,am$CPUE,xlab='Temperature',ylab='CPUE in Nov/Dec',type='b',pch=16)
text(am$predicted,am$CPUE,labels=am$yr)

am$dCPUE = c(diff(am$CPUE),NA)
am$dT = c(diff(am$predicted),NA)

plot(am$predicted,am$dCPUE,xlab='Temperature',ylab='deltaCPUE',type='b',pch=".")
text(am$predicted,am$dCPUE,labels=am$yr)

plot(am$dT,am$dCPUE,xlab='Temperature',ylab='deltaCPUE',type='b',pch=".")
text(am$dT,am$dCPUE,labels=am$yr)
