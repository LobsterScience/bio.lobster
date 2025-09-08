require(ggplot2)
require(terra)
require(tidyterra)
require(sf)
require(bio.lobster)
require(bio.utilities)
require(devtools)
la()


v = readRDS('C:\\Users\\Cooka\\Documents\\bio_data\\bio.lobster\\data\\OFI_lobster_covariates/ofi_pointData500m.rds')
vs = st_as_sf(v,crs=32620)
vs$z = vs$depth *-1

l = readRDS(file.path(git.repo,'bio.lobster.data/mapping_data/LFAPolysSF.rds'))
lp = st_transform(l,crs=32620)
pip <- st_join(vs, lp)
pip = subset(pip,!is.na(LFA))

#scallop sets
a = lobster.db('scallop')

scallop.tows=a[[1]]
scallopSurv = a[[2]]
scallop.tows$Y = convert.dd.dddd(scallop.tows$START_LAT)
scallop.tows$X = convert.dd.dddd(scallop.tows$START_LONG)
scT = subset(scallop.tows,select=c('TOW_SEQ','TOW_DATE','STRATA_ID','X','Y'))
totS = st_as_sf(scT,coords = c('X','Y'),crs=st_crs(4326))
sc = st_transform(totS,crs=32620)

ss = st_join(sc,lp)
ss = subset(ss,!is.na(LFA))

#lengthfreqs Scal

scalSize = rep(scallopSurv$MEAS_VAL,times=round(scallopSurv$ABUNDANCE_STD))

#ggplot()+
#  geom_sf(data=subset(pip,LFA==38),aes(fill=slope,colour=slope)) +
#  scale_fill_gradient(low = "white", high = "red")+
#  scale_colour_gradient(low = "white", high = "red")+
#  geom_sf(data=subset(ss,LFA==38),size=.4)+
#  theme_test_adam()


###rv

x = RV_sets()
require(tidyr)
require(stringr)

#lengthfreqs
xlo = x %>%
        pivot_longer(cols=starts_with("P."),names_to='Length',values_to = 'Density') %>%
        select(Length,Density)
xlo$Length = as.numeric(str_extract(xlo$Length,"\\d+"))
RVSize = with(subset(xlo,!is.na(Density)), rep(Length,round(Density)))

x = subset(x,select=c(mission,setno,LONGITUDE,LATITUDE,DATE))
xs = st_as_sf(x,coords=c('LONGITUDE','LATITUDE'),crs=4326)
xs = st_transform(xs,crs = 32620)
xs = st_join(xs,lp)
xs = subset(xs,!is.na(LFA))
xs$yr = lubridate::year(xs$DATE)


#ILTS
y = ILTS_ITQ_All_Data(redo_base_data = F,size=c(0,300),aggregate=F,species=2550,biomass = F)
#length freqs
ILTSSize = with(subset(y,SA_CORRECTED_PRORATED_N>0),rep(FISH_LENGTH,round(SA_CORRECTED_PRORATED_N)))
y = subset(y,select=c(TRIP_ID,SET_NO, SET_LONG,SET_LAT,SET_DATE))    
ys = st_as_sf(y,coords=c('SET_LONG','SET_LAT'),crs=4326)
ys = st_transform(ys,crs = 32620)
ys = st_join(ys,lp)
ys = subset(ys,!is.na(LFA))
ys$yr = lubridate::year(ys$SET_DATE)

sizs = do.call(rbind, list(data.frame(Source='Scal',Value=scalSize),data.frame(Source='RV',Value=RVSize),data.frame(Source='ILTS',Value=ILTSSize)))

z = lobster.db('greyzone_logs')
z$Date = lubridate::ymd(z$DATE_SAILED)
z$yr = lubridate::year(z$Date)
z$mn = lubridate::month(z$Date)

z$X = convert.dd.dddd(z$ENT_LONGITUDE)*-1
z$Y = convert.dd.dddd(z$ENT_LATITUDE)
zs = st_as_sf(subset(z,!is.na(X)),coords=c('X','Y'),crs=4326)
zs = st_transform(zs,crs = 32620)
zs = st_join(zs,lp)
zs = subset(zs,!is.na(LFA))

#model


data = readRDS( file=file.path(project.datadirectory('Assessment_LFA35_38'),'outputs','SURVEYS','survey_data_all_combined.rds'))
data$depth.scaled = (data$z-mean(data$z))/sd(data$z)
data$depth.scaled2 = data$depth.scaled * data$depth.scaled

model = readRDS(file.path(project.datadirectory('Assessment_LFA35_38'),'outputs','SURVEYS','bestModel_commLobster2000+feb142025.rds'))

ff = readRDS(file.path(bio.directory,"bio.lobster.data","mapping_data",'bathy_byLFA_noLFA37_noMidas_noPass.rds')) #fixed to remove LFA 37 from all predictions Feb 11, 2025
ff = subset(ff,PID %in% 34:38)
ff = subset(ff,z<max(data$z) & z>5)
ff$depth.scaled = (ff$z-mean(data$z))/sd(data$z)

##k from model above
k=3
knots <-   quantile(data$depth.scaled, p = seq(0, 1, len = k)[-c(1,k)])
basis <- as.data.frame(bs(ff$depth.scaled, knots = knots, intercept = FALSE))
names(basis) <- paste0("bs", names(basis))
ff <- cbind(ff, basis)

yy = 2000:2024
o = list()
for(i in 1:length(yy)){
  f = ff
  f$year=yy[i]
  o[[i]]=f
}
ff = do.call(rbind,o)

sg = st_coordinates(ff)
ff$X1000 = sg[,1]
ff$Y1000 = sg[,2]
ff$SOURCE='ILTS'
f = as_tibble(ff)
f$MNH <- 0
f$NEFSC <- 0
f$ILTS_Fall <- f$MNH_Fall <- f$MNH_NEFSC_Fall <- 0
g = predict(model,newdata=f,return_tmb_object = T)
h = g$data
h$pred = model$family$linkinv(h$est)
gsf = st_as_sf(h)

sf_use_s2(FALSE) #needed for cropping

ns_coast =readRDS(file.path(bio.directory,'bio.lobster.data',"mapping_data","CoastSF.rds"))
st_crs(ns_coast) <- 4326 # 'WGS84'; necessary on some installs
ns_coast <- suppressWarnings(suppressMessages(
  st_crop(ns_coast,
          c(xmin = -68, ymin = 41, xmax = -64, ymax = 47.5))))

ns_coast = st_transform(ns_coast,32620) 
st_geometry(ns_coast) <- st_geometry(ns_coast)/1000
st_crs(ns_coast) <- 32620



rL = readRDS(file.path(bio.directory,'bio.lobster.data',"mapping_data","LFAPolys37Split.rds"))
rL = rL[rL$PID %in% c(33:38),]
st_crs(rL) <- 4326
crs_utm20 <- 32620
#rL = rL[-which(!(st_is_valid(rL))),]
rL <- suppressWarnings(suppressMessages(
  st_crop(rL,
          c(xmin = -67.5, ymin = 42, xmax = -62.1, ymax = 46))))
rL <- st_transform(rL, crs_utm20)
st_geometry(rL) <- st_geometry(rL)/1000
st_crs(rL) <- 32620


gsfr = subset(gsf,PID %in% c(38) & Y1000>4850)

#mm = c(0.,max(quantile(gsfr$pred,.999)))
#gsfr$CommB = ifelse(gsfr$pred>mm[2],mm[2],gsfr$pred)
gsfr$CommN = gsfr$pred
gb = ggplot(subset(gsfr, year %in% c(2023))) +
  geom_sf(aes(fill=CommN,color=CommN),size=4) + 
  scale_fill_viridis_c(trans='log') +
  scale_color_viridis_c(trans='log') +
  #  facet_wrap(~year) +
  geom_sf(data=ns_coast,fill='black')+
  geom_sf(data=rL,colour='black',fill=NA)+
  theme_test_adam()+
  coord_sf(xlim = c(st_bbox(gsfr)$xmin,st_bbox(gsfr)$xmax),
           ylim = c(st_bbox(gsfr)$ymin,st_bbox(gsfr)$ymax),
           expand = FALSE)


###se of predictions 
g = predict(model,newdata=f,se=T,nsim=500) #dec 18 2024 need to finish
gg = model$family$linkinv(g)

f$se = apply(gg,1,sd)
f$med = apply(gg,1,median)
f$me = apply(gg,1,mean)

gsfer = st_as_sf(f)
gsfer = subset(gsfer,PID %in% c(38)& Y1000 > 4850)

mm = c(0.,max(quantile(gsfer$se,.99)))
gsfer$PredictionError = gsfer$se
ggplot(subset(gsfer, year %in% c(1970:2023))) +
  geom_sf(aes(fill=PredictionError,color=PredictionError),size=2.1) + 
  scale_fill_viridis_c(trans='sqrt',limits=mm) +
  scale_color_viridis_c(trans='sqrt',limits=mm) +
  facet_wrap(~year) +
  geom_sf(data=ns_coast,fill='black')+
  geom_sf(data=rL,colour='black',fill=NA)+
  theme_test_adam()+
  coord_sf(xlim = c(st_bbox(ns_coast)$xmin,st_bbox(gsfr)$xmax),
           ylim = c(4850,st_bbox(gsfr)$ymax),
           expand = FALSE)


gsfer$CV_Prediction = gsfer$se/gsfer$me
mm = c(0.,max(quantile(gsfer$CV_Prediction,.995)))

ggplot(subset(gsfer, year %in% c(1970:2023))) +
  geom_sf(aes(fill=CV_Prediction,color=CV_Prediction),size=2.1) + 
  scale_fill_viridis_c(limits=mm) +
  scale_color_viridis_c(limits=mm) +
  facet_wrap(~year) +
  geom_sf(data=ns_coast,fill='black')+
  geom_sf(data=rL,colour='black',fill=NA)+
  theme_test_adam()+
  coord_sf(xlim = c(st_bbox(ns_coast)$xmin,st_bbox(gsfr)$xmax),
           ylim = c(st_bbox(gsfr)$ymin,st_bbox(gsfr)$ymax),
           expand = FALSE)





#ggplot()+
#  geom_sf(data=subset(pip,LFA==38),aes(fill=slope,colour=slope)) +
#  scale_fill_gradient(low = "white", high = "red")+
##  scale_colour_gradient(low = "white", high = "red")+
#  geom_sf(data=subset(ss,LFA==38),size=.01)+
#  geom_sf(data=subset(xs,LFA==38& yr>2018),size=.8,colour='blue')+
#  geom_sf(data=subset(ys,LFA==38 & yr>2018),size=.8,colour='green')+
#  theme_test_adam()


###getting xx from git/FrameworkLFA35_38/inst/Scallop/3.scallopRecruitAbunance within 38 and all sizes of lobster
#xxs = st_transform(xx,crs=32620)


#ggplot()+
#  geom_sf(data=subset(pip,LFA==38),aes(fill=slope,colour=slope)) +
#  scale_fill_gradient(low = "white", high = "red")+
#  scale_colour_gradient(low = "white", high = "red")+
#  geom_sf(data=subset(xxs,LFA==38),size=.01)+
#  geom_sf(data=subset(xxs,LFA==38 & ABUNDANCE_STD>0),size=.01,col='purple')+
#   geom_sf(data=subset(xs,LFA==38& yr>2018),size=.8,colour='blue')+
#  geom_sf(data=subset(ys,LFA==38 & yr>2018),size=.8,colour='green')+
#  theme_test_adam()

