
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
survey = readRDS(file=file.path(project.datadirectory('bio.lobster'),'data','BaseDataForClimateModel.rds'))
sf_use_s2(FALSE) #needed for cropping

# Project our survey data coordinates:

survey$lZ = log(survey$z)
survey = subset(survey,!is.na(lZ))

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
survey = subset(survey,!is.na(BT))
survey$pa = ifelse(survey$Berried>0,1,0)
survey = subset(survey, !is.na(Berried))
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


#issues with fitting on a biweekly moving to quarters
survey$m = month(survey$DATE) 
survey$Q = ifelse(survey$m %in% c(10,11,12),1,ifelse(survey$m %in% c(1,2,3),2,ifelse(survey$m %in% c(4,5,6),3,4)))
survey$Time = survey$YEAR+survey$Q/4


fitpa = sdmTMB(pa~
               s(lZ,k=3)+s(BT,k=4)+Q,
             data=as_tibble(survey),
            offset = 'LO',
             time='YEAR', 
             mesh=bspde,
             family=binomial(link='logit'),
             spatial='on',
             spatiotemporal='ar1')
# AIC(fitpa)
#674281.8

fitpaNT = sdmTMB(pa~
               s(lZ,k=3)+Q,
             data=as_tibble(survey),
            offset = 'LO',
             time='YEAR', 
             mesh=bspde,
             family=binomial(link='logit'),
             spatial='on',
             spatiotemporal='ar1')
#AIC(fitpaNT)
#679159.5



fitpaNS = sdmTMB(pa~
               s(lZ,k=3)+s(BT,k=3)+Q,
             data=as_tibble(survey),
            offset = 'LO',
            mesh=bspde,
             family=binomial(link='logit'),
             spatial='off',
             )

saveRDS(list(data=survey,grid=bspde,model=fitpa),file='sdmTMBBerriedpabyQFinal.rds')

x=readRDS(file='sdmTMBBerriedpabyQFinal.rds')

fitpa = x$model
bspde = x$grid
survey= x$data
Glsur = readRDS('GlorysPredictSurface.rds')
x = Glsur


plot_smooth(fitpa,select=1)


x = bio.utilities::rename.df(x,c('bottomT','yr'),c('BT','YEAR'))
x = subset(x,z>0)
x$lZ = log(x$z)
x$X1000 = st_coordinates(x)[,1]
x$Y1000 = st_coordinates(x)[,2]
x = subset(x,exp(lZ)<400)

x = as_tibble(subset(x,select=c(Q,YEAR,BT,X1000,Y1000,lZ)))
x$geometry=NULL

g = predict(fitpa,newdata=x)

  g$pred = fitpa$family$linkinv(g$est)

  gsf = st_as_sf(g,coords = c("X1000","Y1000"),crs=32620,remove=F)


rL = readRDS(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolysSF.rds"))
rL = st_as_sf(rL)
st_crs(rL) <- 4326
rL = st_transform(rL,32620) 
st_geometry(rL) <- st_geometry(st_as_sf(rL$geometry/1000)) 
st_crs(rL) <- 32620


ff = st_join(gsf,rL,join=st_within)
gsf = subset(ff,!is.na(LFA))

#Maps
mm = c(0.001,max(gsf$pred))
ggplot(subset(gsf,Q==3 &YEAR %in% 2012:2022)) +
  geom_sf(aes(fill=pred,color=pred)) + 
  scale_fill_viridis_c() +
  scale_color_viridis_c() +
  facet_wrap(~YEAR) +
 # geom_sf(data=rL,size=1,colour='black',fill=NA)+
  theme( axis.ticks.x = element_blank(),
         axis.text.x = element_blank(),
         axis.title.x = element_blank(),
         axis.ticks.y = element_blank(),
         axis.text.y = element_blank(),
         axis.title.y = element_blank()
  ) +
  coord_sf()
savePlot('BerriedPAQ32012-2022.png') 


with(gsf,plot(BT,pred))
gsfYR = gsf
##################################################################
###projections

 glo = readRDS(file='ClimatologyAndProjections.rds')
 glo$X1000 = st_coordinates(glo)[,1]
 glo$Y1000 = st_coordinates(glo)[,2]
 glo$lZ = log(glo$z)
glo = subset(glo,z<400)
 glo = subset(glo,!is.na(lZ))

rL = readRDS(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolysSF.rds"))
rL = st_as_sf(rL)
st_crs(rL) <- 4326
rL = st_transform(rL,32620) 
st_geometry(rL) <- st_geometry(st_as_sf(rL$geometry/1000)) 
st_crs(rL) <- 32620


glo = st_join(glo,rL,join=st_within)

glo = subset(glo,!is.na(LFA))


#current
y = unique(survey$YEAR)
cdata = subset(glo,select=c(X1000,Y1000,lZ,Clim0.5,Q))
cdata = subset(cdata,exp(lZ)<400)
st_geometry(cdata) <- NULL

cdy = as.data.frame(sapply(cdata,rep.int,length(y)))
cdy$YEAR = rep(y,each=dim(cdata)[1])
cdy$BT = cdy$Clim0.5
g = predict(fitpa,newdata=cdy)
g$pred = fitpa$family$linkinv(g$est)

#average over 2000-2022
ga = aggregate(pred~X1000+Y1000+Q+BT+lZ,data=g,FUN=mean)

  gsf = st_as_sf(ga,coords = c("X1000","Y1000"),crs=32620,remove=F)





ggplot(subset(gsf)) +
  geom_sf(aes(fill=pred,color=pred)) + 
  scale_fill_viridis_c() +
  scale_color_viridis_c() +
  facet_wrap(~Q) +
 # geom_sf(data=rL,size=1,colour='black',fill=NA)+
  theme( axis.ticks.x = element_blank(),
         axis.text.x = element_blank(),
         axis.title.x = element_blank(),
         axis.ticks.y = element_blank(),
         axis.text.y = element_blank(),
         axis.title.y = element_blank()
  ) +
  coord_sf()

savePlot('ClimatologyBerriedPreds.png')

 ga$X = ga$X1000*1000
 ga$Y = ga$Y1000*1000

ga2 = st_as_sf(ga,coords = c("X","Y"),crs=32620,remove=F)
gs2 = ga2 %>% st_transform(4326)
gs2$X = st_coordinates(gs2)[,1]
gs2$Y = st_coordinates(gs2)[,2]
gs2 = subset(gs2,select=c(X,Y,pred,Q))
gs2 = as_tibble(gs2)
saveRDS(gs2,file='CurrentClimatologyBerriedBinomialOutput.rds')
write.csv(gs2,file='CurrentClimatologyBerriedBinomialOutput.csv')
################
#Bnam 2055

y = unique(survey$YEAR)
cdata = subset(glo,select=c(X1000,Y1000,lZ,Clim0.5,BNAM8.5.55,Q))
st_geometry(cdata) <- NULL

cdy = as.data.frame(sapply(cdata,rep.int,length(y)))
cdy$YEAR = rep(y,each=dim(cdata)[1])
cdy$BT = cdy$Clim0.5+cdy$BNAM8.5.55
g = predict(fitpa,newdata=cdy)
g$pred = fitpa$family$linkinv(g$est)

#average over spatial domains for years 2000-2022
gBNAM55 = aggregate(pred~X1000+Y1000+Q,data=g,FUN=mean)

  gsfBNAM55 = st_as_sf(gBNAM55,coords = c("X1000","Y1000"),crs=32620,remove=F)





mm = c(0.001,max(gsfBNAM55$pred))
ggplot(subset(gsfBNAM55)) +
  geom_sf(aes(fill=pred,color=pred)) + 
  scale_fill_viridis_c() +
  scale_color_viridis_c() +
  facet_wrap(~Q) +
 # geom_sf(data=rL,size=1,colour='black',fill=NA)+
  theme( axis.ticks.x = element_blank(),
         axis.text.x = element_blank(),
         axis.title.x = element_blank(),
         axis.ticks.y = element_blank(),
         axis.text.y = element_blank(),
         axis.title.y = element_blank()
  ) +
  coord_sf()
################
#Bnam 2075

y = unique(survey$YEAR)
cdata = subset(glo,select=c(X1000,Y1000,lZ,Clim0.5,BNAM8.5.75,Q))
st_geometry(cdata) <- NULL

cdy = as.data.frame(sapply(cdata,rep.int,length(y)))
cdy$YEAR = rep(y,each=dim(cdata)[1])
cdy$BT = cdy$Clim0.5+cdy$BNAM8.5.75
g = predict(fitpa,newdata=cdy)
g$pred = fitpa$family$linkinv(g$est)

#average over spatial domains for years 2000-2022
gBNAM75 = aggregate(pred~X1000+Y1000+Q,data=g,FUN=mean)

  gsfBNAM75 = st_as_sf(gBNAM75,coords = c("X1000","Y1000"),crs=32620,remove=F)





mm = c(0.001,max(gsfBNAM75$pred))
ggplot(subset(gsfBNAM75)) +
  geom_sf(aes(fill=pred,color=pred)) + 
  scale_fill_viridis_c() +
  scale_color_viridis_c() +
  facet_wrap(~Q) +
 # geom_sf(data=rL,size=1,colour='black',fill=NA)+
  theme( axis.ticks.x = element_blank(),
         axis.text.x = element_blank(),
         axis.title.x = element_blank(),
         axis.ticks.y = element_blank(),
         axis.text.y = element_blank(),
         axis.title.y = element_blank()
  ) +
  coord_sf()


####################################
###Had


y = unique(survey$YEAR)
cdata = subset(glo,select=c(X1000,Y1000,lZ,Clim0.5,HAD90,Q))
st_geometry(cdata) <- NULL

cdy = as.data.frame(sapply(cdata,rep.int,length(y)))
cdy$YEAR = rep(y,each=dim(cdata)[1])
cdy$BT = cdy$Clim0.5+cdy$HAD90
g = predict(fitpa,newdata=cdy)
g$pred = fitpa$family$linkinv(g$est)

#average over spatial domains for years 2000-2022
gHAD90 = aggregate(pred~X1000+Y1000+Q+BT+HAD90,data=g,FUN=mean)

  gsfHAD90 = st_as_sf(gHAD90,coords = c("X1000","Y1000"),crs=32620,remove=F)


###merging back

gsfHAD90 = rename.df(gsfHAD90,c('pred',"BT"),c('predHad90',"BThad90"))

gcc2 = merge(as_tibble(gsfHAD90),as_tibble(gsf),by=c('X1000','Y1000','Q'))
gcc2$geometry.x = NULL
gcc2$geometry = gcc2$geometry.y
gcc2 = st_as_sf(gcc2)

mm = c(0.001,max(gsfHAD50$pred))
ggplot(subset(gcc2)) +
  geom_sf(aes(fill=pred,color=pred)) + 
  scale_fill_viridis_c() +
  scale_color_viridis_c() +
  facet_wrap(~Q) +
 # geom_sf(data=rL,size=1,colour='black',fill=NA)+
  theme( axis.ticks.x = element_blank(),
         axis.text.x = element_blank(),
         axis.title.x = element_blank(),
         axis.ticks.y = element_blank(),
         axis.text.y = element_blank(),
         axis.title.y = element_blank()
  ) +
  coord_sf()



ggplot(subset(gsfHAD50)) +
  geom_sf(aes(fill=HAD50,color=HAD50)) + 
  scale_fill_viridis_c() +
  scale_color_viridis_c() +
  facet_wrap(~Q) +
 # geom_sf(data=rL,size=1,colour='black',fill=NA)+
  theme( axis.ticks.x = element_blank(),
         axis.text.x = element_blank(),
         axis.title.x = element_blank(),
         axis.ticks.y = element_blank(),
         axis.text.y = element_blank(),
         axis.title.y = element_blank()
  ) +
  coord_sf()
