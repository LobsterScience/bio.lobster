#simple abundance trends

require(tidyr)
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
fd=file.path(project.datadirectory('Framework_LFA35_38'),'outputs','SURVEYS')
setwd(fd)
crs_utm20 <- 32620

sf_use_s2(FALSE) #needed for cropping
###data in

ns_coast =readRDS(file.path( project.datadirectory("bio.lobster"), "data","maps","CoastSF.rds"))
st_crs(ns_coast) <- 4326 # 'WGS84'; necessary on some installs
ns_coast <- suppressWarnings(suppressMessages(
  st_crop(ns_coast,
          c(xmin = -68, ymin = 41, xmax = -64, ymax = 47.5))))
ns_coast <- st_transform(ns_coast, crs_utm20)
st_geometry(ns_coast) = st_geometry(ns_coast)
st_crs(ns_coast) <- crs_utm20

ba = readRDS('~/git/bio.lobster.data/mapping_data/bathymetrySF.rds')
ba = ba %>% st_as_sf() 
st_geometry(ba) = st_geometry(ba)/1000
st_crs(ba) = crs_utm20

G <- suppressWarnings(suppressMessages(
  st_crop(ba,
          c(xmin = -82, ymin = 4539, xmax = 383, ymax = 5200))))


rL = readRDS(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolysSF.rds"))
rL = st_as_sf(rL)
st_crs(rL) <- 4326
#rL = st_transform(rL,crs_utm20) 
#st_geometry(rL) <- st_geometry(st_as_sf(rL$geometry/1000)) 
#st_crs(rL) <- crs_utm20

bathy= ba
LFApolys = rL


####################################
#### RV survey
require(bio.survey)
require(bio.lobster)
require(PBSmapping)
la()

rL = readRDS(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolysNoLand.rds"))
rL = st_as_sf(rL)
rL = st_make_valid(rL)

st_crs(rL) <- 4326


x1 =RV_sets() 
x1$LONGITUDE = ifelse(x1$LONGITUDE>0, x1$LONGITUDE*-1, x1$LONGITUDE)
xs = st_as_sf(x1,coords=c('LONGITUDE','LATITUDE'),crs=4326)
polys = readRDS(file.path(project.datadirectory('bio.lobster'),'data','maps','RVSurveyPolys.rds'))
polys = st_make_valid(polys)
xl = st_join(xs,rL,join=st_within)
xlp = st_join(xl,polys,join=st_within)
xlp = subset(xlp,month(DATE) %in% c(6,7,8))
co = st_union(subset(rL,LFA %in% c(34,35,36,37,38)))
ico = st_intersection(co,polys)

lfa = c(34,35,36,37,38)
out = list()
for(i in 1:length(lfa)){
  m = st_intersection(subset(rL,LFA==lfa[i]),polys)
  m$area = st_area(m)/1000000
  m$area = as.numeric(m$area)
  out[[i]]=m
}
strat_within_lfa = bind_rows(out)


#calculate stratified abundances
yrs=1970:2023
gr = expand.grid(lfa[-4],yrs)
out = data.frame(yr=NA,LFA=NA,yst=NA,yst.se=NA,ci.yst.l=NA,ci.yst.u=NA,Yst=NA,ci.Yst.l=NA,ci.Yst.u=NA,dwao=NA,Nsets=NA,NsetswithLobster=NA,gini = NA)

for(i in 1:nrow(gr)){
 n = subset(xlp,LFA==gr[i,1] & YEAR==gr[i,2]) 
 n$Strata = n$Label
 j = subset(strat_within_lfa,LFA==gr[i,1])  
st = list(Strata=j$Label,NH=j$area)

sc = subset(n,select=c(mission,setno,Legal_wt,Strata))
oldClass(sc) <- c("data.frame", "strata.data")
sW = Stratify(sc,st,sc$Legal_wt)
ssW = summary(sW)
bsW=summary(boot.strata(sW,method='BWR',nresamp=1000),ci.method='Percentile')
nt  = sum(sW$Nh)
out[i,] = c(gr[i,2],gr[i,1],ssW[[1]],ssW[[2]],bsW[[1]][1],bsW[[1]][2],ssW[[3]],bsW[[1]][1]*nt,bsW[[1]][2]*nt,ssW$dwao,sum(sW[['nh']]),sum(sW[['nhws']]),ssW$gini)
}
ggplot(subset(out,LFA %ni% 34),aes(yr,yst,ymin=ci.yst.l,ymax=ci.yst.u))+geom_point()+geom_path()+geom_errorbar(colour='grey50',width=0)+facet_wrap(~LFA,scales='free_y',ncol=2)+xlab('Year')+ylab('Commercial Lobster Density')+theme_test(base_size = 14)




#######################################################################################

######################
#comparing to modelled proportions
# x =RV_sets_modelledProps() 
# xs = st_as_sf(x,coords=c('LONGITUDE','LATITUDE'),crs=4326)
# polys = readRDS(file.path(project.datadirectory('bio.lobster'),'data','maps','RVSurveyPolys.rds'))
# polys = st_make_valid(polys)
# xl = st_join(xs,rL,join=st_within)
# xlp = st_join(xl,polys,join=st_within)
# xlp = subset(xlp,month(DATE) %in% c(6,7,8))
# co = st_union(subset(rL,LFA %in% c(34,35,36,37,38)))
# ico = st_intersection(co,polys)
# 
# lfa = c(34,35,36,37,38)
# out = list()
# for(i in 1:length(lfa)){
#   m = st_intersection(subset(rL,LFA==lfa[i]),polys)
#   m$area = st_area(m)/1000000
#   m$area = as.numeric(m$area)
#   out[[i]]=m
# }
# strat_within_lfa = bind_rows(out)
# strat_within_lfa = aggregate(area~Label+LFA,data=strat_within_lfa,FUN=sum)
# 
# 
# #calculate stratified abundances
# yrs=1970:2023
# gr = expand.grid(lfa[-4],yrs)
# out = data.frame(yr=NA,LFA=NA,yst=NA,yst.se=NA,ci.yst.l=NA,ci.yst.u=NA,Yst=NA,ci.Yst.l=NA,ci.Yst.u=NA,dwao=NA,Nsets=NA,NsetswithLobster=NA,gini = NA)
# m=0
# load_all('~/git/bio.survey/')
# for(i in 1:nrow(gr)){
#   m=m+1
#   n = subset(xlp,LFA==gr[i,1] & YEAR==gr[i,2]) 
#   n$Strata = n$Label
#   j = subset(strat_within_lfa,LFA==gr[i,1])  
#   j = subset(j,Label %in% n$Strata)
#   st = list(Strata=j$Label,NH=j$area)
#   sc = subset(n,select=c(mission,setno,Legal_wt,Strata))
#   oldClass(sc) <- c("data.frame", "strata.data")
#   sW = Stratify(sc,st,sc$Legal_wt)
#   #if( sum(sW$nhws)==0) {next(); m=m-1}
#   ssW = summary(sW)
#   bsW=summary(boot.strata(sW,method='BWR',nresamp=1000),ci.method='Percentile')
#   nt  = sum(sW$Nh)
#   out[m,] = c(gr[i,2],gr[i,1],ssW[[1]],ssW[[2]],bsW[[1]][1],bsW[[1]][2],ssW[[3]],bsW[[1]][1]*nt,bsW[[1]][2]*nt,ssW$dwao,sum(sW[['nh']]),sum(sW[['nhws']]),ssW$gini)
# }
# 
# ggplot(out,aes(yr,yst,ymin=ci.yst.l,ymax=ci.yst.u))+geom_point()+geom_path()+geom_errorbar(colour='grey50')+facet_wrap(~LFA,scales='free_y')+xlab('Year')+ylab('Commercial Lobster Density')+theme_test(base_size = 14)+
#   geom_point(data=out1,aes(yr,yst,ymin=ci.yst.l,ymax=ci.yst.u),col='red')+geom_path()
#   
# 
# xx1 = subset(x1,select=c('mission','setno','Legal_wt'))
# xx = subset(x,select=c('mission','setno','Legal_wt'))
# xx$Legal_wtE = xx$Legal_wt
# xx$Legal_wt = NULL
# v = merge(xx,xx1)
# v$d= (v$Legal_wtE-v$Legal_wt)/v$Legal_wt*100
# 
# v$yr = as.numeric(substr(v$mission,4,7))
# with(subset(v,yr<1999),max(Legal_wt,na.rm=T))
# with(subset(v,yr>1998 & Legal_wt>0),median(d))
# 
# v1 = subset(v,yr>1999 & Legal_wt>0)
# 
# g1 = ggplot(subset(v1,Legal_wt<23),aes(x=d))+geom_density()+xlim(c(-4,20))+labs(x='Difference Between Observed and Predicted',y='Density')+theme_test(base_size = 14)
# 
# 
# xx1 = subset(x1,select=c('mission','setno','Legal'))
# xx = subset(x,select=c('mission','setno','Legal'))
# xx$Legal_E = xx$Legal
# xx$Legal = NULL
# v = merge(xx,xx1)
# v$d= (v$Legal_E-v$Legal)/v$Legal*100
# median(v$d[],na.rm=T)
# v$yr = as.numeric(substr(v$mission,4,7))
# with(subset(v,yr<1999),max(Legal,na.rm=T))
# 
# v1 = subset(v,yr>1999 & Legal>0)
#   with(subset(v,yr>1998 & Legal>0),median(d))
# #9.4%
# ggplot(subset(v1,Legal<16),aes(x=d))+geom_density()+xlim(c(-4,20))+labs(x='Difference Between Observed and Predicted',y='Density')+theme_test(base_size = 14)
# 
# 
# 
# 
# xx1 = subset(x1,select=c('mission','setno','Recruit'))
# xx = subset(x,select=c('mission','setno','Recruit'))
# xx$Recruit_E = xx$Recruit
# xx$Recruit = NULL
# v = merge(xx,xx1)
# v$d= (v$Recruit_E-v$Recruit)/v$Recruit*100
# median(v$d[],na.rm=T)
# v$yr = as.numeric(substr(v$mission,4,7))
# with(subset(v,yr<1999),max(Recruit,na.rm=T))
# 
# v1 = subset(v,yr>1999 & Legal>0)
# with(subset(v,yr>1998 & Recruit>0),median(d))


###index from DMR

z = lobster.db('DMR.redo')
z$N = z$Commwt
z$YEAR = z$Year
z = subset(z,select=c(id,YEAR,Depth_Stratum, Region,N))
zw = read.csv(file.path(project.datadirectory('bio.lobster'),'data','MaineDMRSurvey','MENH_Survey_Area.csv'))
zw = subset(zw,REGION==5, select=c(REGION,STRATUM,SQKM))

y = unique(z$YEAR)
out = data.frame(yr=NA,yst=NA,yst.se=NA,ci.yst.l=NA,ci.yst.u=NA,Yst=NA,ci.Yst.l=NA,ci.Yst.u=NA,dwao=NA,Nsets=NA,NsetswithLobster=NA,gini = NA)

for(i in 1:length(y)){
  n = subset(z,YEAR==y[i]) 
  n$Strata = n$Depth_Stratum
  j =   zw
  st = list(Strata=j$STRATUM,NH=j$SQKM)
  sc = subset(n,select=c(id,N,Strata))
  oldClass(sc) <- c("data.frame", "strata.data")
  sW = Stratify(sc,st,sc$N)
  ssW = summary(sW)
  bsW=summary(boot.strata(sW,method='BWR',nresamp=1000),ci.method='Percentile')
  nt  = sum(sW$Nh)
  out[i,] = c(y[i],ssW[[1]],ssW[[2]],bsW[[1]][1],bsW[[1]][2],ssW[[3]],bsW[[1]][1]*nt,bsW[[1]][2]*nt,ssW$dwao,sum(sW[['nh']]),sum(sW[['nhws']]),ssW$gini)
}
ggplot(out,aes(yr,yst,ymin=ci.yst.l,ymax=ci.yst.u))+geom_point()+geom_path()+geom_errorbar(colour='grey50',width=0)+xlab('Year')+ylab('Commercial Lobster Density')+theme_test(base_size = 14)


################################################################################################################################################
#### NEFSC survey
require(bio.survey)
require(bio.lobster)
require(PBSmapping)
la()

rL = readRDS(file.path(bio.directory,'bio.lobster.data','mapping_data',"LFAPolysSF.rds"))
rL = st_as_sf(rL)
rL = st_make_valid(rL)

st_crs(rL) <- 4326

#using strata within an adjacent 
x=NEFSC_sets() 
xs = st_as_sf(x,coords=c('LONGITUDE','LATITUDE'),crs=4326)
polys = st_as_sf(readRDS(file.path(bio.directory,'bio.lobster.data','mapping_data','BTS_Strata.rds')) )
polys = st_make_valid(polys)

xlp = st_join(xs,polys,join=st_within)
xlp = subset(xlp,month(DATE) >8)

lfa = c(34,35,36,37,38)
out = list()
for(i in 1:length(lfa)){
  m = st_intersection(subset(rL,LFA==lfa[i]),polys)
  m$area = st_area(m)/1000000
  m$area = as.numeric(m$area)
  out[[i]]=m
}
strat_within_lfa = bind_rows(out)

po = subset(polys,STRATA %in% unique(strat_within_lfa$STRATA))
po$area = st_area(po)/1000000
po$area = as.numeric(po$area)

xlp = subset(xlp,STRATA %in% unique(po$STRATA))

#calculate stratified abundances
yrs=1970:2023
out = data.frame(yr=NA,yst=NA,yst.se=NA,ci.yst.l=NA,ci.yst.u=NA,Yst=NA,ci.Yst.l=NA,ci.Yst.u=NA,dwao=NA,Nsets=NA,NsetswithLobster=NA,gini = NA)
m=0
for(i in 1:length(yrs)){
  n = subset(xlp,YEAR==yrs[i] & !is.na(STRATUMA)) 
  m=m+1
  if(nrow(n)==0) {next;m=m-1}
  n$Legal_wt =n$Legal_wt/n$OFFSET
  n$Strata = as.numeric(n$STRATUMA)
  j = po  
  st = list(Strata=as.numeric(j$STRATA),NH=j$area)
  sc = subset(n,select=c(MISSION,SETNO,Legal_wt,Strata))
  oldClass(sc) <- c("data.frame", "strata.data")
  sW = Stratify(sc,st,sc$Legal_wt)
  ssW = summary(sW)
  bsW=summary(boot.strata(sW,method='BWR',nresamp=1000),ci.method='Percentile')
  nt  = sum(sW$Nh)
  out[m,] = c(yrs[i],ssW[[1]],ssW[[2]],bsW[[1]][1],bsW[[1]][2],ssW[[3]],bsW[[1]][1]*nt,bsW[[1]][2]*nt,ssW$dwao,sum(sW[['nh']]),sum(sW[['nhws']]),ssW$gini)
}
ggplot(out,aes(yr,yst,ymin=ci.yst.l,ymax=ci.yst.u))+geom_point()+geom_path()+geom_errorbar(colour='grey50',width=0)+xlab('Year')+ylab('Commercial Lobster Density')+theme_test(base_size = 14)

#########################################################################
############
#ILTS Leg 1


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
crs_utm20 <- 32620

y = ILTS_ITQ_All_Data(redo_base_data = F,size=c(82,300),aggregate=T,species=2550,biomass = T)
y = subset(y,select=c(TRIP_ID,SET_NO, SET_LONG,SET_LAT,SET_DATE,SA_CORRECTED_PRORATED_N))    
y$Survey='ILTS'

survey = (y)
survey = subset(survey, month(SET_DATE) %in% c(6:8))
survey$YEAR = year(survey$SET_DATE)
survey = st_as_sf(survey,coords = c('SET_LONG','SET_LAT'),crs=4326)

ns_coast =readRDS(file.path( project.datadirectory("bio.lobster"), "data","maps","CoastSF.rds"))
st_crs(ns_coast) <- 4326 # 'WGS84'; necessary on some installs

survey$Legal_wt = survey$SA_CORRECTED_PRORATED_N

survey = st_transform(survey,crs_utm20)

surv_utm_coords = st_coordinates(survey)
survey$X1000 <- surv_utm_coords[,1] /1000
survey$Y1000 <- surv_utm_coords[,2] /1000
st_geometry(survey) <- NULL
survey = st_as_sf(survey,coords = c('X1000','Y1000'),crs=crs_utm20)

sf_use_s2(FALSE) #needed for cropping
survey <- suppressWarnings(suppressMessages(
  st_crop(survey,
          c(xmin = -82, ymin = 4539, xmax = 383, ymax = 5200))))


# Project our survey data coordinates:


ns_coast <- suppressWarnings(suppressMessages(
  st_crop(ns_coast,
          c(xmin = -68, ymin = 41, xmax = -64, ymax = 47.5))))

ns_coast <- st_transform(ns_coast, crs_utm20)
st_geometry(ns_coast) = st_geometry(ns_coast)
st_crs(ns_coast) <- crs_utm20

survey <- survey %>%   
  st_as_sf()

surv_utm_coords <- st_coordinates(survey)

survey$X1000 <- surv_utm_coords[,1] 
survey$Y1000 <- surv_utm_coords[,2] 
survey = subset(survey,!is.na(Legal_wt))


#add in bathy 
#allocating depth to location
ba = readRDS('~/git/bio.lobster.data/mapping_data/bathymetrySF.rds')
ba = ba %>% st_as_sf() 
st_geometry(ba) = st_geometry(ba)/1000
st_crs(ba) = 32620

ss = st_nearest_feature(survey,ba)
ds = st_distance(survey,ba[ss,],by_element=T)
st_geometry(ba) = NULL
survey$z = ba$z[ss]
survey$z_dist = as.numeric(ds)

ss = as_tibble(survey)
ss$depth_scaled = (ss$z-mean(ss$z))/sd(ss$z)
#building basis splines
k <- 4
knots <- quantile(ss$depth_scaled, p = seq(0, 1, len = k)[-c(1,k)])
bs <- splines::bs(ss$depth_scaled, knots = knots, intercept = FALSE)
bs <- as.data.frame(bs)
names(bs) <- paste0("bs", names(bs))
ss <- ss[, setdiff(names(ss), names(ss)[grep("^bs[0-9]+", names(ss))])]
ss <- cbind(ss, bs)


spde <- make_mesh(ss, xy_cols = c("X1000", "Y1000"),
                  n_knots=300,type = "cutoff_search")
plot(spde)

# Add on the barrier mesh component:
mesh <- sdmTMBextra::add_barrier_mesh(
  spde, ns_coast, range_fraction = 0.1,
  proj_scaling = 1000, plot = TRUE
)
mesh_df_water <- mesh$mesh_sf[mesh$normal_triangles, ]
mesh_df_land <- mesh$mesh_sf[mesh$barrier_triangles, ]
ggplot(ns_coast) +
  geom_sf() +
  geom_sf(data = mesh_df_water, size = 1, colour = "blue") +
  geom_sf(data = mesh_df_land, size = 1, colour = "green")+
  coord_sf()

##keep reml=F for comparing fixed effects, use reml=T from comparing models with different random effects
##AICs are really for fixed effects, 
fit = sdmTMB(Legal_wt~s(z),
             data=ss,
            
                          time='YEAR', 
             mesh=bspde,
             family=tweedie(link='log'),
             spatial='on',
             spatiotemporal = 'ar1'
)



##AICs are really for fixed effects, 
fit2 = sdmTMB(Legal_wt~0,
              data=ss,
              time='YEAR',
              time_varying = ~1+ depth_scaled,
              mesh=bspde,
              family=tweedie(link='log'),
              spatial='on',
              spatiotemporal = 'ar1'
)

fit3 = sdmTMB(Legal_wt~0,
              data=ss,
              time='YEAR',
              time_varying = ~1+ bs1+bs2+bs3+bs4+bs5,
              mesh=bspde,
              family=tweedie(link='log'),
              spatial='on',
              spatiotemporal = 'ar1'
)



#add in bathy 
#allocating depth to location
ba = readRDS('~/git/bio.lobster.data/mapping_data/bathymetrySF.rds')
ba = ba %>% st_as_sf() 
st_geometry(ba) = st_geometry(ba)/1000
st_crs(ba) = 32620

G <- suppressWarnings(suppressMessages(
  st_crop(ba,
          c(xmin = -82, ymin = 4539, xmax = 383, ymax = 5200))))


rL = readRDS(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolysSF.rds"))
rL = st_as_sf(rL)
st_crs(rL) <- 4326
rL = st_transform(rL,32620) 
st_geometry(rL) <- st_geometry(st_as_sf(rL$geometry/1000)) 
st_crs(rL) <- 32620

bathy= ba
LFApolys = rL
ff = readRDS(file.path( project.datadirectory("bio.lobster"), "data","maps","bathy_LFAPolysSF.rds"))
ff = subset(ff,z<max(ss$z))
yy = unique(ss$YEAR)
o = list()
for(i in 1:length(yy)){
  f = ff
  f$YEAR=yy[i]
  o[[i]]=f
}
ff = do.call(rbind,o)

sg = st_coordinates(ff)
ff$X1000 = sg[,1]
ff$Y1000 = sg[,2]
ff$SOURCE='ILTS'
f = as_tibble(ff)
g = predict(fit,newdata=f,return_tmb_object = T)
ga = predict(fit,newdata=f,se_fit = T,nsim=200)
gg = fit$family$linkinv(ga)

g$data$se = apply(gg,1,sd)
ind = get_index(g,bias_correct = F)


with(ind,plot(YEAR,est/1000,lwd=2,type='b',ylim=c(0,max(upr)/1000),main='overall'))
with(ind,lines(YEAR,lwr/1000,lty=2))
with(ind,lines(YEAR,upr/1000,lty=2))


f34 = subset(f,LFA==34)
g34 = predict(fit,newdata=f34,return_tmb_object = T)
ind34 = get_index(g34,bias_correct = T)
with(ind34,plot(YEAR,est/1000,lwd=2,type='b',ylim=c(0,max(upr)/1000),main='LFA34'))
with(ind34,lines(YEAR,lwr/1000,lty=2))
with(ind34,lines(YEAR,upr/1000,lty=2))

ggplot(ind34,aes(x=YEAR,y=est/1000,ymin=lwr/1000,ymax=upr/1000))+geom_point()+geom_path()+geom_ribbon(alpha=.2)+labs(x='Year',y='Commercial Biomass (t)')+theme_test(base_size = 14)


f38 = subset(f,LFA==38)
g38 = predict(fit,newdata=f38,return_tmb_object = T)
ind38 = get_index(g38,bias_correct = T)

ggplot(ind38,aes(x=YEAR,y=est/1000,ymin=lwr/1000,ymax=upr/1000))+geom_point()+geom_path()+geom_ribbon(alpha=.2)+labs(x='Year',y='Commercial Biomass (t)')+theme_test(base_size = 14)

##############################################################################
#Leg 2 of ILTS

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
crs_utm20 <- 32620

y = ILTS_ITQ_All_Data(redo_base_data = F,size=c(82,300),aggregate=T,species=2550,biomass = T)
y = subset(y,select=c(TRIP_ID,SET_NO, SET_LONG,SET_LAT,SET_DATE,SA_CORRECTED_PRORATED_N))    
y$Survey='ILTS'

survey = (y)
survey$YEAR = year(survey$SET_DATE)

survey = subset(survey, month(SET_DATE) >8 & YEAR>2018)
survey = st_as_sf(survey,coords = c('SET_LONG','SET_LAT'),crs=4326)

ns_coast =readRDS(file.path( project.datadirectory("bio.lobster"), "data","maps","CoastSF.rds"))
st_crs(ns_coast) <- 4326 # 'WGS84'; necessary on some installs

survey$Legal_wt = survey$SA_CORRECTED_PRORATED_N

survey = st_transform(survey,crs_utm20)

surv_utm_coords = st_coordinates(survey)
survey$X1000 <- surv_utm_coords[,1] /1000
survey$Y1000 <- surv_utm_coords[,2] /1000
st_geometry(survey) <- NULL
survey = st_as_sf(survey,coords = c('X1000','Y1000'),crs=crs_utm20)

sf_use_s2(FALSE) #needed for cropping
survey <- suppressWarnings(suppressMessages(
  st_crop(survey,
          c(xmin = -82, ymin = 4539, xmax = 383, ymax = 5200))))


# Project our survey data coordinates:


ns_coast <- suppressWarnings(suppressMessages(
  st_crop(ns_coast,
          c(xmin = -68, ymin = 41, xmax = -64, ymax = 47.5))))

ns_coast <- st_transform(ns_coast, crs_utm20)
st_geometry(ns_coast) = st_geometry(ns_coast)
st_crs(ns_coast) <- crs_utm20

survey <- survey %>%   
  st_as_sf()

surv_utm_coords <- st_coordinates(survey)

survey$X1000 <- surv_utm_coords[,1] 
survey$Y1000 <- surv_utm_coords[,2] 
survey = subset(survey,!is.na(Legal_wt))


#add in bathy 
#allocating depth to location
ba = readRDS('~/git/bio.lobster.data/mapping_data/bathymetrySF.rds')
ba = ba %>% st_as_sf() 
st_geometry(ba) = st_geometry(ba)/1000
st_crs(ba) = 32620

ss = st_nearest_feature(survey,ba)
ds = st_distance(survey,ba[ss,],by_element=T)
st_geometry(ba) = NULL
survey$z = ba$z[ss]
survey$z_dist = as.numeric(ds)

ss = as_tibble(survey)
spde <- make_mesh(ss, xy_cols = c("X1000", "Y1000"),
                  n_knots=200,type = "cutoff_search")
plot(spde)

# Add on the barrier mesh component:
bspde <- sdmTMBextra::add_barrier_mesh(
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

##keep reml=F for comparing fixed effects, use reml=T from comparing models with different random effects
##AICs are really for fixed effects, 
fit = sdmTMB(Legal_wt~s(z),
             data=ss,
             time='YEAR', 
             mesh=bspde,
             family=tweedie(link='log'),
             spatial='on',
             spatiotemporal = 'ar1'
)

stripDLLs()

##AICs are really for fixed effects, 
fit2 = sdmTMB(Legal_wt~s(z),
              data=ss,
              time='YEAR', 
              mesh=bspde,
              family=tweedie(link='log'),
              spatial='on',
              spatiotemporal = 'ar1'
)



#add in bathy 
#allocating depth to location
ba = readRDS('~/git/bio.lobster.data/mapping_data/bathymetrySF.rds')
ba = ba %>% st_as_sf() 
st_geometry(ba) = st_geometry(ba)/1000
st_crs(ba) = 32620

G <- suppressWarnings(suppressMessages(
  st_crop(ba,
          c(xmin = -82, ymin = 4539, xmax = 383, ymax = 5200))))


rL = readRDS(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolysSF.rds"))
rL = st_as_sf(rL)
st_crs(rL) <- 4326
rL = st_transform(rL,32620) 
st_geometry(rL) <- st_geometry(st_as_sf(rL$geometry/1000)) 
st_crs(rL) <- 32620

bathy= ba
LFApolys = rL
ff = readRDS(file.path( project.datadirectory("bio.lobster"), "data","maps","bathy_LFAPolysSF.rds"))
ff = subset(ff,z<max(ss$z))
yy = unique(ss$YEAR)
o = list()
for(i in 1:length(yy)){
  f = ff
  f$YEAR=yy[i]
  o[[i]]=f
}
ff = do.call(rbind,o)

sg = st_coordinates(ff)
ff$X1000 = sg[,1]
ff$Y1000 = sg[,2]
ff$SOURCE='ILTS'
f = as_tibble(ff)


f35 = subset(f,LFA==35)
g35 = predict(fit,newdata=f35,return_tmb_object = T)
ind35 = get_index(g35,bias_correct = T)

ggplot(ind35,aes(x=YEAR,y=est/1000,ymin=lwr/1000,ymax=upr/1000))+geom_point()+geom_path()+geom_ribbon(alpha=.2)+labs(x='Year',y='Commercial Biomass (t)')+theme_test(base_size = 14)

f36 = subset(f,LFA==36)
g36 = predict(fit,newdata=f36,return_tmb_object = T)
ind36 = get_index(g36,bias_correct = T)

ggplot(ind36,aes(x=YEAR,y=est/1000,ymin=lwr/1000,ymax=upr/1000))+geom_point()+geom_path()+geom_ribbon(alpha=.2)+labs(x='Year',y='Commercial Biomass (t)')+theme_test(base_size = 14)
