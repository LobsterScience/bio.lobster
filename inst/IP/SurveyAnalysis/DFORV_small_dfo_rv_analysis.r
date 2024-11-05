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
xs = st_as_sf(x,coords=c('LONGITUDE','LATITUDE'),crs=4326)
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
ggplot(out,aes(yr,yst,ymin=ci.yst.l,ymax=ci.yst.u))+geom_point()+geom_path()+geom_errorbar(colour='grey50')+facet_wrap(~LFA,scales='free_y')+xlab('Year')+ylab('Commercial Lobster Density')+theme_test(base_size = 14)




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
#### RV survey
require(bio.survey)
require(bio.lobster)
require(PBSmapping)
la()

rL = readRDS(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolysSF.rds"))
rL = st_as_sf(rL)
rL = st_make_valid(rL)

st_crs(rL) <- 4326

#using strata within an adjacent 
x=NEFSC_sets() 
xs = st_as_sf(x,coords=c('LONGITUDE','LATITUDE'),crs=4326)
polys = sf::read_sf(find.bio.gis('BTS_Strata'),crs=4326) 
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

