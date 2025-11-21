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
setwd("C:/Users/HowseVJ/Documents/GitHub/bio.lobster/inst/Updates/LFA41")
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

ba = readRDS('~/GitHub/bio.lobster.data/mapping_data/bathymetrySF.rds')
ba = ba %>% st_as_sf() 
st_geometry(ba) = st_geometry(ba)/1000
st_crs(ba) = crs_utm20

G <- suppressWarnings(suppressMessages(
  st_crop(ba,
          c(xmin = -82, ymin = 4539, xmax = 383, ymax = 5200))))


rL = readRDS(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolysSF.rds"))
rL = st_as_sf(rL)
rL = st_make_valid(rL)
st_crs(rL) <- 4326

bathy= ba
LFApolys = rL


####################################   How do I split out the spring and fall in this survey
#### Should I be using rv_sets41() instead for this?

#### RV survey
require(bio.survey)
require(bio.lobster)
require(PBSmapping)
la()

rL = readRDS(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolysSF.rds"))
rL = st_as_sf(rL)
rL = st_make_valid(rL)
st_crs(rL) <- 4326

bathy= ba
LFApolys = rL

x1 =RV_sets41() 
x1$LONGITUDE = ifelse(x1$LONGITUDE>0, x1$LONGITUDE*-1, x1$LONGITUDE)
xs = st_as_sf(x1,coords=c('LONGITUDE','LATITUDE'),crs=4326)
polys = readRDS(file.path(project.datadirectory('bio.lobster'),'data','maps','RVSurveyPolys.rds'))
polys = st_make_valid(polys)
xl = st_join(xs,rL,join=st_within)
xlp = st_join(xl,polys,join=st_within)
xlp = subset(xlp,month(DATE) %in% c(6,7,8))
co = st_union(subset(rL,LFA %in% c(41)))
ico = st_intersection(co,polys)

lfa = c(41)
out = list()
for(i in 1:length(lfa)){
  m = st_intersection(subset(rL,LFA==lfa[i]),polys)
  m$area = st_area(m)/1000000
  m$area = as.numeric(m$area)
  out[[i]]=m
}
strat_within_lfa = bind_rows(out)


#calculate stratified abundances
yrs=1970:2024
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


######################################### How do I split out the spring and fall in this survey
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

lfa = c(41)
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
yrs=1970:2024
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






##### Reproductive Potential 
## Am I still getting the "Maturefemalelengthfrequencies" files from the old method???
## find what data from spring and autumn from Each Survey can be extracted and used to make RepPo faster
## Make plots... 