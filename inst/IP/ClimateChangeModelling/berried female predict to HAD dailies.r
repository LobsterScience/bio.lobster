#berried female predict to canMPI dailies

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

x=readRDS(file='sdmTMBBerriedpabyQFinal.rds')

fitpa = x$model
bspde = x$grid
survey= x$data
Glsur = readRDS('GlorysPredictSurface.rds')
x = Glsur


x = bio.utilities::rename.df(x,c('bottomT','yr'),c('BT','YEAR'))
x = subset(x,z>0)
x$lZ = log(x$z)
x$X1000 = st_coordinates(x)[,1]
x$Y1000 = st_coordinates(x)[,2]
x = subset(x,exp(lZ)<400)

x = as_tibble(subset(x,select=c(Q,YEAR,BT,X1000,Y1000,lZ)))
x$geometry=NULL

g = predict(fitpa,newdata=subset(x,YEAR>1999))

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

saveRDS(gsf,'BerriedPredictionSurface.rds')
gsf = readRDS('BerriedPredictionSurface.rds')
had = readRDS(file='HadDailyTemps.rds')

had = st_transform(had,32620)

st_geometry(had) <- st_geometry(st_as_sf(had$geometry/1000)) 
st_crs(had) <- 32620

had$m = month(had$Date) 
had$Q = ifelse(had$m %in% c(10,11,12),1,ifelse(had$m %in% c(1,2,3),2,ifelse(had$m %in% c(4,5,6),3,4)))

ffh  = st_join(had,rL,join=st_within)
had = subset(ffh,!is.na(LFA))


gp = subset(gsf,YEAR %in% 2016:2021 & Q==3)
hp = subset(had,Q==3)
hp$doy = yday(hp$Date)
hp$id = 1:nrow(hp)
y = 2016:2021

for( i in 1:length(y)){
  g = subset(gp,YEAR==y[i])
  gpp = g
  st_geometry(gpp) <- NULL
  doyy = unique(hp$doy)  
  hp$new=NA
  names(hp)[ncol(hp)]=paste('Pred',unique(g$YEAR),sep=".")
 junk = list()
  for(j in 1:length(doyy)){
      hpp = subset(hp,doy==doyy[j])
      dist = st_nearest_feature(hpp,g)
      hpp[,ncol(hpp)] = gpp[dist,'pred']
junk[[j]] = hpp
  }
hp = dplyr::bind_rows(junk)    
}
v = st_equals(hp)


 #making a unique id per location
      hp$id = NA
      for(i in 1:length(v)){
           hp[v[[i]],'id'] = i  
          }


hpu = subset(hp,select=c(id,geometry)) %>% distinct()

hpu = st_join(hpu,rL,join=st_within)
hpu = subset(hpu,!is.na(LFA))
hp = subset(hp , id %in% hpu$id)

#degree days
xx = aggregate(X.2016~Pred.2016,data=hp,FUN=sum)

#median temp
xx = aggregate(X.2016~Pred.2016+id,data=hp,FUN=median)

#temperature range
xx = aggregate(X.2016~Pred.2016+id,data=hp,FUN=function(x){z = quantile(x,c(0.025,0.975)); z[2] -z[1]})
xx$yr=2016
names(xx)[1:3] = c('BerriedProb','Location','Temperature Range')

xx2 = aggregate(X.2017~Pred.2017+id,data=hp,FUN=function(x){z = quantile(x,c(0.025,0.975)); z[2] -z[1]})
xx2$yr = 2017
names(xx2)[1:3] = c('BerriedProb','Location','Temperature Range')

xx3 = aggregate(X.2018~Pred.2018+id,data=hp,FUN=function(x){z = quantile(x,c(0.025,0.975)); z[2] -z[1]})
xx3$yr = 2018
names(xx3)[1:3] = c('BerriedProb','Location','Temperature Range')

xx4 = aggregate(X.2019~Pred.2019+id,data=hp,FUN=function(x){z = quantile(x,c(0.025,0.975)); z[2] -z[1]})
xx4$yr = 2019
names(xx4)[1:3] = c('BerriedProb','Location','Temperature Range')

xx5 = aggregate(X.2020~Pred.2020+id,data=hp,FUN=function(x){z = quantile(x,c(0.025,0.975)); z[2] -z[1]})
xx5$yr = 2020
names(xx5)[1:3] = c('BerriedProb','Location','Temperature Range')

xx6 = aggregate(X.2021~Pred.2021+id,data=hp,FUN=function(x){z = quantile(x,c(0.025,0.975)); z[2] -z[1]})
xx6$yr = 2021
names(xx6)[1:3] = c('BerriedProb','Location','Temperature Range')

xx7 = dplyr::bind_rows(list(xx,xx2,xx3,xx4,xx5,xx6))
names(xx7)[3] = 'Temp_range'
ggplot(xx7,aes(x=Temp_range))+
    geom_histogram(data=xx7,fill = "blue", alpha = 0.2,position='identity',aes(y= ..density..))+
    geom_histogram(data=subset(xx7,BerriedProb>0.7),fill = "red", alpha = 0.2,position='identity',aes(y= ..density..)) +
    facet_wrap(~yr)
    
