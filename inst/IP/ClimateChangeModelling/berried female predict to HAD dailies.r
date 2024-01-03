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
hp = subset(had,Q==3 & Depth_m<500)
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
        print(i)
           hp[v[[i]],'id'] = i  
          }


hpu = subset(hp,select=c(id,geometry)) %>% distinct()

hpu = st_join(hpu,rL,join=st_within)
hpu = subset(hpu,!is.na(LFA))
hp = subset(hp , id %in% hpu$id)

LL  = aggregate(LFA~id,data=hp,FUN=function(x)median(x,na.rm=T))
names(LL)[1] = 'Location'

#temperature range
xx = aggregate(X.2016~Pred.2016+id,data=hp,FUN=function(x){z = quantile(x,c(0.025,0.975)); z[2] -z[1]})
xx$yr=2016
names(xx)[1:3] = c('BerriedProb','Location','Temperature_Range')

xx2 = aggregate(X.2017~Pred.2017+id,data=hp,FUN=function(x){z = quantile(x,c(0.025,0.975)); z[2] -z[1]})
xx2$yr = 2017
names(xx2)[1:3] = c('BerriedProb','Location','Temperature_Range')

xx3 = aggregate(X.2018~Pred.2018+id,data=hp,FUN=function(x){z = quantile(x,c(0.025,0.975)); z[2] -z[1]})
xx3$yr = 2018
names(xx3)[1:3] = c('BerriedProb','Location','Temperature_Range')

xx4 = aggregate(X.2019~Pred.2019+id,data=hp,FUN=function(x){z = quantile(x,c(0.025,0.975)); z[2] -z[1]})
xx4$yr = 2019
names(xx4)[1:3] = c('BerriedProb','Location','Temperature_Range')

xx5 = aggregate(X.2020~Pred.2020+id,data=hp,FUN=function(x){z = quantile(x,c(0.025,0.975)); z[2] -z[1]})
xx5$yr = 2020
names(xx5)[1:3] = c('BerriedProb','Location','Temperature_Range')

xx6 = aggregate(X.2021~Pred.2021+id,data=hp,FUN=function(x){z = quantile(x,c(0.025,0.975)); z[2] -z[1]})
xx6$yr = 2021
names(xx6)[1:3] = c('BerriedProb','Location','Temperature_Range')

xx7 = dplyr::bind_rows(list(xx,xx2,xx3,xx4,xx5,xx6))
names(xx7)[3] = 'Temp_range'

xx7 = merge(xx7,LL)
ggplot(subset(xx7,Temp_range>0.5),aes(x=Temp_range))+
    geom_histogram(data=subset(xx7,Temp_range>0.5),fill = "blue", alpha = 0.2,position='identity',aes(y= ..density..))+
    geom_histogram(data=subset(xx7,Temp_range>.5 & BerriedProb>0.90),fill = "red", alpha = 0.2,position='identity',aes(y= ..density..)) +
    facet_wrap(~LFA)

savePlot('TemperatureRangeBerriedLocation.png')
#median temp
xx = aggregate(X.2016~Pred.2016+id,data=hp,FUN=median)
xx$yr=2016
names(xx)[1:3] = c('BerriedProb','Location','Temperature Median')

xx2 = aggregate(X.2017~Pred.2017+id,data=hp,FUN=median)
xx2$yr = 2017
names(xx2)[1:3] = c('BerriedProb','Location','Temperature Median')

xx3 = aggregate(X.2018~Pred.2018+id,data=hp,FUN=median)
xx3$yr = 2018
names(xx3)[1:3] = c('BerriedProb','Location','Temperature Median')

xx4 = aggregate(X.2019~Pred.2019+id,data=hp,FUN=median)
xx4$yr = 2019
names(xx4)[1:3] = c('BerriedProb','Location','Temperature Median')

xx5 = aggregate(X.2020~Pred.2020+id,data=hp,FUN=median)
xx5$yr = 2020
names(xx5)[1:3] = c('BerriedProb','Location','Temperature Median')

xx6 = aggregate(X.2021~Pred.2021+id,data=hp,FUN=median)
xx6$yr = 2021
names(xx6)[1:3] = c('BerriedProb','Location','Temperature Median')

xx8 = dplyr::bind_rows(list(xx,xx2,xx3,xx4,xx5,xx6))
xx8 = merge(xx8,LL)
names(xx8)[3] = 'Temp_median'
ggplot(subset(xx8),aes(x=Temp_median))+
    geom_histogram(data=subset(xx8,Temp_median>0.5),fill = "blue", alpha = 0.2,position='identity',aes(y= ..density..))+
    geom_histogram(data=subset(xx8,Temp_median>0.5 & BerriedProb>0.75),fill = "red", alpha = 0.2,position='identity',aes(y= ..density..)) +
    facet_wrap(~LFA)

savePlot('TemperatureMedianBerriedLocation_NOYEAR.png')

xx9=merge(xx7,xx8)
xx10 = merge(xx9,hpu,by.x='Location',by.y='id')
xx11 = st_as_sf(xx10)
ggplot(xx11) +
  geom_sf(aes(fill=Temp_range,color=Temp_range)) + 
  scale_fill_viridis_c() +
  scale_color_viridis_c() +
  facet_wrap(~yr) +
 # geom_sf(data=rL,size=1,colour='black',fill=NA)+
  theme( axis.ticks.x = element_blank(),
         axis.text.x = element_blank(),
         axis.title.x = element_blank(),
         axis.ticks.y = element_blank(),
         axis.text.y = element_blank(),
         axis.title.y = element_blank()
  ) +
  coord_sf()
savePlot('TemperatureRangeLocation.png')

ggplot(xx11) +
  geom_sf(aes(fill=Temp_median,color=Temp_median)) + 
  scale_fill_viridis_c() +
  scale_color_viridis_c() +
  facet_wrap(~yr) +
 # geom_sf(data=rL,size=1,colour='black',fill=NA)+
  theme( axis.ticks.x = element_blank(),
         axis.text.x = element_blank(),
         axis.title.x = element_blank(),
         axis.ticks.y = element_blank(),
         axis.text.y = element_blank(),
         axis.title.y = element_blank()
  ) +
  coord_sf()
savePlot('TemperatureMedianLocation.png')

st_geometry(xx11) <- NULL
xx11$Temp_range = round(xx11$Temp_range,2)
xx11$Temp_median = round(xx11$Temp_median,2)

ofi = readRDS(file.path(project.datadirectory('bio.lobster'),'data','OFI_lobster_covariates','ofi_pointData500m.rds'))
ofi$X1000 = st_coordinates(ofi)[,1]/1000
ofi$Y1000 = st_coordinates(ofi)[,2]/1000
st_geometry(ofi) <-NULL
ofi = st_as_sf(ofi,coords=c('X1000','Y1000'),crs=32620)
hp_ofi = st_join(hp,ofi,join=st_nearest_feature,left=T)
hp_ofi$depth = hp_ofi$depth*-1

#depth
xx = aggregate(Depth_m~Pred.2016+id,data=hp_ofi,FUN=median)
xx$yr=2016
names(xx)[1:3] = c('BerriedProb','Location','Depth')

xx2 = aggregate(Depth_m~Pred.2017+id,data=hp,FUN=median)
xx2$yr = 2017
names(xx2)[1:3] = c('BerriedProb','Location','Depth')

xx3 = aggregate(Depth_m~Pred.2018+id,data=hp,FUN=median)
xx3$yr = 2018
names(xx3)[1:3] = c('BerriedProb','Location','Depth')

xx4 = aggregate(Depth_m~Pred.2019+id,data=hp,FUN=median)
xx4$yr = 2019
names(xx4)[1:3] = c('BerriedProb','Location','Depth')

xx5 = aggregate(Depth_m~Pred.2020+id,data=hp,FUN=median)
xx5$yr = 2020
names(xx5)[1:3] = c('BerriedProb','Location','Depth')

xx6 = aggregate(Depth_m~Pred.2021+id,data=hp,FUN=median)
xx6$yr = 2021
names(xx6)[1:3] = c('BerriedProb','Location','Depth')

xx8 = dplyr::bind_rows(list(xx,xx2,xx3,xx4,xx5,xx6))
names(xx8)[3] = 'Depth'
xx8 = merge(xx8,LL)

ggplot(subset(xx8,Depth<500),aes(x=Depth))+
    geom_histogram(data=subset(xx8,Depth<500),fill = "blue", alpha = 0.2,position='identity',aes(y= ..density..))+
    geom_histogram(data=subset(xx8,Depth < 500 & BerriedProb>0.90),fill = "red", alpha = 0.2,position='identity',aes(y= ..density..)) +
    facet_wrap(~LFA)
savePlot('DepthBerriedLocation.png')
##apply these conditions to predictions from model to make sure we are still finding the right places

hpu = subset(hp,select=c(id,geometry)) %>% distinct()

hp$PredCond = ifelse(hp$Pred.2016>.75,1,0)
ggplot(hp) +
  geom_sf(aes(fill=Pred.2016,color=Pred.2016)) + 
  scale_fill_viridis_c() +
  scale_color_viridis_c() +
  # geom_sf(data=rL,size=1,colour='black',fill=NA)+
  #stat_contour(data=hp[cond,],aes(color=after_stat(level)),bin=0.75)+
  theme( axis.ticks.x = element_blank(),
         axis.text.x = element_blank(),
         axis.title.x = element_blank(),
         axis.ticks.y = element_blank(),
         axis.text.y = element_blank(),
         axis.title.y = element_blank()
  ) +
  coord_sf()

hp$PredCond = ifelse(hp$Pred.2017>.05,1,0)
ggplot(hp) +
  geom_sf(aes(fill=PredCond,color=PredCond)) + 
geom_sf(data=rL,size=1,colour='black',fill=NA)+
 
  theme( axis.ticks.x = element_blank(),
         axis.text.x = element_blank(),
         axis.title.x = element_blank(),
         axis.ticks.y = element_blank(),
         axis.text.y = element_blank(),
         axis.title.y = element_blank()
  ) +
  coord_sf()

#convert to a raster then draw polys around locations of high prob berried
require(stars)
hps = st_rasterize(hp %>% dplyr::select(PredCond,geometry))
  hpST = st_as_stars(hp)
vv = st_contour(hps,contour_lines=F,breaks=.99)
vvh = st_as_sf(vv)

hpp = hp
st_geometry(hpp) <-NULL
hp$medPre = apply(hpp[,c('Pred.2016','Pred.2017','Pred.2018','Pred.2019','Pred.2020','Pred.2021')],1,mean)

hpup = subset(hp,select=c(id,LFA,geometry,Pred.2016,Pred.2017,Pred.2018,Pred.2019,Pred.2020,Pred.2021)) %>% distinct()

st_geometry(hpup) <-NULL

hpup$medPred = apply(hpup[,c('Pred.2016','Pred.2017','Pred.2018','Pred.2019','Pred.2020','Pred.2021')],1,mean)


#temperature 
  xx = aggregate(cbind(X.2016,X.2035,X.2055,X.2099)~id+LFA,data=hp,FUN=function(x){z = quantile(x,c(0.025,0.975)); z[2] -z[1]})
  names(xx)[3:6] = c('Temp2016_range','Temp2035_range','Temp2055_range','Temp2099_range')

  xxm = aggregate(cbind(X.2016,X.2035,X.2055,X.2099)~id+LFA,data=hp,FUN=function(x){mean(x)})
  names(xxm)[3:6] = c('Temp2016_med','Temp2035_med','Temp2055_med','Temp2099_med')

  xxd = aggregate(Depth_m~id,data=hp,FUN=function(x){mean(x)})

  xxs = st_as_sf(merge(merge(merge(xxm,xxd),xx),hpu))
  xxs = merge(xxs,hpup[,c('id','Pred.2016')])





xx12 = xx11
st_geometry(xx12) = NULL
xx12$LFA.x=NULL
xx12$LFA = xx12$LFA.y
xx13 = merge(xx8[,c('Location','Depth')],xx12)


require(mgcv)
  gg = gam(BerriedProb~s(Depth)+s(Temp_range)+s(Temp_median)+ti(Temp_range,Temp_median,by=Depth)+as.factor(LFA),data=xx13,family=betar(link='logit'))

#Depth Range

DR = aggregate(Depth~LFA, data=subset(xx8,BerriedProb>0.75 & Depth <500),FUN=function(x) quantile(x,probs=c(0.25,0.75)))


#Temp Range
TRR = aggregate(Temp_range~LFA, data=subset(xx13,BerriedProb>0.75 & Depth <500),FUN=function(x) quantile(x,probs=c(0.25,0.75)))

#Temp Med
TM = aggregate(Temp_median~LFA, data=subset(xx13,BerriedProb>0.75 & Depth <500),FUN=function(x) quantile(x,probs=c(0.25,0.75)))

saveRDS(list(DR,TRR,TM),file='HADberriedFemaleEnvCondition.rds')



testR = function(x,depth=DR,tempr=TRR,tempm=TM,vars=c('t','tr','d')){
  junk=list()
       xl = unique(x$LFA)
      for(i in 1:length(xl)){
             xii = xi = subset(x,LFA==xl[i])
      st_geometry(xi)<-NULL
          d = xi[,1]
          tr = xi[,2]
          tm = xi[,3]
          di = subset(depth,LFA==xl[i])$Depth
          tri = subset(tempr,LFA==xl[i])$Temp_range
          tmi = subset(tempm,LFA==xl[i])$Temp_median
      v = which(dplyr::between(d, di[1],di[2]))
      j = which(dplyr::between(tr, tri[1],tri[2]))
      k = which(dplyr::between(tm, tmi[1],tmi[2]))
     if(length(vars)==3) l = intersect(intersect(v,j),k)
     if(all(length(vars)==2 & vars[1] %in%c('t','tr') & vars[2] %in%c('t','tr')))  l = intersect(j,k)
     if(all(length(vars)==2 & vars[1] %in%c('t','d') & vars[2] %in%c('t','d')))  l = intersect(j,v)
     
      xii$TF = 0
      xii$TF[l] = 1
      junk[[i]] = xii
    }
    x = dplyr::bind_rows(junk)
      return(x)
}

fbase = testR(xxs[,c('Depth_m','Temp2016_range','Temp2016_med','id','Pred.2016','LFA')],vars=c('t','d'))

f35 = testR(xxs[,c('Depth_m','Temp2035_range','Temp2035_med','id','Pred.2016','LFA')])

f55 = testR(xxs[,c('Depth_m','Temp2055_range','Temp2055_med','id','Pred.2016',"LFA")])


f99 = testR(xxs[,c('Depth_m','Temp2099_range','Temp2099_med','id','Pred.2016',"LFA")])

plbase = ggplot(fbase) +
  geom_sf(aes(fill=TF,color=TF)) + 
  scale_fill_viridis_c() +
  scale_color_viridis_c() +
  # geom_sf(data=rL,size=1,colour='black',fill=NA)+
  theme( axis.ticks.x = element_blank(),
         axis.text.x = element_blank(),
         axis.title.x = element_blank(),
         axis.ticks.y = element_blank(),
         axis.text.y = element_blank(),
         axis.title.y = element_blank()
  ) +
  coord_sf()



pl35 = ggplot(f35) +
  geom_sf(aes(fill=TF,color=TF)) + 
  scale_fill_viridis_c() +
  scale_color_viridis_c() +
  # geom_sf(data=rL,size=1,colour='black',fill=NA)+
  theme( axis.ticks.x = element_blank(),
         axis.text.x = element_blank(),
         axis.title.x = element_blank(),
         axis.ticks.y = element_blank(),
         axis.text.y = element_blank(),
         axis.title.y = element_blank()
  ) +
  coord_sf()


pl55 = ggplot(f55) +
  geom_sf(aes(fill=TF,color=TF)) + 
  scale_fill_viridis_c() +
  scale_color_viridis_c() +
  # geom_sf(data=rL,size=1,colour='black',fill=NA)+
  theme( axis.ticks.x = element_blank(),
         axis.text.x = element_blank(),
         axis.title.x = element_blank(),
         axis.ticks.y = element_blank(),
         axis.text.y = element_blank(),
         axis.title.y = element_blank()
  ) +
  coord_sf()

pl99 = ggplot(f99) +
  geom_sf(aes(fill=TF,color=TF)) + 
  scale_fill_viridis_c() +
  scale_color_viridis_c() +
  # geom_sf(data=rL,size=1,colour='black',fill=NA)+
  theme( axis.ticks.x = element_blank(),
         axis.text.x = element_blank(),
         axis.title.x = element_blank(),
         axis.ticks.y = element_blank(),
         axis.text.y = element_blank(),
         axis.title.y = element_blank()
  ) +
  coord_sf()

cowplot::plot_grid(plbase,pl35,pl55,pl99,ncol=2,labels=c(2016,2035,2055,2099))

