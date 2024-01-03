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
had = readRDS(file='MPIDailyTemps.rds')

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
hp$t2016 = round(hp$X.2016,1) 
hp$t2035 = round(hp$X.2035,1) 
hp$t2055 = round(hp$X.2055,1) 
hp$t2099 = round(hp$X.2099,1) 

#temp relationship extracted from the model
  ff = plot_smooth(fitpa,select=2,return_data=T)
  ff$Pred = fitpa$family$linkinv(ff$est)
  ff = ff[,c('BT','Pred')]
  ff$BT = as.integer(round(ff$BT,1)*10)/10
  ts = data.frame(BT=seq(min(ff$BT),max(ff$BT),by=.1))
  ts$BT = as.integer(round(ts$BT,1)*10)/10
  ff1 = merge(ff,ts,all.y=T)
  require(zoo)
  ff1$Pred = na.approx(ff1$Pred)
  ff1$PredS = ff1$Pred/max(ff1$Pred)
  #rescale -1 to 1
  ff1$ResP = 2*(ff1$Pred-min(ff1$Pred)) / (max(ff1$Pred)-min(ff1$Pred))-1

ff1$diffs =  c(0,diff(ff1$Pred))

diffMat = matrix(0,nrow=length(ff1$BT),ncol=length(ff1$BT))
for(i in 1:nrow(ff1)){
  for(j in 1:nrow(ff1)){
      b = ff1$Pred[i:j]
      cc = c(0,diff(b))
      diffMat[i,j] = sum(cc)
  }

}
rownames(diffMat) = colnames(diffMat) = ff1$BT
saveRDS(list(hp,ff,diffMat),'berriedfemaleMPIStepWiseNov242023.rds')

#using this functional relationship and the current spatial model to predict where might they go which all live in hp

hp1 = merge(hp,ff1[,c('BT','Pred')],by.x='t2016',by.y='BT',all=T)
hp1 = rename.df(hp1,'Pred','Pred2016_T')
hp2 = merge(hp1,ff1[,c('BT','Pred')],by.x='t2035',by.y='BT')
hp2 = rename.df(hp2,'Pred','Pred2035_T')
hp3 = merge(hp2,ff1[,c('BT','Pred')],by.x='t2055',by.y='BT')
hp3 = rename.df(hp3,'Pred','Pred2055_T')
hp4 = merge(hp3,ff1[,c('BT','Pred')],by.x='t2099',by.y='BT')
hp4 = rename.df(hp4,'Pred','Pred2099_T')

hp$diff1635 = NA
for(i in 1:nrow(hp)){
  hpp = hp[i,]
  st_geometry(hpp) <- NULL
  t1=hpp[1,'t2016']
  t2=hpp[1,'t2035']
  if(t2>max(as.numeric(rownames(diffMat)))) t2 = max(as.numeric(rownames(diffMat))) 
  hp$diff1635[i] = diffMat[which(rownames(diffMat)==t1),which(colnames(diffMat)==t2)]
}

hp$diff1655 = NA
for(i in 1:nrow(hp)){
  hpp = hp[i,]
  st_geometry(hpp) <- NULL
  t1=hpp[1,'t2016']
  t2=hpp[1,'t2055']
  if(t2>max(as.numeric(rownames(diffMat)))) t2 = max(as.numeric(rownames(diffMat))) 
  
  hp$diff1655[i] = diffMat[which(rownames(diffMat)==t1),which(colnames(diffMat)==t2)]
}


hp$diff1699 = NA
for(i in 1:nrow(hp)){
  hpp = hp[i,]
  st_geometry(hpp) <- NULL
  t1=hpp[1,'t2016']
  t2=hpp[1,'t2099']
  if(t2>max(as.numeric(rownames(diffMat)))) t2 = max(as.numeric(rownames(diffMat))) 
  
  hp$diff1699[i] = diffMat[which(rownames(diffMat)==t1),which(colnames(diffMat)==t2)]
}


hp$diff3555 = NA
for(i in 1:nrow(hp)){
  hpp = hp[i,]
  st_geometry(hpp) <- NULL
  t1=hpp[1,'t2035']
  t2=hpp[1,'t2055']
  if(t1>max(as.numeric(rownames(diffMat)))) t1 = max(as.numeric(rownames(diffMat))) 
 if(t2>max(as.numeric(rownames(diffMat)))) t2 = max(as.numeric(rownames(diffMat))) 

  hp$diff3555[i] = diffMat[which(rownames(diffMat)==t1),which(colnames(diffMat)==t2)]
}


hp$diff3599 = NA
for(i in 1:nrow(hp)){
  hpp = hp[i,]
  st_geometry(hpp) <- NULL
  t1=hpp[1,'t2035']
  t2=hpp[1,'t2099']
  if(t1>max(as.numeric(rownames(diffMat)))) t1 = max(as.numeric(rownames(diffMat))) 

  if(t2>max(as.numeric(rownames(diffMat)))) t2 = max(as.numeric(rownames(diffMat))) 

  hp$diff3599[i] = diffMat[which(rownames(diffMat)==t1),which(colnames(diffMat)==t2)]
}


hp$diff5599 = NA
for(i in 1:nrow(hp)){
  hpp = hp[i,]
  st_geometry(hpp) <- NULL
  t1=hpp[1,'t2055']
  t2=hpp[1,'t2099']
   if(t1>max(as.numeric(rownames(diffMat)))) t1 = max(as.numeric(rownames(diffMat))) 

    if(t2>max(as.numeric(rownames(diffMat)))) t2 = max(as.numeric(rownames(diffMat))) 

  hp$diff5599[i] = diffMat[which(rownames(diffMat)==t1),which(colnames(diffMat)==t2)]
}

saveRDS(list(hp,ff,diffMat),'berriedfemaleMPIStepWiseNov242023.rds')
v = readRDS('berriedfemaleMPIStepWiseNov242023.rds')
hp = v[[1]]
ff = v[[2]]
diffMat = v[[3]]

###everything
 ggplot(hp) +
  geom_sf(aes(fill=diff1655,color=diff1655)) + 
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

###subset to only habitats that had berried females in the predicted model
 ggplot(subset(hp,Pred.2016>.009) )+
   geom_sf(aes(fill=diff1635,color=diff1635), size=2.5) + 
   scale_colour_distiller(palette='RdYlGn') +
   scale_fill_distiller(palette='RdYlGn') + 
   # geom_sf(data=rL,size=1,colour='black',fill=NA)+
   theme( axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank()
   ) +
   coord_sf()
 
 ggplot(subset(hp,Pred.2016>.009) )+
   geom_sf(aes(fill=diff3555,color=diff3555),size=2.5) + 
   scale_colour_distiller(palette='RdYlGn') +
   scale_fill_distiller(palette='RdYlGn') + 
   # geom_sf(data=rL,size=1,colour='black',fill=NA)+
   theme( axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank()
   ) +
   coord_sf()
 
 
 ggplot(subset(hp,Pred.2016>.009) )+
   geom_sf(aes(fill=diff5599,color=diff5599), size=2.5) + 
   scale_colour_distiller(palette='RdYlGn') +
   scale_fill_distiller(palette='RdYlGn') + 
   # geom_sf(data=rL,size=1,colour='black',fill=NA)+
   theme( axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank()
   ) +
   coord_sf()
 