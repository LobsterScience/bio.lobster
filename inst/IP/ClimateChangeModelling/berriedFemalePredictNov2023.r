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
  hp$diff1635[i] = diffMat[which(rownames(diffMat)==t1),which(colnames(diffMat)==t2)]
}

hp$diff1655 = NA
for(i in 1:nrow(hp)){
  hpp = hp[i,]
  st_geometry(hpp) <- NULL
  t1=hpp[1,'t2016']
  t2=hpp[1,'t2055']
  hp$diff1655[i] = diffMat[which(rownames(diffMat)==t1),which(colnames(diffMat)==t2)]
}


hp$diff1699 = NA
for(i in 1:nrow(hp)){
  hpp = hp[i,]
  st_geometry(hpp) <- NULL
  t1=hpp[1,'t2016']
  t2=hpp[1,'t2099']
  hp$diff1699[i] = diffMat[which(rownames(diffMat)==t1),which(colnames(diffMat)==t2)]
}


hp$diff3555 = NA
for(i in 1:nrow(hp)){
  hpp = hp[i,]
  st_geometry(hpp) <- NULL
  t1=hpp[1,'t2035']
  t2=hpp[1,'t2055']
  hp$diff3555[i] = diffMat[which(rownames(diffMat)==t1),which(colnames(diffMat)==t2)]
}


hp$diff3599 = NA
for(i in 1:nrow(hp)){
  hpp = hp[i,]
  st_geometry(hpp) <- NULL
  t1=hpp[1,'t2035']
  t2=hpp[1,'t2099']
  hp$diff3599[i] = diffMat[which(rownames(diffMat)==t1),which(colnames(diffMat)==t2)]
}


hp$diff5599 = NA
for(i in 1:nrow(hp)){
  hpp = hp[i,]
  st_geometry(hpp) <- NULL
  t1=hpp[1,'t2055']
  t2=hpp[1,'t2099']
  hp$diff5599[i] = diffMat[which(rownames(diffMat)==t1),which(colnames(diffMat)==t2)]
}

saveRDS(list(hp,ff,diffMat),'berriedfemaleStepWiseNov242023.rds')

g = readRDS('berriedfemaleStepWiseNov242023.rds')
hp=g[[1]]
ff=g[[2]]
diffMat = g[[3]]
###2016Pred
ggplot(subset(hp,Pred.2016>0.009)) +
  geom_sf(aes(fill=Pred.2016,color=Pred.2016),size=2.5) + 
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



 g1 = ggplot(subset(hp,Pred.2016>0.009)) +
  geom_sf(aes(fill=diff1635,color=diff1635),size=2.5) + 
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

 
 g1 = ggplot(subset(hp,Pred.2016>0.009)) +
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
 
 
###subset to only habitats that had berried females in the predicted model
 ggplot(subset(hp,Pred.2016>.009) )+
  geom_sf(aes(fill=diff5599,color=diff5599),size=2.5) + 
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

##############################################################
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

