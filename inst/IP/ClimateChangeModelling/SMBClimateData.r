#ipdw for SMB

require(ipdw)
require(spatstat)
require(sf)
require(ggplot2)
require(bio.lobster)
sf_use_s2(FALSE) #needed for cropping
layerDir=file.path(project.datadirectory("bio.lobster"), "data","maps")
l = readRDS(file.path( layerDir,"LFAPolysSF.rds"))
st_crs(l) <- 4326
l <- suppressWarnings(suppressMessages(
  st_crop(l,
          c(xmin = -66.6, ymin = 44, xmax = -65.8, ymax = 44.6))))

l = st_transform(l,crs=32620)
l = subset(l,LFA %in% c('34'))


pts = read.csv('C:/Users/Cooka/Downloads/SMB_dataforaquasite_june2023.csv')
pts = st_as_sf(pts,coords = c("lon","lat"),crs=4326)
pts = st_transform(pts,crs=32620)

pts = st_filter(pts,l)
ggplot(pts)+geom_sf()+geom_sf(data=l,fill=NA)

#cr = costrasterGen(xymat=pts,pols=l,extent='pols',projstr=projection(l))

library(tidyr)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(viridis)

p = pts %>% select(X2020BottomTemp,X2050BottomTemp,X2020SurfTemp,X2050SurfTemp) %>%
    pivot_longer(., cols = c(X2020BottomTemp,X2050BottomTemp,X2020SurfTemp,X2050SurfTemp), names_to = "Var", values_to = "Val")
p$Var = substring(p$Var,2, nchar(p$Var))

ggplot() +
  geom_sf(data=p,aes(fill=Val),shape=21,size=4)+
  scale_fill_viridis()+
  geom_sf(data=l,fill=NA)+
  facet_wrap(~factor(Var,levels = c('2020BottomTemp','2050BottomTemp','2020SurfTemp','2050SurfTemp')))+
  labs(fill='Temperature') +
  theme_minimal()

pts$BottomTempDiff = pts$X2050BottomTemp - pts$X2020BottomTemp
pts$SurfTempDiff = pts$X2050SurfTemp - pts$X2020SurfTemp

p = pts %>% select(BottomTempDiff, SurfTempDiff) %>%
  pivot_longer(., cols = c(BottomTempDiff, SurfTempDiff), names_to = "Var", values_to = "Val")


ggplot() +
  geom_sf(data=p,aes(fill=Val),shape=21,size=4.5)+
  scale_fill_viridis()+
  geom_sf(data=l,fill=NA)+
  facet_wrap(~factor(Var,levels = c('BottomTempDiff','SurfTempDiff')))+
  labs(fill=expression(Delta*Temperature)) +
    theme_minimal()
##Arag
p = pts %>% select(X2020BottomArag,X2050BottomArag,X2020SurfArag,X2050SurfArag) %>%
  pivot_longer(., cols = c(X2020BottomArag,X2050BottomArag,X2020SurfArag,X2050SurfArag), names_to = "Var", values_to = "Val")
p$Var = substring(p$Var,2, nchar(p$Var))

ggplot() +
  geom_sf(data=p,aes(fill=Val),shape=21,size=4)+
  scale_fill_viridis()+
  geom_sf(data=l,fill=NA)+
  facet_wrap(~factor(Var,levels = c('2020BottomArag','2050BottomArag','2020SurfArag','2050SurfArag')))+
  labs(fill='Aragonite') +
  theme_minimal()

pts$BottomAragDiff = pts$X2050BottomArag - pts$X2020BottomArag
pts$SurfAragDiff = pts$X2050SurfArag - pts$X2020SurfArag

p = pts %>% select(BottomAragDiff, SurfAragDiff) %>%
  pivot_longer(., cols = c(BottomAragDiff, SurfAragDiff), names_to = "Var", values_to = "Val")


ggplot() +
  geom_sf(data=p,aes(fill=Val),shape=21,size=4.5)+
  scale_fill_viridis()+
  geom_sf(data=l,fill=NA)+
  facet_wrap(~factor(Var,levels = c('BottomAragDiff','SurfAragDiff')))+
  labs(fill=expression(Delta*Aragonite)) +
  theme_minimal()


####calcite

p = pts %>% select(X2020BottomCalc,X2050BottomCalc,X2020SurfCalc,X2050SurfCalc) %>%
  pivot_longer(., cols = c(X2020BottomCalc,X2050BottomCalc,X2020SurfCalc,X2050SurfCalc), names_to = "Var", values_to = "Val")
p$Var = substring(p$Var,2, nchar(p$Var))

ggplot() +
  geom_sf(data=p,aes(fill=Val),shape=21,size=4)+
  scale_fill_viridis()+
  geom_sf(data=l,fill=NA)+
  facet_wrap(~factor(Var,levels = c('2020BottomCalc','2050BottomCalc','2020SurfCalc','2050SurfCalc')))+
  labs(fill='Calcite') +
  theme_minimal()

pts$BottomCalcDiff = pts$X2050BottomCalc - pts$X2020BottomCalc
pts$SurfCalcDiff = pts$X2050SurfCalc - pts$X2020SurfCalc

p = pts %>% select(BottomCalcDiff, SurfCalcDiff) %>%
  pivot_longer(., cols = c(BottomCalcDiff, SurfCalcDiff), names_to = "Var", values_to = "Val")


ggplot() +
  geom_sf(data=p,aes(fill=Val),shape=21,size=4.5)+
  scale_fill_viridis()+
  geom_sf(data=l,fill=NA)+
  facet_wrap(~factor(Var,levels = c('BottomCalcDiff','SurfCalcDiff')))+
  labs(fill=expression(Delta*Calcite)) +
  theme_minimal()
