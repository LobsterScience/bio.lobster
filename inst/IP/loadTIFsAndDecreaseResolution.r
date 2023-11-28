#read in tiffs
require(stars)
require(bio.lobster)
require(sf)
require(raster)
require(dplyr)
require(starsExtra)

slo = read_stars(file.path(project.datadirectory('bio.lobster'),'data','OFI_lobster_covariates','slope_11x11nw.tif'),proxy=F)

slost = st_as_sf(slo)

v = dim(slost)[1]

out=list()
s = round(seq(0,v,length.out=100))

for(i in 1:(length(s)-1)){
	
	sl = slost[(s[i]+1):(s[i+1]),]
	sl = st_centroid(sl)
	out[[i]] = sl
	print(i)
}

out1 = bind_rows(out)
out1$X1000 = st_coordinates(out1)[,1]/1000
out1$Y1000 = st_coordinates(out1)[,2]/1000

saveRDS(out1,file.path(project.datadirectory('bio.lobster'),'data','OFI_lobster_covariates','slope_pointData.rds'))

#bathy
slo = read_stars(file.path(project.datadirectory('bio.lobster'),'data','OFI_lobster_covariates','ofi_DEM_UTMZ20.tif'),proxy=F)

#decrease resolution to 500m
    r500 = st_as_stars(st_bbox(slo), dx = 500)
     slo500= st_warp(slo, r500,method='cubicspline',use_gdal=T,no_data_value=99)
	slost = st_as_sf(slo500)

 

v = dim(slost)[1]

out=list()
s = round(seq(0,v,length.out=100))

for(i in 1:(length(s)-1)){
	
	sl = slost[(s[i]+1):(s[i+1]),]
	sl = st_centroid(sl)
	out[[i]] = sl
	print(i)
}

out1 = bind_rows(out)
out1$X1000 = st_coordinates(out1)[,1]/1000
out1$Y1000 = st_coordinates(out1)[,2]/1000
names(out1)[1] = 'depth'

ggplot(subset(out1,depth>-500)) +
  geom_sf(aes(fill=depth,color=depth)) + 
  scale_fill_viridis_c() +
  scale_color_viridis_c() +
  theme( axis.ticks.x = element_blank(),
         axis.text.x = element_blank(),
         axis.title.x = element_blank(),
         axis.ticks.y = element_blank(),
         axis.text.y = element_blank(),
         axis.title.y = element_blank()
  ) +
  coord_sf()


saveRDS(out1,file.path(project.datadirectory('bio.lobster'),'data','OFI_lobster_covariates','bathy_pointData500m.rds'))

out2=out1

#slope from new domain

sll = slope(slo500)
sllo = st_as_sf(sll)

 

v = dim(sllo)[1]

out=list()
s = round(seq(0,v,length.out=100))

for(i in 1:(length(s)-1)){
	
	sl = sllo[(s[i]+1):(s[i+1]),]
	sl = st_centroid(sl)
	out[[i]] = sl
	print(i)
}

out1 = bind_rows(out)
out1$slope = as.numeric(out1$slope)
saveRDS(out1,file.path(project.datadirectory('bio.lobster'),'data','OFI_lobster_covariates','slope_pointData500m.rds'))

require(ggplot2)
ggplot(out1) +
  geom_sf(aes(fill=slope,color=slope)) + 
  scale_fill_viridis_c() +
  scale_color_viridis_c() +
  theme( axis.ticks.x = element_blank(),
         axis.text.x = element_blank(),
         axis.title.x = element_blank(),
         axis.ticks.y = element_blank(),
         axis.text.y = element_blank(),
         axis.title.y = element_blank()
  ) +
  coord_sf()

out3=out1

###ruggedness


slo = read_stars(file.path(project.datadirectory('bio.lobster'),'data','OFI_lobster_covariates','rie_11x11nw.tif'),proxy=F)
     slo500= st_warp(slo, r500,method='cubicspline',use_gdal=T,no_data_value=99)
	slost = st_as_sf(slo500)

 

v = dim(slost)[1]

out=list()
s = round(seq(0,v,length.out=100))

for(i in 1:(length(s)-1)){
	
	sl = slost[(s[i]+1):(s[i+1]),]
	sl = st_centroid(sl)
	out[[i]] = sl
	print(i)
}

out1 = bind_rows(out)
names(out1)[1] = 'rie'

ggplot(subset(out1)) +
  geom_sf(aes(fill=(rie),color=(rie))) + 
  scale_fill_viridis_c() +
  scale_color_viridis_c() +
  theme( axis.ticks.x = element_blank(),
         axis.text.x = element_blank(),
         axis.title.x = element_blank(),
         axis.ticks.y = element_blank(),
         axis.text.y = element_blank(),
         axis.title.y = element_blank()
  ) +
  coord_sf()


saveRDS(out1,file.path(project.datadirectory('bio.lobster'),'data','OFI_lobster_covariates','rie_pointData500m.rds'))

out13 = st_join(out3,out1,join=st_nearest_feature,left=T)
out132 = st_join(out13,out2,join=st_nearest_feature,left=T)

out123 = subset(out132,select=c(slope,depth,rie))

saveRDS(out123,file.path(project.datadirectory('bio.lobster'),'data','OFI_lobster_covariates','ofi_pointData500m.rds'))
