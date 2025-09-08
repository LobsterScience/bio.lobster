
t = compileAbundPresAbs_vessel_corr(size=F)
ts = st_as_sf(t,coords=c('LONGITUDE','LATITUDE'),crs=4326)
b = ggLobsterMap('38')


b+geom_sf(data=ts,size=.1)+
  coord_sf(ylim=c(43.8,45),		xlim=c(-67.5,-66.2) ,
           expand = FALSE)

cx = st_read('C:/Users/cooka/Downloads/SWNB_Network_Sites/SWNB_20241223.shp')

b+geom_sf(data=cx)

r =read.csv('C:/Users/cooka/Downloads/Station Proposal LFA 38_2025.csv')
rs = st_as_sf(r,coords=c('Longitude','Latitude'),crs=4326)

b+geom_sf(data=cx) +geom_sf(data=subset(rs,STATION %in% c(391,392)))


b+geom_sf(data=cx) +geom_sf(data=subset(ts,SOURCE %in% c('ILTS','MNR','DFO_RV','NEFSC_RV','Scallop Survey')),size=.1)+
  coord_sf(ylim=c(43.8,45),		xlim=c(-67.5,-66.2) ,
           expand = FALSE)


